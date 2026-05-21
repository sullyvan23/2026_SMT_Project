
OF_pos_fly_ball <- player_positions %>% filter(player_id %in% c(7:9)) %>% left_join(fly_balls[,c(1:2,5)], by = c("game_string", "play_per_game"))
OF_pos_fly_ball <- OF_pos_fly_ball %>% filter(!is.na(timestamp_hit)) %>% mutate(time_diff = abs(timestamp - timestamp_hit))
OF_pos_fly_ball <- OF_pos_fly_ball %>% group_by(game_string, play_per_game) %>% filter(time_diff == min(time_diff))

ball_pos_fly_ball <- ball_positions %>% left_join(fly_balls[,c(1:2,5:6)], by = c("game_string", "play_per_game"))
ball_pos_fly_ball <- ball_pos_fly_ball %>% filter(!is.na(timestamp_hit))
ball_pos_fly_ball <- ball_pos_fly_ball %>% filter(timestamp >= timestamp_hit   &   timestamp <= timestamp_down)

####################################################################################################################################

ex_bpfb <- ball_pos_fly_ball %>% filter(game_string == "y1_d143_ADQ_ANI"   &   play_per_game == 17)
ex_bpfb <- ex_bpfb %>% mutate(timestamp = timestamp - first(timestamp))
ex_bpfb <- ex_bpfb %>% mutate(timestamp_sqrd = timestamp^2)
plot(ex_bpfb$timestamp, ex_bpfb$ball_position_z)

check_model <- gam(ball_position_y ~ timestamp + timestamp_sqrd, data = ex_bpfb)
plot(ex_bpfb$timestamp, ex_bpfb$ball_position_y, col = "black")
points(ex_bpfb$timestamp, predict(check_model), col = "red")
RMSE(ex_bpfb$ball_position_z, predict(check_model))


ball_x_model <- gam(ball_position_x ~ timestamp + timestamp_sqrd, data = ex_bpfb)
ball_y_model <- gam(ball_position_y ~ timestamp + timestamp_sqrd, data = ex_bpfb)
ball_z_model <- gam(ball_position_z ~ timestamp + timestamp_sqrd, data = ex_bpfb)
test_function <- function(time) {
  time2 <- time^2
  times_dataset <- data.frame(timestamp = time, timestamp_sqrd = time2)

  ball_x <- predict(ball_x_model, newdata = times_dataset)
  ball_y <- predict(ball_y_model, newdata = times_dataset)
  ball_z <- predict(ball_z_model, newdata = times_dataset)
  dist_from_home <- sqrt(ball_x^2 + ball_y^2)
  ground_dataset <- data.frame(home_dist = dist_from_home, ball_position_x = ball_x, ball_position_y = ball_y)

  ground <- predict(ground_ANI_model, newdata = ground_dataset)

  return(ball_z - ground)
}

(uniroot(test_function, c(1000,9000))$root)/1000
### 2.265237

####################################################################################################################################


ball_pos_fly_ball <- ball_pos_fly_ball %>% group_by(game_string, play_per_game) %>% mutate(timestamp = timestamp - first(timestamp),
                                                                                           timestamp_sqrd = timestamp^2) %>% 
                                                                                    relocate(timestamp_sqrd, .after = timestamp)


check <- ball_pos_fly_ball %>% group_by(game_string, play_per_game) %>% summarise(time = max(timestamp))
check <- check %>% filter(time >= 2)

nrow(check)
### 4424
check <- check %>% mutate(play_key = paste0(game_string, play_per_game))

ball_pos_fly_ball_1.1 <- ball_pos_fly_ball %>% mutate(play_key = paste0(game_string, play_per_game), home_dist = sqrt(ball_position_x^2 + ball_position_y^2)) %>%
                                               filter(play_key %in% check$play_key)

####################################################################################################################################

ball_pos_fly_ball_1.2 <- ball_pos_fly_ball_1.1 %>% mutate(x_speed = 0.68181818 * (ball_position_x - lag(ball_position_x)) / ((timestamp - lag(timestamp))/1000),
                                                          y_speed = 0.68181818 * (ball_position_y - lag(ball_position_y)) / ((timestamp - lag(timestamp))/1000),
                                                          z_speed = 0.68181818 * (ball_position_z - lag(ball_position_z)) / ((timestamp - lag(timestamp))/1000),
                                                          xy_speed = 0.68181818 * (home_dist - lag(home_dist)) / ((timestamp - lag(timestamp))/1000),
                                                          speed = sqrt(x_speed^2 + y_speed^2 + z_speed^2),
                                                          xy_accel = (xy_speed - lag(xy_speed)) / ((timestamp - lag(timestamp))/1000))
ball_pos_fly_ball_1.2 <- ball_pos_fly_ball_1.2 %>% mutate(position_angle = atan(ball_position_x/ball_position_y),
                                                          speed_angle = atan(x_speed/y_speed),
                                                          angle_diff = position_angle - speed_angle)

ball_pos_fly_ball_1.3 <- ball_pos_fly_ball_1.2 %>% slice(-c(1:3))

ball_pos_fly_ball_1.4 <- ball_pos_fly_ball_1.3 %>% summarise(y_accel_sd = sd(y_accel),  max_angle_diff = max(angle_diff), max_abs_x = max(abs(ball_position_x)))

plot(ball_pos_fly_ball_1.4$y_accel_sd)

ball_pos_fly_ball_1.1 <- ball_pos_fly_ball_1.1 %>% mutate(play_key = paste0(game_string, play_per_game))

ggplot(ball_pos_fly_ball_1.1 %>% filter(play_key == "y1_d061_VKA_PHD28"), aes(x = ball_position_x, y = ball_position_y, color = ball_position_z)) + 
       geom_point() + scale_color_gradient2(high = "green", low = "red", mid = "white") + xlim(-300, 300) + ylim(-10, 450)

ggplot(ball_pos_fly_ball_1.1 %>% filter(play_key == "y1_d061_VKA_PHD28"), aes(x = timestamp, y = ball_position_y)) + geom_point()

####################################################################################################################################

group <- 0
ball_pos_fly_ball_2 <- ball_pos_fly_ball_1.1 %>% group_by(game_string, play_per_game) %>%
                       summarise({group <<- group + 1
                                  message(group/4424)
                                  
                         ball_x_model <- gam(ball_position_x ~ timestamp + timestamp_sqrd, data = pick(everything()) )
                         ball_y_model <- gam(ball_position_y ~ timestamp + timestamp_sqrd, data = pick(everything()) )
                         ball_z_model <- gam(ball_position_z ~ timestamp + timestamp_sqrd, data = pick(everything()) )

                         x_rmse <- RMSE(ball_position_x, predict(ball_x_model), na.rm = TRUE)
                         y_rmse <- RMSE(ball_position_y, predict(ball_y_model), na.rm = TRUE)
                         z_rmse <- RMSE(ball_position_z, predict(ball_z_model), na.rm = TRUE)

                         ground_dist_function <- function(time) {
                            time2 <- time^2
                            times_dataset <- data.frame(timestamp = time, timestamp_sqrd = time2)

                            ball_x <- predict(ball_x_model, newdata = times_dataset)
                            ball_y <- predict(ball_y_model, newdata = times_dataset)
                            ball_z <- predict(ball_z_model, newdata = times_dataset)
                            dist_from_home <- sqrt(ball_x^2 + ball_y^2)
                            ground_dataset <- data.frame(home_dist = dist_from_home, ball_position_x = ball_x, ball_position_y = ball_y)

                            ground <- switch( first(home_team),
                              "ANI" = predict(ground_ANI_model, newdata = ground_dataset),
                              "ARN" = predict(ground_ARN_model, newdata = ground_dataset),
                              "PHD" = predict(ground_PHD_model, newdata = ground_dataset),
                              "VAS" = predict(ground_VAS_model, newdata = ground_dataset),
                              NA
                            )

                            return(ball_z - ground)
                          }

                         ball_hits_ground_time <- tryCatch({uniroot(ground_dist_function, c(1000,9000))$root},
                                                            error = function(e) {-1})

                         ground_times <- data.frame(
                            timestamp = ball_hits_ground_time,
                            timestamp_sqrd = ball_hits_ground_time^2
                          )

                          tibble(
                            time_to_ground = ball_hits_ground_time,
                            ground_x = predict(ball_x_model, newdata = ground_times),
                            ground_y = predict(ball_y_model, newdata = ground_times),
                            ground_z = predict(ball_z_model, newdata = ground_times),
                            rmse_x = x_rmse,
                            rmse_y = y_rmse,
                            rmse_z = z_rmse,
                          )
                         }
                       )


ggplot(ball_pos_fly_ball_1.1 %>% filter(play_key == "y1_d145_ADQ_ANI96"), aes(x = ball_position_x, y = ball_position_y, color = timestamp)) + 
       geom_point() + scale_color_gradient2(high = "green", low = "red", mid = "white")

ggplot(ball_pos_fly_ball_1.1 %>% filter(play_key == "y1_d117_BXH_VAS48"), aes(x = timestamp, y = ball_position_z)) + geom_point()

### only eliminating 2, watched animations input errors / nat applicable plays so not errors on me
ball_pos_fly_ball_3 <- ball_pos_fly_ball_2 %>% filter(time_to_ground > 0   &   ground_y < 1000)

####################################################################################################################################

ball_pos_fly_ball_tl <- ball_pos_fly_ball_1.2 %>% mutate(time_left = last(timestamp) - timestamp) %>% 
                                                  filter(time_left >= 50   &   time_left <= 550)
check <- ball_pos_fly_ball_tl %>% summarise(min_xy_speed = min(xy_speed))
hist(check$min_xy_speed, breaks = 50)
ggplot(ball_pos_fly_ball_1.1 %>% filter(play_key == "y1_d117_BXH_VAS48"), aes(x = home_dist, y = ball_position_z)) + geom_point()


ball_pos_fly_ball_tl <- ball_pos_fly_ball_tl %>% filter(sum(xy_speed < -10   |   xy_speed > 100) == 0)

group <- 0
ball_pos_fly_ball_2.1 <- ball_pos_fly_ball_tl %>% group_by(game_string, play_per_game) %>%
                       summarise({group <<- group + 1
                                  message(group/4446)
                                  
                         ball_x_model <- gam(ball_position_x ~ timestamp + timestamp_sqrd, data = pick(everything()) )
                         ball_y_model <- gam(ball_position_y ~ timestamp + timestamp_sqrd, data = pick(everything()) )
                         ball_z_model <- gam(ball_position_z ~ timestamp + timestamp_sqrd, data = pick(everything()) )

                         x_rmse <- RMSE(ball_position_x, predict(ball_x_model), na.rm = TRUE)
                         y_rmse <- RMSE(ball_position_y, predict(ball_y_model), na.rm = TRUE)
                         z_rmse <- RMSE(ball_position_z, predict(ball_z_model), na.rm = TRUE)

                         ground_dist_function <- function(time) {
                            time2 <- time^2
                            times_dataset <- data.frame(timestamp = time, timestamp_sqrd = time2)

                            ball_x <- predict(ball_x_model, newdata = times_dataset)
                            ball_y <- predict(ball_y_model, newdata = times_dataset)
                            ball_z <- predict(ball_z_model, newdata = times_dataset)
                            dist_from_home <- sqrt(ball_x^2 + ball_y^2)
                            ground_dataset <- data.frame(home_dist = dist_from_home, ball_position_x = ball_x, ball_position_y = ball_y)

                            ground <- switch( first(home_team),
                              "ANI" = predict(ground_ANI_model, newdata = ground_dataset),
                              "ARN" = predict(ground_ARN_model, newdata = ground_dataset),
                              "PHD" = predict(ground_PHD_model, newdata = ground_dataset),
                              "VAS" = predict(ground_VAS_model, newdata = ground_dataset),
                              NA
                            )

                            return(ball_z - ground)
                          }

                         ball_hits_ground_time <- tryCatch({uniroot(ground_dist_function, c(1000,9000))$root},
                                                            error = function(e) {-1})

                         ground_times <- data.frame(
                            timestamp = ball_hits_ground_time,
                            timestamp_sqrd = ball_hits_ground_time^2
                          )

                          tibble(
                            time_to_ground = ball_hits_ground_time,
                            ground_x = predict(ball_x_model, newdata = ground_times),
                            ground_y = predict(ball_y_model, newdata = ground_times),
                            ground_z = predict(ball_z_model, newdata = ground_times),
                            rmse_x = x_rmse,
                            rmse_y = y_rmse,
                            rmse_z = z_rmse,
                          )
                         }
                       )


####################################################################################################################################

all_pos_fly_balls <- fly_balls %>% left_join(ball_pos_fly_ball_3, by = c("game_string", "play_per_game")) %>% filter(!is.na(time_to_ground))
all_pos_fly_balls <- all_pos_fly_balls %>% left_join(OF_pos_fly_ball[,c(1:2,4:7)], by = c("game_string", "play_per_game"))

all_fly_ball_stats <- all_pos_fly_balls %>% mutate(hit_dist = sqrt(ground_x^2 + ground_y^2),
                                                   OF_ball_dist = sqrt((field_x - ground_x)^2 + (field_y - ground_y)^2),
                                                   spray_angle = atan(ground_x/ground_y)) %>% ungroup()

all_fly_ball_stats <- all_fly_ball_stats %>% mutate(ANI_wall = predict(wall_ANI_model, newdata = all_fly_ball_stats) - hit_dist,
                                                    ARN_wall = predict(wall_ARN_model, newdata = all_fly_ball_stats) - hit_dist,
                                                    PHD_wall = predict(wall_PHD_model, newdata = all_fly_ball_stats) - hit_dist,
                                                    VAS_wall = predict(wall_VAS_model, newdata = all_fly_ball_stats) - hit_dist)
all_fly_ball_stats <- all_fly_ball_stats %>% mutate(wall_ball_dist = case_when(
                                                    home_team == "ANI" ~ ANI_wall,
                                                    home_team == "ARN" ~ ARN_wall,
                                                    home_team == "PHD" ~ PHD_wall,
                                                    home_team == "VAS" ~ VAS_wall)) %>% select(-c(ANI_wall, ARN_wall, PHD_wall, VAS_wall))

all_fly_ball_stats <- all_fly_ball_stats %>% mutate(caught = ifelse(player_id_down == player_id, 1, 0))

####################################################################################################################################

batted_ball_stats <- ball_pos_fly_ball %>% group_by(game_string, play_per_game) %>% mutate(y_diff = ball_position_y - lag(ball_position_y)) %>% slice(-1)
batted_ball_stats <- batted_ball_stats %>% filter(sum(y_diff < 0) == 0)

batted_ball_stats <- batted_ball_stats %>% mutate(mph = 0.68181818 * sqrt((ball_position_x - first(ball_position_x))^2 +
                                                        (ball_position_y - first(ball_position_y))^2 +
                                                        (ball_position_z - first(ball_position_z))^2) / ((timestamp - first(timestamp))/1000))
batted_ball_stats <- batted_ball_stats %>% mutate(angle = atan( (ball_position_z - first(ball_position_z)) /
                                                                sqrt((ball_position_x - first(ball_position_x))^2 + (ball_position_y - first(ball_position_y)))^2 ))

ev_la_stats <- batted_ball_stats %>% slice(2)

### think I just won't use these due to some weird stuff with actual speeds and such
