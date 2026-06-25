
fly_balls <- bind_rows(ball_caught %>% mutate(caught = 1), 
                       ball_down %>% mutate(caught = 0))

fly_balls <- fly_balls %>% rename(timestamp_done = timestamp) %>%
                           mutate(timestamp_hit = timestamp_done - (1000*time_air)) %>% relocate(timestamp_hit, .before = timestamp_done)

fly_balls <- fly_balls %>% group_by(home_team) %>%
                           mutate(caught_height = case_when(home_team == "ANI"  ~  ball_position_z - predict(ground_ANI_model, newdata = pick(everything())),
                                                            home_team == "ARN"  ~  ball_position_z - predict(ground_ARN_model, newdata = pick(everything())),
                                                            home_team == "PHD"  ~  ball_position_z - predict(ground_PHD_model, newdata = pick(everything())),
                                                            home_team == "VAS"  ~  ball_position_z - predict(ground_VAS_model, newdata = pick(everything()))),
                                  caught_height = ifelse(caught == 1, caught_height, NA)) %>% ungroup()


hist(fly_balls$caught_height, breaks = 50)
### centered at 6
### realistic max at 8

############################################################################################################################################################################################

fly_balls_proj <- fly_balls[,c(1:2,4:5,8)] %>% left_join(ball_positions[,1:6], by = c("game_string", "play_per_game"))
fly_balls_proj <- fly_balls_proj %>% filter(timestamp <= timestamp_done, timestamp >= timestamp_hit)%>% 
                                     group_by(game_string, play_per_game) %>% 
                                     mutate(timestamp = (timestamp - first(timestamp))/1000) %>% slice(  (n()-5) : (n()-1) )


group <- 0
fly_balls_proj_pos <- fly_balls_proj %>% group_by(game_string, play_per_game) %>%
                       summarise({group <<- group + 1
                                  message(group/nrow(fly_balls))
                                  
                         ball_x_model <- gam(ball_position_x ~ s(timestamp, k = 3), data = pick(everything()) )
                         ball_y_model <- gam(ball_position_y ~ s(timestamp, k = 3), data = pick(everything()) )
                         ball_z_model <- gam(ball_position_z ~ s(timestamp, k = 3), data = pick(everything()) )

                         x_rmse <- RMSE(ball_position_x, predict(ball_x_model), na.rm = TRUE)
                         y_rmse <- RMSE(ball_position_y, predict(ball_y_model), na.rm = TRUE)
                         z_rmse <- RMSE(ball_position_z, predict(ball_z_model), na.rm = TRUE)

                         ground_dist_function <- function(time) {
                            times_dataset <- data.frame(timestamp = time)

                            ball_x <- predict(ball_x_model, newdata = times_dataset)
                            ball_y <- predict(ball_y_model, newdata = times_dataset)
                            ball_z <- predict(ball_z_model, newdata = times_dataset)
                           
                            ground_dataset <- data.frame(ball_position_x = ball_x, ball_position_y = ball_y)

                            ground <- switch( first(home_team),
                              "ANI" = predict(ground_ANI_model, newdata = ground_dataset),
                              "ARN" = predict(ground_ARN_model, newdata = ground_dataset),
                              "PHD" = predict(ground_PHD_model, newdata = ground_dataset),
                              "VAS" = predict(ground_VAS_model, newdata = ground_dataset),
                              NA
                            )

                            return(ball_z - ground)
                          }

                         ball_hits_ground_time <- tryCatch({uniroot(ground_dist_function, c(1,9))$root},
                                                            error = function(e) {-1})

                         ground_times <- data.frame(
                            timestamp = ball_hits_ground_time
                          )

                          tibble(
                            time_to_ground = ball_hits_ground_time,
                            ground_x = predict(ball_x_model, newdata = ground_times),
                            ground_y = predict(ball_y_model, newdata = ground_times),
                            ground_z = predict(ball_z_model, newdata = ground_times),
                            rmse_x = x_rmse,
                            rmse_y = y_rmse,
                            rmse_z = z_rmse
                          )
                         }
                       )


group <- 0
fly_balls_proj_8ft <- fly_balls_proj %>% group_by(game_string, play_per_game) %>%
                       summarise({group <<- group + 1
                                  message(group/nrow(fly_balls))
                                  
                         ball_x_model <- gam(ball_position_x ~ s(timestamp, k = 3), data = pick(everything()) )
                         ball_y_model <- gam(ball_position_y ~ s(timestamp, k = 3), data = pick(everything()) )
                         ball_z_model <- gam(ball_position_z ~ s(timestamp, k = 3), data = pick(everything()) )

                         x_rmse <- RMSE(ball_position_x, predict(ball_x_model), na.rm = TRUE)
                         y_rmse <- RMSE(ball_position_y, predict(ball_y_model), na.rm = TRUE)
                         z_rmse <- RMSE(ball_position_z, predict(ball_z_model), na.rm = TRUE)

                         ground_dist_function <- function(time) {
                            times_dataset <- data.frame(timestamp = time)

                            ball_x <- predict(ball_x_model, newdata = times_dataset)
                            ball_y <- predict(ball_y_model, newdata = times_dataset)
                            ball_z <- predict(ball_z_model, newdata = times_dataset)
                           
                            ground_dataset <- data.frame(ball_position_x = ball_x, ball_position_y = ball_y)

                            ground <- switch( first(home_team),
                              "ANI" = predict(ground_ANI_model, newdata = ground_dataset),
                              "ARN" = predict(ground_ARN_model, newdata = ground_dataset),
                              "PHD" = predict(ground_PHD_model, newdata = ground_dataset),
                              "VAS" = predict(ground_VAS_model, newdata = ground_dataset),
                              NA
                            )

                            return(ball_z - (ground + 8))
                          }

                         ball_hits_ground_time <- tryCatch({uniroot(ground_dist_function, c(1,9))$root},
                                                            error = function(e) {-1})

                         ground_times <- data.frame(
                            timestamp = ball_hits_ground_time
                          )

                          tibble(
                            time_eight_ft = ball_hits_ground_time,
                            eight_ft_x = predict(ball_x_model, newdata = ground_times),
                            eight_ft_y = predict(ball_y_model, newdata = ground_times),
                            eight_ft_z = predict(ball_z_model, newdata = ground_times),
                            rmse_x = x_rmse,
                            rmse_y = y_rmse,
                            rmse_z = z_rmse
                          )
                         }
                       )

############################################################################################################################################################################################

fly_ball_land <- fly_balls %>% left_join(fly_balls_proj_pos, by = c("game_string", "play_per_game"))
fly_ball_land <- fly_ball_land %>% left_join(fly_balls_proj_8ft, by = c("game_string", "play_per_game"), suffix = c("_ground", "_8ft"))
fly_ball_land <- fly_ball_land %>% filter(!time_to_ground == -1, !time_eight_ft == -1)

plot(fly_ball_land$time_air, fly_ball_land$time_eight_ft)
fly_ball_land <- fly_ball_land %>% filter(!(time_air > 7 & time_eight_ft < 4))

fly_ball_land <- fly_ball_land %>% mutate(ground_dist = sqrt(ground_x^2 + ground_y^2),
                                          eight_ft_dist = sqrt(eight_ft_x^2 + eight_ft_y^2))
plot(fly_ball_land$ground_dist, fly_ball_land$eight_ft_dist)

############################################################################################################################################################################################

could_catch <- fly_ball_land %>% select(game_string, play_per_game, timestamp_hit, player_id, caught, time_to_ground:ground_z, time_eight_ft:eight_ft_z)
could_catch <- could_catch %>% left_join(player_positions[,1:6] %>% filter(player_id %in% c(3:9)),
                                         by = c("game_string", "play_per_game", "timestamp_hit" = "timestamp"),
                                         suffix = c("_event", ""))

could_catch <- could_catch %>% mutate(ground_dist = sqrt((field_x - ground_x)^2 + (field_y - ground_y)^2),
                                      eight_ft_dist = sqrt((field_x - eight_ft_x)^2 + (field_y - eight_ft_y)^2),
                                      OF = ifelse(player_id >= 7, 1, 0),
                                      player_caught = ifelse(caught == 1  &  player_id_event == player_id, 1, 0)) %>%
                               filter(!is.na(player_id))

ggplot(could_catch, aes(x = ground_dist, y = time_to_ground, color = player_caught)) + 
       geom_point() + scale_color_gradient2(high = "green", low = "red", mid = "white")


set.seed(229)
cc_fold <- createFolds(could_catch$player_caught, k = 5)

act <- c()
pred <- c()
for(fold in cc_fold) {
  train <- could_catch[-fold, ]
  test <- could_catch[fold, ]
  model <- gam(player_caught ~ te(ground_dist, time_to_ground) + te(eight_ft_dist, time_eight_ft) + OF, 
               family = binomial, data = train)
  act <- c(act, test$caught)
  pred <- c(pred, predict(model, newdata = test, type = "response"))
}
logLoss(act, pred)
### 16.50932

plot(model, pages = 1)


catch_chance_model <- gam(player_caught ~ te(ground_dist, time_to_ground) + te(eight_ft_dist, time_eight_ft) + OF, 
                          family = binomial, data = could_catch)
could_catch <- could_catch %>% ungroup %>% mutate(player_catch_prob = round(predict(catch_chance_model, type = "response"), 4))


could_catch_players <- could_catch %>% group_by(game_string, play_per_game) %>%
                                       filter(player_catch_prob >= 0.01  |  player_catch_prob == max(player_catch_prob))











