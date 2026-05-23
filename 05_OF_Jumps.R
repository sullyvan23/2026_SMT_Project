
OF_positions <- player_positions %>% filter(player_id %in% c(7:9)) %>% mutate(play_key = paste0(game_string, play_per_game))

OF_catchable_positions <- catch_prob_fbs[,c("play_key", "player_id")] %>% left_join(OF_positions, by = c("play_key", "player_id"))
OF_catchable_positions <- OF_catchable_positions %>% dplyr::select(play_key, player_id, timestamp, field_x, field_y)
OF_catchable_positions <- OF_catchable_positions %>% left_join(fly_balls[,c("play_key", "timestamp_hit")], by = "play_key")
OF_catchable_positions <- OF_catchable_positions %>% filter(timestamp >= timestamp_hit)

OF_catchable_positions <- OF_catchable_positions %>% group_by(play_key, player_id) %>%
                                                     mutate(timestamp = (timestamp - first(timestamp)) / 1000)

OF_catchable_positions <- OF_catchable_positions %>% mutate(x_velo = 0.68181818 * lead(field_x - lag(field_x, 1)) / 
                                                                                  (timestamp - lag(timestamp, 1)),
                                                            y_velo = 0.68181818 * lead(field_y - lag(field_y, 1)) / 
                                                                                  (timestamp - lag(timestamp, 1)),
                                                            velo = sqrt(x_velo^2 + y_velo^2),
                                                            x_accel = (x_velo - lag(x_velo, 1)) / 
                                                                      (timestamp - lag(timestamp, 1)),
                                                            y_accel = (y_velo - lag(y_velo, 1)) / 
                                                                      (timestamp - lag(timestamp, 1)),
                                                            accel = (velo - lag(velo, 1)) / 
                                                                    (timestamp - lag(timestamp, 1)) )


ggplot(OF_catchable_positions %>% filter(play_key == "y1_d061_VKA_PHD228"), aes(x = field_x, y = field_y, color = timestamp)) + 
geom_point() + scale_color_gradient2(high = "green", low = "red")

ggplot(OF_catchable_positions %>% filter(play_key == "y1_d143_ADQ_ANI19"), aes(x = timestamp, y = accel)) + geom_point()

hist(OF_catchable_positions$velo[OF_catchable_positions$velo < 25], breaks = 100)
### < 25
hist(OF_catchable_positions$accel[OF_catchable_positions$accel < 30 & OF_catchable_positions$accel > -30], breaks = 100)
### > -15  and  < 15

#################################################################################################################################################

OF_jump_range <- OF_catchable_positions %>% filter(timestamp >= 0.25   &   timestamp <= 0.75)

group <- 0
OF_jump_range <- OF_jump_range %>% group_by(play_key, player_id) %>%
                 group_modify(~{group <<- group + 1
                              message(group)
                                  
                              x_model <- gam(field_x ~ s(timestamp, k = 3), data = .x )
                              y_model <- gam(field_y ~ s(timestamp, k = 3), data = .x )

                              .x$pred_x <- predict(x_model) 
                              .x$pred_y <- predict(y_model)

                              .x$rmse_x <- RMSE(.x$pred_x, .x$field_x)
                              .x$rmse_y <- RMSE(.x$pred_y, .x$field_y)

                              .x})

hist(OF_jump_range$rmse_x, breaks = 100)
### < 0.025
hist(OF_jump_range$rmse_y, breaks = 100)
### < 0.1

OF_jump_range_use <- OF_jump_range %>% filter(rmse_x < 0.025   &   rmse_y < 0.1)

OF_jump_range_use <- OF_jump_range_use %>% mutate(pred_x_velo = 0.68181818 * lead(pred_x - lag(pred_x, 1)) / 
                                                           (timestamp - lag(timestamp, 1)),
                                                  pred_y_velo = 0.68181818 * lead(pred_y - lag(pred_y, 1)) / 
                                                           (timestamp - lag(timestamp, 1)),
                                                  pred_velo = sqrt(pred_x_velo^2 + pred_y_velo^2),
                                                  pred_x_accel = (pred_x_velo - lag(pred_x_velo, 1)) / 
                                                            (timestamp - lag(timestamp, 1)),
                                                  pred_y_accel = (pred_y_velo - lag(pred_y_velo, 1)) / 
                                                            (timestamp - lag(timestamp, 1)),
                                                  pred_accel = (pred_velo - lag(pred_velo, 1)) / 
                                                          (timestamp - lag(timestamp, 1)) )

#################################################################################################################################################

OF_jump_0.5 <- OF_jump_range_use %>% mutate(time_to_0.5 = abs(timestamp - 0.5))
OF_jump_0.5 <- OF_jump_0.5 %>% filter(time_to_0.5 == min(time_to_0.5))

hist(OF_jump_0.5$pred_accel, breaks = 100)

ggplot(OF_jump_range %>% filter(play_key == "y1_d109_SPL_PHD170"), aes(x = field_x, y = field_y)) + geom_point()

OF_jump_0.5 <- OF_jump_0.5 %>% filter(pred_accel > -10   &   pred_accel < 20)

#################################################################################################################################################

jump_catch_prob <- OF_jump_0.5[,c(1:3,13:14,17:22)] %>% left_join(catch_prob_fbs[,c(1,4:10,17,21,40)], by = c("play_key", "player_id"))

jump_catch_prob <- jump_catch_prob %>% mutate(time_to_ground = time_to_ground - timestamp)
jump_catch_prob <- jump_catch_prob %>% mutate(jump_x = pred_x - field_x,
                                              jump_y = pred_y - field_y,
                                              jump_dist = sqrt(jump_x^2 + jump_y^2))
jump_catch_prob <- jump_catch_prob %>% mutate(OF_ball_x_dist = ground_x - pred_x,
                                              OF_ball_y_dist = ground_y - pred_y,
                                              OF_ball_dist = sqrt(OF_ball_x_dist^2 + OF_ball_y_dist^2))

ggplot(OF_catchable_positions %>% filter(play_key == "y1_d164_FNQ_PHD92"), aes(x = field_x, y = field_y)) + geom_point()


jump_catch_prob <- jump_catch_prob %>% mutate(jump_to_ball = ((jump_x * OF_ball_x_dist) + (jump_y * OF_ball_y_dist)) / 
                                                             sqrt(OF_ball_x_dist^2 + OF_ball_y_dist^2))
jump_catch_prob <- jump_catch_prob %>% mutate(jump_on_angle = acos(jump_to_ball / jump_dist))
jump_catch_prob <- jump_catch_prob %>% mutate(unit_x = OF_ball_x_dist / OF_ball_dist,
                                              unit_y = OF_ball_y_dist / OF_ball_dist,
                                              jump_to_side = jump_dist * sin(jump_on_angle),
                                              pos_side_diff = abs( (field_x + (jump_to_ball * unit_x) + (jump_to_side * unit_y)) - pred_x ),
                                              neg_side_diff = abs( (field_x + (jump_to_ball * unit_x) + (jump_to_side * -unit_y)) - pred_x ),
                                              jump_to_side = ifelse(pos_side_diff < neg_side_diff,
                                                                    jump_to_side, -jump_to_side)) %>% 
                                       dplyr::select(-c(unit_x, unit_y, pos_side_diff, neg_side_diff)) %>%
                                       relocate(jump_to_side, .before = jump_on_angle)
jump_catch_prob <- jump_catch_prob %>% mutate(jump_on_angle = atan2(jump_to_side, jump_to_ball))


jump_catch_prob <- jump_catch_prob %>% mutate(velo_ball = ((pred_x_velo * OF_ball_x_dist) + (pred_y_velo * OF_ball_y_dist)) / 
                                                             sqrt(OF_ball_x_dist^2 + OF_ball_y_dist^2))
jump_catch_prob <- jump_catch_prob %>% mutate(velo_on_angle = acos(velo_ball / pred_velo))
jump_catch_prob <- jump_catch_prob %>% mutate(unit_x = OF_ball_x_dist / OF_ball_dist,
                                              unit_y = OF_ball_y_dist / OF_ball_dist,
                                              velo_side = pred_velo * sin(velo_on_angle),
                                              pos_side_diff = abs( ((velo_ball * unit_x) + (velo_side * unit_y)) - pred_x_velo ),
                                              neg_side_diff = abs( ((velo_ball * unit_x) + (velo_side * -unit_y)) - pred_x_velo ),
                                              velo_side = ifelse(pos_side_diff < neg_side_diff,
                                                                    velo_side, -velo_side)) %>% 
                                       dplyr::select(-c(unit_x, unit_y, pos_side_diff, neg_side_diff)) %>%
                                       relocate(velo_side, .before = velo_on_angle)
jump_catch_prob <- jump_catch_prob %>% mutate(velo_on_angle = atan2(velo_side, velo_ball))


jump_catch_prob <- jump_catch_prob %>% mutate(accel_ball = ((pred_x_accel * OF_ball_x_dist) + (pred_y_accel * OF_ball_y_dist)) / 
                                                             sqrt(OF_ball_x_dist^2 + OF_ball_y_dist^2))
jump_catch_prob <- jump_catch_prob %>% mutate(accel_on_angle = acos(accel_ball / sqrt(pred_x_accel^2 + pred_y_accel^2) ))
jump_catch_prob <- jump_catch_prob %>% mutate(unit_x = OF_ball_x_dist / OF_ball_dist,
                                              unit_y = OF_ball_y_dist / OF_ball_dist,
                                              accel_side = sqrt(pred_x_accel^2 + pred_y_accel^2) * sin(accel_on_angle),
                                              pos_side_diff = abs( ((accel_ball * unit_x) + (accel_side * unit_y)) - pred_x_accel ),
                                              neg_side_diff = abs( ((accel_ball * unit_x) + (accel_side * -unit_y)) - pred_x_accel ),
                                              accel_side = ifelse(pos_side_diff < neg_side_diff,
                                                                    accel_side, -accel_side)) %>% 
                                       dplyr::select(-c(unit_x, unit_y, pos_side_diff, neg_side_diff)) %>%
                                       relocate(accel_side, .before = accel_on_angle)
jump_catch_prob <- jump_catch_prob %>% mutate(accel_on_angle = atan2(accel_side, accel_ball))


#################################################################################################################################################

























OF_catchable_check <- OF_jump_range %>% summarise(max_velo = max(velo, na.rm = TRUE),
                                                  max_accel = max(accel, na.rm = TRUE),
                                                  max_decel = min(accel, na.rm = TRUE),
                                                  data_points = n())


ggplot(OF_jump_range %>% filter(play_key == "y1_d061_VKA_PHD22"), aes(x = field_x, y = field_y)) + geom_point()

#################################################################################################################################################



hist(OF_catchable_check$max_velo, breaks = 100)
### < 20
hist(OF_catchable_check$max_accel[OF_catchable_check$max_accel < 50], breaks = 100)
### < 20
hist(OF_catchable_check$max_decel[OF_catchable_check$max_decel > -50], breaks = 100)
### > -15

OF_plays_use <- OF_catchable_check %>% filter(max_velo < 20   &   max_accel < 20   &   max_decel > -15)

hist(OF_plays_use$max_velo, breaks = 100)
hist(OF_plays_use$max_accel, breaks = 100)
hist(OF_plays_use$max_decel, breaks = 100)


#################################################################################################################################################



OF_catchable_positions_0.5 <- OF_catchable_positions[,1:5] %>% filter(timestamp >= 0.3   &   timestamp <= 0.7)
OF_catchable_positions_0.5 <- OF_catchable_positions_0.5 %>% mutate(timestamp_2 = timestamp^2, timestamp_3 = timestamp^3) %>%
                                                             relocate(timestamp_2:timestamp_3, .after = timestamp)

group <- 0
OF_pred_positions_0.5 <- OF_catchable_positions_0.5 %>% group_by(play_key, player_id) %>%
                       group_modify(~{group <<- group + 1
                                      message(group/3444)
                                  
                                    x_model <- lm(field_x ~ timestamp + timestamp_2 + timestamp_3, data = .x )
                                    y_model <- lm(field_y ~ timestamp + timestamp_2 + timestamp_3, data = .x )

                                    .x$pred_x <- predict(x_model) 
                                    .x$pred_y <- predict(y_model)

                                    dt <- c(NA, diff(.x$timestamp))

                                    .x$velo_x <- c(NA, diff(.x$pred_x) / dt[-1])
                                    .x$velo_y <- c(NA, diff(.x$pred_y) / dt[-1])
                                    .x$speed <- sqrt(.x$velo_x^2 + .x$velo_y^2)
                                
                                    .x$accel_x <- c(NA, diff(.x$velo_x) / dt[-c(1,2)])
                                    .x$accel_y <- c(NA, diff(.x$velo_y) / dt[-c(1,2)])
                            
                                    .x$accel <- sqrt(.x$accel_x^2 + .x$accel_y^2)

                                    .x})









ggplot(OF_catchable_positions_0.5 %>% filter(play_key == "y1_d174_MTW_VAS226"), aes(x = field_x, y = field_y, color = timestamp)) + 
geom_point() + scale_color_gradient2(high = "green", low = "red")

ggplot(OF_catchable_positions_0.5 %>% filter(play_key == "y1_d061_VKA_PHD22"), aes(x = timestamp, y = field_x)) + geom_point()
ggplot(OF_pred_positions_0.5 %>% filter(play_key == "y1_d061_VKA_PHD22"), aes(x = timestamp, y = pred_x)) + geom_point()


OF_timing_checks <- OF_catchable_positions %>% filter(timestamp >= 0.3   &   timestamp <= 0.7) %>% summarise(velo = max(velo), accel = max(abs(accel)) )

hist(OF_timing_checks$velo, breaks = 100)
hist(OF_timing_checks$accel, breaks = 100)
