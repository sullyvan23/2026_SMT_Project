
OF_positions <- player_positions %>% filter(player_id %in% c(7:9)) %>% mutate(play_key = paste0(game_string, play_per_game))

OF_catchable_positions <- catch_prob_fbs[,c("play_key", "player_id")] %>% left_join(OF_positions, by = c("play_key", "player_id"))
OF_catchable_positions <- OF_catchable_positions %>% dplyr::select(play_key, player_id, timestamp, field_x, field_y)
OF_catchable_positions <- OF_catchable_positions %>% left_join(fly_balls[,c("play_key", "timestamp_hit")], by = "play_key")
OF_catchable_positions <- OF_catchable_positions %>% filter(timestamp >= timestamp_hit)

OF_catchable_positions <- OF_catchable_positions %>% group_by(play_key, player_id) %>%
                                                     mutate(timestamp = (timestamp - first(timestamp)) / 1000)

OF_catchable_positions <- OF_catchable_positions %>% mutate(x_velo = 0.68181818 * (lead(field_x, 2) - lag(field_x, 2)) / 
                                                                                  (lead(timestamp, 2) - lag(timestamp, 2)),
                                                            y_velo = 0.68181818 * (lead(field_y, 2) - lag(field_y, 2)) / 
                                                                                  (lead(timestamp, 2) - lag(timestamp, 2)),
                                                            velo = sqrt(x_velo^2 + y_velo^2),
                                                            x_accel = (lead(x_velo, 2) - lag(x_velo, 2)) / 
                                                                      (lead(timestamp, 2) - lag(timestamp, 2)),
                                                            y_accel = (lead(y_velo, 2) - lag(y_velo, 2)) / 
                                                                      (lead(timestamp, 2) - lag(timestamp, 2)),
                                                            accel = (lead(velo, 2) - lag(velo, 2)) / 
                                                                    (lead(timestamp, 2) - lag(timestamp, 2)) )

ggplot(OF_catchable_positions %>% filter(play_key == "y1_d174_MTW_VAS226"), aes(x = field_x, y = field_y, color = timestamp)) + 
geom_point() + scale_color_gradient2(high = "green", low = "red")

ggplot(OF_catchable_positions %>% filter(play_key == "y1_d174_MTW_VAS226"), aes(x = timestamp, y = field_y)) + geom_point()



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
