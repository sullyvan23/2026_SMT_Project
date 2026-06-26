
catch_prob_data <- could_catch_players %>% select(-c(field_x:eight_ft_dist, player_catch_prob)) %>% 
                                           left_join(player_positions[,1:6], by = c("game_string", "play_per_game", "player_id"))
catch_prob_data <- catch_prob_data %>% filter(timestamp >= timestamp_hit, timestamp <= timestamp_done)


group <- 0
catch_prob_data <- catch_prob_data %>% group_by(game_string, play_per_game, player_id) %>%
                 group_modify(~{group <<- group + 1
                              message(group/nrow(could_catch_players))
                                  
                              x_model <- gam(field_x ~ s(timestamp, k = 10), data = .x )
                              y_model <- gam(field_y ~ s(timestamp, k = 10), data = .x )

                              .x$pred_x <- predict(x_model) 
                              .x$pred_y <- predict(y_model)

                              .x$rmse_x <- RMSE(.x$pred_x, .x$field_x)
                              .x$rmse_y <- RMSE(.x$pred_y, .x$field_y)

                              .x})

hist(catch_prob_data$rmse_x, breaks = 100)
hist(catch_prob_data$rmse_y, breaks = 100)


catch_prob_data <- catch_prob_data %>% mutate(time_left_ground = ((timestamp_hit + (time_to_ground*1000)) - timestamp)/1000,
                                              time_left_8ft = ((timestamp_hit + (time_eight_ft*1000)) - timestamp)/1000,
                                              OF_ground_x_dist = pred_x - ground_x,
                                              OF_ground_y_dist = pred_y - ground_y,
                                              OF_8ft_x_dist = pred_x - eight_ft_x,
                                              OF_8ft_y_dist = pred_y - eight_ft_y,
                                              OF_x_velo = 0.681818 * (pred_x - lag(pred_x)) / ((timestamp - lag(timestamp))/1000),
                                              OF_x_velo = ifelse(is.na(OF_x_velo), lead(OF_x_velo), OF_x_velo),
                                              OF_y_velo = 0.681818 * (pred_y - lag(pred_y)) / ((timestamp - lag(timestamp))/1000),
                                              OF_y_velo = ifelse(is.na(OF_y_velo), lead(OF_y_velo), OF_y_velo),
                                              OF_ground_dist = sqrt(OF_ground_x_dist^2 + OF_ground_y_dist^2), 
                                              OF_8ft_dist = sqrt(OF_8ft_x_dist^2 + OF_8ft_y_dist^2),
                                              OF_velo = sqrt(OF_x_velo^2 + OF_y_velo^2))

catch_prob_data <- catch_prob_data %>% mutate(OF_ground_front_dist = ((OF_ground_x_dist * pred_x) + (OF_ground_y_dist * pred_y)) / 
                                                                      sqrt(pred_x^2 + pred_y^2),
                                              OF_ground_angle = acos(OF_ground_front_dist / OF_ground_dist),
                                              OF_8ft_front_dist = ((OF_8ft_x_dist * pred_x) + (OF_8ft_y_dist * pred_y)) / 
                                                                      sqrt(pred_x^2 + pred_y^2),
                                              OF_8ft_angle = acos(OF_8ft_front_dist / OF_8ft_dist))

catch_prob_data <- catch_prob_data %>% mutate(OF_ground_velo = -((OF_ground_x_dist * OF_x_velo) + (OF_ground_y_dist * OF_y_velo)) / 
                                                               OF_ground_dist,
                                              OF_ground_velo_angle = acos(OF_ground_velo / OF_velo),
                                              OF_8ft_velo = -((OF_8ft_x_dist * OF_x_velo) + (OF_8ft_y_dist * OF_y_velo)) / 
                                                            OF_8ft_dist,
                                              OF_8ft_velo_angle = acos(OF_8ft_velo / OF_velo))


catch_prob_data <- catch_prob_data %>% mutate(spray_angle = atan(ground_x / ground_y))
catch_prob_data <- catch_prob_data %>% group_by(home_team) %>%
                                       mutate(ground_wall = case_when(home_team == "ANI"  ~  predict(wall_ANI_model, newdata = pick(everything())),
                                                                      home_team == "ARN"  ~  predict(wall_ARN_model, newdata = pick(everything())),
                                                                      home_team == "PHD"  ~  predict(wall_PHD_model, newdata = pick(everything())),
                                                                      home_team == "VAS"  ~  predict(wall_VAS_model, newdata = pick(everything())))) %>% ungroup()
catch_prob_data <- catch_prob_data %>% mutate(spray_angle = atan(eight_ft_x / eight_ft_y))
catch_prob_data <- catch_prob_data %>% group_by(home_team) %>%
                                       mutate(eight_ft_wall = case_when(home_team == "ANI"  ~  predict(wall_ANI_model, newdata = pick(everything())),
                                                                        home_team == "ARN"  ~  predict(wall_ARN_model, newdata = pick(everything())),
                                                                        home_team == "PHD"  ~  predict(wall_PHD_model, newdata = pick(everything())),
                                                                        home_team == "VAS"  ~  predict(wall_VAS_model, newdata = pick(everything())))) %>% ungroup() %>%
                                       select(-spray_angle)

catch_prob_data <- catch_prob_data %>% mutate(wall_ground_dist = ground_wall - sqrt(ground_x^2 + ground_y^2),
                                              wall_8ft_dist = eight_ft_wall - sqrt(eight_ft_x^2 + eight_ft_y^2))

catch_prob_data <- catch_prob_data %>% left_join(lineups_players[,c(1,7,9:10)], by = c("game_string", "play_per_game", "player_id"))
catch_prob_data <- catch_prob_data %>% group_by(game_string, play_per_game, player_id, timestamp) %>%
                                       mutate(player_code = ifelse(first(player_code) != last(player_code), NA, player_code)) %>%
                                       slice(1)
catch_prob_data <- catch_prob_data %>% left_join(player_speed[,1:2], by = "player_code")
catch_prob_data <- catch_prob_data %>% mutate(speed_95 = ifelse(is.na(speed_95), mean(player_speed$speed_95), speed_95)) %>% 
                                       rename(player_speed = speed_95)


catch_prob_data <- catch_prob_data %>% relocate(player_caught, .after = last_col())

write.csv(catch_prob_data, "catch_prob_data.csv", row.names = FALSE)





