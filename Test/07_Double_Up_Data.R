
doubled_up_data <- doubled_up_results %>% left_join(final_catch_prob_results, by = c("game_string", "play_per_game"))

doubled_up_data <- doubled_up_data %>% left_join(player_positions[,1:6], by = c("game_string", "play_per_game", "timestamp", "player_id_br" = "player_id"))
group <- 0
doubled_up_data <- doubled_up_data %>% group_by(game_string, play_per_game, player_id_br) %>%
                 group_modify(~{group <<- group + 1
                              message(group/nrow(doubled_up_results))
                                  
                              x_model <- gam(field_x ~ s(timestamp, k = 10), data = .x )
                              y_model <- gam(field_y ~ s(timestamp, k = 10), data = .x )

                              .x$pred_x <- predict(x_model, newdata = .x) 
                              .x$pred_y <- predict(y_model, newdata = .x)

                              .x$rmse_x <- RMSE(.x$pred_x, .x$field_x)
                              .x$rmse_y <- RMSE(.x$pred_y, .x$field_y)

                              .x})
doubled_up_data <- doubled_up_data %>% mutate(runner_x_velo = 0.681818 * (pred_x - lag(pred_x)) / ((timestamp - lag(timestamp))/1000),
                                              runner_y_velo = 0.681818 * (pred_y - lag(pred_y)) / ((timestamp - lag(timestamp))/1000),
                                              runner_velo = sqrt(runner_x_velo^2 + runner_y_velo^2),
                                              across(c(runner_x_velo:runner_velo), ~ ifelse(is.na(.), lead(.), .)))
doubled_up_data <- doubled_up_data %>% mutate(og_base_x = case_when(player_id_br == 11  ~  x_1b,
                                                                    player_id_br == 12  ~  x_2b,
                                                                    player_id_br == 13  ~  x_3b),
                                              og_base_y = case_when(player_id_br == 11  ~  y_1b,
                                                                    player_id_br == 12  ~  y_2b,
                                                                    player_id_br == 13  ~  y_3b))

doubled_up_data <- doubled_up_data %>% left_join(catch_prob_data %>% select(game_string, play_per_game, player_id, timestamp, pred_x, pred_y, OF_x_velo, OF_y_velo, OF_velo,
                                                                            time_left_ground, ground_x, ground_y, time_left_8ft, eight_ft_x, eight_ft_y, player_code),
                                                 by = c("game_string", "play_per_game", "timestamp", "player_id"),
                                                 suffix = c("_runner", "_OF"))


doubled_up_data <- doubled_up_data %>% mutate(runner_og_x_dist = pred_x_runner - og_base_x,
                                              runner_og_y_dist = pred_y_runner - og_base_y,
                                              runner_og_dist = sqrt(runner_og_x_dist^2 + runner_og_y_dist^2),
                                              OF_og_x_dist = pred_x_OF - og_base_x,
                                              OF_og_y_dist = pred_y_OF - og_base_y,
                                              OF_og_dist = sqrt(OF_og_x_dist^2 + OF_og_y_dist^2),
                                              ground_og_dist = sqrt((ground_x - og_base_x)^2 + (ground_y - og_base_y)^2),
                                              eight_ft_og_dist = sqrt((eight_ft_x - og_base_x)^2 + (eight_ft_y - og_base_y)^2))

doubled_up_data <- doubled_up_data %>% mutate(runner_og_velo = -((runner_og_x_dist * runner_x_velo) + (runner_og_y_dist * runner_y_velo)) / 
                                                               runner_og_dist,
                                              runner_og_velo_angle = acos(runner_og_velo / runner_velo),
                                              OF_og_velo = -((OF_og_x_dist * OF_x_velo) + (OF_og_y_dist * OF_y_velo)) / 
                                                            OF_og_dist,
                                              OF_og_velo_angle = acos(OF_og_velo / OF_velo))


doubled_up_data <- doubled_up_data %>% relocate(player_code, .after = OF_og_velo_angle)
doubled_up_data <- doubled_up_data %>% left_join(lineups_players[,c(1,7,9:10)], by = c("game_string", "play_per_game", "player_id_br" = "player_id"),
                                                 suffix = c("_OF", "_runner"))
doubled_up_data <- doubled_up_data %>% left_join(throw_speed[,1:2], by = c("player_code_OF" = "player_code"))
doubled_up_data <- doubled_up_data %>% left_join(player_speed[,1:2], by = c("player_code_runner" = "player_code"), suffix = c("_throw", "_runner"))
doubled_up_data <- doubled_up_data %>% mutate(speed_95_throw = ifelse(is.na(speed_95_throw), mean(throw_speed$speed_95), speed_95_throw),
                                              speed_95_runner = ifelse(is.na(speed_95_runner), mean(player_speed$speed_95), speed_95_runner))

doubled_up_data <- doubled_up_data %>% group_by(game_string, play_per_game, player_id_br, timestamp) %>%
                                       mutate(if_caught_catch_prob = catch_prob / sum(catch_prob),
                                              caught_prob = sum(catch_prob))

write.csv(doubled_up_data, "doubled_up_data.csv", row.names = FALSE)



doubled_up_data_sum <- doubled_up_data %>% mutate(across(c(player_id, pred_x_OF:OF_velo, OF_og_x_dist:OF_og_dist, OF_og_velo:OF_og_velo_angle, speed_95_throw),
                                                         ~ weighted.mean(., if_caught_catch_prob)))
doubled_up_data_sum <- doubled_up_data_sum %>% slice(1) %>% select(-c(player_code_OF, if_caught_catch_prob))

write.csv(doubled_up_data_sum, "doubled_up_data_sum.csv", row.names = FALSE)



