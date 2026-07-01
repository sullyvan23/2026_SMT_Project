
tag_up_data <- tag_results %>% left_join(final_catch_prob_results, by = c("game_string", "play_per_game"))

tag_up_data <- tag_up_data %>% left_join(player_positions[,1:6], by = c("game_string", "play_per_game", "timestamp", "player_id_br" = "player_id"))
group <- 0
tag_up_data <- tag_up_data %>% group_by(game_string, play_per_game, player_id_br) %>%
                 group_modify(~{group <<- group + 1
                              message(group/nrow(tag_results))
                                  
                              x_model <- gam(field_x ~ s(timestamp, k = 10), data = .x )
                              y_model <- gam(field_y ~ s(timestamp, k = 10), data = .x )

                              .x$pred_x <- predict(x_model, newdata = .x) 
                              .x$pred_y <- predict(y_model, newdata = .x)

                              .x$rmse_x <- RMSE(.x$pred_x, .x$field_x)
                              .x$rmse_y <- RMSE(.x$pred_y, .x$field_y)

                              .x})

tag_up_data <- tag_up_data %>% mutate(dist_1st = sqrt((pred_x - x_1b)^2 + (pred_y - y_1b)^2),
                                      dist_2nd = sqrt((pred_x - x_2b)^2 + (pred_y - y_2b)^2),
                                      dist_3rd = sqrt((pred_x - x_3b)^2 + (pred_y - y_3b)^2),
                                      dist_home = sqrt((pred_x - x_home)^2 + (pred_y - y_home)^2))
tag_up_data <- tag_up_data %>% mutate(basepath = case_when(pred_y < 0 | (pred_y < 50 & pred_x > 0)  ~  4, 
                                                           pred_y >= 50 & pred_x > x_2b  ~  1 + (dist_1st / (dist_1st + dist_2nd)),
                                                           pred_y >= y_3b & pred_x <= x_2b  ~  2 + (dist_2nd / (dist_2nd + dist_3rd)),
                                                           pred_y < y_3b & pred_x <= x_home  ~  3 + (dist_3rd / (dist_3rd + dist_home)) ))

tag_up_data <- tag_up_data %>% mutate(og_base_x = case_when(player_id_br == 11  ~  x_1b,
                                                            player_id_br == 12  ~  x_2b,
                                                            player_id_br == 13  ~  x_3b),
                                      og_base_y = case_when(player_id_br == 11  ~  y_1b,
                                                            player_id_br == 12  ~  y_2b,
                                                            player_id_br == 13  ~  y_3b))
tag_up_data <- tag_up_data %>% mutate(next_base_x = case_when(player_id_br == 11  ~  x_2b,
                                                              player_id_br == 12  ~  x_3b,
                                                              player_id_br == 13  ~  x_home),
                                      next_base_y = case_when(player_id_br == 11  ~  y_2b,
                                                              player_id_br == 12  ~  y_3b,
                                                              player_id_br == 13  ~  y_home))

tag_up_data <- tag_up_data %>% left_join(catch_prob_data %>% select(game_string, play_per_game, player_id, timestamp, pred_x, pred_y, OF_x_velo, OF_y_velo, OF_velo,
                                                                    time_to_ground, time_left_ground, ground_x, ground_y, OF_ground_x_dist, OF_ground_y_dist, OF_ground_dist, 
                                                                    player_code),
                                         by = c("game_string", "play_per_game", "timestamp", "player_id"),
                                         suffix = c("_runner", "_OF"))


tag_up_data <- tag_up_data %>% mutate(runner_basepath_velo = (basepath - lag(basepath)) / ((timestamp - lag(timestamp))/1000),
                                      runner_basepath_velo = ifelse(is.na(runner_basepath_velo), lead(runner_basepath_velo), runner_basepath_velo),
                                      og_basepath_dist = basepath - player_id_br + 10,
                                      OF_next_x_dist = pred_x_OF - next_base_x,
                                      OF_next_y_dist = pred_y_OF - next_base_y,
                                      OF_next_dist = sqrt(OF_next_x_dist^2 + OF_next_y_dist^2),
                                      ground_next_dist = sqrt((ground_x - next_base_x)^2 + (ground_y - next_base_y)^2))

tag_up_data <- tag_up_data %>% mutate(OF_ground_next_dist = ((OF_next_x_dist * OF_ground_x_dist) + (OF_next_y_dist * OF_ground_y_dist)) / 
                                                            OF_next_dist,
                                      OF_ground_next_angle = acos(OF_ground_next_dist / OF_ground_dist),
                                      OF_next_velo = -((OF_next_x_dist * OF_x_velo) + (OF_next_y_dist * OF_y_velo)) / 
                                                      OF_next_dist,
                                      OF_next_velo_angle = acos(OF_next_velo / OF_velo))


tag_up_data <- tag_up_data %>% relocate(player_code, .after = OF_next_velo_angle)
tag_up_data <- tag_up_data %>% left_join(lineups_players[,c(1,7,9:10)], by = c("game_string", "play_per_game", "player_id_br" = "player_id"),
                                         suffix = c("_OF", "_runner"))
tag_up_data <- tag_up_data %>% left_join(throw_speed[,1:2], by = c("player_code_OF" = "player_code"))
tag_up_data <- tag_up_data %>% left_join(player_speed[,1:2], by = c("player_code_runner" = "player_code"), suffix = c("_throw", "_runner"))
tag_up_data <- tag_up_data %>% mutate(speed_95_throw = ifelse(is.na(speed_95_throw), mean(throw_speed$speed_95), speed_95_throw),
                                      speed_95_runner = ifelse(is.na(speed_95_runner), mean(player_speed$speed_95), speed_95_runner))

tag_up_data <- tag_up_data %>% left_join(baserunners, by = c("game_string", "play_per_game"))
tag_up_data <- tag_up_data %>% mutate(runners_front = case_when(player_id_br == 11  ~  ifelse(second == 1, second + third, 0),
                                                                player_id_br == 12  ~  third,
                                                                player_id_br == 13  ~  0)) %>%
                               select(-c(first:third))

tag_up_data <- tag_up_data %>% group_by(game_string, play_per_game, player_id_br, timestamp) %>%
                               mutate(if_caught_catch_prob = catch_prob / sum(catch_prob),
                                      caught_prob = sum(catch_prob))


write.csv(tag_up_data, "tag_up_data.csv", row.names = FALSE)

##############################################################################################################################################################################################

tag_up_data_sum <- tag_up_data %>% mutate(across(c(player_id, pred_x_OF:OF_velo, OF_ground_x_dist:OF_ground_dist, OF_next_x_dist:OF_next_dist, OF_ground_next_dist:OF_next_velo_angle, 
                                                   speed_95_throw),
                                                 ~ weighted.mean(., if_caught_catch_prob)))
tag_up_data_sum <- tag_up_data_sum %>% slice(1) %>% select(-c(catch_prob, player_code_OF, if_caught_catch_prob))


write.csv(tag_up_data_sum, "tag_up_data_sum.csv", row.names = FALSE)







