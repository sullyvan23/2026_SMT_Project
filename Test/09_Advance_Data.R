
advance_data <- ball_down_results %>% select(-force) %>% left_join(final_catch_prob_results, by = c("game_string", "play_per_game"))


advance_data <- advance_data %>% left_join(player_positions[,1:6], by = c("game_string", "play_per_game", "timestamp", "player_id_br" = "player_id"))
advance_data <- advance_data %>% group_by(game_string, play_per_game, player_id_br) %>% filter(sum(is.na(field_x)) == 0)

advance_data <- advance_data %>% mutate(dist_1st = sqrt((field_x - x_1b)^2 + (field_y - y_1b)^2),
                                        dist_2nd = sqrt((field_x - x_2b)^2 + (field_y - y_2b)^2),
                                        dist_3rd = sqrt((field_x - x_3b)^2 + (field_y - y_3b)^2),
                                        dist_home = sqrt((field_x - x_home)^2 + (field_y - y_home)^2))
advance_data <- advance_data %>% mutate(basepath = case_when(field_y < 0 | (field_y < 50 & field_x > 0)  ~  4, 
                                                             field_y >= 50 & field_x > x_2b  ~  1 + (dist_1st / (dist_1st + dist_2nd)),
                                                             field_y >= y_3b & field_x <= x_2b  ~  2 + (dist_2nd / (dist_2nd + dist_3rd)),
                                                             field_y < y_3b & field_x <= x_home  ~  3 + (dist_3rd / (dist_3rd + dist_home)) ))

group <- 0
advance_data <- advance_data %>% group_by(game_string, play_per_game, player_id_br) %>%
                 group_modify(~{group <<- group + 1
                              message(group/nrow(tag_results))
                                  
                              basepath_model <- gam(basepath ~ s(timestamp, k = 10), data = .x )

                              .x$pred_basepath <- predict(basepath_model, newdata = .x) 

                              .x$rmse_bp <- RMSE(.x$pred_basepath, .x$basepath)

                              .x})

advance_data <- advance_data %>% select(-basepath) %>% rename(basepath = pred_basepath)

advance_data <- advance_data %>% left_join(catch_prob_data %>% select(game_string, play_per_game, player_id, timestamp, pred_x, pred_y, OF_x_velo, OF_y_velo, OF_velo,
                                                                      time_to_ground, time_left_ground, ground_x, ground_y, OF_ground_x_dist, OF_ground_y_dist, OF_ground_dist, 
                                                                      player_code),
                                           by = c("game_string", "play_per_game", "timestamp", "player_id"),
                                           suffix = c("_runner", "_OF"))

advance_data <- advance_data %>% mutate(runner_basepath_velo = (basepath - lag(basepath)) / ((timestamp - lag(timestamp))/1000),
                                        runner_basepath_velo = ifelse(is.na(runner_basepath_velo), lead(runner_basepath_velo), runner_basepath_velo),
                                        runner_basepath_accel = (runner_basepath_velo - lag(runner_basepath_velo)) / ((timestamp - lag(timestamp))/1000),
                                        runner_basepath_accel = ifelse(is.na(runner_basepath_accel), lead(runner_basepath_accel), runner_basepath_accel),
                                        og_basepath_dist = basepath - player_id_br + 10)

advance_data <- advance_data %>% relocate(player_code, .after = og_basepath_dist)
advance_data <- advance_data %>% left_join(lineups_players[,c(1,7,9:10)], by = c("game_string", "play_per_game", "player_id_br" = "player_id"),
                                           suffix = c("_OF", "_runner"))
advance_data <- advance_data %>% left_join(throw_speed[,1:2], by = c("player_code_OF" = "player_code"))
advance_data <- advance_data %>% left_join(player_speed[,1:2], by = c("player_code_runner" = "player_code"), suffix = c("_throw", "_runner"))
advance_data <- advance_data %>% mutate(speed_95_throw = ifelse(is.na(speed_95_throw), mean(throw_speed$speed_95), speed_95_throw),
                                        speed_95_runner = ifelse(is.na(speed_95_runner), mean(player_speed$speed_95), speed_95_runner))
  
advance_data <- advance_data %>% left_join(baserunners, by = c("game_string", "play_per_game"))
advance_data <- advance_data %>% mutate(runners_front = case_when(player_id_br == 11  ~  ifelse(second == 1, second + third, 0),
                                                                  player_id_br == 12  ~  third,
                                                                  player_id_br == 13  ~  0)) %>%
                                 select(-c(first:third))

advance_data <- advance_data %>% group_by(game_string, play_per_game, player_id_br, timestamp) %>%
                                 mutate(if_caught_catch_prob = ifelse(sum(catch_prob) == 0,
                                                                      OF_ground_dist / sum(OF_ground_dist),
                                                                      catch_prob / sum(catch_prob)),
                                        caught_prob = sum(catch_prob))

advance_data <- advance_data %>% group_by(game_string, play_per_game, player_id_br, timestamp) %>%
                                 mutate(caught_prob = sum(catch_prob),
                                        if_caught_catch_prob = ifelse(caught_prob == 0,
                                                                      OF_ground_dist / sum(OF_ground_dist),
                                                                      catch_prob / caught_prob))

check <- advance_data %>% filter(game_string == "y1_d062_VKA_PHD", play_per_game == 217, timestamp == 6557434)

##############################################################################################################################################################################################

advance_one_data <- advance_data
advance_two_data <- advance_data %>% filter(player_id_br <= 12)
advance_three_data <- advance_data %>% filter(player_id_br == 11)


advance_one_data <- advance_one_data %>% mutate(next_base_x = case_when(player_id_br == 11  ~  x_2b,
                                                                        player_id_br == 12  ~  x_3b,
                                                                        player_id_br == 13  ~  x_home),
                                                next_base_y = case_when(player_id_br == 11  ~  y_2b,
                                                                        player_id_br == 12  ~  y_3b,
                                                                        player_id_br == 13  ~  y_home))
advance_two_data <- advance_two_data %>% mutate(next2_base_x = case_when(player_id_br == 11  ~  x_3b,
                                                                        player_id_br == 12  ~  x_home),
                                                next2_base_y = case_when(player_id_br == 11  ~  y_3b,
                                                                        player_id_br == 12  ~  y_home))
advance_three_data <- advance_three_data %>% mutate(next3_base_x = x_home,
                                                    next3_base_y = y_home)


advance_one_data <- advance_one_data %>% mutate(OF_next_x_dist = pred_x - next_base_x,
                                                OF_next_y_dist = pred_y - next_base_y,
                                                OF_next_dist = sqrt(OF_next_x_dist^2 + OF_next_y_dist^2),
                                                ground_next_dist = sqrt((ground_x - next_base_x)^2 + (ground_y - next_base_y)^2),          
                                                OF_ground_next_dist = ((OF_next_x_dist * OF_ground_x_dist) + (OF_next_y_dist * OF_ground_y_dist)) / 
                                                                      OF_next_dist,
                                                OF_ground_next_angle = acos(OF_ground_next_dist / OF_ground_dist),
                                                OF_next_velo = -((OF_next_x_dist * OF_x_velo) + (OF_next_y_dist * OF_y_velo)) / 
                                                               OF_next_dist,
                                                OF_next_velo_angle = acos(OF_next_velo / OF_velo))
advance_two_data <- advance_two_data %>% mutate(OF_next2_x_dist = pred_x - next2_base_x,
                                                OF_next2_y_dist = pred_y - next2_base_y,
                                                OF_next2_dist = sqrt(OF_next2_x_dist^2 + OF_next2_y_dist^2),
                                                ground_next2_dist = sqrt((ground_x - next2_base_x)^2 + (ground_y - next2_base_y)^2),
                                                OF_ground_next2_dist = ((OF_next2_x_dist * OF_ground_x_dist) + (OF_next2_y_dist * OF_ground_y_dist)) / 
                                                                      OF_next2_dist,
                                                OF_ground_next2_angle = acos(OF_ground_next2_dist / OF_ground_dist),
                                                OF_next2_velo = -((OF_next2_x_dist * OF_x_velo) + (OF_next2_y_dist * OF_y_velo)) / 
                                                               OF_next2_dist,
                                                OF_next2_velo_angle = acos(OF_next2_velo / OF_velo))
advance_three_data <- advance_three_data %>% mutate(OF_next3_x_dist = pred_x - next3_base_x,
                                                    OF_next3_y_dist = pred_y - next3_base_y,
                                                    OF_next3_dist = sqrt(OF_next3_x_dist^2 + OF_next3_y_dist^2),
                                                    ground_next3_dist = sqrt((ground_x - next3_base_x)^2 + (ground_y - next3_base_y)^2),
                                                    OF_ground_next3_dist = ((OF_next3_x_dist * OF_ground_x_dist) + (OF_next3_y_dist * OF_ground_y_dist)) / 
                                                                      OF_next3_dist,
                                                    OF_ground_next3_angle = acos(OF_ground_next3_dist / OF_ground_dist),
                                                    OF_next3_velo = -((OF_next3_x_dist * OF_x_velo) + (OF_next3_y_dist * OF_y_velo)) / 
                                                                   OF_next3_dist,
                                                    OF_next3_velo_angle = acos(OF_next3_velo / OF_velo))


advance_one_data <- advance_one_data %>% mutate(advance_one = ifelse(succ_bases_advanced >= 1, 1, 0))
advance_two_data <- advance_two_data %>% mutate(advance_two = ifelse(succ_bases_advanced >= 2, 1, 0))
advance_three_data <- advance_three_data %>% mutate(advance_three = ifelse(succ_bases_advanced == 3, 1, 0))

##############################################################################################################################################################################################

advance_one_data_sum <- advance_one_data %>% mutate(across(c(player_id, pred_x:OF_velo, OF_ground_x_dist:OF_ground_dist, OF_next_x_dist:OF_next_dist, OF_ground_next_dist:OF_next_velo_angle, 
                                                             speed_95_throw),
                                                    ~ weighted.mean(., if_caught_catch_prob, na.rm = TRUE)))
advance_one_data_sum <- advance_one_data_sum %>% slice(1) %>% select(-c(catch_prob, player_code_OF, if_caught_catch_prob))

advance_two_data_sum <- advance_two_data %>% mutate(across(c(player_id, pred_x:OF_velo, OF_ground_x_dist:OF_ground_dist, OF_next2_x_dist:OF_next2_dist, 
                                                             OF_ground_next2_dist:OF_next2_velo_angle, speed_95_throw),
                                                    ~ weighted.mean(., if_caught_catch_prob, na.rm = TRUE)))
advance_two_data_sum <- advance_two_data_sum %>% slice(1) %>% select(-c(catch_prob, player_code_OF, if_caught_catch_prob))

advance_three_data_sum <- advance_three_data %>% mutate(across(c(player_id, pred_x:OF_velo, OF_ground_x_dist:OF_ground_dist, OF_next3_x_dist:OF_next3_dist, 
                                                                 OF_ground_next3_dist:OF_next3_velo_angle, speed_95_throw),
                                                        ~ weighted.mean(., if_caught_catch_prob, na.rm = TRUE)))
advance_three_data_sum <- advance_three_data_sum %>% slice(1) %>% select(-c(catch_prob, player_code_OF, if_caught_catch_prob))



write.csv(advance_one_data_sum, "advance_one_data_sum.csv", row.names = FALSE)
write.csv(advance_two_data_sum, "advance_two_data_sum.csv", row.names = FALSE)
write.csv(advance_three_data_sum, "advance_three_data_sum.csv", row.names = FALSE)












