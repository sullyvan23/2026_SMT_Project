
#### gathering all runner data
doubled_up_data <- doubled_up_results %>% left_join(final_catch_prob_results, by = c("game_string", "play_per_game"))

doubled_up_data <- doubled_up_data %>% left_join(player_positions[,1:6], by = c("game_string", "play_per_game", "timestamp", "player_id_br" = "player_id"))
doubled_up_data <- doubled_up_data %>% group_by(game_string, play_per_game, player_id_br) %>% filter(sum(is.na(field_x)) == 0)

### finding origibal base positions
doubled_up_data <- doubled_up_data %>% mutate(og_base_x = case_when(player_id_br == 11  ~  x_1b,
                                                                    player_id_br == 12  ~  x_2b,
                                                                    player_id_br == 13  ~  x_3b),
                                              og_base_y = case_when(player_id_br == 11  ~  y_1b,
                                                                    player_id_br == 12  ~  y_2b,
                                                                    player_id_br == 13  ~  y_3b))
### calculating basepath
doubled_up_data <- doubled_up_data %>% mutate(dist_1st = sqrt((field_x - x_1b)^2 + (field_y - y_1b)^2),
                                              dist_2nd = sqrt((field_x - x_2b)^2 + (field_y - y_2b)^2),
                                              dist_3rd = sqrt((field_x - x_3b)^2 + (field_y - y_3b)^2),
                                              dist_home = sqrt((field_x - x_home)^2 + (field_y - y_home)^2))
doubled_up_data <- doubled_up_data %>% mutate(basepath = case_when(field_y < 0 | (field_y < 50 & field_x > 0)  ~  4, 
                                                                   field_y >= 50 & field_x > x_2b  ~  1 + (dist_1st / (dist_1st + dist_2nd)),
                                                                   field_y >= y_3b & field_x <= x_2b  ~  2 + (dist_2nd / (dist_2nd + dist_3rd)),
                                                                   field_y < y_3b & field_x <= x_home  ~  3 + (dist_3rd / (dist_3rd + dist_home)) ))

### using predicted basepath from GAM for smoother velocities, especially when getting near base
group <- 0
doubled_up_data <- doubled_up_data %>% group_by(game_string, play_per_game, player_id_br) %>%
                 group_modify(~{group <<- group + 1
                              message(group/nrow(doubled_up_results))
                                  
                              basepath_model <- gam(basepath ~ s(timestamp, k = 10), data = .x )

                              .x$pred_basepath <- predict(basepath_model, newdata = .x) 

                              .x$rmse_bp <- RMSE(.x$pred_basepath, .x$basepath)

                              .x})
doubled_up_data <- doubled_up_data %>% select(-basepath) %>% rename(basepath = pred_basepath)

### getting all needed data from catch probablity that could affect the play
doubled_up_data <- doubled_up_data %>% left_join(catch_prob_data %>% select(game_string, play_per_game, player_id, timestamp, pred_x, pred_y, OF_x_velo, OF_y_velo, OF_velo,
                                                                            time_to_ground, time_left_ground, ground_x, ground_y, OF_ground_x_dist, OF_ground_y_dist, OF_ground_dist, 
                                                                            wall_ground_dist, player_code),
                                                 by = c("game_string", "play_per_game", "timestamp", "player_id"))

### getting runner velos and acces, along with distances of OF and ground
### used ground dist for all distances now for simplicity
doubled_up_data <- doubled_up_data %>% mutate(runner_basepath_velo = (basepath - lag(basepath)) / ((timestamp - lag(timestamp))/1000),
                                              runner_basepath_velo = ifelse(is.na(runner_basepath_velo), lead(runner_basepath_velo), runner_basepath_velo),
                                              runner_basepath_accel = (runner_basepath_velo - lag(runner_basepath_velo)) / ((timestamp - lag(timestamp))/1000),
                                              runner_basepath_accel = ifelse(is.na(runner_basepath_accel), lead(runner_basepath_accel), runner_basepath_accel),
                                              og_basepath_dist = basepath - player_id_br + 10,
                                              OF_og_x_dist = pred_x - og_base_x,
                                              OF_og_y_dist = pred_y - og_base_y,
                                              OF_og_dist = sqrt(OF_og_x_dist^2 + OF_og_y_dist^2),
                                              ground_og_dist = sqrt((ground_x - og_base_x)^2 + (ground_y - og_base_y)^2))

## getting angle of fielder needed direction and velocity relative to direction towards original base
doubled_up_data <- doubled_up_data %>% mutate(OF_ground_og_dist = ((OF_og_x_dist * OF_ground_x_dist) + (OF_og_y_dist * OF_ground_y_dist)) / 
                                                                  OF_og_dist,
                                              OF_ground_og_angle = acos(OF_ground_og_dist / OF_ground_dist),
                                              OF_og_velo = -((OF_og_x_dist * OF_x_velo) + (OF_og_y_dist * OF_y_velo)) / 
                                                            OF_og_dist,
                                              OF_og_velo_angle = acos(OF_og_velo / OF_velo))

### getting speed of runner and speed of fielder throw
doubled_up_data <- doubled_up_data %>% relocate(player_code, .after = OF_og_velo_angle)
doubled_up_data <- doubled_up_data %>% left_join(lineups_players[,c(1,7,9:10)], by = c("game_string", "play_per_game", "player_id_br" = "player_id"),
                                                 suffix = c("_OF", "_runner"))
doubled_up_data <- doubled_up_data %>% left_join(throw_speed[,1:2], by = c("player_code_OF" = "player_code"))
doubled_up_data <- doubled_up_data %>% left_join(player_speed[,1:2], by = c("player_code_runner" = "player_code"), suffix = c("_throw", "_runner"))
doubled_up_data <- doubled_up_data %>% mutate(speed_95_throw = ifelse(is.na(speed_95_throw), mean(throw_speed$speed_95), speed_95_throw),
                                              speed_95_runner = ifelse(is.na(speed_95_runner), mean(player_speed$speed_95), speed_95_runner))

### how many other runners are there for potential model use
doubled_up_data <- doubled_up_data %>% left_join(baserunners, by = c("game_string", "play_per_game"))
doubled_up_data <- doubled_up_data %>% mutate(other_runners = first + second + third - 1) %>%
                                       select(-c(first:third))

### seeing probability the fielder will catc the ball given the ball is caught
doubled_up_data <- doubled_up_data %>% group_by(game_string, play_per_game, player_id_br, timestamp) %>%
                                       mutate(if_caught_catch_prob = catch_prob / sum(catch_prob),
                                              caught_prob = sum(catch_prob))


write.csv(doubled_up_data, "doubled_up_data.csv", row.names = FALSE)

##############################################################################################################################################################################################

### weighing fielder statistics based on if_caught_catch_prob, now only one row per timestamp
doubled_up_data_sum <- doubled_up_data %>% mutate(across(c(player_id, pred_x:OF_velo, OF_ground_x_dist:OF_ground_dist, OF_og_x_dist:OF_og_dist, OF_ground_og_dist:OF_og_velo_angle, 
                                                           speed_95_throw),
                                                         ~ weighted.mean(., if_caught_catch_prob)))
doubled_up_data_sum <- doubled_up_data_sum %>% slice(1) %>% select(-c(catch_prob, player_code_OF, if_caught_catch_prob))


write.csv(doubled_up_data_sum, "doubled_up_data_sum.csv", row.names = FALSE)



