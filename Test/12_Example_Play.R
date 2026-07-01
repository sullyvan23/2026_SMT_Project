
example_play <- tag_up_data_sum %>% filter(game_string == "y1_d061_VKA_PHD", play_per_game == 91, player_id_br == 11)

example_play <- example_play %>% mutate(next2_base_x = x_3b,
                                        next2_base_y = y_3b,
                                        next3_base_x = x_home,
                                        next3_base_y = y_home) %>%
                                 relocate(next2_base_x:next3_base_y, .after = next_base_y)
example_play <- example_play %>% mutate(dist_1st = sqrt((pred_x_runner - x_1b)^2 + (pred_y_runner - y_1b)^2),
                                        dist_2nd = sqrt((pred_x_runner - x_2b)^2 + (pred_y_runner - y_2b)^2),
                                        dist_3rd = sqrt((pred_x_runner - x_3b)^2 + (pred_y_runner - y_3b)^2),
                                        dist_home = sqrt((pred_x_runner - x_home)^2 + (pred_y_runner - y_home)^2))
example_play <- example_play %>% mutate(basepath = case_when(pred_y_runner < 0 | (pred_y_runner < 50 & pred_x_runner > 0)  ~  4, 
                                                             pred_y_runner >= 50 & pred_x_runner > x_2b  ~  1 + (dist_1st / (dist_1st + dist_2nd)),
                                                             pred_y_runner >= y_3b & pred_x_runner <= x_2b  ~  2 + (dist_2nd / (dist_2nd + dist_3rd)),
                                                             pred_y_runner < y_3b & pred_x_runner <= x_home  ~  3 + (dist_3rd / (dist_3rd + dist_home)) ))
example_play <- example_play %>% mutate(og_basepath_dist = basepath - player_id_br + 10)
example_play <- example_play %>% mutate(OF_og_x_dist = pred_x_OF - og_base_x,
                                        OF_og_y_dist = pred_y_OF - og_base_y,
                                        OF_og_dist = sqrt(OF_og_x_dist^2 + OF_og_y_dist^2),
                                        ground_og_dist = sqrt((ground_x - og_base_x)^2 + (ground_y - og_base_y)^2),
                                        OF_ground_og_dist = ((OF_og_x_dist * OF_ground_x_dist) + (OF_og_y_dist * OF_ground_y_dist)) / 
                                                            OF_og_dist,
                                        OF_ground_og_angle = acos(OF_ground_og_dist / OF_ground_dist),
                                        OF_next2_x_dist = pred_x_OF - next2_base_x,
                                        OF_next2_y_dist = pred_y_OF - next2_base_y,
                                        OF_next2_dist = sqrt(OF_next2_x_dist^2 + OF_next2_y_dist^2),
                                        ground_next2_dist = sqrt((ground_x - next2_base_x)^2 + (ground_y - next2_base_y)^2),
                                        OF_ground_next2_dist = ((OF_next2_x_dist * OF_ground_x_dist) + (OF_next2_y_dist * OF_ground_y_dist)) / 
                                                              OF_next2_dist,
                                        OF_ground_next2_angle = acos(OF_ground_next2_dist / OF_ground_dist),
                                        ground_next3_dist = sqrt((ground_x - next3_base_x)^2 + (ground_y - next3_base_y)^2))

example_play <- example_play %>% mutate(basepath_velo = (basepath - lag(basepath)) / ((timestamp - lag(timestamp))/1000),
                                        basepath_velo = ifelse(is.na(basepath_velo), lead(basepath_velo), basepath_velo))



time_data <- example_play[1,] %>% select(game_string, play_per_game, player_id_br, timestamp, time_left_ground, time_to_ground, 
                                         basepath, ground_og_dist, ground_next_dist, ground_next2_dist, ground_next3_dist,
                                         runner_og_velo, OF_ground_og_angle, OF_ground_next_angle, OF_ground_next2_angle,
                                         og_basepath_dist, caught_prob, speed_95_runner, speed_95_throw)
time_data <- time_data %>% mutate(field_x = predict(basepath_x_model, newdata = time_data),
                                  field_y = predict(basepath_y_model, newdata = time_data),
                                  runner_og_dist = sqrt((field_x - x_1b)^2 + (field_y - y_1b)^2))






