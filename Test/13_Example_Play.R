
example_play <- tag_up_data_sum %>% filter(game_string == "y1_d061_VKA_PHD", play_per_game == 91, player_id_br == 11)

example_play <- example_play %>% mutate(next2_base_x = x_3b,
                                        next2_base_y = y_3b,
                                        next3_base_x = x_home,
                                        next3_base_y = y_home) %>%
                                 relocate(next2_base_x:next3_base_y, .after = next_base_y)
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



example_time <- example_play[43,] %>% select(game_string, play_per_game, player_id_br, timestamp, time_left_ground, time_to_ground, ground_x, ground_y,
                                             basepath, ground_og_dist, ground_next_dist, ground_next2_dist, ground_next3_dist,
                                             runner_basepath_velo, OF_ground_og_angle, OF_ground_next_angle, OF_ground_next2_angle,
                                             og_basepath_dist, caught_prob, speed_95_runner, speed_95_throw)
example_time <- example_time %>% slice(rep(1,21))
example_time <- example_time %>% mutate(basepath = 1.05 + (row_number())/20,
                                        og_basepath_dist = basepath - 1,
                                        batter_final_base = predict(batter_bases_model, newdata = example_time))

example_time <- example_time %>% mutate(doubled_up_prob = 1 - predict(doubled_up_model, newdata = example_time, type = "response"),
                                        tag_up_prob = predict(tag_up_model, newdata = example_time, type = "response"),
                                        advance_one_prob = predict(advance_one_model, newdata = example_time, type = "response"),
                                        advance_two_prob = predict(advance_two_model, newdata = example_time, type = "response"))
example_time <- example_time %>% mutate(advance_three_prob = predict(advance_three_model, newdata = example_time, type = "response"))


example_time <- example_time %>% mutate(doubled = doubled_up_prob * caught_prob,
                                        stay = (1 - doubled_up_prob - tag_up_prob) * caught_prob,
                                        tag = tag_up_prob * caught_prob,
                                        advance_0 = (1 - advance_one_prob) * (1 - caught_prob),
                                        advance_1 = (advance_one_prob - advance_two_prob) * (1 - caught_prob),
                                        advance_2 = (advance_two_prob - advance_three_prob) * (1 - caught_prob),
                                        advance_3 = advance_three_prob * (1 - caught_prob))


example_time <- example_time %>% mutate(d_sit = "2.5 ___",
                                        s_sit = "1.5 1__",
                                        t_sit = "1.5 _2_",
                                        a0_sit = "1.5 1__",
                                        a1_sit = "0.5 12_",
                                        a2_sit = "0.5 _23",
                                        a3_sit = "0.5 _2_",
                                        add_a3 = 1)

### write.csv(example_time, "example_time.csv", row.names = FALSE)
### adding run expectancies

### example_time <- read_csv("example_time.csv")






