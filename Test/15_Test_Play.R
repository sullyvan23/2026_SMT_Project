
test_play <- one_on_data_sum[1,]

next_time_check <- test_play %>% slice(rep(1,21))
next_time_check <- next_time_check %>% mutate(time_left_ground = time_left_ground - 0.05,
                                              runner_basepath_velo = runner_basepath_velo + ((row_number() - 11) * 0.001),
                                              basepath = basepath + (runner_basepath_velo*0.05),
                                              og_basepath_dist = basepath - player_id_br + 10)

next_time_check <- next_time_check %>% mutate(doubled_up_prob = 1 - predict(doubled_up_model, newdata = next_time_check, type = "response"),
                                              tag_up_prob = predict(tag_up_model, newdata = next_time_check, type = "response"),
                                              advance_one_prob = predict(advance_one_model, newdata = next_time_check, type = "response"),
                                              advance_two_prob = predict(advance_two_model, newdata = next_time_check, type = "response"))
next_time_check <- next_time_check %>% mutate(advance_three_prob = predict(advance_three_model, newdata = next_time_check, type = "response"))
next_time_check <- next_time_check %>% mutate(doubled = doubled_up_prob * caught_prob,
                                              stay = (1 - doubled_up_prob - tag_up_prob) * caught_prob,
                                              tag = tag_up_prob * caught_prob,
                                              advance_0 = (1 - advance_one_prob) * (1 - caught_prob),
                                              advance_1 = (advance_one_prob - advance_two_prob) * (1 - caught_prob),
                                              advance_2 = (advance_two_prob - advance_three_prob) * (1 - caught_prob),
                                              advance_3 = advance_three_prob * (1 - caught_prob))

next_time_check <- next_time_check %>% mutate(run_exp = rowSums(across(doubled:advance_3) * across(d_sit_re:a3_sit_re)))

