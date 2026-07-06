
test_play <- one_on_data_sum %>% filter(game_string == "y1_d155_VAS_PHD", play_per_game == 266)

next_time_check <- test_play[2,] %>% slice(rep(1,21))
next_time_check <- next_time_check %>% mutate(runner_basepath_velo = test_play$runner_basepath_velo[1] + ((row_number() - 11) * ((0.1 * fps)/10)),
                                              og_basepath_dist = test_play$og_basepath_dist[1] + (runner_basepath_velo * fps),
                                              basepath = og_basepath_dist - player_id_br + 12)

next_time_check <- next_time_check %>% mutate(doubled_up_prob = 1 - predict(final_doubled_model, newdata = next_time_check, type = "response"),
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

next_time_check <- next_time_check %>% mutate(run_exp = rowSums(across(doubled:advance_3) * across(d_sit_re:a3_sit_re))) %>%
                                      arrange(desc(run_exp))

##################################################################################################################################################################

test_play <- one_on_data_sum %>% filter(game_string == "y1_d061_VKA_PHD", play_per_game == 91) ### 1st, succ tag, 60% caught most of time
test_play <- one_on_data_sum %>% filter(game_string == "y1_d182_LRQ_ARN", play_per_game == 160)  ### 1st, doubled up
test_play <- one_on_data_sum %>% filter(game_string == "y1_d125_MEX_ANI", play_per_game == 312)  ### 1st, go kinda far, caught


test_play <- one_on_data_sum %>% filter(game_string == "y1_d211_QHX_ANI", play_per_game == 40) ### tag, high caught prob whole time
test_play <- test_play %>% mutate(doubled_up_prob = 1 - predict(final_doubled_model, newdata = test_play, type = "response"),
                                  tag_up_prob = predict(tag_up_model, newdata = test_play, type = "response"),
                                  advance_one_prob = predict(a1_model, newdata = test_play, type = "response"),
                                  advance_two_prob = predict(a2_model, newdata = test_play, type = "response"))
test_play <- test_play %>% mutate(advance_three_prob = predict(a3_model, newdata = test_play, type = "response"))
test_play <- test_play %>% mutate(doubled = doubled_up_prob * caught_prob,
                                  stay = (1 - doubled_up_prob - tag_up_prob) * caught_prob,
                                  tag = tag_up_prob * caught_prob,
                                  advance_0 = (1 - advance_one_prob) * (1 - caught_prob),
                                  advance_1 = (advance_one_prob - advance_two_prob) * (1 - caught_prob),
                                  advance_2 = (advance_two_prob - advance_three_prob) * (1 - caught_prob),
                                  advance_3 = advance_three_prob * (1 - caught_prob))

test_play <- test_play %>% mutate(run_exp = rowSums(across(doubled:advance_3) * across(d_sit_re:a3_sit_re)))

##################################################################################################################################################################

model_play <- test_play[1,]

for(i in 2:nrow(test_play)) {
  speeds <- seq(model_play$runner_basepath_velo[i-1] + 
            round((as.numeric(predict(lowest_next_velo_model, newdata = data.frame(basepath_velo = model_play$runner_basepath_velo[i-1])))) 
                  * (model_play$fps[i-1] / 0.05), 4),
            model_play$runner_basepath_velo[i-1] + 
            round((as.numeric(predict(highest_next_velo_model, newdata = data.frame(basepath_velo = model_play$runner_basepath_velo[i-1])))) 
                  * (model_play$fps[i-1] / 0.05), 4),
            by = 0.0001)
  
  next_time_check <- test_play[i,] %>% slice(rep(1,length(speeds)))
  next_time_check$runner_basepath_velo <- speeds
  next_time_check <- next_time_check %>% mutate(runner_basepath_velo = ifelse(runner_basepath_velo > 0.3, 0.3, runner_basepath_velo),
                                                og_basepath_dist = model_play$og_basepath_dist[i-1] + (runner_basepath_velo*fps),
                                                og_basepath_dist = ifelse(og_basepath_dist < 0, 0, og_basepath_dist),
                                                basepath = og_basepath_dist - player_id_br + 12)
  
  next_time_check <- next_time_check %>% mutate(doubled_up_prob = 1 - predict(final_doubled_model, newdata = next_time_check, type = "response"),
                                                tag_up_prob = predict(tag_up_model, newdata = next_time_check, type = "response"),
                                                advance_one_prob = predict(a1_model, newdata = next_time_check, type = "response"),
                                                advance_two_prob = predict(a2_model, newdata = next_time_check, type = "response"))
  next_time_check <- next_time_check %>% mutate(advance_three_prob = predict(a3_model, newdata = next_time_check, type = "response"))
  next_time_check <- next_time_check %>% mutate(doubled = doubled_up_prob * caught_prob,
                                                stay = (1 - doubled_up_prob - tag_up_prob) * caught_prob,
                                                tag = tag_up_prob * caught_prob,
                                                advance_0 = (1 - advance_one_prob) * (1 - caught_prob),
                                                advance_1 = (advance_one_prob - advance_two_prob) * (1 - caught_prob),
                                                advance_2 = (advance_two_prob - advance_three_prob) * (1 - caught_prob),
                                                advance_3 = advance_three_prob * (1 - caught_prob))
  
  next_time_check <- next_time_check %>% mutate(run_exp = rowSums(across(doubled:advance_3) * across(d_sit_re:a3_sit_re)),
                                                speed_percentile = row_number()/n()) %>%
                                        arrange(desc(run_exp))

  model_play <- bind_rows(model_play, next_time_check[1,])
}


plot(-test_play$time_left_ground, test_play$basepath, col = "black", ylim = c(min(test_play$basepath,model_play$basepath), max(test_play$basepath,model_play$basepath)))
points(-model_play$time_left_ground, model_play$basepath, col = "red")


plot(-test_play$time_left_ground, test_play$run_exp, col = "black", ylim = c(min(test_play$run_exp,model_play$run_exp), max(test_play$run_exp,model_play$run_exp)))
points(-model_play$time_left_ground, model_play$run_exp, col = "red")












