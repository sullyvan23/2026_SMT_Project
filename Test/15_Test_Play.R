
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

##################################################################################################################################################################\

ellipse <- expand.grid(runner_basepath_velo = seq(-0.3, 0.3, by = 0.01),
                       runner_basepath_accel = seq(-0.2, 0.2, by = 0.01)) %>%
           mutate(ellipse = (runner_basepath_velo^2 / 0.3^2) +
                            (runner_basepath_accel^2 / 0.2^2)) %>%
           filter(ellipse <= 1)

ellipse <- ellipse %>% mutate(check = ellipse * -runner_basepath_velo^3/3)

ggplot(ellipse, aes(x = runner_basepath_velo, y = runner_basepath_accel, color = check)) + 
       geom_point() + scale_color_gradient2(high = "green", low = "red", mid = "white")

##################################################################################################################################################################


test_play <- one_on_data_sum %>% filter(game_string == "y1_d125_MEX_ANI", play_per_game == 312)  ### 1st, go kinda far, dropped
test_play <- one_on_data_sum %>% filter(game_string == "y1_d211_QHX_ANI", play_per_game == 40) ### tag, decently high caught prob whole time
test_play <- one_on_data_sum %>% filter(game_string == "y1_d073_XPO_PHD", play_per_game == 379) ### 3rd, easy tag
test_play <- one_on_data_sum %>% filter(game_string == "y1_d199_TES_ARN", play_per_game == 197) ### 2nd, tag
test_play <- one_on_data_sum %>% filter(game_string == "y1_d166_FNQ_PHD", play_per_game == 80) ### not caught easily
test_play <- one_on_data_sum %>% filter(game_string == "y1_d120_MKS_ARN", play_per_game == 230) ### not caught easily
test_play <- one_on_data_sum %>% filter(game_string == "y1_d168_BTL_ARN", play_per_game == 123) ### not caught easily
test_play <- one_on_data_sum %>% filter(game_string == "y1_d178_AVV_ARN", play_per_game == 137) ### infield fly
test_play <- one_on_data_sum %>% filter(game_string == "y1_d061_VKA_PHD", play_per_game == 91) ### 1st, succ tag, 60% caught most of time


test_play <- one_on_data_sum %>% filter(game_string == "y1_d182_LRQ_ARN", play_per_game == 160)  ### 1st, doubled up
test_play <- test_play %>% mutate(time_since_hit = time_to_ground - time_left_ground) %>% relocate(time_since_hit, .after = time_left_ground)
test_play <- test_play %>% mutate(doubled_up_prob = 1 - predict(final_doubled_model, newdata = test_play, type = "response"),
                                  tag_up_prob = predict(tag_up_model, newdata = test_play, type = "response"),
                                  advance_one_prob = predict(a1_model, newdata = test_play, type = "response"),
                                  advance_two_prob = predict(a2_model, newdata = test_play, type = "response"))
test_play <- test_play %>% mutate(advance_three_prob = predict(a3_model, newdata = test_play, type = "response"))
test_play <- test_play %>% mutate(doubled = doubled_up_prob * caught_prob,
                                  stay = (1 - doubled_up_prob - tag_up_prob) * caught_prob,
                                  tag = tag_up_prob * caught_prob,
                                  advance_0 = (1 - advance_one_prob) * (1 - caught_prob),
                                  advance_1 = (advance_one_prob - ifelse(player_id_br == 13, 0, advance_two_prob)) * (1 - caught_prob),
                                  advance_2 = (advance_two_prob - ifelse(player_id_br >= 12, 0, advance_three_prob)) * (1 - caught_prob),
                                  advance_3 = advance_three_prob * (1 - caught_prob))
max_speed <- max(test_play$runner_basepath_velo, test_play$speed_95_runner[1]/0.681818/95)
max_accel <- max(max_speed*(95/140), test_play$speed_95_runner[1]/0.681818/140)
test_play <- test_play %>% mutate(run_exp = rowSums(across(doubled:advance_3) * across(d_sit_re:a3_sit_re), na.rm = TRUE),
                                  ellipse = (runner_basepath_velo^2 / max_speed^2) +
                                            (runner_basepath_accel^2 / max_accel^2) +
                                            ifelse(og_basepath_dist <= 0.5  &  runner_basepath_velo <= 0,
                                                   (og_basepath_dist-0.525)^2 / 0.5^2,
                                                    0))

##################################################################################################################################################################
model_play <- test_play[1:4,] %>% mutate(runner_basepath_velo = round(runner_basepath_velo, 4))
fps <- model_play$fps[1]

for(i in 5:nrow(test_play)) {
  accels <- seq(round(model_play$runner_basepath_accel[i-1] + ((1-model_play$ellipse[i-1]^1) * (model_play$runner_basepath_accel[i-1] - model_play$runner_basepath_accel_2[i-1])) +
                      model_play$ellipse[i-1]^1 * ifelse(model_play$og_basepath_dist[i-1] <= 0.5  &  model_play$runner_basepath_velo[i-1] <= 0,
                                                         -(model_play$runner_basepath_velo[i-1]/10) + ((-0.5+model_play$og_basepath_dist[i-1]) * 
                                                                                                        model_play$runner_basepath_accel[i-1]/10),
                                                         -(model_play$runner_basepath_velo[i-1]/80)) -
                      (0.004 * (fps/0.05)), 
                      4),
                round(model_play$runner_basepath_accel[i-1] + ((1-model_play$ellipse[i-1]^1) * (model_play$runner_basepath_accel[i-1] - model_play$runner_basepath_accel_2[i-1])) +
                      model_play$ellipse[i-1]^1 * ifelse(model_play$og_basepath_dist[i-1] <= 0.5  &  model_play$runner_basepath_velo[i-1] <= 0,
                                                         -(model_play$runner_basepath_velo[i-1]/10) + ((-0.5+model_play$og_basepath_dist[i-1]) * 
                                                                                                        model_play$runner_basepath_accel[i-1]/10),
                                                         -(model_play$runner_basepath_velo[i-1]/80)) +
                      (0.004 * (fps/0.05)), 
                      4),
                by = 0.0005)
  
  next_time_check <- test_play[i,] %>% slice(rep(1,length(accels)))
  next_time_check$runner_basepath_accel <- accels
  next_time_check <- next_time_check %>% mutate(runner_basepath_velo = model_play$runner_basepath_velo[i-1] + (runner_basepath_accel*fps),
                                                og_basepath_dist = model_play$og_basepath_dist[i-1] + (runner_basepath_velo*fps),
                                                og_basepath_dist = ifelse(og_basepath_dist < 0.025, 0.025, og_basepath_dist),
                                                basepath = og_basepath_dist + player_id_br - 10,
                                                runner_basepath_accel_2 = model_play$runner_basepath_accel[i-1],
                                                ellipse = (runner_basepath_velo^2 / max_speed^2) +
                                                          (runner_basepath_accel^2 / max_accel^2) +
                                                          ifelse(og_basepath_dist <= 0.5  &  runner_basepath_velo <= 0,
                                                                 (og_basepath_dist-0.525)^2 / 0.5^2,
                                                                  0))

  j <- 0.0001
  while(min(next_time_check$ellipse) > 1) {
    next_time_check <- next_time_check %>% slice_min(ellipse) %>%
                                           mutate(runner_basepath_velo = ifelse(og_basepath_dist == 0.025, 0, runner_basepath_velo))
    ifelse((next_time_check$runner_basepath_velo[1]^2 / max_speed^2) <= (next_time_check$runner_basepath_accel[1]^2 / max_accel^2),
           next_time_check <- next_time_check %>% mutate(runner_basepath_accel = runner_basepath_accel - (sign(runner_basepath_accel) * j)),
           next_time_check <- next_time_check %>% mutate(runner_basepath_velo = runner_basepath_velo - (sign(runner_basepath_velo) * j)))
    
    next_time_check <- next_time_check %>% mutate(ellipse = (runner_basepath_velo^2 / max_speed^2) +
                                                            (runner_basepath_accel^2 / max_accel^2) +
                                                            ((og_basepath_dist-0.525)^2 / 0.5^2))
    j <- j+0.0001
  }
  
  next_time_check <- next_time_check %>% filter(ellipse <= 1)
  next_time_check <- next_time_check %>% mutate(doubled_up_prob = 1 - predict(final_doubled_model, newdata = next_time_check, type = "response"),
                                                tag_up_prob = predict(tag_up_model, newdata = next_time_check, type = "response"),
                                                advance_one_prob = predict(a1_model, newdata = next_time_check, type = "response"),
                                                advance_two_prob = predict(a2_model, newdata = next_time_check, type = "response"))
  next_time_check <- next_time_check %>% mutate(advance_three_prob = predict(a3_model, newdata = next_time_check, type = "response"))
  next_time_check <- next_time_check %>% mutate(doubled = doubled_up_prob * caught_prob,
                                                stay = (1 - doubled_up_prob - tag_up_prob) * caught_prob,
                                                tag = tag_up_prob * caught_prob,
                                                advance_0 = (1 - advance_one_prob) * (1 - caught_prob),
                                                advance_1 = (advance_one_prob - ifelse(player_id_br == 13, 0, advance_two_prob)) * (1 - caught_prob),
                                                advance_2 = (advance_two_prob - ifelse(player_id_br >= 12, 0, advance_three_prob)) * (1 - caught_prob),
                                                advance_3 = advance_three_prob * (1 - caught_prob))
  
  next_time_check <- next_time_check %>% mutate(run_exp = rowSums(across(doubled:advance_3) * across(d_sit_re:a3_sit_re), na.rm = TRUE),
                                                jerk_now = row_number() / nrow(next_time_check)) %>%
                                        arrange(desc(run_exp))

  model_play <- bind_rows(model_play, next_time_check[1,])
}
##################################################################################################################################################################

plot(-test_play$time_left_ground, test_play$basepath, col = "black", ylim = c(min(test_play$basepath,model_play$basepath), max(test_play$basepath,model_play$basepath)))
points(-model_play$time_left_ground, model_play$basepath, col = "red")

ellipse <- expand.grid(runner_basepath_velo = seq(-max_speed, max_speed, by = 0.01),
                       runner_basepath_accel = seq(-max_accel, max_accel, by = 0.01)) %>%
           mutate(ellipse = (runner_basepath_velo^2 / max_speed^2) +
                            (runner_basepath_accel^2 / max_accel^2)) %>%
           filter(ellipse <= 1)
plot(ellipse$runner_basepath_velo, ellipse$runner_basepath_accel, col = "black")
points(test_play$runner_basepath_velo, test_play$runner_basepath_accel, col = "blue")
points(model_play$runner_basepath_velo, model_play$runner_basepath_accel, col = "red")

plot(-test_play$time_left_ground, test_play$run_exp, col = "black", ylim = c(min(test_play$run_exp,model_play$run_exp), max(test_play$run_exp,model_play$run_exp)))
points(-model_play$time_left_ground, model_play$run_exp, col = "red")


plot(test_play$runner_basepath_velo, test_play$runner_basepath_accel, col = "black")
plot(model_play$runner_basepath_velo, model_play$runner_basepath_accel, col = "red")

plot(test_play$og_basepath_dist, test_play$runner_basepath_velo, col = "black")
points(model_play$og_basepath_dist, model_play$runner_basepath_velo, col = "red")



ellipse <- expand.grid(runner_basepath_velo = seq(-max_speed, max_speed, by = 0.01),
                       runner_basepath_accel = seq(-max_accel, max_accel, by = 0.01),
                       og_basepath_dist = seq(0, 0.25, by = 0.01)) %>%
           mutate(ellipse = (runner_basepath_velo^2 / max_speed^2) +
                            (runner_basepath_accel^2 / max_accel^2) +
                            ((og_basepath_dist-0.525)^2 / 0.5^2)) %>%
           filter(ellipse <= 1)
plot(ellipse$og_basepath_dist, ellipse$runner_basepath_velo, col = "black")
points(test_play$og_basepath_dist, test_play$runner_basepath_velo, col = "blue")
points(model_play$og_basepath_dist, model_play$runner_basepath_velo, col = "red")

plot(ellipse$og_basepath_dist, ellipse$runner_basepath_accel, col = "black")
points(test_play$og_basepath_dist, test_play$runner_basepath_accel, col = "blue")
points(model_play$og_basepath_dist, model_play$runner_basepath_accel, col = "red")




ellipse <- expand.grid(runner_basepath_velo = seq(-max_speed, max_speed, by = 0.01),
                       runner_basepath_accel = seq(-max_accel, max_accel, by = 0.01)) %>%
           mutate(ellipse = (runner_basepath_velo^2 / max_speed^2) +
                            (runner_basepath_accel^2 / max_accel^2)) %>%
           filter(ellipse <= 1)
plot(ellipse$runner_basepath_velo, ellipse$runner_basepath_accel, col = "black")
points(test_play$runner_basepath_velo, test_play$runner_basepath_accel, col = "blue")
points(model_play$runner_basepath_velo, model_play$runner_basepath_accel, col = "red")






