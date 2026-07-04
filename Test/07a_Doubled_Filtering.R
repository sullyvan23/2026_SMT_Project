
doubled_up_final <- doubled_up_data_sum_pred %>% group_by(game_string, play_per_game, player_id_br) %>% slice(n())

ggplot(doubled_up_final, aes(x = og_basepath_dist, y = ground_og_dist, color = safe_back)) + 
       geom_point() + scale_color_gradient2(high = "green", low = "red", mid = "white", midpoint = 0.5)

doubled_up_final <- doubled_up_final %>% rename(pred_final_basepath_dist = og_basepath_dist,
                                                pred_final_velo = runner_basepath_velo)



set.seed(883)
final_doubled_folds <- createFolds(doubled_up_final$safe_back, k = 5)


act <- c()
pred <- c()
for(fold in final_doubled_folds) {
  print("-")
  train <- doubled_up_final[-fold, ]
  test <- doubled_up_final[fold, ]
  model <- gam(safe_back ~ pred_final_basepath_dist + ground_og_dist + pred_final_velo, 
               family = binomial, data = train)
  act <- c(act, test$safe_back)
  pred <- c(pred, predict(model, newdata = test, type = "response"))
}
logLoss(act, pred)
### 0.0302579

plot(model, page = 1)
summary(model)


final_doubled_model <- gam(safe_back ~ pred_final_basepath_dist + ground_og_dist + pred_final_velo, 
                           family = binomial, data = doubled_up_final)
doubled_up_final <- doubled_up_final %>% ungroup() %>% mutate(final_doubled_prob = 1-predict(final_doubled_model, type = "response"))

####################################################################################################################################################################

doubled_by_time <- doubled_up_data_sum_pred %>% filter(game_string != "y1_d172_OWV_VAS"  &  play_per_game != 226) %>%
                                                mutate(time_left_ground = round(time_left_ground)) %>% group_by(safe_back, time_left_ground) %>%
                                                summarise(og_basepath_dist = mean(og_basepath_dist),
                                                          runner_basepath_velo = mean(runner_basepath_velo))

doubled_by_time <- doubled_up_data_sum_2 %>% filter(game_string != "y1_d172_OWV_VAS"  &  play_per_game != 226) %>%
                                                mutate(time_left_ground = round(time_left_ground)) %>% group_by(safe_back, time_left_ground) %>%
                                                summarise(og_basepath_dist = mean(og_basepath_dist),
                                                          runner_basepath_velo = mean(runner_basepath_velo))

####################################################################################################################################################################

doubled_up_data_sum_2 <- doubled_up_data_sum %>% group_by(game_string, play_per_game, player_id_br) %>%
                                                 mutate(final_basepath_dist = last(og_basepath_dist),
                                                        final_velo = last(runner_basepath_velo))

ggplot(doubled_up_data_sum_2, aes(x = og_basepath_dist, y = time_left_ground, color = final_basepath_dist)) + 
       geom_point() + scale_color_gradient2(high = "green", low = "red", mid = "white", midpoint = 0.4)


act <- c()
pred <- c()
for(fold in double_up_folds) {
  print("-")
  train <- doubled_up_data_sum_2[-fold, ]
  test <- doubled_up_data_sum_2[fold, ]
  model <- gam(final_basepath_dist ~ te(og_basepath_dist, time_left_ground, k = 3) + ti(runner_basepath_velo, time_left_ground, k = 3), 
               data = train)
  act <- c(act, test$safe_back)
  pred <- c(pred, predict(model, newdata = test))
}
RMSE(act, pred)
### 0.7007678


act <- c()
pred <- c()
for(fold in double_up_folds) {
  print("-")
  train <- doubled_up_data_sum_2[-fold, ]
  test <- doubled_up_data_sum_2[fold, ]
  model <- randomForest(final_basepath_dist ~ og_basepath_dist + runner_basepath_velo + time_left_ground, 
                        data = train, ntree = 100)
  act <- c(act, test$safe_back)
  pred <- c(pred, predict(model, newdata = test))
}
RMSE(act, pred)
### 0.7007678

last_dist_model <- gam(final_basepath_dist ~ te(og_basepath_dist, time_left_ground, k = 3) + ti(runner_basepath_velo, time_left_ground, k = 3), 
                       data = doubled_up_data_sum_2)
summary(last_dist_model)
doubled_up_data_sum_2_pred <- doubled_up_data_sum_2 %>% ungroup() %>% mutate(pred_final_basepath_dist = predict(last_dist_model))


act <- c()
pred <- c()
for(fold in double_up_folds) {
  print("-")
  train <- doubled_up_data_sum_2[-fold, ]
  test <- doubled_up_data_sum_2[fold, ]
  model <- gam(final_velo ~ te(time_left_ground, runner_basepath_velo, k = 3), 
               data = train)
  act <- c(act, test$safe_back)
  pred <- c(pred, predict(model, newdata = test))
}
RMSE(act, pred)
### 1.02938

plot(model, page = 1)
summary(model)


last_velo_model <- gam(final_velo ~ te(time_left_ground, runner_basepath_velo, k = 3), data = doubled_up_data_sum_2)
doubled_up_data_sum_2_pred <- doubled_up_data_sum_2_pred %>% ungroup() %>% mutate(pred_final_velo = predict(last_velo_model))

doubled_up_data_sum_2_pred <- doubled_up_data_sum_2_pred %>% mutate(doubled_prob = 1-predict(final_doubled_model, newdata = doubled_up_data_sum_2_pred, type = "response"))


check <- doubled_up_data_sum_2_pred %>% filter(time_left_ground < 1.25, time_left_ground > 0.75, 
                                               og_basepath_dist < 0.2, og_basepath_dist > 0.15,
                                               runner_basepath_velo < -0.13, runner_basepath_velo > -0.16)


