
doubled_up_data_sum <- doubled_up_data_sum %>% mutate(key = paste0(game_string, play_per_game, "_", player_id_br)) %>%
                                               relocate(key, .after = player_id_br)
set.seed(926)
double_up_folds <- groupKFold(doubled_up_data_sum$key, k = 5)

ggplot(doubled_up_data_sum, aes(x = runner_og_dist, y = time_left_ground, color = safe_back)) + 
       geom_point() + scale_color_gradient2(high = "green", low = "red", mid = "white", midpoint = 0.5)


act <- c()
pred <- c()
for(fold in double_up_folds) {
  print("-")
  train <- doubled_up_data_sum[-fold, ]
  test <- doubled_up_data_sum[fold, ]
  model <- bam(safe_back ~ te(runner_og_dist, ground_og_dist, time_left_ground, k = 3) + ground_og_dist, 
               family = binomial, data = train)
  act <- c(act, test$safe_back)
  pred <- c(pred, predict(model, newdata = test, type = "response"))
}
logLoss(act, pred)
### 0.146737

plot(model, page = 1)
summary(model)


doubled_up_model <- bam(safe_back ~ te(runner_og_dist, time_left_ground, k = 3) + ground_og_dist + ti(runner_og_dist, runner_og_velo, k = 3) +
                                    ti(OF_ground_og_angle, caught_prob, k = 3) + speed_95_throw + speed_95_runner, 
                        family = binomial, data = doubled_up_data_sum)
plot(doubled_up_model, page = 1)
summary(doubled_up_model)


doubled_up_data_sum_pred <- doubled_up_data_sum %>% ungroup() %>% mutate(doubled_prob = 1-predict(doubled_up_model, type = "response"))

####################################################################################################################################################################

doubled_up_final <- doubled_up_data_sum %>% group_by(game_string, play_per_game, player_id_br) %>% slice(n())

ggplot(doubled_up_final, aes(x = runner_og_dist, y = ground_og_dist, color = safe_back)) + 
       geom_point() + scale_color_gradient2(high = "green", low = "red", mid = "white", midpoint = 0.5)

####################################################################################################################################################################


tag_up_data_sum <- tag_up_data_sum %>% mutate(key = paste0(game_string, play_per_game, "_", player_id_br)) %>%
                                       relocate(key, .after = player_id_br)
set.seed(298)
tag_up_folds <- groupKFold(tag_up_data_sum$key, k = 5)


act <- c()
pred <- c()
for(fold in tag_up_folds) {
  print("-")
  train <- tag_up_data_sum[-fold, ]
  test <- tag_up_data_sum[fold, ]
  model <- bam(succ_tag ~ te(runner_og_dist, time_left_ground, k = 3) + ground_next_dist + ti(runner_og_dist, runner_og_velo, k = 3) +
                          ti(OF_ground_next_angle, caught_prob, k = 3), 
               family = binomial, data = train)
  act <- c(act, test$succ_tag) 
  pred <- c(pred, predict(model, newdata = test, type = "response"))
}
logLoss(act, pred)
### 0.1988888

plot(model, page = 1)
summary(model)



tag_up_model <- bam(succ_tag ~ te(runner_og_dist, time_left_ground, k = 3) + ground_next_dist + ti(runner_og_dist, runner_og_velo, k = 3) +
                               ti(OF_ground_next_angle, caught_prob, k = 3) + speed_95_throw + speed_95_runner, 
                    family = binomial, data = tag_up_data_sum)
plot(tag_up_model, page = 1)
summary(tag_up_model)


tag_up_data_sum_pred <- tag_up_data_sum %>% ungroup() %>% mutate(tag_prob = predict(tag_up_model, type = "response"))

####################################################################################################################################################################

advance_one_data_sum <- advance_one_data_sum %>% mutate(key = paste0(game_string, play_per_game, "_", player_id_br)) %>%
                                                 relocate(key, .after = player_id_br)
advance_two_data_sum <- advance_two_data_sum %>% mutate(key = paste0(game_string, play_per_game, "_", player_id_br)) %>%
                                                 relocate(key, .after = player_id_br)
advance_three_data_sum <- advance_three_data_sum %>% mutate(key = paste0(game_string, play_per_game, "_", player_id_br)) %>%
                                                     relocate(key, .after = player_id_br)

set.seed(827)
advance_one_folds <- groupKFold(advance_one_data_sum$key, k = 5)
advance_two_folds <- groupKFold(advance_two_data_sum$key, k = 5)
advance_three_folds <- groupKFold(advance_three_data_sum$key, k = 5)


act <- c()
pred <- c()
for(fold in advance_one_folds) {
  print("-")
  train <- advance_one_data_sum[-fold, ]
  test <- advance_one_data_sum[fold, ]
  model <- bam(advance_one ~ og_basepath_dist + ground_next_dist + time_to_ground + runner_basepath_velo, 
               family = binomial, data = train)
  act <- c(act, test$advance_one) 
  pred <- c(pred, predict(model, newdata = test, type = "response"))
}
logLoss(act, pred)
### 0.09640015

plot(model, page = 1)
summary(model)


advance_one_model <- bam(advance_one ~ og_basepath_dist + ground_next_dist + time_to_ground + runner_basepath_velo, 
                         family = binomial, data = advance_one_data_sum)
plot(advance_one_model, page = 1)
summary(advance_one_model)


advance_one_data_sum_pred <- advance_one_data_sum %>% ungroup() %>% mutate(advance_one_prob = predict(advance_one_model, type = "response"))




