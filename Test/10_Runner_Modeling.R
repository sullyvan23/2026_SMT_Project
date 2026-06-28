
doubled_up_data_sum <- doubled_up_data_sum %>% mutate(key = paste0(game_string, play_per_game, "_", player_id_br)) %>%
                                               relocate(key, .after = player_id_br)
set.seed(926)
double_up_folds <- groupKFold(doubled_up_data_sum$key, k = 2)


act <- c()
pred <- c()
for(fold in double_up_folds) {
  print("-")
  train <- doubled_up_data_sum[-fold, ]
  test <- doubled_up_data_sum[fold, ]
  model <- bam(safe_back ~ s(runner_og_dist, k = 3) + te(ground_og_dist, time_left_ground, k = 3) + te(eight_ft_og_dist, time_left_8ft, k = 3) +
                           s(runner_og_velo, k = 3), 
               family = binomial, data = train)
  act <- c(act, test$safe_back)
  pred <- c(pred, predict(model, newdata = test, type = "response"))
}
logLoss(act, pred)
### 0.0591762

plot(model, page = 1)
summary(model)

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
  model <- bam(succ_tag ~ te(runner_og_dist, time_left_8ft, k = 3) + s(eight_ft_next_dist, k = 3) + s(runner_og_velo, k = 3), 
               family = binomial, data = train)
  act <- c(act, test$succ_tag)
  pred <- c(pred, predict(model, newdata = test, type = "response"))
}
logLoss(act, pred)
### 0.2023107

plot(model, page = 1)
summary(model)



tag_up_model <- bam(succ_tag ~ te(runner_og_dist, time_left_8ft, k = 3) + s(eight_ft_next_dist, k = 3) + s(runner_og_velo, k = 3) + s(OF_next_velo, k = 3) +
                               speed_95_throw + speed_95_runner + runners_front, 
                    family = binomial, data = tag_up_data_sum)
plot(tag_up_model, page = 1)
summary(tag_up_model)

####################################################################################################################################################################






