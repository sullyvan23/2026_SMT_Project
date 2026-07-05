
doubled_up_data_sum <- doubled_up_data_sum %>% mutate(key = paste0(game_string, play_per_game, "_", player_id_br)) %>%
                                               relocate(key, .after = player_id_br)
set.seed(926)
double_up_folds <- groupKFold(doubled_up_data_sum$key, k = 5)
double_up_folds_2 <- createFolds(doubled_up_data_sum$safe_back, k = 5)
double_up_folds_3 <- groupKFold(doubled_up_data_sum$key, k = 2)

ggplot(doubled_up_data_sum, aes(x = runner_og_dist, y = time_left_ground, color = safe_back)) + 
       geom_point() + scale_color_gradient2(high = "green", low = "red", mid = "white", midpoint = 0.5)


act <- c()
pred <- c()
for(fold in double_up_folds_3) {
  print("-")
  train <- doubled_up_data_sum[-fold, ]
  test <- doubled_up_data_sum[fold, ]
  model <- gam(safe_back ~ og_basepath_dist + runner_basepath_velo + time_left_ground + ground_og_dist, 
               family = binomial, data = train)
  act <- c(act, test$safe_back)
  pred <- c(pred, predict(model, newdata = test, type = "response"))
}
logLoss(act, pred)
### 0.03804737

plot(model, page = 1)
summary(model)


doubled_up_model <- gam(safe_back ~ og_basepath_dist +  time_left_ground + ground_og_dist + runner_basepath_velo + speed_95_throw, 
                        family = binomial, data = doubled_up_data_sum)
summary(doubled_up_model)
plot(doubled_up_model, page = 1)


doubled_up_data_sum_pred <- doubled_up_data_sum %>% ungroup() %>% mutate(doubled_prob = 1-predict(doubled_up_model, type = "response"))

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
  model <- gam(succ_tag ~ og_basepath_dist + time_left_ground + ground_next_dist + runner_basepath_velo, 
               family = binomial, data = train)
  act <- c(act, test$succ_tag) 
  pred <- c(pred, predict(model, newdata = test, type = "response"))
}
logLoss(act, pred)
### 0.189933

plot(model, page = 1)
summary(model)



tag_up_model <- gam(succ_tag ~ og_basepath_dist + time_left_ground + ground_next_dist + runner_basepath_velo + OF_ground_next_angle + speed_95_throw + speed_95_runner, 
                    family = binomial, data = tag_up_data_sum)
summary(tag_up_model)
plot(tag_up_model, page = 1)


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

####################################################################################################################################################################

act <- c()
pred <- c()
for(fold in advance_one_folds) {
  print("-")
  train <- advance_one_data_sum[-fold, ]
  test <- advance_one_data_sum[fold, ]
  model <- gam(advance_one ~ og_basepath_dist + ground_next_dist + time_left_ground + caught_prob, 
               family = binomial, data = train)
  act <- c(act, test$advance_one) 
  pred <- c(pred, predict(model, newdata = test, type = "response"))
}
logLoss(act, pred)
### 0.07494819

plot(model, page = 1)
summary(model)


advance_one_model <- gam(advance_one ~ og_basepath_dist + ground_next_dist + time_left_ground + caught_prob, 
                         family = binomial, data = advance_one_data_sum)
summary(advance_one_model)
plot(advance_one_model, page = 1)


advance_one_data_sum_pred <- advance_one_data_sum %>% ungroup() %>% mutate(advance_one_prob = predict(advance_one_model, type = "response"))

####################################################################################################################################################################


advance_two_data_sum <- advance_two_data_sum[,1:50] %>% left_join(advance_one_data_sum_pred[,c("game_string", "play_per_game", "player_id_br", "timestamp", "advance_one_prob")],
                                                                  by = c("game_string", "play_per_game", "player_id_br", "timestamp"))


act <- c()
pred <- c()
for(fold in advance_two_folds) {
  print("-")
  train <- advance_two_data_sum[-fold, ]
  test <- advance_two_data_sum[fold, ]
  model <- gam(advance_two ~ og_basepath_dist + time_left_ground + ground_next2_dist + OF_ground_next2_angle + time_to_ground, 
               family = binomial, data = train)
  act <- c(act, test$advance_two) 
  pred <- c(pred, predict(model, newdata = test, type = "response"))
}
logLoss(act, pred)
### 0.3971099

plot(model, page = 1)
summary(model)


advance_two_model <- gam(advance_two ~ og_basepath_dist + time_left_ground + ground_next2_dist + OF_ground_next2_angle + time_to_ground + speed_95_runner, 
                         family = binomial, data = advance_two_data_sum)
summary(advance_two_model)
plot(advance_two_model, page = 1)


advance_two_data_sum_pred <- advance_two_data_sum %>% ungroup() %>% mutate(advance_two_prob = predict(advance_two_model, type = "response"))

####################################################################################################################################################################

advance_three_data_sum <- advance_three_data_sum[,1:50] %>% left_join(advance_two_data_sum_pred[,c("game_string", "play_per_game", "player_id_br", "timestamp", "advance_one_prob",
                                                                                                "advance_two_prob")],
                                                                      by = c("game_string", "play_per_game", "player_id_br", "timestamp"))


act <- c()
pred <- c()
for(fold in advance_three_folds) {
  print("-")
  train <- advance_three_data_sum[-fold, ]
  test <- advance_three_data_sum[fold, ]
  model <- gam(advance_three ~ og_basepath_dist + time_left_ground + ground_next3_dist + advance_two_prob, 
               family = binomial, data = train)
  act <- c(act, test$advance_three) 
  pred <- c(pred, predict(model, newdata = test, type = "response"))
}
logLoss(act, pred)
### 0.4731024

plot(model, page = 1)
summary(model)


advance_three_model <- gam(advance_three ~ og_basepath_dist + time_left_ground + ground_next3_dist + advance_two_prob + speed_95_runner, 
                           family = binomial, data = advance_three_data_sum)
summary(advance_three_model)
plot(advance_three_model, page = 1)


advance_three_data_sum_pred <- advance_three_data_sum %>% ungroup() %>% mutate(advance_three_prob = predict(advance_three_model, type = "response"))

####################################################################################################################################################################

check <- advance_three_data_sum_pred %>% select(game_string, play_per_game, time_left_ground, advance_one_prob, advance_two_prob, advance_three_prob)
check <- check %>% group_by(game_string, play_per_game) %>% mutate(time_left_ground = round(time_left_ground / max(time_left_ground), 1))
check <- check %>% group_by(time_left_ground) %>% summarise(advance_one_prob = mean(advance_one_prob),
                                                            advance_two_prob = mean(advance_two_prob),
                                                            advance_three_prob = mean(advance_three_prob),
                                                            count = n())



check <- advance_two_data_sum_pred %>% select(time_left_ground, advance_two, advance_two_prob)
check <- check %>% mutate(time_left_ground = round(time_left_ground, 1))
check <- check %>% group_by(time_left_ground) %>% summarise(advance_two = mean(advance_two),
                                                            advance_two_prob = mean(advance_two_prob),
                                                            count = n())
check <- check %>% filter(count >= 50)

plot(check$time_left_ground, check$advance_two, col = "black")
points(check$time_left_ground, check$advance_two_prob, col = "red")



check <- advance_two_data_sum_pred %>% select(game_string, play_per_game, time_left_ground, advance_two, advance_two_prob)
check <- check %>% group_by(game_string, play_per_game) %>% mutate(time_left_ground = round(time_left_ground / max(time_left_ground), 1))
check <- check %>% group_by(time_left_ground) %>% summarise(advance_two = mean(advance_two),
                                                            advance_two_prob = mean(advance_two_prob),
                                                            count = n())

plot(check$time_left_ground, check$advance_two)
plot(check$time_left_ground, check$advance_two_prob)







