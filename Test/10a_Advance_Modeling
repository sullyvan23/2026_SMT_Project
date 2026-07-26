
### originally did advance one, two, three models, using the previously done datasets, but sometimes the advance 3 % would be greater than advance 2 % when both really low

### advance one dataset used for runners on 3rd base, took out a play with some inconsistent data
advance_one_final <- advance_one_data_sum %>% group_by(game_string, play_per_game, player_id_br) %>% slice(n())
advance_one_final <- advance_one_final %>% filter(game_string != "y1_d092_PHD_VAS", play_per_game != 27)

### 10-fold cross validation
set.seed(376)
a1_folds <- createFolds(advance_one_final$advance_one, k = 10)

act <- c()
pred <- c()
for(fold in a1_folds) {
  print("-")
  train <- advance_one_final[-fold, ]
  test <- advance_one_final[fold, ]
  model <- gam(advance_one ~ og_basepath_dist + ground_next_dist + runner_basepath_velo, 
               family = binomial, data = train)
  act <- c(act, test$advance_one) 
  pred <- c(pred, predict(model, newdata = test, type = "response"))
}
logLoss(act, pred)
### 0.04271648

a1_model <- gam(advance_one ~ og_basepath_dist + ground_next_dist + runner_basepath_velo, 
                family = binomial, data = advance_one_final)
summary(a1_model)

####################################################################################################################################################################

### finding positions of next bases
advance_data <- advance_data %>% mutate(next_base_x = case_when(player_id_br == 11  ~  x_2b,
                                                                player_id_br == 12  ~  x_3b,
                                                                player_id_br == 13  ~  x_home),
                                        next_base_y = case_when(player_id_br == 11  ~  y_2b,
                                                                player_id_br == 12  ~  y_3b,
                                                                player_id_br == 13  ~  y_home))
advance_data <- advance_data %>% mutate(next2_base_x = case_when(player_id_br == 11  ~  x_3b,
                                                                 player_id_br == 12  ~  x_home,
                                                                 TRUE  ~  NA),
                                        next2_base_y = case_when(player_id_br == 11  ~  y_3b,
                                                                 player_id_br == 12  ~  y_home,
                                                                 TRUE  ~  NA))
advance_data <- advance_data %>% mutate(next3_base_x = case_when(player_id_br == 11  ~  x_home,
                                                                 TRUE  ~  NA),
                                        next3_base_y = case_when(player_id_br == 11  ~  y_home,
                                                                 TRUE  ~  NA))

### calculating distances and angle/direction/velo variables
advance_data <- advance_data %>% mutate(OF_next_x_dist = pred_x - next_base_x,
                                        OF_next_y_dist = pred_y - next_base_y,
                                        OF_next_dist = sqrt(OF_next_x_dist^2 + OF_next_y_dist^2),
                                        ground_next_dist = sqrt((ground_x - next_base_x)^2 + (ground_y - next_base_y)^2),          
                                        OF_ground_next_dist = ((OF_next_x_dist * OF_ground_x_dist) + (OF_next_y_dist * OF_ground_y_dist)) / 
                                                              OF_next_dist,
                                        OF_ground_next_angle = acos(OF_ground_next_dist / OF_ground_dist),
                                        OF_next_velo = -((OF_next_x_dist * OF_x_velo) + (OF_next_y_dist * OF_y_velo)) / 
                                                       OF_next_dist,
                                        OF_next_velo_angle = acos(OF_next_velo / OF_velo))
advance_data <- advance_data %>% mutate(OF_next2_x_dist = pred_x - next2_base_x,
                                        OF_next2_y_dist = pred_y - next2_base_y,
                                        OF_next2_dist = sqrt(OF_next2_x_dist^2 + OF_next2_y_dist^2),
                                        ground_next2_dist = sqrt((ground_x - next2_base_x)^2 + (ground_y - next2_base_y)^2),
                                        OF_ground_next2_dist = ((OF_next2_x_dist * OF_ground_x_dist) + (OF_next2_y_dist * OF_ground_y_dist)) / 
                                                              OF_next2_dist,
                                        OF_ground_next2_angle = acos(OF_ground_next2_dist / OF_ground_dist),
                                        OF_next2_velo = -((OF_next2_x_dist * OF_x_velo) + (OF_next2_y_dist * OF_y_velo)) / 
                                                       OF_next2_dist,
                                        OF_next2_velo_angle = acos(OF_next2_velo / OF_velo))
advance_data <- advance_data %>% mutate(OF_next3_x_dist = pred_x - next3_base_x,
                                        OF_next3_y_dist = pred_y - next3_base_y,
                                        OF_next3_dist = sqrt(OF_next3_x_dist^2 + OF_next3_y_dist^2),
                                        ground_next3_dist = sqrt((ground_x - next3_base_x)^2 + (ground_y - next3_base_y)^2),
                                        OF_ground_next3_dist = ((OF_next3_x_dist * OF_ground_x_dist) + (OF_next3_y_dist * OF_ground_y_dist)) / 
                                                          OF_next3_dist,
                                        OF_ground_next3_angle = acos(OF_ground_next3_dist / OF_ground_dist),
                                        OF_next3_velo = -((OF_next3_x_dist * OF_x_velo) + (OF_next3_y_dist * OF_y_velo)) / 
                                                       OF_next3_dist,
                                        OF_next3_velo_angle = acos(OF_next3_velo / OF_velo))

### weighing outfielder variables by if_caught_catch_prob
advance_data_sum <- advance_data %>% group_by(game_string, play_per_game, player_id_br, timestamp) %>% 
                                     mutate(across(c(player_id, pred_x:OF_velo, OF_ground_x_dist:OF_ground_dist, OF_next_x_dist:OF_next_dist, OF_ground_next_dist:OF_next_velo_angle, 
                                                     speed_95_throw, next_base_x:OF_next3_velo_angle),
                                            ~ weighted.mean(., if_caught_catch_prob)))
advance_data_sum <- advance_data_sum %>% slice(1) %>% select(-c(catch_prob, player_code_OF, if_caught_catch_prob))
advance_data_sum$succ_bases_advanced <- as.numeric(advance_data_sum$succ_bases_advanced)+1

write.csv(advance_data_sum, "advance_data_sum.csv", row.names = FALSE)

### splitting datatsets for runners on first and runners on second
baserunner_1st_final <- advance_data_sum %>% group_by(game_string, play_per_game, player_id_br) %>% filter(player_id_br == 11) %>% slice(n()) %>%
                                             filter(runner_basepath_velo < 0.35)
baserunner_2nd_final <- advance_data_sum %>% group_by(game_string, play_per_game, player_id_br) %>% filter(player_id_br == 12) %>% slice(n()) %>%
                                             filter(runner_basepath_velo < 0.36)

### 10-fold cross validation for both
set.seed(288)
b1_folds <- createFolds(baserunner_1st_final$succ_bases_advanced, k = 10)
b2_folds <- createFolds(baserunner_2nd_final$succ_bases_advanced, k = 10)

act <- c()
pred_hold <- matrix(ncol = 4)
pred <- c()
for(fold in b1_folds) {
  print("-")
  train <- baserunner_1st_final[-fold, ]
  test <- baserunner_1st_final[fold, ]
  model <- gam(succ_bases_advanced ~ og_basepath_dist + runner_basepath_velo + te(ground_x, ground_y, k = 5) + ti(og_basepath_dist, ground_next_dist, k = 3),
               family = ocat(R = 4), data = train)
  act <- c(act, test$succ_bases_advanced) 
  pred_hold <- rbind(pred_hold, as.matrix(predict(model, newdata = test, type = "response")))
}
pred_hold <- pred_hold[-1,]
for(i in 1:length(act)) {
    pred <- c(pred, pred_hold[i,act[i]])
}
act <- act/act
logLoss(act, pred)
### 0.7874714

summary(model)
plot(model, pages = 1)

act <- c()
pred_hold <- matrix(ncol = 3)
pred <- c()
for(fold in b2_folds) {
  print("-")
  train <- baserunner_2nd_final[-fold, ]
  test <- baserunner_2nd_final[fold, ]
  model <- gam(succ_bases_advanced ~ og_basepath_dist + runner_basepath_velo + te(ground_x, ground_y, k = 5),
               family = ocat(R = 3), data = train)
  act <- c(act, test$succ_bases_advanced) 
  pred_hold <- rbind(pred_hold, as.matrix(predict(model, newdata = test, type = "response")))
}
pred_hold <- pred_hold[-1,]
for(i in 1:length(act)) {
    pred <- c(pred, pred_hold[i,act[i]])
}
act <- act/act
logLoss(act, pred)
### 0.4129648


### updating variable names for final runner model
baserunner_1st_final <- baserunner_1st_final %>% mutate(forward_basepath = og_basepath_dist, forward_velo = runner_basepath_velo)
baserunner_2nd_final <- baserunner_2nd_final %>% mutate(forward_basepath = og_basepath_dist, forward_velo = runner_basepath_velo)
advance_one_final <- advance_one_final %>% mutate(forward_basepath = og_basepath_dist, forward_velo = runner_basepath_velo)

### final models
advance_first_model <- gam(succ_bases_advanced ~ forward_basepath + forward_velo + te(ground_x, ground_y, k = 5) + ti(og_basepath_dist, ground_next_dist, k = 3) + 
                                                 speed_95_runner,
                           family = ocat(R = 4), data = baserunner_1st_final)

advance_second_model <- gam(succ_bases_advanced ~ forward_basepath + forward_velo + te(ground_x, ground_y, k = 5) + 
                                                  speed_95_runner,
                           family = ocat(R = 3), data = baserunner_2nd_final)

advance_third_model <- gam(advance_one ~ forward_basepath + ground_next_dist + forward_velo, 
                           family = binomial, data = advance_one_final)

summary(advance_second_model)
plot(advance_first_model, pages = 1)

####################################################################################################################################################################









