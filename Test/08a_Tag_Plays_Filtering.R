
tag_end <- tag_up_data_sum %>% group_by(game_string, play_per_game, player_id_br) %>% slice(n())

ggplot(tag_end, aes(x = og_basepath_dist, y = runner_basepath_velo, color = att_tag)) + 
       geom_point() + scale_color_gradient2(high = "green", low = "red", mid = "white", midpoint = 0.5)


set.seed(191)
att_tag_folds <- createFolds(tag_end$key, k = 10)


act <- c()
pred <- c()
for(fold in att_tag_folds) {
  print("-")
  train <- tag_end[-fold, ]
  test <- tag_end[fold, ]
  model <- gam(att_tag ~ te(og_basepath_dist, ground_next_dist, k = 3), 
               family = binomial, data = train)
  act <- c(act, test$att_tag)
  pred <- c(pred, predict(model, newdata = test, type = "response"))
}
logLoss(act, pred)
### 0.152545


plot(model, page = 1)
summary(model)



att_tag_model <- gam(att_tag ~ te(og_basepath_dist, ground_next_dist, k = 3), 
                     family = binomial, data = tag_end)

tag_end <- tag_end %>% ungroup() %>% mutate(att_tag_prob = round(predict(att_tag_model, type = "response"), 3)) %>% relocate(att_tag_prob, .after = att_tag)

tag_end <- tag_end %>% left_join(baserunners, by = c("game_string", "play_per_game")) %>%
                       mutate(others = first + second + third - 1) %>% relocate(others, .after = att_tag_prob)



tag_end_2 <- tag_end %>% filter(!(att_tag_prob >= 0.15  &  att_tag == 0))

ggplot(tag_end, aes(x = og_basepath_dist, y = runner_basepath_velo, color = att_tag)) + 
       geom_point() + scale_color_gradient2(high = "green", low = "red", mid = "white", midpoint = 0.5)


tag_up_data_sum_2 <- tag_end_2[,1:3] %>% left_join(tag_up_data_sum, by = c("game_string", "play_per_game", "player_id_br"))

##############################################################################################################################################################################

tag_check <- tag_up_data_sum_2 %>% mutate(time_left_ground = round(time_left_ground)) %>% group_by(time_left_ground, succ_tag) %>%
                                   summarise(og_basepath_dist = mean(og_basepath_dist), runner_basepath_velo = mean(runner_basepath_velo),
                                             count = n())

##############################################################################################################################################################################

set.seed(298)
tag_up_folds_2 <- groupKFold(tag_up_data_sum_2$key, k = 5)


act <- c()
pred <- c()
for(fold in tag_up_folds_2) {
  print("-")
  train <- tag_up_data_sum_2[-fold, ]
  test <- tag_up_data_sum_2[fold, ]
  model <- gam(succ_tag ~ og_basepath_dist + time_left_ground + ground_next_dist + OF_ground_next_angle, 
               family = binomial, data = train)
  act <- c(act, test$succ_tag) 
  pred <- c(pred, predict(model, newdata = test, type = "response"))
}
logLoss(act, pred)
### 0.414123

plot(model, page = 1)
summary(model)



tag_up_model_2 <- gam(succ_tag ~ og_basepath_dist + time_left_ground + ground_next_dist + OF_ground_next_angle + runner_basepath_velo + speed_95_runner + speed_95_throw, 
                      family = binomial, data = tag_up_data_sum_2)


tag_up_data_sum_2_pred <- tag_up_data_sum_2 %>% ungroup() %>% mutate(tag_prob = predict(tag_up_model_2, type = "response")) %>%
                                                relocate(tag_prob, .after = succ_tag)

summary(tag_up_model_2)
plot(tag_up_model_2, page = 1)

##############################################################################################################################################################################

tag_up_data_sum_3 <- tag_up_data_sum %>% mutate(basepath_decel_dist = ifelse(runner_basepath_velo > 0, og_basepath_dist + (runner_basepath_velo^2 / 0.2), og_basepath_dist),
                                                basepath_decel_time = ifelse(runner_basepath_velo > 0, runner_basepath_velo / 0.1, og_basepath_dist),
                                                velo_back = ifelse(runner_basepath_velo > 0, 0, -runner_basepath_velo),
                                                avg_velo_back = ifelse(sqrt(velo_back^2 + (0.2*(basepath_decel_dist/2)))/2 > 0.15, 0.15, sqrt(velo_back^2 + (0.2*(basepath_decel_dist/2)))/2),
                                                time_back = (basepath_decel_dist/avg_velo_back) + basepath_decel_time)

tag_up_data_sum_3 <- tag_up_data_sum_3 %>% mutate(get_back = ifelse(time_back < time_left_ground, 0, time_back - time_left_ground)) %>%
                                                  relocate(get_back, .after = succ_tag)

##############################################################################################################################################################################

tag_end <- tag_up_data_sum %>% group_by(game_string, play_per_game, player_id_br) %>% slice(n())


set.seed(637)
tag_end_folds <- createFolds(tag_end$succ_tag, k = 10)


act <- c()
pred <- c()
for(fold in tag_end_folds) {
  print("-")
  train <- tag_end[-fold, ]
  test <- tag_end[fold, ]
  model <- gam(succ_tag ~ te(og_basepath_dist, ground_next_dist, k = 3) + runner_basepath_velo + caught_prob + speed_95_throw, 
               family = binomial, data = train)
  act <- c(act, test$succ_tag) 
  pred <- c(pred, predict(model, newdata = test, type = "response"))
}
logLoss(act, pred)
### 0.1527536

plot(model, page = 1)
summary(model)



tag_end_model <- gam(succ_tag ~ te(og_basepath_dist, ground_next_dist, k = 3) + runner_basepath_velo + caught_prob + speed_95_throw + speed_95_runner, 
                     family = binomial, data = tag_end)
summary(tag_end_model)
plot(tag_end_model, pages = 1)

##############################################################################################################################################################################

tag_up_data_sum_try <- tag_up_data_sum %>% filter(time_left_ground >= 0.5, (time_left_ground/time_to_ground) <= 0.5)

ggplot(tag_up_data_sum_try, aes(x = og_basepath_dist, y = time_left_ground, color = succ_tag)) + 
       geom_point() + scale_color_gradient2(high = "green", low = "red", mid = "white", midpoint = 0.5)


set.seed(298)
try_tag_folds <- groupKFold(tag_up_data_sum_try$key, k = 5)


act <- c()
pred <- c()
for(fold in try_tag_folds) {
  print("-")
  train <- tag_up_data_sum_try[-fold, ]
  test <- tag_up_data_sum_try[fold, ]
  model <- gam(succ_tag ~ og_basepath_dist + time_left_ground + ground_next_dist + runner_basepath_velo, 
               family = binomial, data = train)
  act <- c(act, test$succ_tag) 
  pred <- c(pred, predict(model, newdata = test, type = "response"))
}
logLoss(act, pred)
### 0.1751757

plot(model, page = 1)
summary(model)









