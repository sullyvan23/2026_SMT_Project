
tag_end <- tag_up_data_sum %>% group_by(game_string, play_per_game, player_id_br) %>% slice(n())
tag_end <- tag_up_data_sum %>% group_by(game_string, play_per_game, player_id_br) %>% slice(n())
tag_end <- tag_end %>% rename(back_basepath = og_basepath_dist,
                              back_velo = runner_basepath_velo)
tag_end <- tag_end %>% mutate(back_basepath = ifelse(back_basepath <= 0.14  &  back_velo > 0, 0.025, back_basepath),
                              back_velo = ifelse(back_basepath <= 0.14  &  back_velo > 0, 0, back_velo))


ggplot(tag_end, aes(x = back_basepath, y = back_velo, color = succ_tag)) + 
       geom_point() + scale_color_gradient2(high = "green", low = "red", mid = "white", midpoint = 0.5)

set.seed(637)
tag_end_folds <- createFolds(tag_end$succ_tag, k = 10)


act <- c()
pred <- c()
for(fold in tag_end_folds) {
  print("-")
  train <- tag_end[-fold, ]
  test <- tag_end[fold, ]
  model <- gam(succ_tag ~ te(back_basepath, ground_next_dist, k = 3) + wall_ground_dist, 
               family = binomial, data = train)
  act <- c(act, test$succ_tag) 
  pred <- c(pred, predict(model, newdata = test, type = "response"))
}
logLoss(act, pred)
### 0.1477914

plot(model, page = 1)
summary(model)



tag_end_model <- gam(succ_tag ~ te(back_basepath, ground_next_dist, k = 3) + wall_ground_dist + speed_95_throw + speed_95_runner, 
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









