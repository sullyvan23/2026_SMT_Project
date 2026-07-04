
doubled_up_final <- doubled_up_data_sum_pred %>% group_by(game_string, play_per_game, player_id_br) %>% slice(n())

ggplot(doubled_up_final, aes(x = og_basepath_dist, y = ground_og_dist, color = safe_back)) + 
       geom_point() + scale_color_gradient2(high = "green", low = "red", mid = "white", midpoint = 0.5)



set.seed(883)
final_doubled_folds <- createFolds(doubled_up_final$safe_back, k = 5)


act <- c()
pred <- c()
for(fold in final_doubled_folds) {
  print("-")
  train <- doubled_up_final[-fold, ]
  test <- doubled_up_final[fold, ]
  model <- gam(safe_back ~ og_basepath_dist + ground_og_dist + runner_basepath_velo, 
               family = binomial, data = train)
  act <- c(act, test$safe_back)
  pred <- c(pred, predict(model, newdata = test, type = "response"))
}
logLoss(act, pred)
### 0.0302579

plot(model, page = 1)
summary(model)


final_doubled_model <- gam(safe_back ~ og_basepath_dist + ground_og_dist + runner_basepath_velo, 
                           family = binomial, data = doubled_up_final)
doubled_up_final <- doubled_up_final %>% ungroup() %>% mutate(final_doubled_prob = 1-predict(final_doubled_model, type = "response"))








####################################################################################################################################################################










