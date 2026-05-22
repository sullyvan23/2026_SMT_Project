library(mgcv)
library(Metrics)

catch_prob_fbs <- all_fly_ball_stats %>% filter(player_id_down == 255   |   player_id == player_id_down)

plot(catch_prob_fbs$OF_need_velo, catch_prob_fbs$caught)

catch_prob_fbs <- catch_prob_fbs %>% filter(OF_need_velo <= 20)

################################################################################################################################################

catch_prob_folds <- createFolds(catch_prob_fbs$caught, k = 5)

act <- c()
pred <- c()
for(fold in catch_prob_folds) {
  train <- catch_prob_fbs[-fold, ]
  test <- catch_prob_fbs[fold, ]
  model <- gam(caught ~ OF_abs_need_x_velo + OF_abs_need_y_velo, family = binomial, data = train)
  act <- c(act, test$caught)
  pred <- c(pred, predict(model, newdata = test, type = "response"))
}
logLoss(act, pred)
### 0.2293848

plot(model, page=1)
plot(pred, act)


model <- gam(caught ~ s(OF_need_velo, k = 4), family = binomial, data = train)

