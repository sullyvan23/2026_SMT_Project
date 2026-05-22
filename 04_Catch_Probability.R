library(mgcv)
library(Metrics)

catch_prob_fbs <- all_fly_ball_stats %>% filter(player_id_down == 255   |   player_id == player_id_down)
catch_prob_fbs <- catch_prob_fbs %>% filter(!is.na(player_id))

catch_correlations <- round(cor(catch_prob_fbs[,c(4:10,12:39)] , catch_prob_fbs$caught), 3)

plot(catch_prob_fbs$OF_need_velo, catch_prob_fbs$caught)

catch_prob_fbs <- catch_prob_fbs %>% filter(OF_need_velo <= 20)

################################################################################################################################################

set.seed(394)
catch_prob_folds <- createFolds(catch_prob_fbs$caught, k = 5)

act <- c()
pred <- c()
for(fold in catch_prob_folds) {
  train <- catch_prob_fbs[-fold, ]
  test <- catch_prob_fbs[fold, ]
  model <- gam(caught ~ s(OF_need_front_accel, k = 4) + s(OF_need_side_accel, k = 4), family = binomial, data = train)
  act <- c(act, test$caught)
  pred <- c(pred, predict(model, newdata = test, type = "response"))
}
logLoss(act, pred)
### 0.2059814

plot(model, page=1)
plot(pred, act)


model <- gam(caught ~ s(OF_need_accel, k = 3), family = binomial, data = train)
### 0.2266662
model <- gam(caught ~ s(OF_abs_need_front_accel, k = 6) + s(OF_abs_need_side_accel, k = 6), family = binomial, data = train)
### 0.2197144




