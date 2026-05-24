
jump_correlations <- round(cor(jump_catch_prob[,4:42] , jump_catch_prob$caught), 3)

################################################################################################################################################

set.seed(282)
jump_folds <- createFolds(jump_catch_prob$caught, k = 5)

act <- c()
pred <- c()
for(fold in jump_folds) {
  train <- jump_catch_prob[-fold, ]
  test <- jump_catch_prob[fold, ]
  model <- gam(caught ~ s(OF_need_front_accel, k = 5) + s(OF_abs_need_side_accel, k = 4) + time_to_ground +
                        s(wall_ball_dist, k = 5) +
                        s(jump_to_ball, k = 3), 
               family = binomial, data = train)
  act <- c(act, test$caught)
  pred <- c(pred, predict(model, newdata = test, type = "response"))
}
logLoss(act, pred)
### 0.1460862

plot(model, page=1)
plot(pred, act)



base_model <- gam(caught ~ s(OF_need_front_accel, k = 5) + s(OF_abs_need_side_accel, k = 4) + time_to_ground +
                           s(wall_ball_dist, k = 5), 
                           family = binomial, data = jump_catch_prob)
jump_correlations <- round(cor(jump_catch_prob[,4:42] , (jump_catch_prob$caught - predict(base_model, type = "response")) ), 3)

plot(jump_catch_prob$pred_y_accel, (jump_catch_prob$caught - predict(base_model, type = "response")))
