
jump_correlations <- round(cor(jump_catch_prob[,4:45] , jump_catch_prob$caught), 3)

################################################################################################################################################

set.seed(282)
jump_folds <- createFolds(jump_catch_prob$caught, k = 5)

act <- c()
pred <- c()
for(fold in jump_folds) {
  train <- jump_catch_prob[-fold, ]
  test <- jump_catch_prob[fold, ]
  model <- gam(caught ~ s(OF_ball_dist, k = 3) + s(OF_need_velo, k = 3) + OF_need_accel + s(OF_need_jerk, k = 3) + s(OF_ball_angle, k = 3) +
               s(wall_ball_dist, k = 5) +
               jump_dist_decr + accel_ball, 
               family = binomial, data = train)
  act <- c(act, test$caught)
  pred <- c(pred, predict(model, newdata = test, type = "response"))
}
logLoss(act, pred)
### 0.1353259

plot(model, page=1)
plot(pred, act)



base_model <- gam(caught ~ s(OF_ball_dist, k = 3) + s(OF_need_velo, k = 3) + OF_need_accel + s(OF_need_jerk, k = 3) + s(OF_ball_angle, k = 3) +
                  s(wall_ball_dist, k = 5) +
                  jump_dist_decr + accel_ball, 
                  family = binomial, data = jump_catch_prob)
jump_correlations <- round(cor(jump_catch_prob[,4:45] , (jump_catch_prob$caught - predict(base_model, type = "response")) ), 3)

plot(jump_catch_prob$jump_to_side, (jump_catch_prob$caught - predict(base_model, type = "response")))


jump_catch_prob_pred <- jump_catch_prob %>% ungroup() %>% mutate(catch_prob = round( predict(base_model, type = "response") ,3) )
jump_catch_prob_pred <- jump_catch_prob_pred %>% left_join(catch_prob_pred[,c(1,8,46)], by = c("play_key", "player_id"), suffix = c("", "_initial"))
jump_catch_prob_pred <- jump_catch_prob_pred %>% mutate(catch_prob_increase = catch_prob - catch_prob_initial)

################################################################################################################################################

jump_catch_prob_pred_2 <- jump_catch_prob_pred

just_catch <- gam(caught ~ s(OF_ball_dist, k = 3) + s(OF_need_velo, k = 3) + OF_need_accel + s(OF_need_jerk, k = 3) + s(OF_ball_angle, k = 3) +
                  s(wall_ball_dist, k = 5), 
                  family = binomial, data = jump_catch_prob_pred_2)

jump_catch_prob_pred_2 <- jump_catch_prob_pred_2 %>% mutate(catch_prob_initial = round( predict(just_catch, type = "response"), 3))
jump_catch_prob_pred_2 <- jump_catch_prob_pred_2 %>% mutate(catch_prob_increase = catch_prob - catch_prob_initial)



