library(mgcv)
library(Metrics)

catch_prob_fbs <- all_fly_ball_stats %>% filter(player_id_down == 255   |   player_id == player_id_down)

catch_correlations <- round(cor(catch_prob_fbs[,c(4:10,12:39)] , catch_prob_fbs$caught), 3)

plot(catch_prob_fbs$OF_need_velo, catch_prob_fbs$caught)

catch_prob_fbs <- catch_prob_fbs %>% filter(OF_need_velo <= 20)

################################################################################################################################################

ggplot(catch_prob_fbs, aes(x = OF_need_side_accel, y = OF_need_front_accel, color = caught)) + 
geom_point() + scale_color_gradient2(high = "green", low = "red", midpoint = 0.5)

ggplot(catch_prob_fbs, aes(x = OF_abs_need_side_accel, y = OF_abs_need_front_accel, color = caught)) + 
geom_point() + scale_color_gradient2(high = "green", low = "red", midpoint = 0.5)

ggplot(catch_prob_fbs, aes(x = OF_ball_angle, y = OF_need_accel, color = caught)) + 
geom_point() + scale_color_gradient2(high = "green", low = "red", midpoint = 0.5)

################################################################################################################################################

set.seed(394)
catch_prob_folds <- createFolds(catch_prob_fbs$caught, k = 5)

act <- c()
pred <- c()
for(fold in catch_prob_folds) {
  train <- catch_prob_fbs[-fold, ]
  test <- catch_prob_fbs[fold, ]
  model <- gam(caught ~ s(OF_need_front_accel, k = 5) + s(OF_abs_need_side_accel, k = 4) + time_to_ground +
               s(wall_ball_dist, k = 5) + tail, 
               family = binomial, data = train)
  act <- c(act, test$caught)
  pred <- c(pred, predict(model, newdata = test, type = "response"))
}
logLoss(act, pred)
### 0.1506213

plot(model, page=1)
plot(pred, act)

model <- gam(caught ~ s(OF_need_front_accel, k = 5) + s(OF_need_side_accel, k = 5), 
             family = binomial, data = train)
### 0.2065961
model <- gam(caught ~ s(OF_ball_angle, k = 3) + OF_ball_dist + s(time_to_ground, k = 3), 
             family = binomial, data = train)
### 0.1988068
model <- gam(caught ~ s(OF_ball_angle, k = 3) + s(time_to_ground, k = 3) + s(OF_need_accel, k = 4), 
             family = binomial, data = train)
### 0.1759485
model <- gam(caught ~ s(OF_need_front_accel, k = 5) + s(OF_need_side_accel, k = 5) + s(time_to_ground, k = 3), 
             family = binomial, data = train)
### 0.1719989
model <- gam(caught ~ s(OF_need_front_accel, k = 5) + s(OF_abs_need_side_accel, k = 4) + s(time_to_ground, k = 3), 
             family = binomial, data = train)
### 0.1719527
model <- gam(caught ~ s(OF_need_front_accel, k = 5) + s(OF_abs_need_side_accel, k = 4) + time_to_ground +
             s(wall_ball_dist, k = 5), 
             family = binomial, data = train)
### 0.1506627



base_model <- gam(caught ~ s(OF_need_front_accel, k = 5) + s(OF_abs_need_side_accel, k = 4) + time_to_ground +
                  s(wall_ball_dist, k = 5), 
                  family = binomial, data = catch_prob_fbs)

catch_correlations <- round(cor(catch_prob_fbs[,c(4:10,12:39)] , (catch_prob_fbs$caught - predict(base_model, type = "response")) ), 3)
plot(catch_prob_fbs$min_xy_speed, (catch_prob_fbs$caught - predict(base_model, type = "response")))
plot(predict(base_model, type = "response"), catch_prob_fbs$caught)

catch_prob_pred <- catch_prob_fbs %>% mutate(catch_prob = round( predict(base_model, type = "response") ,3) )




