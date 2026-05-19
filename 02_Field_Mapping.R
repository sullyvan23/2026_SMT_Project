library(ggplot2)
library(caret)
library(mgcv)

home_count <- lineups %>% group_by(home_team) %>% summarise(count = n())

ball_bounces <- ball_events %>% filter(ball_eventcode == 16, !is.na(timestamp))
ball_bounces <- ball_bounces %>% left_join(ball_positions[,1:6], by = c("game_string", "play_per_game", "timestamp")) %>% filter(!is.na(ball_position_z))
ball_bounces <- ball_bounces %>% mutate(home_dist = sqrt(ball_position_x^2 + ball_position_y^2))

ball_wall_hits <- ball_events %>% filter(ball_eventcode == 10, !is.na(timestamp))
ball_wall_hits <- ball_wall_hits %>% left_join(ball_positions[,1:6], by = c("game_string", "play_per_game", "timestamp")) %>% filter(!is.na(ball_position_z))
ball_wall_hits <- ball_wall_hits %>% filter(ball_position_y >= abs(ball_position_x))
ball_wall_hits <- ball_wall_hits %>% mutate(home_dist = sqrt(ball_position_x^2 + ball_position_y^2),
                                            spray_angle = atan(ball_position_x/ball_position_y))

bounces_ANI <- ball_bounces %>% filter(home_team == "ANI")
bounces_ARN <- ball_bounces %>% filter(home_team == "ARN")
bounces_PHD <- ball_bounces %>% filter(home_team == "PHD")
bounces_VAS <- ball_bounces %>% filter(home_team == "VAS")

ggplot(bounces_ANI, aes(x = ball_position_x, y = ball_position_y, color = ball_position_z)) + 
       geom_point() + 
       scale_color_gradient2(high = "green", low = "red", mid = "white")

plot(bounces_ANI$home_dist, bounces_ANI$ball_position_z)


wall_ANI <- ball_wall_hits %>% filter(home_team == "ANI")
wall_ARN <- ball_wall_hits %>% filter(home_team == "ARN") %>% filter(home_dist < 450)
wall_PHD <- ball_wall_hits %>% filter(home_team == "PHD")
wall_VAS <- ball_wall_hits %>% filter(home_team == "VAS") %>% filter(ball_position_y < 500, ball_position_x < 270)

plot(wall_VAS$ball_position_x, wall_VAS$ball_position_y)
plot(wall_VAS$spray_angle, wall_VAS$home_dist)

########################################################################################################################################################

set.seed(24)
folds_ANI <- createFolds(bounces_ANI$ball_position_z, k = 10)
folds_ARN <- createFolds(bounces_ARN$ball_position_z, k = 10)
folds_PHD <- createFolds(bounces_PHD$ball_position_z, k = 10)
folds_VAS <- createFolds(bounces_VAS$ball_position_z, k = 10)


RMSE(bounces_ANI$ball_position_z, rep(mean(bounces_ANI$ball_position_z), nrow(bounces_ANI)))
### 0.5459184
RMSE(bounces_ARN$ball_position_z, rep(mean(bounces_ARN$ball_position_z), nrow(bounces_ARN)))
### 0.5094557
RMSE(bounces_PHD$ball_position_z, rep(mean(bounces_PHD$ball_position_z), nrow(bounces_PHD)))
### 0.5843483
RMSE(bounces_VAS$ball_position_z, rep(mean(bounces_VAS$ball_position_z), nrow(bounces_VAS)))
### 0.6494595


act <- c()
pred <- c()
for(fold in folds_ANI) {
  train <- bounces_ANI[-fold, ]
  test <- bounces_ANI[fold, ]
  model <- gam(ball_position_z ~ s(home_dist, k = 6) + ball_position_x + ball_position_y, data = train)
  act <- c(act, test$ball_position_z)
  pred <- c(pred, predict(model, newdata = test))
}
RMSE(act, pred)
### 0.5286989


act <- c()
pred <- c()
for(fold in folds_ARN) {
  train <- bounces_ARN[-fold, ]
  test <- bounces_ARN[fold, ]
  model <- gam(ball_position_z ~ s(home_dist, k = 4), data = train)
  act <- c(act, test$ball_position_z)
  pred <- c(pred, predict(model, newdata = test))
}
RMSE(act, pred)
### 0.4469922


act <- c()
pred <- c()
for(fold in folds_PHD) {
  train <- bounces_PHD[-fold, ]
  test <- bounces_PHD[fold, ]
  model <- gam(ball_position_z ~ s(home_dist, k = 8) + s(ball_position_x, k = 4) + ball_position_y, data = train)
  act <- c(act, test$ball_position_z)
  pred <- c(pred, predict(model, newdata = test))
}
RMSE(act, pred)
### 0.4665293
  

act <- c()
pred <- c()
for(fold in folds_VAS) {
  train <- bounces_VAS[-fold, ]
  test <- bounces_VAS[fold, ]
  model <- gam(ball_position_z ~ s(home_dist, k = 8) + s(ball_position_x, k = 8) + s(ball_position_y, k = 7), data = train)
  act <- c(act, test$ball_position_z)
  pred <- c(pred, predict(model, newdata = test))
}
RMSE(act, pred)
### 0.49297


ground_ANI_model <- gam(ball_position_z ~ s(home_dist, k = 6) + ball_position_x + ball_position_y, data = bounces_ANI)
ground_ARN_model <- gam(ball_position_z ~ s(home_dist, k = 4), data = bounces_ARN)
ground_PHD_model <- gam(ball_position_z ~ s(home_dist, k = 8) + s(ball_position_x, k = 4) + ball_position_y, data = bounces_PHD)
ground_VAS_model <- gam(ball_position_z ~ s(home_dist, k = 8) + s(ball_position_x, k = 8) + s(ball_position_y, k = 7), data = bounces_VAS)


ggplot(bounces_VAS, aes(x = ball_position_x, y = ball_position_y, color = ball_position_z - predict(ground_VAS_model))) + 
       geom_point() + 
       scale_color_gradient2(high = "green", low = "red", mid = "white")

ggplot(bounces_PHD, aes(x = ball_position_x, y = ball_position_y, color = predict(ground_PHD_model))) + 
       geom_point() + 
       scale_color_gradient2(high = "green", low = "red", mid = "white")

########################################################################################################################################################

set.seed(29)
folds_ANI <- createFolds(wall_ANI$ball_position_z, k = nrow(wall_ANI))
folds_ARN <- createFolds(wall_ARN$ball_position_z, k = nrow(wall_ARN))
folds_PHD <- createFolds(wall_PHD$ball_position_z, k = nrow(wall_PHD))
folds_VAS <- createFolds(wall_VAS$ball_position_z, k = nrow(wall_VAS))


act <- c()
pred <- c()
for(fold in folds_ANI) {
  train <- wall_ANI[-fold, ]
  test <- wall_ANI[fold, ]
  model <- gam(home_dist ~ s(spray_angle, k = 18), data = train)
  act <- c(act, test$home_dist)
  pred <- c(pred, predict(model, newdata = test))
}
RMSE(act, pred)
### 3.843956
plot(wall_ANI$spray_angle, wall_ANI$home_dist, col = "black")
points(wall_ANI$spray_angle, predict(model, newdata = wall_ANI), col = "red")


act <- c()
pred <- c()
for(fold in folds_ARN) {
  train <- wall_ARN[-fold, ]
  test <- wall_ARN[fold, ]
  model <- gam(home_dist ~ s(spray_angle, k = 9), data = train)
  act <- c(act, test$home_dist)
  pred <- c(pred, predict(model, newdata = test))
}
RMSE(act, pred)
### 4.190833
plot(wall_ARN$spray_angle, wall_ARN$home_dist, col = "black")
points(wall_ARN$spray_angle, predict(model, newdata = wall_ARN), col = "red")


act <- c()
pred <- c()
for(fold in folds_PHD) {
  train <- wall_PHD[-fold, ]
  test <- wall_PHD[fold, ]
  model <- gam(home_dist ~ s(spray_angle, k = 11), data = train)
  act <- c(act, test$home_dist)
  pred <- c(pred, predict(model, newdata = test))
}
RMSE(act, pred)
### 3.314397
plot(wall_PHD$spray_angle, wall_PHD$home_dist, col = "black")
points(wall_PHD$spray_angle, predict(model, newdata = wall_PHD), col = "red")


act <- c()
pred <- c()
for(fold in folds_VAS) {
  train <- wall_VAS[-fold, ]
  test <- wall_VAS[fold, ]
  model <- gam(home_dist ~ s(spray_angle, k = 25), data = train)
  act <- c(act, test$home_dist)
  pred <- c(pred, predict(model, newdata = test))
}
RMSE(act, pred)
### 2.320351
plot(wall_VAS$spray_angle, wall_VAS$home_dist, col = "black")
points(wall_VAS$spray_angle, predict(model, newdata = wall_VAS), col = "red")


wall_ANI_model <- gam(home_dist ~ s(spray_angle, k = 18), data = wall_ANI)
wall_ARN_model <- gam(home_dist ~ s(spray_angle, k = 9), data = wall_ARN)
wall_PHD_model <- gam(home_dist ~ s(spray_angle, k = 11), data = wall_PHD)
wall_VAS_model <- gam(home_dist ~ s(spray_angle, k = 25), data = wall_VAS)

