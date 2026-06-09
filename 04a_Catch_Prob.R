library(dplyr)
library(mgcv)
library(caret)

catch_prob_all_time <- catch_prob_fbs[,c(1:8,17,19,45)] %>% left_join(fly_balls[,5:7], by = "play_key")
catch_prob_all_time <- catch_prob_all_time %>% mutate(timestamp_ground = timestamp_hit + (1000*time_to_ground))

OF_positions <- player_positions %>% filter(player_id %in% c(7:9)) %>% mutate(play_key = paste0(game_string, play_per_game))
OF_need_positions <- OF_positions %>% filter(play_key %in% catch_prob_all_time$play_key)

catch_prob_all_time <- catch_prob_all_time %>% left_join(OF_need_positions[,c(3:6,11)], by = c("play_key", "player_id"))
catch_prob_all_time <- catch_prob_all_time %>% filter(timestamp >= (timestamp_hit-200), timestamp <= timestamp_down)


group <- 0
catch_prob_all_time <- catch_prob_all_time %>% group_by(play_key, player_id) %>%
                 group_modify(~{group <<- group + 1
                              message(group)
                                  
                              x_model <- gam(field_x ~ s(timestamp, k = 10), data = .x )
                              y_model <- gam(field_y ~ s(timestamp, k = 10), data = .x )

                              .x$pred_x <- predict(x_model) 
                              .x$pred_y <- predict(y_model)

                              .x$rmse_x <- RMSE(.x$pred_x, .x$field_x)
                              .x$rmse_y <- RMSE(.x$pred_y, .x$field_y)

                              .x})

hist(catch_prob_all_time$rmse_x, breaks = 100)
hist(catch_prob_all_time$rmse_y, breaks = 100)


catch_prob_all_time_2 <- catch_prob_all_time %>% filter(rmse_x <= 0.3, rmse_y <= 0.4)

catch_prob_all_time_2 <- catch_prob_all_time_2 %>% mutate(OF_ball_x_dist = ground_x - pred_x,
                                                      OF_ball_y_dist = ground_y - pred_y,
                                                      OF_ball_dist = sqrt(OF_ball_x_dist^2 + OF_ball_y_dist^2),
                                                      x_velo = 0.68181818 * (pred_x - lag(pred_x)) / ((timestamp - lag(timestamp))/1000),
                                                      y_velo = 0.68181818 * (pred_y - lag(pred_y)) / ((timestamp - lag(timestamp))/1000),
                                                      speed = sqrt(x_velo^2 + y_velo^2),
                                                      x_accel = 0.68181818 * (x_velo - lag(x_velo)) / ((timestamp - lag(timestamp))/1000),
                                                      y_accel = 0.68181818 * (y_velo - lag(y_velo)) / ((timestamp - lag(timestamp))/1000),
                                                      tang_accel = 0.68181818 * (speed - lag(speed)) / ((timestamp - lag(timestamp))/1000),
                                                      accel_mag = sqrt(x_accel^2 + y_accel^2)) %>% filter(timestamp >= timestamp_hit)

hist(catch_prob_all_time_2$accel_mag, breaks = 100)

catch_prob_all_time_2 <- catch_prob_all_time_2 %>% group_by(play_key, player_id) %>% filter(sum(accel_mag > 20) == 0)



catch_prob_all_time_2 <- catch_prob_all_time_2 %>% mutate(time_left = (timestamp_ground - timestamp)/1000) %>% relocate(time_left, .after = time_to_ground)

catch_prob_all_time_2 <- catch_prob_all_time_2 %>% mutate(OF_ball_front_dist = -((OF_ball_x_dist * field_x) + (OF_ball_y_dist * field_y)) / 
                                                                          sqrt(field_x^2 + field_y^2))
catch_prob_all_time_2 <- catch_prob_all_time_2 %>% mutate(OF_ball_angle = acos(-OF_ball_front_dist / OF_ball_dist))
catch_prob_all_time_2 <- catch_prob_all_time_2 %>% mutate(unit_x = field_x / sqrt(field_x^2 + field_y^2),
                                                    unit_y = field_y / sqrt(field_x^2 + field_y^2),
                                                    OF_ball_side_dist = OF_ball_dist * sin(OF_ball_angle),
                                                    pos_side_diff = abs( (field_x + (OF_ball_front_dist * -unit_x) + (OF_ball_side_dist * unit_y)) - ground_x ),
                                                    neg_side_diff = abs( (field_x + (OF_ball_front_dist * -unit_x) + (OF_ball_side_dist * -unit_y)) - ground_x ),
                                                    OF_ball_side_dist = ifelse(pos_side_diff < neg_side_diff,
                                                                               OF_ball_side_dist, -OF_ball_side_dist)) %>% 
                                             dplyr::select(-c(unit_x, unit_y, pos_side_diff, neg_side_diff)) %>%
                                             relocate(OF_ball_side_dist, .before = OF_ball_angle)
catch_prob_all_time_2 <- catch_prob_all_time_2 %>% mutate(OF_ball_angle = atan2(OF_ball_side_dist, OF_ball_front_dist))


catch_prob_all_time_2 <- catch_prob_all_time_2 %>% mutate(velo_ball = ((x_velo * OF_ball_x_dist) + (y_velo * OF_ball_y_dist)) / 
                                                             sqrt(OF_ball_x_dist^2 + OF_ball_y_dist^2))
catch_prob_all_time_2 <- catch_prob_all_time_2 %>% mutate(velo_on_angle = acos(velo_ball / speed))
catch_prob_all_time_2 <- catch_prob_all_time_2 %>% mutate(unit_x = OF_ball_x_dist / OF_ball_dist,
                                              unit_y = OF_ball_y_dist / OF_ball_dist,
                                              velo_side = speed * sin(velo_on_angle),
                                              pos_side_diff = abs( ((velo_ball * unit_x) + (velo_side * unit_y)) - x_velo ),
                                              neg_side_diff = abs( ((velo_ball * unit_x) + (velo_side * -unit_y)) - x_velo ),
                                              velo_side = ifelse(pos_side_diff < neg_side_diff,
                                                                    velo_side, -velo_side)) %>% 
                                       dplyr::select(-c(unit_x, unit_y, pos_side_diff, neg_side_diff)) %>%
                                       relocate(velo_side, .before = velo_on_angle)
catch_prob_all_time_2 <- catch_prob_all_time_2 %>% mutate(velo_on_angle = atan2(velo_side, velo_ball))


catch_prob_all_time_2 <- catch_prob_all_time_2 %>% mutate(accel_ball = ((x_accel * OF_ball_x_dist) + (y_accel * OF_ball_y_dist)) / 
                                                             sqrt(OF_ball_x_dist^2 + OF_ball_y_dist^2))
catch_prob_all_time_2 <- catch_prob_all_time_2 %>% mutate(accel_on_angle = acos(accel_ball / accel_mag ))
catch_prob_all_time_2 <- catch_prob_all_time_2 %>% mutate(unit_x = OF_ball_x_dist / OF_ball_dist,
                                              unit_y = OF_ball_y_dist / OF_ball_dist,
                                              accel_side = accel_mag * sin(accel_on_angle),
                                              pos_side_diff = abs( ((accel_ball * unit_x) + (accel_side * unit_y)) - x_accel ),
                                              neg_side_diff = abs( ((accel_ball * unit_x) + (accel_side * -unit_y)) - x_accel ),
                                              accel_side = ifelse(pos_side_diff < neg_side_diff,
                                                                    accel_side, -accel_side)) %>% 
                                       dplyr::select(-c(unit_x, unit_y, pos_side_diff, neg_side_diff)) %>%
                                       relocate(accel_side, .before = accel_on_angle)
catch_prob_all_time_2 <- catch_prob_all_time_2 %>% mutate(accel_side = ifelse(is.na(accel_side), 0, accel_side),
                                              accel_on_angle = atan2(accel_side, accel_ball))


catch_prob_all_time_2 <- catch_prob_all_time_2 %>% filter(time_left >= 0)

#####################################################################################################################################################################

lagged_player_movement <- catch_prob_all_time_2 %>% group_by(play_key, player_id) %>% filter(caught == 1, row_number() == n()) %>% select(OF_ball_dist, OF_ball_angle, time_left, caught)

hist(lagged_player_movement$OF_ball_dist, breaks = 100)

lagged_player_movement <- lagged_player_movement %>% filter(OF_ball_dist > 16)

catch_prob_all_time_2 <- catch_prob_all_time_2 %>% filter(!(play_key %in% lagged_player_movement$play_key))


save.image("save.Rdata")

#####################################################################################################################################################################

set.seed(279)
catch_prob_folds_2 <- createFolds(catch_prob_all_time_2$caught, k = 2)


act <- c()
pred <- c()
for(fold in catch_prob_folds_2) {
  train <- catch_prob_all_time_2[-fold, ]
  test <- catch_prob_all_time_2[fold, ]
  model <- gam(caught ~ s(OF_ball_dist, k = 6) + s(OF_ball_angle, k = 5) + s(time_left, k = 6) + 
               s(velo_ball, k = 3) + s(accel_ball, k = 3) + s(wall_ball_dist, k = 5) + s(launch_angle, k = 3), 
               family = binomial, data = train)
  act <- c(act, test$caught)
  pred <- c(pred, predict(model, newdata = test, type = "response"))
}
logLoss(act, pred)
### 0.1478224
mean(act == ifelse(pred > 0.5, 1, 0))
### 0.9513262

plot(model, page=1)
plot(pred, act)



catch_prob_model <- gam(caught ~ s(OF_ball_dist, k = 6) + s(OF_ball_angle, k = 3) + s(time_left, k = 6) + 
                        s(velo_ball, k = 3) + s(accel_ball, k = 3) + s(wall_ball_dist, k = 5) + s(launch_angle, k = 3), 
                        family = binomial, data = catch_prob_all_time_2)

catch_prob_all_time_pred <- catch_prob_all_time_2 %>% ungroup() %>% mutate(catch_prob = predict(catch_prob_model, type = "response"))


catch_correlations <- round(cor(catch_prob_all_time_pred[,3:41] , (catch_prob_all_time_pred$caught - catch_prob_all_time_pred$catch_prob) ), 3)


ggplot(catch_prob_all_time_2, aes(x = OF_ball_dist, y = time_left, color = caught)) + 
geom_point() + scale_color_gradient2(high = "green", low = "red", midpoint = 0.5)

ggplot(catch_prob_all_time_pred, aes(x = OF_ball_dist, y = time_left, color = caught - catch_prob)) + 
geom_point() + scale_color_gradient2(high = "green", low = "red", midpoint = 0)


caught_catch_prob <- catch_prob_all_time_pred %>% group_by(play_key, player_id) %>% filter(caught == 1, row_number() == n())

#####################################################################################################################################################################

ball_need_positions <- ball_positions %>% mutate(play_key = paste0(game_string, play_per_game)) %>% filter(play_key %in% catch_prob_all_time_2$play_key)

catch_prob_all_time_3 <- catch_prob_all_time_2 %>% left_join(ball_need_positions[,c(3:6,11)], by = c("play_key", "timestamp"))
catch_prob_all_time_3 <- catch_prob_all_time_3 %>% mutate(xyz_ball_dist = sqrt((pred_x - ball_position_x)^2 + (pred_y - ball_position_y)^2 + (ball_position_z - 3)^2) ) %>% 
                                                   filter(!is.na(xyz_ball_dist))


set.seed(238)
catch_prob_folds_3 <- createFolds(catch_prob_all_time_3$caught, k = 2)


act <- c()
pred <- c()
for(fold in catch_prob_folds_3) {
  train <- catch_prob_all_time_3[-fold, ]
  test <- catch_prob_all_time_3[fold, ]
  model <- gam(caught ~ s(OF_ball_dist, k = 6) + s(OF_ball_angle, k = 5) + s(time_left, k = 6) + 
               s(velo_ball, k = 3) + s(accel_ball, k = 3) + s(wall_ball_dist, k = 5) + s(launch_angle, k = 3) + s(xyz_ball_dist, k = 3), 
               family = binomial, data = train)
  act <- c(act, test$caught)
  pred <- c(pred, predict(model, newdata = test, type = "response"))
}
logLoss(act, pred)
### 0.1448625

plot(model, page=1)


catch_prob_model_2 <- gam(caught ~ s(OF_ball_dist, k = 6) + s(OF_ball_angle, k = 3) + s(time_left, k = 6) + 
                        s(velo_ball, k = 3) + s(accel_ball, k = 3) + s(wall_ball_dist, k = 5) + s(launch_angle, k = 3) + s(xyz_ball_dist, k = 3), 
                        family = binomial, data = catch_prob_all_time_3)

catch_prob_all_time_pred_2 <- catch_prob_all_time_3 %>% ungroup() %>% mutate(catch_prob = predict(catch_prob_model_2, type = "response"))


caught_catch_prob <- catch_prob_all_time_pred_2 %>% group_by(play_key, player_id) %>% filter(caught == 1, row_number() == n())

#####################################################################################################################################################################
library(randomForest)

set.seed(108)
catch_prob_folds_4 <- groupKFold(catch_prob_all_time_3$play_key, k = 2)


catch_prob_all_time_pred_3 <- data.frame()
for(fold in catch_prob_folds_4) {
  train <- catch_prob_all_time_3[-fold, ] %>% ungroup()
  test <- catch_prob_all_time_3[fold, ] %>% ungroup()
  model <- randomForest(as.factor(caught) ~ OF_ball_dist + OF_ball_angle + time_left + velo_ball + accel_ball + time_to_ground + wall_ball_dist + xy_ball_dist, 
                        data = train, ntree = 300)

  test <- test %>% mutate(catch_prob = round( pmin( pmax( predict(model, newdata = test, type = "prob")[, 2], 0.001 ), 0.999 ), 3) )
  catch_prob_all_time_pred_3 <- bind_rows(catch_prob_all_time_pred_3, test)
}
logLoss(catch_prob_all_time_pred_3$caught, catch_prob_all_time_pred_3$catch_prob)
### 0.1647268


catch_correlations <- round(cor(catch_prob_all_time_pred_3[,3:45] , (catch_prob_all_time_pred_3$caught - catch_prob_all_time_pred_3$catch_prob) ), 3)



group <- 0
catch_prob_all_time_pred_4 <- catch_prob_all_time_pred_3 %>% group_by(play_key, player_id) %>%
                 group_modify(~{group <<- group + 1
                              message(group)
                                
                              model <- gam(catch_prob ~ s(time_left, k = 5), data = .x )
                              .x$smooth_catch_prob <- round( pmin( pmax(predict(model), 0.001), 0.999), 3)
                              .x})
logLoss(catch_prob_all_time_pred_4$caught, catch_prob_all_time_pred_4$smooth_catch_prob)
### 0.01693772


caught <- catch_prob_all_time_pred_4 %>% filter(caught == 1) %>% group_by(play_key, player_id) %>% filter(row_number() == 1)

