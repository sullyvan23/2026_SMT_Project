basepath_deviation <- advance_one_data_sum %>% select(game_string:player_id_br, timestamp, pred_x_runner:pred_y_runner, rmse_x:rmse_y, basepath, speed_95_runner)
basepath_deviation <- bind_rows(basepath_deviation, tag_up_data_sum %>% select(game_string:player_id_br, timestamp, pred_x_runner:pred_y_runner, rmse_x:rmse_y, basepath, speed_95_runner))
basepath_deviation <- bind_rows(basepath_deviation, doubled_up_data_sum %>% select(game_string:player_id_br, timestamp, pred_x_runner:pred_y_runner, rmse_x:rmse_y, basepath, speed_95_runner))
basepath_deviation <- basepath_deviation %>% filter(basepath < 4)
basepath_deviation <- basepath_deviation %>% group_by(game_string, play_per_game, player_id_br, timestamp) %>% slice(1)

ggplot(basepath_deviation, aes(x = pred_x_runner, y = pred_y_runner, color = basepath)) + 
       geom_point() + scale_color_gradient2(high = "green", low = "red", mid = "white", midpoint = 2.5)

plot(basepath_deviation$basepath, basepath_deviation$pred_x_runner)
plot(basepath_deviation$basepath, basepath_deviation$pred_y_runner)

####################################################################################################################################################################################

set.seed(377)
basepath_folds <- groupKFold(basepath_deviation$basepath, k = 5)

act <- c()
pred <- c()
for(fold in basepath_folds) {
  print("-")
  train <- basepath_deviation[-fold, ]
  test <- basepath_deviation[fold, ]
  model <- gam(pred_x_runner ~ s(basepath, k = 50), 
               data = train)
  act <- c(act, test$pred_x_runner) 
  pred <- c(pred, predict(model, newdata = test))
}
RMSE(act, pred)

act <- c()
pred <- c()
for(fold in basepath_folds) {
  print("-")
  train <- basepath_deviation[-fold, ]
  test <- basepath_deviation[fold, ]
  model <- gam(pred_y_runner ~ s(basepath, k = 50), 
               data = train)
  act <- c(act, test$pred_y_runner) 
  pred <- c(pred, predict(model, newdata = test))
}
RMSE(act, pred)

plot(model, page = 1)


basepath_x_model <- gam(pred_x_runner ~ s(basepath, k = 50), data = basepath_deviation)
basepath_y_model <- gam(pred_y_runner ~ s(basepath, k = 50), data = basepath_deviation)


basepath_pred_positions <- expand.grid(basepath = seq(1, 4, by = 0.01))
basepath_pred_positions <- basepath_pred_positions %>% mutate(field_x = predict(basepath_x_model, newdata = basepath_pred_positions),
                                                              field_y = predict(basepath_y_model, newdata = basepath_pred_positions))

plot(basepath_pred_positions$field_x, basepath_pred_positions$field_y)

####################################################################################################################################################################################

basepath <- basepath_deviation %>% group_by(game_string, play_per_game, player_id_br) %>% 
                               mutate(basepath_velo = (basepath - lag(basepath)) / ((timestamp - lag(timestamp))/1000),
                                      basepath_accel = (basepath_velo - lag(basepath_velo)) / ((timestamp - lag(timestamp))/1000),
                                      basepath_jerk = (basepath_accel - lag(basepath_accel)) / ((timestamp - lag(timestamp))/1000),
                                      next_accel_diff = lead(basepath_accel) - basepath_accel)

basepath <- basepath %>% mutate(fps = timestamp - lag(timestamp))
basepath <- basepath %>% filter(!is.na(basepath_jerk), abs(basepath - round(basepath)) > 0.05)

hist(basepath$rmse_x, breaks = 100)
hist(basepath$rmse_y, breaks = 100)

basepath <- basepath %>% filter(rmse_x <= 0.05, rmse_y <= 0.05)

plot(basepath$basepath_velo, basepath$basepath_accel)
plot(basepath$basepath_accel, basepath$basepath_jerk)

####################################################################################################################################################################################

possible_next_speeds <- basepath %>% mutate(basepath_accel = basepath_accel * sign(basepath_velo),
                                            basepath_jerk = basepath_jerk * sign(basepath_velo),
                                            basepath_velo = abs(basepath_velo))

possible_next_speeds <- possible_next_speeds %>% mutate(basepath_velo = round(basepath_velo, 2),
                                                        basepath_accel = round(basepath_accel, 2)) %>%
                                               group_by(basepath_velo, basepath_accel) %>% filter(fps == 50) %>%
                                               summarise(basepath_jerk = mean(basepath_jerk),
                                                         highest_next = quantile(next_accel_diff, probs = 0.8, na.rm = TRUE),
                                                         lowest_next = quantile(next_accel_diff, probs = 0.2, na.rm = TRUE),
                                                         speed_95_runner = mean(speed_95_runner),
                                                         count = n())

possible_next_speeds <- possible_next_speeds %>% filter(count >= 50)

ggplot(possible_next_speeds, aes(x = basepath_velo, y = basepath_accel, color = highest_next)) + 
       geom_point() + scale_color_gradient2(high = "green", low = "red", mid = "white", midpoint = mean(possible_next_speeds$highest_next))

ggplot(possible_next_speeds, aes(x = basepath_velo, y = basepath_accel, color = lowest_next)) + 
       geom_point() + scale_color_gradient2(high = "green", low = "red", mid = "white", midpoint = mean(possible_next_speeds$lowest_next))



set.seed(299)
next_speed_folds <- createFolds(possible_next_speeds$basepath_velo, k = 10)


act <- c()
pred <- c()
for(fold in next_speed_folds) {
  train <- possible_next_speeds[-fold, ]
  test <- possible_next_speeds[fold, ]
  model <- gam(highest_next ~ te(basepath_velo, basepath_accel, k = 4), 
               data = train)
  act <- c(act, test$highest_next) 
  pred <- c(pred, predict(model, newdata = test))
}
RMSE(act, pred)
### 0.002071038

plot(model, pages = 1)
summary(model)

act <- c()
pred <- c()
for(fold in next_speed_folds) {
  train <- possible_next_speeds[-fold, ]
  test <- possible_next_speeds[fold, ]
  model <- gam(lowest_next ~ te(basepath_velo, basepath_accel, k = 5), 
               data = train)
  act <- c(act, test$lowest_next) 
  pred <- c(pred, predict(model, newdata = test))
}
RMSE(act, pred)
### 0.001493879



highest_next_accel_diff_model <- gam(highest_next ~ te(basepath_velo, basepath_accel, k = 4),  data = possible_next_speeds)
lowest_next_accel_diff_model <- gam(lowest_next ~ te(basepath_velo, basepath_accel, k = 5),  data = possible_next_speeds)

possible_next_speeds <- possible_next_speeds %>% ungroup() %>% mutate(pred_highest_next = predict(highest_next_accel_diff_model),
                                                                      pred_lowest_next = predict(lowest_next_accel_diff_model))


ggplot(possible_next_speeds, aes(x = basepath_velo, y = basepath_accel, color = pred_highest_next)) + 
       geom_point() + scale_color_gradient2(high = "green", low = "red", mid = "white", midpoint = mean(possible_next_speeds$pred_highest_next))

ggplot(possible_next_speeds, aes(x = basepath_velo, y = basepath_accel, color = pred_lowest_next)) + 
       geom_point() + scale_color_gradient2(high = "green", low = "red", mid = "white", midpoint = mean(possible_next_speeds$pred_lowest_next))

####################################################################################################################################################################################

basepath_last_velos <- basepath %>% mutate(basepath_velo_2 = lag(basepath_velo),
                                           basepath_velo_3 = lag(basepath_velo, 2),
                                           velo_diff = lead(basepath_velo) - basepath_velo) %>%
                                    filter(!is.na(basepath_velo_3), !is.na(velo_diff))
basepath_last_velos <- basepath_last_velos %>% mutate(basepath_velo_2 = (round(basepath_velo_2, 2) * sign(basepath_velo)) - round(abs(basepath_velo), 2),
                                                      basepath_velo_3 = (round(basepath_velo_3, 2) * sign(basepath_velo)) - round(abs(basepath_velo), 2),
                                                      basepath_velo = round(abs(basepath_velo), 2)) %>%
                                               group_by(basepath_velo, basepath_velo_2, basepath_velo_3) %>%
                                               summarise(high_diff = quantile(velo_diff, probs = 0.9, na.rm = TRUE),
                                                         low_diff = quantile(velo_diff, probs = 0.1, na.rm = TRUE),
                                                         count = n())

basepath_last_velos <- basepath_last_velos %>% filter(count > 20)

ggplot(basepath_last_velos, aes(x = basepath_velo, y = basepath_velo_2, color = high_diff)) + 
       geom_point() + scale_color_gradient2(high = "green", low = "red", mid = "white", midpoint = mean(basepath_last_velos$high_diff))
ggplot(basepath_last_velos, aes(x = basepath_velo, y = basepath_velo_2, color = low_diff)) + 
       geom_point() + scale_color_gradient2(high = "green", low = "red", mid = "white", midpoint = mean(basepath_last_velos$low_diff))





set.seed(299)
next_speed_folds <- createFolds(basepath_last_velos$high_diff, k = 10)


act <- c()
pred <- c()
for(fold in next_speed_folds) {
  train <- basepath_last_velos[-fold, ]
  test <- basepath_last_velos[fold, ]
  model <- gam(high_diff ~ s(basepath_velo, k = 3) + te(basepath_velo_2, basepath_velo_3, k = 3), 
               data = train)
  act <- c(act, test$high_diff) 
  pred <- c(pred, predict(model, newdata = test))
}
RMSE(act, pred)
### 0.001818805

plot(model, pages = 1)
summary(model)

act <- c()
pred <- c()
for(fold in next_speed_folds) {
  train <- basepath_last_velos[-fold, ]
  test <- basepath_last_velos[fold, ]
  model <- gam(low_diff ~ s(basepath_velo, k = 3) + te(basepath_velo_2, basepath_velo_3, k = 3), 
               data = train)
  act <- c(act, test$low_diff) 
  pred <- c(pred, predict(model, newdata = test))
}
RMSE(act, pred)
### 0.002370982

