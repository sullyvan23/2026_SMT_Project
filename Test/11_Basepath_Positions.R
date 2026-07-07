basepath_deviation <- advance_one_data_sum %>% select(game_string:player_id_br, timestamp, pred_x_runner:pred_y_runner, basepath, speed_95_runner)
basepath_deviation <- bind_rows(basepath_deviation, tag_up_data_sum %>% select(game_string:player_id_br, timestamp, pred_x_runner:pred_y_runner, basepath, speed_95_runner))
basepath_deviation <- bind_rows(basepath_deviation, doubled_up_data_sum %>% select(game_string:player_id_br, timestamp, pred_x_runner:pred_y_runner, basepath, speed_95_runner))
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
                                      next_velo_diff = lead(basepath_velo) - basepath_velo)

basepath <- basepath %>% mutate(fps = timestamp - lag(timestamp))
basepath <- basepath %>% filter(!is.na(basepath_jerk), abs(basepath - round(basepath)) > 0.05)

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
                                                         highest_next = quantile(next_velo_diff, probs = 0.75, na.rm = TRUE),
                                                         lowest_next = quantile(next_velo_diff, probs = 0.25, na.rm = TRUE),
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
  model <- gam(highest_next ~ s(basepath_velo, k = 3) + s(basepath_accel, k = 3) + s(basepath_jerk, k = 3), 
               data = train)
  act <- c(act, test$highest_next) 
  pred <- c(pred, predict(model, newdata = test))
}
RMSE(act, pred)
### 0.004071842

plot(model, pages = 1)
summary(model)

act <- c()
pred <- c()
for(fold in next_speed_folds) {
  train <- possible_next_speeds[-fold, ]
  test <- possible_next_speeds[fold, ]
  model <- gam(lowest_next ~ te(runner_basepath_velo, runner_basepath_accel, k = 5) + speed_95_runner, 
               data = train)
  act <- c(act, test$lowest_next) 
  pred <- c(pred, predict(model, newdata = test))
}
RMSE(act, pred)
### 0.002924959



highest_next_velo_diff_model <- gam(highest_next ~ te(runner_basepath_velo, runner_basepath_accel, k = 5) + speed_95_runner,  data = possible_next_speeds)
lowest_next_velo_diff_model <- gam(lowest_next ~ te(runner_basepath_velo, runner_basepath_accel, k = 5) + speed_95_runner,  data = possible_next_speeds)

possible_next_speeds <- possible_next_speeds %>% ungroup() %>% mutate(pred_highest_next = predict(highest_next_velo_model),
                                                                      pred_lowest_next = predict(lowest_next_velo_model))


ggplot(possible_next_speeds, aes(x = runner_basepath_velo, y = runner_basepath_accel, color = pred_highest_next)) + 
       geom_point() + scale_color_gradient2(high = "green", low = "red", mid = "white", midpoint = mean(possible_next_speeds$pred_highest_next))

ggplot(possible_next_speeds, aes(x = runner_basepath_velo, y = runner_basepath_accel, color = pred_lowest_next)) + 
       geom_point() + scale_color_gradient2(high = "green", low = "red", mid = "white", midpoint = mean(possible_next_speeds$pred_lowest_next))
