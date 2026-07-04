
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

basepath_deviation <- basepath_deviation %>% group_by(game_string, play_per_game, player_id_br) %>% 
                                             mutate(basepath_velo = (basepath - lag(basepath)) / ((timestamp - lag(timestamp))/1000))

basepath_deviation <- basepath_deviation %>% mutate(next_basepath_velo = lead(basepath_velo),
                                                    diff = next_basepath_velo - basepath_velo)

plot(basepath_deviation$basepath, basepath_deviation$basepath_velo)
plot(basepath_deviation$basepath, basepath_deviation$next_basepath_velo)

basepath_deviation <- basepath_deviation %>% filter(abs(basepath - round(basepath)) > 0.05)


plot(basepath_deviation$basepath_velo, basepath_deviation$diff)

####################################################################################################################################################################################

possible_next_speeds <- basepath_deviation %>% mutate(next_basepath_velo = next_basepath_velo * sign(basepath_velo),
                                                      basepath_velo = round(abs(basepath_velo), 2)) %>% 
                                               group_by(basepath_velo) %>% filter(!is.na(basepath_velo)) %>%
                                               summarise(highest_next = quantile(next_basepath_velo, probs = 0.95, na.rm = TRUE),
                                                         lowest_next = quantile(next_basepath_velo, probs = 0.05, na.rm = TRUE),
                                                         count = n())
possible_next_speeds <- possible_next_speeds %>% filter(basepath_velo <= 0.3)

possible_next_speeds <- bind_rows(possible_next_speeds,
                                  possible_next_speeds[2:31,] %>% mutate(across(c(basepath_velo:lowest_next), ~ -.)) %>%
                                                                  rename(lowest_next = highest_next, highest_next = lowest_next))


set.seed(299)
next_speed_folds <- createFolds(possible_next_speeds$basepath_velo, k = 61)


act <- c()
pred <- c()
for(fold in next_speed_folds) {
  train <- possible_next_speeds[-fold, ]
  test <- possible_next_speeds[fold, ]
  model <- gam(highest_next ~ s(basepath_velo, k = 7), 
               data = train)
  act <- c(act, test$highest_next) 
  pred <- c(pred, predict(model, newdata = test))
}
RMSE(act, pred)
### 0.00060472


act <- c()
pred <- c()
for(fold in next_speed_folds) {
  train <- possible_next_speeds[-fold, ]
  test <- possible_next_speeds[fold, ]
  model <- gam(lowest_next ~ s(basepath_velo, k = 6), 
               data = train)
  act <- c(act, test$lowest_next) 
  pred <- c(pred, predict(model, newdata = test))
}
RMSE(act, pred)
### 0.001753441



highest_next_velo_model <- gam(highest_next ~ s(basepath_velo, k = 7),  data = possible_next_speeds)
lowest_next_velo_model <- gam(lowest_next ~ s(basepath_velo, k = 6),  data = possible_next_speeds)








