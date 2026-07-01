
basepath_deviation <- advance_one_data_sum %>% select(game_string:player_id_br, timestamp, pred_x_runner:pred_y_runner, basepath, speed_95_runner)
basepath_deviation <- basepath_deviation %>% filter(basepath < 4)

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
                                             mutate(basepath_velo = (basepath - lag(basepath)) / ((timestamp - lag(timestamp))/1000),
                                                    basepath_accel = (basepath_velo - lag(basepath_velo)) / ((timestamp - lag(timestamp))/1000))

plot(basepath_deviation$basepath_velo, basepath_deviation$basepath_accel)

basepath_deviation <- basepath_deviation %>% mutate(last_base = floor(basepath)) %>%
                                             filter(last_base == lag(last_base, 4))

basepath_deviation <- basepath_deviation %>% mutate(next_basepath_velo = lead(basepath_velo),
                                                    diff = next_basepath_velo - basepath_velo)

plot(basepath_deviation$basepath_velo, basepath_deviation$diff)

### prob can change by 0.1 every time
