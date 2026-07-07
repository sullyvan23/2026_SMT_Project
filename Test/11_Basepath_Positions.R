
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
                                             mutate(basepath_velo = (basepath - lag(basepath)) / ((timestamp - lag(timestamp))/1000),
                                                    basepath_velo_2 = lag(basepath_velo),
                                                    basepath_velo_3 = lag(basepath_velo, 2),
                                                    next_dist_diff = basepath - lag(basepath))

basepath_deviation <- basepath_deviation %>% mutate(fps = timestamp - lag(timestamp))
basepath_deviation <- basepath_deviation %>% filter(!is.na(basepath_velo_3), abs(basepath - round(basepath)) > 0.05)

####################################################################################################################################################################################

possible_next_speeds <- basepath_deviation %>% mutate(basepath_velo_2 = basepath_velo_2 * sign(basepath_velo),
                                                      basepath_velo_3 = basepath_velo_3 * sign(basepath_velo),
                                                      dist_next_base = ifelse(basepath_velo > 0, ceiling(basepath) - basepath,
                                                                                                 basepath - floor(basepath)),
                                                      basepath_velo = abs(basepath_velo))

possible_next_speeds <- possible_next_speeds %>% mutate(basepath_velo = round(basepath_velo, 3),
                                                        basepath_velo_2 = round(basepath_velo_2, 3),
                                                        basepath_velo_3 = round(basepath_velo_3, 3)) %>%
                                               group_by(basepath_velo, basepath_velo_2, basepath_velo_3) %>% filter(fps == 50) %>%
                                               summarise(highest_next = quantile(next_dist_diff, probs = 0.95, na.rm = TRUE),
                                                         lowest_next = quantile(next_dist_diff, probs = 0.05, na.rm = TRUE),
                                                         speed_95_runner = mean(speed_95_runner),
                                                         dist_next_base = mean(dist_next_base),
                                                         count = n())

library(tidyr)
possible_next_speeds <- possible_next_speeds %>% uncount(count)

ggplot(possible_next_speeds, aes(x = basepath_velo, y = basepath_velo_2, color = highest_next)) + 
       geom_point() + scale_color_gradient2(high = "green", low = "red", mid = "white", midpoint = mean(possible_next_speeds$highest_next))

ggplot(possible_next_speeds, aes(x = basepath_velo, y = basepath_velo_2, color = lowest_next)) + 
       geom_point() + scale_color_gradient2(high = "green", low = "red", mid = "white", midpoint = mean(possible_next_speeds$lowest_next))


set.seed(299)
next_speed_folds <- createFolds(possible_next_speeds$basepath_velo, k = 10)


act <- c()
pred <- c()
for(fold in next_speed_folds) {
  print("-")
  train <- possible_next_speeds[-fold, ]
  test <- possible_next_speeds[fold, ]
  model <- gam(highest_next ~ te(basepath_velo, basepath_velo_2, basepath_velo_3, k = 4) + s(dist_next_base, k = 3), 
               data = train)
  act <- c(act, test$highest_next) 
  pred <- c(pred, predict(model, newdata = test))
}
RMSE(act, pred)
### 0.001241472

plot(model, pages = 1)
summary(model)

act <- c()
pred <- c()
for(fold in next_speed_folds) {
  train <- possible_next_speeds[-fold, ]
  test <- possible_next_speeds[fold, ]
  model <- gam(lowest_next ~ te(basepath_velo, basepath_velo_2, basepath_velo_3, k = 4) + dist_next_base, 
               data = train)
  act <- c(act, test$lowest_next) 
  pred <- c(pred, predict(model, newdata = test))
}
RMSE(act, pred)
### 0.003259433



highest_next_velo_model <- gam(highest_next ~ te(runner_basepath_velo, runner_basepath_accel, k = 5) + speed_95_runner,  data = possible_next_speeds)
lowest_next_velo_model <- gam(lowest_next ~ te(runner_basepath_velo, runner_basepath_accel, k = 5) + speed_95_runner,  data = possible_next_speeds)

possible_next_speeds <- possible_next_speeds %>% ungroup() %>% mutate(pred_highest_next = predict(highest_next_velo_model),
                                                                      pred_lowest_next = predict(lowest_next_velo_model))


ggplot(possible_next_speeds, aes(x = runner_basepath_velo, y = runner_basepath_accel, color = pred_highest_next)) + 
       geom_point() + scale_color_gradient2(high = "green", low = "red", mid = "white", midpoint = mean(possible_next_speeds$pred_highest_next))

ggplot(possible_next_speeds, aes(x = runner_basepath_velo, y = runner_basepath_accel, color = pred_lowest_next)) + 
       geom_point() + scale_color_gradient2(high = "green", low = "red", mid = "white", midpoint = mean(possible_next_speeds$pred_lowest_next))



