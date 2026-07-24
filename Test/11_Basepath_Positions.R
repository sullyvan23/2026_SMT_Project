basepath_deviation <- advance_one_data_sum %>% select(game_string:player_id_br, timestamp, field_x:field_y, rmse_bp, basepath, speed_95_runner)
basepath_deviation <- bind_rows(basepath_deviation, tag_up_data_sum %>% select(game_string:player_id_br, timestamp, field_x:field_y, rmse_bp, basepath, speed_95_runner))
basepath_deviation <- bind_rows(basepath_deviation, doubled_up_data_sum %>% select(game_string:player_id_br, timestamp, field_x:field_y, rmse_bp, basepath, speed_95_runner))
basepath_deviation <- basepath_deviation %>% filter(basepath < 4)
basepath_deviation <- basepath_deviation %>% group_by(game_string, play_per_game, player_id_br, timestamp) %>% slice(1)

ggplot(basepath_deviation, aes(x = field_x, y = field_y, color = basepath)) + 
       geom_point() + scale_color_gradient2(high = "green", low = "red", mid = "white", midpoint = 2.5)

plot(basepath_deviation$basepath, basepath_deviation$field_x)
plot(basepath_deviation$basepath, basepath_deviation$field_y)

####################################################################################################################################################################################

set.seed(377)
basepath_folds <- groupKFold(basepath_deviation$basepath, k = 2)

act <- c()
pred <- c()
for(fold in basepath_folds) {
  print("-")
  train <- basepath_deviation[-fold, ]
  test <- basepath_deviation[fold, ]
  model <- gam(field_x ~ s(basepath, k = 20), 
               data = train)
  act <- c(act, test$field_x) 
  pred <- c(pred, predict(model, newdata = test))
}
RMSE(act, pred)
### 1.364287

act <- c()
pred <- c()
for(fold in basepath_folds) {
  print("-")
  train <- basepath_deviation[-fold, ]
  test <- basepath_deviation[fold, ]
  model <- gam(field_y ~ s(basepath, k = 20), 
               data = train)
  act <- c(act, test$field_y) 
  pred <- c(pred, predict(model, newdata = test))
}
RMSE(act, pred)
### 1.545511

plot(model, page = 1)


basepath_x_model <- gam(field_x ~ s(basepath, k = 20), data = basepath_deviation)
basepath_y_model <- gam(field_y ~ s(basepath, k = 20), data = basepath_deviation)

plot(basepath_x_model, page = 1)


basepath_pred_positions <- expand.grid(basepath = seq(1, 4, by = 0.01),
                                       player_id_br = seq(11, 13, by = 1))
basepath_pred_positions <- basepath_pred_positions %>% mutate(field_x = predict(basepath_x_model, newdata = basepath_pred_positions),
                                                              field_y = predict(basepath_y_model, newdata = basepath_pred_positions))

plot(basepath_pred_positions$field_x, basepath_pred_positions$field_y)

####################################################################################################################################################################################

basepath <- basepath_deviation %>% group_by(game_string, play_per_game, player_id_br) %>% 
                               mutate(basepath_velo = (basepath - lag(basepath)) / ((timestamp - lag(timestamp))/1000),
                                      basepath_accel = (basepath_velo - lag(basepath_velo)) / ((timestamp - lag(timestamp))/1000),
                                      basepath_accel_2 = lag(basepath_accel),
                                      next_accel_diff = lead(basepath_accel) - basepath_accel,
                                      next_velo_diff = lead(basepath_velo) - basepath_velo,
                                      accel_diff_diff = next_accel_diff - (basepath_accel - basepath_accel_2))

basepath <- basepath %>% mutate(fps = timestamp - lag(timestamp))
basepath <- basepath %>% filter(!is.na(basepath_accel_2), !is.na(next_accel_diff), abs(basepath - round(basepath)) > 0.05)

basepath <- basepath %>% filter(rmse_x <= 0.05, rmse_y <= 0.05)

basepath <- basepath %>% mutate(og_basepath_dist = basepath - player_id_br + 10)


hist(basepath$rmse_x, breaks = 100)
hist(basepath$rmse_y, breaks = 100)


accel_diff_diff_check <- basepath %>% filter(abs(accel_diff_diff) < 0.02)
hist(accel_diff_diff_check$accel_diff_diff, breaks = 100)

plot(basepath$basepath_velo, basepath$basepath_accel)
plot(basepath$basepath_accel, basepath$basepath_jerk)
plot(basepath$og_basepath_dist, basepath$basepath_accel)

####################################################################################################################################################################################

player95_to_basepath <- basepath %>% group_by(speed_95_runner) %>% mutate(speed_95_runner = round(speed_95_runner, 1)) %>%
                                     summarise(basepath_velo = quantile(basepath_velo, probs = 0.99, na.rm = TRUE),
                                               basepath_accel = quantile(basepath_accel, probs = 0.99, na.rm = TRUE),
                                               count = n())

####################################################################################################################################################################################

possible_next_speeds <- basepath %>% mutate(next_velo_diff = next_velo_diff * sign(basepath_velo),
                                            next_accel_diff = next_accel_diff * sign(basepath_velo),
                                            basepath_accel = basepath_accel * sign(basepath_velo),
                                            basepath_accel_2 = basepath_accel_2 * sign(basepath_velo),
                                            basepath_velo = abs(basepath_velo))

possible_next_speeds <- possible_next_speeds %>% mutate(basepath_velo = round(basepath_velo, 2),
                                                        basepath_accel = round(basepath_accel, 2)) %>%
                                               group_by(basepath_velo, basepath_accel) %>% filter(fps == 50) %>%
                                               summarise(basepath_accel_2 = mean(basepath_accel_2),
                                                         next_velo_diff = mean(next_velo_diff),
                                                         next_accel_diff = mean(next_accel_diff),
                                                         speed_95_runner = mean(speed_95_runner),
                                                         count = n())

possible_next_speeds <- possible_next_speeds %>% filter(count >= 20)

ggplot(possible_next_speeds, aes(x = basepath_velo, y = basepath_accel, color = next_velo_diff)) + 
       geom_point() + scale_color_gradient2(high = "green", low = "red", mid = "white", midpoint = mean(possible_next_speeds$next_velo_diff))

####################################################################################################################################################################################

possible_next_speeds <- basepath %>% mutate(next_velo_diff = next_velo_diff * sign(basepath_velo),
                                            next_accel_diff = next_accel_diff * sign(basepath_velo),
                                            accel_diff_diff = accel_diff_diff * sign(basepath_velo),
                                            basepath_accel = basepath_accel * sign(basepath_velo),
                                            basepath_accel_2 = basepath_accel_2 * sign(basepath_velo),
                                            basepath_velo = abs(basepath_velo))

possible_next_speeds <- possible_next_speeds %>% mutate(basepath_velo = round(basepath_velo, 2),
                                                        basepath_accel = round(basepath_accel, 2)) %>%
                                               group_by(basepath_velo, basepath_accel) %>% filter(fps == 50) %>%
                                               summarise(basepath_accel_2 = mean(basepath_accel_2),
                                                         highest_next = quantile(accel_diff_diff, probs = 0.95, na.rm = TRUE),
                                                         lowest_next = quantile(accel_diff_diff, probs = 0.05, na.rm = TRUE),
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
  model <- gam(highest_next ~ te(basepath_velo, basepath_accel, k = 3), 
               data = train)
  act <- c(act, test$highest_next) 
  pred <- c(pred, predict(model, newdata = test))
}
RMSE(act, pred)
### 0.0007685658

plot(model, pages = 1)
summary(model)

act <- c()
pred <- c()
for(fold in next_speed_folds) {
  train <- possible_next_speeds[-fold, ]
  test <- possible_next_speeds[fold, ]
  model <- gam(lowest_next ~ te(basepath_velo, basepath_accel, k = 4), 
               data = train)
  act <- c(act, test$lowest_next) 
  pred <- c(pred, predict(model, newdata = test))
}
RMSE(act, pred)
### 0.0006629285



highest_next_accel_diff_model <- gam(highest_next ~ te(basepath_velo, basepath_accel, k = 3),  data = possible_next_speeds)
lowest_next_accel_diff_model <- gam(lowest_next ~ te(basepath_velo, basepath_accel, k = 4),  data = possible_next_speeds)

possible_next_speeds <- possible_next_speeds %>% ungroup() %>% mutate(pred_highest_next = predict(highest_next_accel_diff_model),
                                                                      pred_lowest_next = predict(lowest_next_accel_diff_model))


ggplot(possible_next_speeds, aes(x = basepath_velo, y = basepath_accel, color = pred_highest_next)) + 
       geom_point() + scale_color_gradient2(high = "green", low = "red", mid = "white", midpoint = mean(possible_next_speeds$pred_highest_next))

ggplot(possible_next_speeds, aes(x = basepath_velo, y = basepath_accel, color = pred_lowest_next)) + 
       geom_point() + scale_color_gradient2(high = "green", low = "red", mid = "white", midpoint = mean(possible_next_speeds$pred_lowest_next))

####################################################################################################################################################################################

basepath_last_velos <- basepath %>% filter(sum(basepath_velo >= 0.305) == 0) %>%
                                     mutate(basepath_velo_2 = lag(basepath_velo),
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

basepath_last_velos <- basepath_last_velos %>% filter(count > 10)

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

