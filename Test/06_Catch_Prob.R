
catch_prob_data <- could_catch_players %>% select(-c(field_x:eight_ft_dist, player_catch_prob)) %>% 
                                           left_join(player_positions[,1:6], by = c("game_string", "play_per_game", "player_id"))
catch_prob_data <- catch_prob_data %>% filter(timestamp >= timestamp_hit, timestamp <= timestamp_done)


group <- 0
catch_prob_data <- catch_prob_data %>% group_by(game_string, play_per_game, player_id) %>%
                 group_modify(~{group <<- group + 1
                              message(group/nrow(could_catch_players))
                                  
                              x_model <- gam(field_x ~ s(timestamp, k = 10), data = .x )
                              y_model <- gam(field_y ~ s(timestamp, k = 10), data = .x )

                              .x$pred_x <- predict(x_model) 
                              .x$pred_y <- predict(y_model)

                              .x$rmse_x <- RMSE(.x$pred_x, .x$field_x)
                              .x$rmse_y <- RMSE(.x$pred_y, .x$field_y)

                              .x})

hist(catch_prob_data$rmse_x, breaks = 100)
hist(catch_prob_data$rmse_y, breaks = 100)


catch_prob_data <- catch_prob_data %>% mutate(time_left_ground = ((timestamp_hit + (time_to_ground*1000)) - timestamp)/1000,
                                              time_left_8ft = ((timestamp_hit + (time_eight_ft*1000)) - timestamp)/1000,
                                              OF_ground_x_dist = pred_x - ground_x,
                                              OF_ground_y_dist = pred_y - ground_y,
                                              OF_8ft_x_dist = pred_x - eight_ft_x,
                                              OF_8ft_y_dist = pred_y - eight_ft_y,
                                              OF_x_velo = 0.681818 * (pred_x - lag(pred_x)) / ((timestamp - lag(timestamp))/1000),
                                              OF_x_velo = ifelse(is.na(OF_x_velo), lead(OF_x_velo), OF_x_velo),
                                              OF_y_velo = 0.681818 * (pred_y - lag(pred_y)) / ((timestamp - lag(timestamp))/1000),
                                              OF_y_velo = ifelse(is.na(OF_y_velo), lead(OF_y_velo), OF_y_velo),
                                              OF_ground_dist = sqrt(OF_ground_x_dist^2 + OF_ground_y_dist^2), 
                                              OF_8ft_dist = sqrt(OF_8ft_x_dist^2 + OF_8ft_y_dist^2),
                                              OF_velo = sqrt(OF_x_velo^2 + OF_y_velo^2))

catch_prob_data <- catch_prob_data %>% mutate(OF_ground_front_dist = ((OF_ground_x_dist * pred_x) + (OF_ground_y_dist * pred_y)) / 
                                                                      sqrt(pred_x^2 + pred_y^2),
                                              OF_ground_angle = acos(OF_ground_front_dist / OF_ground_dist),
                                              OF_8ft_front_dist = ((OF_8ft_x_dist * pred_x) + (OF_8ft_y_dist * pred_y)) / 
                                                                      sqrt(pred_x^2 + pred_y^2),
                                              OF_8ft_angle = acos(OF_8ft_front_dist / OF_8ft_dist))

catch_prob_data <- catch_prob_data %>% mutate(OF_ground_velo = -((OF_ground_x_dist * OF_x_velo) + (OF_ground_y_dist * OF_y_velo)) / 
                                                               OF_ground_dist,
                                              OF_ground_velo_angle = acos(OF_ground_velo / OF_velo),
                                              OF_8ft_velo = -((OF_8ft_x_dist * OF_x_velo) + (OF_8ft_y_dist * OF_y_velo)) / 
                                                            OF_8ft_dist,
                                              OF_8ft_velo_angle = acos(OF_8ft_velo / OF_velo))


catch_prob_data <- catch_prob_data %>% mutate(spray_angle = atan(ground_x / ground_y))
catch_prob_data <- catch_prob_data %>% group_by(home_team) %>%
                                       mutate(ground_wall = case_when(home_team == "ANI"  ~  predict(wall_ANI_model, newdata = pick(everything())),
                                                                      home_team == "ARN"  ~  predict(wall_ARN_model, newdata = pick(everything())),
                                                                      home_team == "PHD"  ~  predict(wall_PHD_model, newdata = pick(everything())),
                                                                      home_team == "VAS"  ~  predict(wall_VAS_model, newdata = pick(everything())))) %>% ungroup()
catch_prob_data <- catch_prob_data %>% mutate(spray_angle = atan(eight_ft_x / eight_ft_y))
catch_prob_data <- catch_prob_data %>% group_by(home_team) %>%
                                       mutate(eight_ft_wall = case_when(home_team == "ANI"  ~  predict(wall_ANI_model, newdata = pick(everything())),
                                                                        home_team == "ARN"  ~  predict(wall_ARN_model, newdata = pick(everything())),
                                                                        home_team == "PHD"  ~  predict(wall_PHD_model, newdata = pick(everything())),
                                                                        home_team == "VAS"  ~  predict(wall_VAS_model, newdata = pick(everything())))) %>% ungroup() %>%
                                       select(-spray_angle)

catch_prob_data <- catch_prob_data %>% mutate(wall_ground_dist = ground_wall - sqrt(ground_x^2 + ground_y^2),
                                              wall_8ft_dist = eight_ft_wall - sqrt(eight_ft_x^2 + eight_ft_y^2))

catch_prob_data <- catch_prob_data %>% left_join(lineups_players[,c(1,7,9:10)], by = c("game_string", "play_per_game", "player_id"))
catch_prob_data <- catch_prob_data %>% group_by(game_string, play_per_game, player_id, timestamp) %>%
                                       mutate(player_code = ifelse(first(player_code) != last(player_code), NA, player_code)) %>%
                                       slice(1)
catch_prob_data <- catch_prob_data %>% left_join(player_speed[,1:2], by = "player_code")
catch_prob_data <- catch_prob_data %>% mutate(speed_95 = ifelse(is.na(speed_95), mean(player_speed$speed_95), speed_95)) %>% 
                                       rename(player_speed = speed_95)

lag_check <- catch_prob_data %>% group_by(game_string, play_per_game, player_id) %>% slice(n())
lag_check <- lag_check %>% filter(caught == 1) %>% group_by(game_string, play_per_game) %>%
                           summarise(dist = min(OF_ground_dist, OF_8ft_dist))
lag_check <- lag_check %>% filter(dist > 10) %>% mutate(play_key = paste0(game_string, play_per_game))

catch_prob_data <- catch_prob_data %>% mutate(play_key = paste0(game_string, play_per_game)) %>%
                                       filter(!play_key %in% lag_check$play_key) %>% select(-play_key)

catch_prob_data <- catch_prob_data %>% relocate(player_caught, .after = last_col())

write.csv(catch_prob_data, "catch_prob_data.csv", row.names = FALSE)

####################################################################################################################################################################
library(mgcv)
library(randomForest)
library(xgboost)
library(caret)
library(Metrics)

catch_prob_data <- catch_prob_data %>% mutate(key = paste0(game_string, play_per_game, "_", player_id)) %>%
                                       relocate(key, .after = player_id)

set.seed(148)
catch_prob_folds <- groupKFold(catch_prob_data$key, k = 2)

####################################################################################################################################################################

act <- c()
pred <- c()
for(fold in catch_prob_folds) {
  print("-")
  train <- catch_prob_data[-fold, ]
  test <- catch_prob_data[fold, ]
  model <- randomForest(as.factor(player_caught) ~ OF_ground_dist + time_left_ground + OF_ground_angle + OF_ground_velo + OF_ground_velo_angle + wall_ground_dist +
                                                   player_speed + OF, 
                        data = train, ntree = 300)
  act <- c(act, test$player_caught)
  pred <- c(pred,  pmin( pmax( predict(model, newdata = test, type = "prob")[, 2], 0.001 ), 0.999 )  )
}
logLoss(act, pred)
### 1.623544

####################################################################################################################################################################

act <- c()
pred <- c()
for(fold in catch_prob_folds) {
  print("-")
  train <- catch_prob_data[-fold, ]
  test <- catch_prob_data[fold, ]
  model <- bam(player_caught ~ te(OF_ground_dist, time_left_ground, k = 5) + te(OF_8ft_dist, time_left_8ft, k = 4) + te(OF_ground_angle, OF_8ft_angle, k = 3) +
                               te(OF_ground_velo, OF_8ft_velo, k = 3) + s(wall_8ft_dist, k = 5) + time_since_hit + player_speed, 
               family = binomial, data = train)
  act <- c(act, test$player_caught)
  pred <- c(pred, predict(model, newdata = test, type = "response"))
}
logLoss(act, pred)
### 0.1849643

plot(model, page = 1)
summary(model)

####################################################################################################################################################################

catch_prob_model <- gam(player_caught ~ te(OF_ground_dist, time_left_ground, k = 5) + te(OF_8ft_dist, time_left_8ft, k = 4) + te(OF_ground_angle, OF_8ft_angle, k = 3) +
                                        te(OF_ground_velo, OF_8ft_velo, k = 3) + s(wall_8ft_dist, k = 5) + time_since_hit + player_speed, 
                                        family = binomial, data = catch_prob_data)

catch_prob_data <- catch_prob_data %>% ungroup() %>% mutate(catch_prob = predict(catch_prob_model, type = "response"),
                                                            catch_odds = predict(catch_prob_model))

write.csv(catch_prob_data, "catch_prob_data.csv", row.names = FALSE)

####################################################################################################################################################################

caught_by_prob_data <- catch_prob_data %>% select(game_string, play_per_game, timestamp, player_id_event, caught, player_id, catch_odds)
caught_by_prob_data <- caught_by_prob_data %>% pivot_wider(names_from = player_id, values_from = catch_odds)

caught_by_prob_data <- caught_by_prob_data %>% rename(b1 = "3", b2 = "4", b3 = "5", ss = "6",
                                                      lf = "7", cf = "8", rf = "9")
caught_by_prob_data <- caught_by_prob_data %>% relocate(b1, b2, b3, ss, lf, cf, rf, .after = caught)
caught_by_prob_data <- caught_by_prob_data %>% mutate(across(c(b1:rf), ~ ifelse(is.na(.)  |  . < -25, -25, .)))

caught_by_prob_data <- caught_by_prob_data %>% mutate(caught_by = ifelse(caught == 1, player_id_event-2, 0))


caught_by_model <- gam(list(caught_by ~ b1 + b2 + rf,
                                      ~ b1 + b2 + ss + cf + rf,
                                      ~ b2 + b3 + ss + lf,
                                      ~ b1 + b2 + b3 + ss + lf + cf + rf,
                                      ~ b3 + ss + lf + cf,
                                      ~ b1 + b2 + b3 + ss + lf + cf + rf,
                                      ~ b1 + b2 + ss + cf + rf),
                      family = multinom(K = 7), data = caught_by_prob_data)

summary(caught_by_model)

caught_by_prob_results <- cbind(caught_by_prob_data, predict(caught_by_model, type = "response"))

check <- caught_by_prob_results %>% filter(game_string == "y1_d199_TES_ARN", play_per_game == 197)

write.csv(caught_by_prob_results, "caught_by_prob_results.csv", row.names = FALSE)

####################################################################################################################################################################

temp <- caught_by_prob_results %>% pivot_longer(cols = "1":"8",
                                                names_to = "player_id",
                                                values_to = "catch_prob")
temp <- temp %>% select(-c(b1:caught_by)) %>%
                 mutate(player_id = as.numeric(player_id) + 1)

final_catch_prob_results <- catch_prob_data %>% select(game_string, play_per_game, timestamp, player_id) %>%
                                                left_join(temp %>% select(-c(player_id_event:caught)), 
                                                          by = c("game_string", "play_per_game", "timestamp", "player_id"))

write.csv(final_catch_prob_results, "final_catch_prob_results.csv", row.names = FALSE)

####################################################################################################################################################################






variables <- c("")
act <- c()
pred <- c()
for(fold in catch_prob_folds) {
  print("-")
  train <- as.matrix(catch_prob_data[-fold, variables])
  test <- as.matrix(catch_prob_data[fold, variables])
  xgb <- xgboost(
    booster = "gbtree",
    objective = "binary:logistic",
    eval_metric = "logloss",
    data = as.matrix(all_went_data_2[-fold, 2:7]),
    label = catch_prob_data[-fold, "player_caught"],
    nrounds = 100,
    eta = 0.1,
    verbose = 0
  )
  act <- c(act, test$player_caught)
  pred <- c(pred, predict(model, newdata = test, type = "response"))
}
logLoss(act, pred)


####################################################################################################################################################################








