
### finding positions of all players who could potentially catch the ball during time the ball is in the air
catch_prob_data <- could_catch_players %>% select(-c(field_x:eight_ft_dist, player_catch_prob)) %>% 
                                           left_join(player_positions[,1:6], by = c("game_string", "play_per_game", "player_id"))
catch_prob_data <- catch_prob_data %>% filter(timestamp >= timestamp_hit, timestamp <= timestamp_done)

### using GAM to smooth out positions, goves more smooth speeds and such without any weird jumps that would result in abormal movement data
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

# hist(catch_prob_data$rmse_x, breaks = 100)
# hist(catch_prob_data$rmse_y, breaks = 100)

### filtering for only plays where modeled positions don't differ greatly from the given data
catch_prob_data <- catch_prob_data %>% filter(rmse_x <= 0.2, rmse_y <= 0.3)

### time left to get to positions
catch_prob_data <- catch_prob_data %>% mutate(time_left_ground = ((timestamp_hit + (time_to_ground*1000)) - timestamp)/1000,
                                              time_left_8ft = ((timestamp_hit + (time_eight_ft*1000)) - timestamp)/1000,
                                              time_since_hit = time_to_ground - time_left_ground) %>%
                                       filter(time_left_ground >= 0)

### recaltulating 8ft dist to be estimate where the ball actually is if its lower than 8ft now on its descent
catch_prob_data <- catch_prob_data %>% mutate(eight_ft_x = ifelse(time_left_8ft <= 0,
                                                                  ((eight_ft_x * time_left_ground) + (ground_x * -time_left_8ft)) / (time_left_ground - time_left_8ft),
                                                                  eight_ft_x),
                                              eight_ft_y = ifelse(time_left_8ft <= 0,
                                                                  ((eight_ft_y * time_left_ground) + (ground_y * -time_left_8ft)) / (time_left_ground - time_left_8ft),
                                                                  eight_ft_y))

### calculating distances and velocities
catch_prob_data <- catch_prob_data %>% mutate(OF_ground_x_dist = pred_x - ground_x,
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

### calculating distances and angles to show direction of needed movement
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

### finding distance between projected ground 8ft positions and the wall
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

### giving player speeds, player not on main 4 teams or not enough data, mean speed is used
catch_prob_data <- catch_prob_data %>% left_join(lineups_players[,c(1,7,9:10)], by = c("game_string", "play_per_game", "player_id"))
catch_prob_data <- catch_prob_data %>% group_by(game_string, play_per_game, player_id, timestamp) %>%
                                       mutate(player_code = ifelse(first(player_code) != last(player_code), NA, player_code)) %>%
                                       slice(1)
catch_prob_data <- catch_prob_data %>% left_join(player_speed[,1:2], by = "player_code")
catch_prob_data <- catch_prob_data %>% mutate(speed_95 = ifelse(is.na(speed_95), mean(player_speed$speed_95), speed_95)) %>% 
                                       rename(player_speed = speed_95)

### filtering out plays where the ball is caught but player is not near the ball (likely because ball and player times aren't matching up correctly)
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

### two fold because such a large amount of data, all timestamps from a play are in one fold
catch_prob_data <- catch_prob_data %>% mutate(key = paste0(game_string, play_per_game, "_", player_id)) %>%
                                       relocate(key, .after = player_id)

set.seed(148)
catch_prob_folds <- groupKFold(catch_prob_data$key, k = 2)

####################################################################################################################################################################

### tried random forest, didn't do great
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

### GAM model only based on ground variables, 2-fold cross validation
act <- c()
pred <- c()
for(fold in catch_prob_folds) {
  print("-")
  train <- catch_prob_data[-fold, ]
  test <- catch_prob_data[fold, ]
  model <- bam(player_caught ~ te(OF_ground_dist, time_left_ground) + s(OF_ground_angle, k = 3) + OF_ground_velo + s(OF_ground_velo_angle, k = 3) + 
                               te(wall_ground_dist, wall_8ft_dist) + ti(time_left_ground, OF_ground_angle) + s(time_since_hit) + player_speed, 
               family = binomial, data = train, discrete = TRUE)
  act <- c(act, test$player_caught)
  pred <- c(pred, predict(model, newdata = test, type = "response"))
}
logLoss(act, pred)
### 0.199862


### GAM model only based on 8ft variables, 2-fold cross validation
act <- c()
pred <- c()
for(fold in catch_prob_folds) {
  print("-")
  train <- catch_prob_data[-fold, ]
  test <- catch_prob_data[fold, ]
  model <- bam(player_caught ~ te(OF_8ft_dist, time_left_8ft) + s(OF_8ft_angle, k = 3) + OF_8ft_velo + s(OF_8ft_velo_angle, k = 3) + 
                               te(wall_ground_dist, wall_8ft_dist) + ti(time_left_8ft, OF_8ft_angle) + s(time_since_hit) + player_speed, 
               family = binomial, data = train, discrete = TRUE)
  act <- c(act, test$player_caught)
  pred <- c(pred, predict(model, newdata = test, type = "response"))
}
logLoss(act, pred)
### 0.1880228

plot(model, page = 1)
summary(model)

####################################################################################################################################################################

### final ground and 8ft models
### anytime I tried using both in one model I would find a weird play or two where the catch probabilities didn't seem right
catch_ground_model <- bam(player_caught ~ te(OF_ground_dist, time_left_ground) + s(OF_ground_angle, k = 3) + OF_ground_velo + s(OF_ground_velo_angle, k = 3) + 
                                           te(wall_ground_dist, wall_8ft_dist) + ti(time_left_ground, OF_ground_angle) + s(time_since_hit) + player_speed,
                                            family = binomial, data = catch_prob_data,
                                            discrete = TRUE)
catch_8ft_model <- bam(player_caught ~ te(OF_8ft_dist, time_left_8ft) + s(OF_8ft_angle, k = 3) + OF_8ft_velo + s(OF_8ft_velo_angle, k = 3) + 
                                       te(wall_ground_dist, wall_8ft_dist) + ti(time_left_8ft, OF_8ft_angle) + s(time_since_hit) + player_speed, 
                                          family = binomial, data = catch_prob_data,
                                          discrete = TRUE)

### getting odds of both ground and 8ft models, finding max and min
catch_prob_data <- catch_prob_data %>% ungroup() %>% mutate(catch_ground_odds = predict(catch_ground_model),
                                                            catch_8ft_odds = predict(catch_8ft_model),
                                                            catch_max_odds = pmax(catch_ground_odds, catch_8ft_odds),
                                                            catch_min_odds = pmin(catch_ground_odds, catch_8ft_odds))

####################################################################################################################################################################

### plots to check what max and min look like and how much of an effect
check <- catch_prob_data %>% mutate(catch_max_odds = round(catch_max_odds), catch_min_odds = round(catch_min_odds)) %>% 
                             group_by(catch_max_odds, catch_min_odds) %>% summarise(catch_prob = mean(player_caught))

ggplot(check %>% filter(catch_max_odds >= -5), aes(x = catch_max_odds, y = catch_min_odds, color = catch_prob)) + 
       geom_point() + scale_color_gradient2(high = "green", low = "red", mid = "white", midpoint = 0.5)


### 2-fold cross validation checking
act <- c()
pred <- c()
for(fold in catch_prob_folds) {
  print("-")
  train <- catch_prob_data[-fold, ]
  test <- catch_prob_data[fold, ]
  model <- bam(player_caught ~ catch_max_odds + s(catch_min_odds, by = time_left_ground), 
               family = binomial, data = train, discrete = TRUE)
  act <- c(act, test$player_caught)
  pred <- c(pred, predict(model, newdata = test, type = "response"))
}
logLoss(act, pred)
### 0.1746564

plot(model, page = 1)
summary(model)

####################################################################################################################################################################

### final individual catch probability model, only using max odds
catch_prob_model <- gam(player_caught ~ catch_max_odds, family = binomial, data = catch_prob_data)

catch_prob_data <- catch_prob_data %>% ungroup() %>% mutate(catch_odds = predict(catch_prob_model),
                                                            catch_prob = predict(catch_prob_model, type = "response"))


check <- catch_prob_data %>% filter(game_string == "y1_d186_RRM_VAS", play_per_game == 53)

check <- catch_prob_data %>% group_by(game_string, play_per_game, player_id) %>% slice(n())

write.csv(catch_prob_data, "catch_prob_data.csv", row.names = FALSE)

####################################################################################################################################################################

### pivoting catch probabilities together so all players catch odds for a time are in one row 
caught_by_prob_data <- catch_prob_data %>% select(game_string, play_per_game, timestamp, player_id_event, caught, player_id, catch_odds)
caught_by_prob_data <- caught_by_prob_data %>% pivot_wider(names_from = player_id, values_from = catch_odds)

caught_by_prob_data <- caught_by_prob_data %>% rename(b1 = "3", b2 = "4", b3 = "5", ss = "6",
                                                      lf = "7", cf = "8", rf = "9")
caught_by_prob_data <- caught_by_prob_data %>% relocate(b1, b2, b3, ss, lf, cf, rf, .after = caught)
caught_by_prob_data <- caught_by_prob_data %>% mutate(across(c(b1:rf), ~ ifelse(is.na(.)  |  . < -35, -35, .)))

caught_by_prob_data <- caught_by_prob_data %>% mutate(caught_by = ifelse(caught == 1, player_id_event-2, 0))

### final model for catch probabilities, only used relevent player numbers for each
### example " ~ b1 + b2 + rf" is for catch probability of first basemen or not, all other player catch odds did not show significant results in summary()
caught_by_model <- gam(list(caught_by ~ b1 + b2 + rf,
                                      ~ b1 + b2 + ss + cf + rf,
                                      ~ b2 + b3 + ss + lf,
                                      ~ b1 + b2 + b3 + ss + lf + cf + rf,
                                      ~ b3 + ss + lf + cf,
                                      ~ b1 + b2 + b3 + ss + lf + cf + rf,
                                      ~ b1 + b2 + ss + cf + rf),
                      family = multinom(K = 7), data = caught_by_prob_data)

summary(caught_by_model)

### final data and catch probabilties
caught_by_prob_results <- cbind(caught_by_prob_data, predict(caught_by_model, type = "response"))
write.csv(caught_by_prob_results, "caught_by_prob_results.csv", row.names = FALSE)

check <- caught_by_prob_results %>% filter(game_string == "y1_d199_TES_ARN", play_per_game == 197)

####################################################################################################################################################################

### making dataset for just catch probabilities, no other thinsg like original odds and such
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


### also tested with xgboost later on, wasn't better than GAM
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

