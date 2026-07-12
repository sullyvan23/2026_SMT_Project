
batter_advance <- advance_data %>% ungroup() %>% select(game_string, play_per_game, ground_x, ground_y, time_to_ground, caught_prob) %>%
                                   group_by(game_string, play_per_game) %>% slice(1)
batter_advance <- batter_advance %>% left_join(player_positions[,1:6] %>% filter(player_id == 10), by = c("game_string", "play_per_game"))
batter_advance <- batter_advance %>% slice(n())

batter_advance <- batter_advance %>% mutate(dist_1st = sqrt((field_x - x_1b)^2 + (field_y - y_1b)^2),
                                            dist_2nd = sqrt((field_x - x_2b)^2 + (field_y - y_2b)^2),
                                            dist_3rd = sqrt((field_x - x_3b)^2 + (field_y - y_3b)^2),
                                            dist_home = sqrt((field_x - x_home)^2 + (field_y - y_home)^2))
batter_advance <- batter_advance %>% mutate(basepath = case_when(field_y < 0 | (field_y < 50 & field_x > 0)  ~  4, 
                                                                 field_y >= 50 & field_x > x_2b  ~  1 + (dist_1st / (dist_1st + dist_2nd)),
                                                                 field_y >= y_3b & field_x <= x_2b  ~  2 + (dist_2nd / (dist_2nd + dist_3rd)),
                                                                 field_y < y_3b & field_x <= x_home  ~  3 + (dist_3rd / (dist_3rd + dist_home)) ))
batter_advance <- batter_advance %>% mutate(final_base = round(basepath))
batter_advance <- batter_advance %>% filter(final_base < 4)



set.seed(829)
bat_folds <- createFolds(batter_advance$final_base, k = 10)

act <- c()
pred <- c()
for(fold in bat_folds) {
  print("-")
  train <- batter_advance[-fold, ]
  test <- batter_advance[fold, ]
  model <- gam(final_base ~ te(ground_x, ground_y, k = 6), 
               data = train)
  act <- c(act, test$final_base)
  pred <- c(pred, predict(model, newdata = test))
}
RMSE(act, pred)
### 0.4390282

plot(model, page = 1)
summary(model)

####################################################################################################################################################################

act <- c()
pred_hold <- matrix(ncol = 3)
pred <- c()
for(fold in bat_folds) {
  print("-")
  train <- batter_advance[-fold, ]
  test <- batter_advance[fold, ]
  model <- gam(final_base ~ te(ground_x, ground_y, k = 6) + caught_prob, 
               family = ocat(R = 3), data = train)
  act <- c(act, test$final_base)
  pred_hold <- rbind(pred_hold, as.matrix(predict(model, newdata = test, type = "response")))
}
pred_hold <- pred_hold[-1,]
for(i in 1:length(act)) {
    pred <- c(pred, pred_hold[i,act[i]])
}
logLoss(act, pred)
### 0.600435


batter_bases_model <- gam(final_base ~ te(ground_x, ground_y, k = 6) + time_to_ground, family = ocat(R = 3), data = batter_advance)
plot(batter_bases_model, pages = 1)

batter_advance <- cbind(batter_advance, predict(batter_bases_model, type = "response"))
batter_advance <- batter_advance %>% rename(advance_1 = "1",
                                            advance_2 = "2",
                                            advance_3 = "3")




