library(dplyr)
library(ggplot2)
library(Metrics)
library(kknn)
library(caret)

### plays where ball is caught in air
ball_caught <- ball_events %>% group_by(game_string, play_per_game) %>% slice(2:3) %>%
                               filter(first(ball_eventcode) == 4, last(ball_eventcode) %in% c(2,7))

### time in air and distance
ball_caught <- ball_caught %>% mutate(time_air = (last(timestamp) - first(timestamp))/1000) %>% slice(2)
ball_caught <- ball_caught %>% left_join(ball_positions[,1:6], by = c("game_string", "play_per_game", "timestamp"))
ball_caught <- ball_caught %>% filter(!is.na(ball_position_y)) %>%
                               mutate(ball_distance = sqrt(ball_position_x^2 + ball_position_y^2))
ball_caught <- ball_caught %>% filter(time_air >= 2)

### half inning
ball_caught <- ball_caught %>% left_join(lineups[,c(1,7,3)], by = c("game_string", "play_per_game")) %>% relocate(half_inning, .after = play_per_game)


##############################################################################################################################################################################################

### baserunners by position
baserunners <- player_positions %>% filter(player_id %in% c(11:13)) %>% 
                                    distinct(game_string, play_per_game, player_id) %>% group_by(game_string, play_per_game) %>%
                                    summarise(first = sum(player_id == 11),
                                              second = sum(player_id == 12), 
                                              third = sum(player_id == 13))

### baserunner positions
baserunners_caught <- ball_caught[,1:3] %>% left_join(player_positions[,1:6] %>% filter(player_id %in% c(11:13)),
                                                      by = c("game_string", "play_per_game"))
baserunners_caught <- baserunners_caught %>% filter(!is.na(timestamp))
baserunners_caught <- baserunners_caught %>% left_join(ball_events[,1:5], by = c("game_string", "play_per_game", "timestamp"),
                                                       suffix = c("_br", ""))

### assuming 18 inch (1.5 feet) bases, using middle of each bases for position
y_1b <- sqrt(90^2 / 2)
x_1b <- y_1b - (sqrt(2*(1.5^2))/2)
x_2b <- 0
y_2b <- (2*y_1b) - (sqrt(2*(1.5^2))/2)
x_3b <- -x_1b
y_3b <- y_1b
x_home <- 0
y_home <- (17/12)/2


### distances for seeing how far on basepath player is
baserunners_caught <- baserunners_caught %>% mutate(dist_1st = sqrt((field_x - x_1b)^2 + (field_y - y_1b)^2),
                                                    dist_2nd = sqrt((field_x - x_2b)^2 + (field_y - y_2b)^2),
                                                    dist_3rd = sqrt((field_x - x_3b)^2 + (field_y - y_3b)^2),
                                                    dist_home = sqrt((field_x - x_home)^2 + (field_y - y_home)^2))
baserunners_caught <- baserunners_caught %>% mutate(basepath = case_when(field_y < 0 | (field_y < 50 & field_x > 0)  ~  4, 
                                                                         field_y >= 50 & field_x > x_2b  ~  1 + (dist_1st / (dist_1st + dist_2nd)),
                                                                         field_y >= y_3b & field_x <= x_2b  ~  2 + (dist_2nd / (dist_2nd + dist_3rd)),
                                                                         field_y < y_3b & field_x <= x_home  ~  3 + (dist_3rd / (dist_3rd + dist_home)) ))

baserunners_caught <- baserunners_caught %>% mutate(next_play = play_per_game + 1)
baserunners_caught <- baserunners_caught %>% left_join(lineups[,c(1,7,3)], by = c("game_string", "next_play" = "play_per_game"),
                                                       suffix = c("", "_next"))



### attempting to filter out fly balls with 2 outs
caught_end_positions <- ball_caught[,1:4] %>% left_join(baserunners_caught[,c(1:2,4:5,14)], by = c("game_string", "play_per_game", "timestamp"))
caught_end_positions <- caught_end_positions %>% filter(!is.na(player_id_br))

final_br_positions <- baserunners_caught %>% group_by(game_string, play_per_game, player_id_br) %>% slice(n())
caught_end_positions <- caught_end_positions %>% left_join(final_br_positions[,c(1:2,5,14)], by = c("game_string", "play_per_game", "player_id_br"),
                                                           suffix = c("_caught", "_end"))

caught_end_positions <- caught_end_positions %>% mutate(og_base_dist = case_when(player_id_br == 11  ~  basepath_caught - 1,
                                                                                 player_id_br == 12  ~  basepath_caught - 2,
                                                                                 player_id_br == 13  ~  basepath_caught - 3),
                                                        after_catch_dist = basepath_end - basepath_caught)

caught_end_positions <- caught_end_positions %>% mutate(next_play = play_per_game + 1)
caught_end_positions <- caught_end_positions %>% left_join(lineups[,c(1,7,3)], by = c("game_string", "next_play" = "play_per_game"),
                                                           suffix = c("", "_next"))

caught_end_positions <- caught_end_positions %>% mutate(not_end = ifelse(half_inning == half_inning_next, 1, 0),
                                                        not_end = ifelse(is.na(not_end), 0, not_end))

ggplot(caught_end_positions, aes(x = og_base_dist, y = after_catch_dist, color = not_end)) + 
       geom_point() + scale_color_gradient2(high = "green", low = "red", mid = "white", midpoint = 0.5)



less_2_outs_caught <- caught_end_positions %>% filter(!(og_base_dist > 0.5  &  after_catch_dist > -0.01))

ggplot(less_2_outs_caught, aes(x = og_base_dist, y = after_catch_dist, color = not_end)) + 
       geom_point() + scale_color_gradient2(high = "green", low = "red", mid = "white", midpoint = 0.5)


##############################################################################################################################################################################################

baserunners_less2 <- less_2_outs_caught[,-4] %>% left_join(player_positions[,1:6] %>% filter(player_id %in% c(11:13)),
                                                           by = c("game_string", "play_per_game", "player_id_br" = "player_id"))
baserunners_less2 <- baserunners_less2 %>% left_join(ball_events[,1:5], by = c("game_string", "play_per_game", "timestamp"),
                                                     suffix = c("_br", ""))

baserunners_less2 <- baserunners_less2 %>% group_by(game_string, play_per_game, player_id_br) %>% 
                                           mutate(ball_possessed = 0, 
                                                  ball_eventcode = ifelse(is.na(ball_eventcode), " ", ball_eventcode)) %>%
                                           group_modify(~{
                                             for(i in 2:nrow(.x)) {
                                                
                                                if(.x$ball_eventcode[i] %in% c(2,7)) {
                                                  .x$ball_possessed[i] = 1
                                                }
  
                                                if(.x$ball_possessed[i-1] == 1) {
                                                  .x$ball_possessed[i] = 1
                                                  .x$player_id[i] = .x$player_id[i-1]
                                                }
  
                                                if(.x$ball_eventcode[i] %in% c(3,8,9,10,16)) {
                                                  .x$ball_possessed[i] = 0
                                                  .x$player_id[i] = NA
                                                }
                                               
                                             }
                                             
                                             .x
                                           })
baserunners_less2 <- baserunners_less2 %>% left_join(player_positions[,1:6], by = c("game_string", "play_per_game", "player_id", "timestamp"), 
                                                     suffix = c("_runner", "_fielder"))


baserunners_less2 <- baserunners_less2 %>% mutate(final_dist = case_when(player_id_br == 11  ~  basepath_end - 1,
                                                                         player_id_br == 12  ~  basepath_end - 2,
                                                                         player_id_br == 13  ~  basepath_end - 3))


pot_tag_up <- baserunners_less2 %>% filter(after_catch_dist >= 0.3)
going_back <- baserunners_less2 %>% filter(after_catch_dist < 0.3)


##############################################################################################################################################################################################


















    
knn_grid <- expand.grid(k = seq(20, 50, by = 5),
                        logloss = NA)

set.seed(199)
less_2_outs_folds <- createFolds(caught_end_positions$not_end, k = 5)

for(i in 1:nrow(knn_grid)) {
  act <- c()
  pred <- c()
  for(fold in less_2_outs_folds) {
    train <- caught_end_positions[-fold,] %>% mutate(not_end = as.factor(not_end))
    test <- caught_end_positions[fold,]
    
    knn <- train.kknn(
      not_end ~ og_base_dist + after_catch_dist,
      data = train,
      kmax = knn_grid$k[i],
      kernel = "optimal"
    )
    act <- c(act, test$not_end)
    pred <- c(pred, predict(knn, newdata = test, type = "prob")[,2])
  }

  pred <- pmin(pmax(0.001, pred), 0.999)
  knn_grid$logloss[i] <- logLoss(act, pred)
}


baserunners_caught <- baserunners_caught %>% group_by(game_string, play_per_game, player_id_br) %>% 
                                             mutate(ball_possessed = 0, 
                                                    ball_eventcode = ifelse(is.na(ball_eventcode), " ", ball_eventcode)) %>%
                                             group_modify(~{
                                               for(i in 2:nrow(.x)) {
                                                  
                                                  if(.x$ball_eventcode[i] %in% c(2,7)) {
                                                    .x$ball_possessed[i] = 1
                                                  }
    
                                                  if(.x$ball_possessed[i-1] == 1) {
                                                    .x$ball_possessed[i] = 1
                                                    .x$player_id[i] = .x$player_id[i-1]
                                                  }
    
                                                  if(.x$ball_eventcode[i] %in% c(3,8,9,10,16)) {
                                                    .x$ball_possessed[i] = 0
                                                    .x$player_id[i] = NA
                                                  }
                                                 
                                               }
                                               
                                               .x
                                             })
baserunners_caught <- baserunners_caught %>% ungroup() %>% filter(ball_possessed == 1)

baserunners_caught <- baserunners_caught %>% left_join(player_positions[,1:6], by = c("game_string", "play_per_game", "player_id", "timestamp"), 
                                                       suffix = c("_runner", "_fielder"))

baserunners_caught <- baserunners_caught %>% mutate(base_run_dist = case_when(player_id_br == 11  ~  sqrt((field_x_runner - 63.11)^2 + (field_y_runner - 63.11)^2),
                                                                              player_id_br == 12  ~  sqrt(field_x_runner^2 + (field_y_runner - 126.22)^2),
                                                                              player_id_br == 13  ~  sqrt((field_x_runner + 63.11)^2 + (field_y_runner - 63.11)^2)),
                                                    base_field_dist = case_when(player_id_br == 11  ~  sqrt((field_x_fielder - 63.11)^2 + (field_y_fielder - 63.11)^2),
                                                                                player_id_br == 12  ~  sqrt(field_x_fielder^2 + (field_y_fielder - 126.22)^2),
                                                                                player_id_br == 13  ~  sqrt((field_x_fielder + 63.11)^2 + (field_y_fielder - 63.11)^2)))

baserunners_caught <- baserunners_caught %>% group_by(game_string, play_per_game, player_id_br) %>% 
                                             mutate(base_run_dist_diff = base_run_dist - lag(base_run_dist))


potential_dp <- baserunners_caught %>% filter(base_run_dist_diff < -0.1, base_field_dist < 3)












