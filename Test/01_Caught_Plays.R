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
ball_caught <- ball_caught %>% filter(time_air >= 2, abs(ball_position_x) <= ball_position_y, ball_distance >= 155)

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

baserunners_less2 <- less_2_outs_caught[,-4] %>% distinct() %>%
                                                 left_join(player_positions[,1:6] %>% filter(player_id %in% c(11:13)),
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
  
                                                if(.x$ball_eventcode[i] %in% c(3,5,8,9,10,16)) {
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

baserunners_less2 <- baserunners_less2 %>% left_join(baserunners, by = c("game_string", "next_play" = "play_per_game"))

pot_tag_up <- baserunners_less2 %>% filter(after_catch_dist >= 0.3)
going_back <- baserunners_less2 %>% filter(after_catch_dist < 0.3)

##############################################################################################################################################################################################

going_back <- going_back %>% ungroup() %>%
                             mutate(og_run_dist = case_when(player_id_br == 11  ~  sqrt((field_x_runner - x_1b)^2 + (field_y_runner - y_1b)^2),
                                                            player_id_br == 12  ~  sqrt((field_x_runner - x_2b)^2 + (field_y_runner - y_2b)^2),
                                                            player_id_br == 13  ~  sqrt((field_x_runner - x_3b)^2 + (field_y_runner - y_3b)^2)),
                                    og_field_dist = case_when(player_id_br == 11  ~  sqrt((field_x_fielder - x_1b)^2 + (field_y_fielder - y_1b)^2),
                                                              player_id_br == 12  ~  sqrt((field_x_fielder - x_2b)^2 + (field_y_fielder - y_2b)^2),
                                                              player_id_br == 13  ~  sqrt((field_x_fielder - x_3b)^2 + (field_y_fielder - y_3b)^2)),
                                    safe_est = case_when(player_id_br == 11  ~ ifelse(is.na(first), 0.5, first),
                                                         player_id_br == 12  ~  ifelse(is.na(second), 0.5, second),
                                                         player_id_br == 13  ~  ifelse(is.na(third), 0.5, third)))

going_back <- going_back %>% group_by(game_string, play_per_game, player_id_br) %>% mutate(runner_within_4 = 0) %>%
                             group_modify(~{
                               for(i in 2:nrow(.x)) {
                                  
                                  if(.x$og_run_dist[i] <= 4  |  .x$runner_within_4[i-1] == 1) {
                                    .x$runner_within_4[i] = 1
                                  }
                               }
                               
                               .x
                             })

going_back <- going_back %>% mutate(pos_safe_est = ifelse(sum(runner_within_4 == 0  &  og_field_dist <= 5, na.rm = TRUE) > 0, 0, 1))

ggplot(going_back %>% filter(og_field_dist < 10), aes(x = og_run_dist, y = og_field_dist, color = pos_safe_est)) + 
       geom_point() + scale_color_gradient2(high = "green", low = "red", mid = "white", midpoint = 0.5)


going_back_results <- going_back %>% summarise(og_base_dist = first(og_base_dist),
                                               after_catch_dist = first(after_catch_dist),
                                               safe_back = first(pos_safe_est))

##############################################################################################################################################################################################

pot_tag_up <- pot_tag_up %>% ungroup() %>%
                             mutate(next_run_dist = case_when(player_id_br == 11  ~  sqrt((field_x_runner - x_2b)^2 + (field_y_runner - y_2b)^2),
                                                              player_id_br == 12  ~  sqrt((field_x_runner - x_3b)^2 + (field_y_runner - y_3b)^2),
                                                              player_id_br == 13  ~  sqrt((field_x_runner - x_home)^2 + (field_y_runner - y_home)^2)),
                                    run_field_dist = sqrt((field_x_fielder - field_x_runner)^2 + (field_y_fielder - field_y_runner)^2),
                                    safe_est = case_when(player_id_br == 11  ~ ifelse(is.na(second), 0.5, second),
                                                         player_id_br == 12  ~  ifelse(is.na(third), 0.5, third),
                                                         player_id_br == 13  ~  0.5))

pot_tag_up <- pot_tag_up %>% group_by(game_string, play_per_game, player_id_br) %>% mutate(runner_within_4 = 0) %>%
                             group_modify(~{
                               for(i in 2:nrow(.x)) {
                                  
                                  if(.x$next_run_dist[i] <= 4  |  .x$runner_within_4[i-1] == 1) {
                                    .x$runner_within_4[i] = 1
                                  }
                               }
                               
                               .x
                             })

pot_tag_up <- pot_tag_up %>% mutate(pos_safe_est = ifelse(sum(runner_within_4 == 0  &  run_field_dist <= 5, na.rm = TRUE) > 0, 0, 1))

ggplot(pot_tag_up %>% filter(run_field_dist < 10), aes(x = next_run_dist, y = run_field_dist, color = safe_est)) + 
       geom_point() + scale_color_gradient2(high = "green", low = "red", mid = "white", midpoint = 0.5)


pot_tag_up_results <- pot_tag_up %>% summarise(og_base_dist = first(og_base_dist),
                                               after_catch_dist = first(after_catch_dist),
                                               final_dist = first(final_dist),
                                               safe_tag = first(pos_safe_est))



tag_check <- pot_tag_up %>% left_join(ball_caught[,c(1:2,4)], by = c("game_string", "play_per_game"), suffix = c("", "_caught"))
tag_check <- tag_check %>% filter(timestamp >= (timestamp_caught - 500))
tag_check <- tag_check %>% group_by(game_string, play_per_game, player_id_br) %>% 
                           summarise(max_next_run_dist = max(next_run_dist))


pot_tag_up_results <- pot_tag_up_results %>% left_join(tag_check, by = c("game_string", "play_per_game", "player_id_br"))
pot_tag_up_results <- pot_tag_up_results %>% filter(max_next_run_dist > 82) %>% select(-max_next_run_dist)

##############################################################################################################################################################################################

doubled_up_results <- going_back_results[,c(1:3,6)]

tag_results <- bind_rows(pot_tag_up_results[,c(1:3,7)], going_back_results[,1:3])
tag_results <- tag_results %>% mutate(att_tag = ifelse(is.na(safe_tag), 0, 1),
                                      succ_tag = ifelse(is.na(safe_tag), 0, safe_tag)) %>%
                               relocate(att_tag, .before = safe_tag)

write.csv(doubled_up_results, "doubled_up_results.csv", row.names = FALSE)
write.csv(tag_results, "tag_results.csv", row.names = FALSE)

##############################################################################################################################################################################################
