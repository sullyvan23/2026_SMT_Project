library(dplyr)

### plays where ball is caught in air
ball_caught <- ball_events %>% group_by(game_string, play_per_game) %>%
                               filter(sum(ball_eventcode == 4) > 0) %>% slice(2:3) %>%
                               filter(last(ball_eventcode) %in% c(2,7))
ball_caught <- ball_caught %>% mutate(time_air = (last(timestamp) - first(timestamp))/1000)


baserunners_caught <- ball_caught[,c(1:2,10)] %>% slice(1) %>% 
                                                  left_join(player_positions[,1:6] %>% filter(player_id %in% c(11:13)),
                                                            by = c("game_string", "play_per_game"))
baserunners_caught <- baserunners_caught %>% filter(!is.na(timestamp))
baserunners_caught <- baserunners_caught %>% left_join(ball_events[,1:5], by = c("game_string", "play_per_game", "timestamp"),
                                                       suffix = c("_br", ""))

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












