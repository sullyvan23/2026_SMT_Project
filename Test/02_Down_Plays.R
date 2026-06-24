
### plays where ball hits ground
ball_down <- ball_events %>% group_by(game_string, play_per_game) %>% filter(sum(player_id %in% c(3:9)) > 0) %>% slice(2:3) %>%
                             filter(first(ball_eventcode) == 4, !last(ball_eventcode) %in% c(2,5,7,11))

### time in air and distance
ball_down <- ball_down %>% mutate(time_air = (last(timestamp) - first(timestamp))/1000) %>% slice(2)
ball_down <- ball_down %>% left_join(ball_positions[,1:6], by = c("game_string", "play_per_game", "timestamp"))
ball_down <- ball_down %>% filter(!is.na(ball_position_y)) %>%
                           mutate(ball_distance = sqrt(ball_position_x^2 + ball_position_y^2))
ball_down <- ball_down %>% filter(time_air >= 2, abs(ball_position_x) <= ball_position_y, ball_distance >= 155)

### half inning
ball_down <- ball_down %>% left_join(lineups[,c(1,7,3)], by = c("game_string", "play_per_game")) %>% relocate(half_inning, .after = play_per_game)
ball_down <- ball_down %>% distinct(game_string, play_per_game, .keep_all = TRUE)

##############################################################################################################################################################################################

### baserunner positions
baserunners_down <- ball_down[,1:3] %>% distinct() %>%
                                        left_join(player_positions[,1:6] %>% filter(player_id %in% c(11:13)),
                                                  by = c("game_string", "play_per_game"))
baserunners_down <- baserunners_down %>% filter(!is.na(timestamp))
baserunners_down <- baserunners_down %>% left_join(ball_events[,1:5] %>% distinct(), by = c("game_string", "play_per_game", "timestamp"),
                                                       suffix = c("_br", ""))



### distances for seeing how far on basepath player is
baserunners_down <- baserunners_down %>% mutate(dist_1st = sqrt((field_x - x_1b)^2 + (field_y - y_1b)^2),
                                                dist_2nd = sqrt((field_x - x_2b)^2 + (field_y - y_2b)^2),
                                                dist_3rd = sqrt((field_x - x_3b)^2 + (field_y - y_3b)^2),
                                                dist_home = sqrt((field_x - x_home)^2 + (field_y - y_home)^2))
baserunners_down <- baserunners_down %>% mutate(basepath = case_when(field_y < 0 | (field_y < 50 & field_x > 0)  ~  4, 
                                                                     field_y >= 50 & field_x > x_2b  ~  1 + (dist_1st / (dist_1st + dist_2nd)),
                                                                     field_y >= y_3b & field_x <= x_2b  ~  2 + (dist_2nd / (dist_2nd + dist_3rd)),
                                                                     field_y < y_3b & field_x <= x_home  ~  3 + (dist_3rd / (dist_3rd + dist_home)) ))

### final base
baserunners_down_end <- baserunners_down %>% group_by(game_string, play_per_game, player_id_br) %>% slice(n())
plot(baserunners_down_end$player_id_br, baserunners_down_end$basepath)
plot(baserunners_down_end$field_x, baserunners_down_end$field_y)

baserunners_down_end <- baserunners_down_end %>% mutate(next_play = play_per_game + 1)
baserunners_down_end <- baserunners_down_end %>% left_join(baserunners, by = c("game_string", "next_play" = "play_per_game"))

baserunners_down_end <- baserunners_down_end %>% group_by(game_string, play_per_game, player_id_br) %>%
                                                 mutate(final_base = case_when(basepath < 1.5  ~  1,
                                                                               basepath < 2.5 & basepath >= 1.5  ~  2,
                                                                               basepath < 3.25 & basepath >= 2.5  ~  3,
                                                                               TRUE  ~  4))
plot(baserunners_down_end$final_base, baserunners_down_end$basepath)


baserunners_down <- baserunners_down %>% group_by(game_string, play_per_game, player_id_br) %>%
                                         mutate(final_base = case_when(last(basepath) < 1.5  ~  1,
                                                                       last(basepath) < 2.5 & last(basepath) >= 1.5  ~  2,
                                                                       last(basepath) < 3.25 & last(basepath) >= 2.5  ~  3,
                                                                       TRUE  ~  4))

##############################################################################################################################################################################################

baserunners_down <- baserunners_down %>% left_join(ball_events[,1:5], by = c("game_string", "play_per_game", "timestamp"),
                                                   suffix = c("_br", ""))

baserunners_down <- baserunners_down %>% group_by(game_string, play_per_game, player_id_br) %>% 
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
baserunners_down <- baserunners_down %>% left_join(player_positions[,1:6], by = c("game_string", "play_per_game", "player_id", "timestamp"), 
                                                   suffix = c("_runner", "_fielder"))


baserunners_down <- baserunners_down %>% left_join(baserunners, by = c("game_string", "play_per_game"))
baserunners_down <- baserunners_down %>% mutate(force = case_when(player_id_br == 11  ~  ifelse(final_base == 2, 1, 0),
                                                                  player_id_br == 12  ~  ifelse(first == 1  &  final_base == 3, 1, 0),
                                                                  player_id_br == 13  ~  ifelse(first+second == 2, 1, 0))) %>%
                                         select(-c(first:third))

baserunners_down <- baserunners_down %>% mutate(next_play = play_per_game + 1)
baserunners_down <- baserunners_down %>% left_join(baserunners, by = c("game_string", "next_play" = "play_per_game"))


baserunners_down <- baserunners_down %>% ungroup() %>%
                             mutate(next_run_dist = case_when(final_base == 1  ~  sqrt((field_x_runner - x_1b)^2 + (field_y_runner - y_1b)^2),
                                                              final_base == 2  ~  sqrt((field_x_runner - x_2b)^2 + (field_y_runner - y_2b)^2),
                                                              final_base == 3  ~  sqrt((field_x_runner - x_3b)^2 + (field_y_runner - y_3b)^2),
                                                              final_base == 4  ~  sqrt((field_x_runner - x_home)^2 + (field_y_runner - y_home)^2)),
                                    run_field_dist = sqrt((field_x_fielder - field_x_runner)^2 + (field_y_fielder - field_y_runner)^2),
                                    next_field_dist = case_when(final_base == 1  ~  sqrt((field_x_fielder - x_1b)^2 + (field_y_fielder - y_1b)^2),
                                                                final_base == 2  ~  sqrt((field_x_fielder - x_2b)^2 + (field_y_fielder - y_2b)^2),
                                                                final_base == 3  ~  sqrt((field_x_fielder - x_3b)^2 + (field_y_fielder - y_3b)^2),
                                                                final_base == 4  ~  sqrt((field_x_fielder - x_home)^2 + (field_y_fielder - y_home)^2)),
                                    min_field_dist = ifelse(force == 1, pmin(run_field_dist, next_field_dist), run_field_dist),
                                    safe_est = case_when(final_base == 1  ~ ifelse(is.na(first), 0.5, first),
                                                         final_base == 2  ~ ifelse(is.na(second), 0.5, second),
                                                         final_base == 3  ~  ifelse(is.na(third), 0.5, third),
                                                         final_base == 4  ~  0.5))


baserunners_down <- baserunners_down %>% group_by(game_string, play_per_game, player_id_br) %>% mutate(runner_within_4 = 0) %>%
                                         group_modify(~{
                                           for(i in 2:nrow(.x)) {
                                              
                                              if(.x$next_run_dist[i] <= 4  |  .x$runner_within_4[i-1] == 1) {
                                                .x$runner_within_4[i] = 1
                                              }
                                           }
                                           
                                           .x
                                         })

baserunners_down <- baserunners_down %>% mutate(min_field_dist = ifelse(player_id == 10, NA, min_field_dist))

baserunners_down <- baserunners_down %>% mutate(pos_safe_est = ifelse(sum(runner_within_4 == 0  &  min_field_dist <= 5, na.rm = TRUE) > 0, 0, 1))

##############################################################################################################################################################################################

ball_down_results <- baserunners_down %>% summarise(force = first(force),
                                                    final_base = first(final_base),
                                                    safe_advance = first(pos_safe_est))
ball_down_results <- ball_down_results %>% mutate(att_bases_advanced = case_when(player_id_br == 11  ~  final_base - 1,
                                                                                 player_id_br == 12  ~  final_base - 2,
                                                                                 player_id_br == 13  ~  final_base - 3),
                                                  succ_bases_advanced = att_bases_advanced + (safe_advance - 1))


write.csv(ball_down_results, "ball_down_results.csv", row.names = FALSE)








