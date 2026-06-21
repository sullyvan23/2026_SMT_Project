
ball_lands_positions <- ball_lands_go %>% left_join(baserunners[,1:3], by = "play_key")
ball_lands_positions <- ball_lands_positions %>% mutate(forced = case_when(original_base == 1  ~  1,
                                                                           (original_base == 2) & (first == 1)  ~  1,
                                                                           (original_base == 3) & (first == 1) & (second == 1)  ~  1,
                                                                           TRUE  ~  0)) %>% select(-c(first:second))

ball_lands_positions <- ball_lands_positions %>% left_join(baserunner_positions[,c(3:6,11)], by = c("play_key", "player_id_br" = "player_id"))
ball_lands_positions <- ball_lands_positions %>% left_join(ball_events[,1:5], by = c("game_string", "play_per_game", "timestamp"))

ball_lands_positions <- ball_lands_positions %>% group_by(play_key, player_id_br) %>% mutate(ball_possessed = 0, 
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


ball_lands_positions <- ball_lands_positions %>% left_join(player_positions_2[,c(3:6,11)], by = c("play_key", "player_id", "timestamp"), suffix = c("_runner", "_fielder"))

ball_lands_positions <- ball_lands_positions %>% mutate(final_base_dist = case_when(final_base == 2  ~  sqrt(field_x_runner^2 + (field_y_runner - 126.22)^2),
                                                                                   final_base == 3  ~  sqrt((field_x_runner + 63.11)^2 + (field_y_runner - 63.11)^2),
                                                                                   final_base == 4  ~  sqrt(field_x_runner^2 + (field_y_runner - 0.71)^2)))
ball_lands_positions <- ball_lands_positions %>% mutate(run_field_dist = sqrt((field_x_runner - field_x_fielder)^2 + (field_y_runner - field_y_fielder)^2) ) 

ball_lands_positions <- ball_lands_positions %>% mutate(play_key_next = paste0(game_string, play_per_game+1)) %>% 
                                                 left_join(baserunners, by = c("play_key_next" = "play_key"))

ball_lands_positions <- ball_lands_positions %>% mutate(safe_est = case_when(final_base == 2  ~  second,
                                                                             final_base == 3  ~  third,
                                                                             final_base == 4  ~  0.5))


ball_lands_positions <- ball_lands_positions %>% filter(!(forced == 1  &  att_bases_advanced == 0))
force_play_positions <- ball_lands_positions %>% filter(forced == 1, att_bases_advanced == 1)
not_forced_play_positions <- ball_lands_positions %>% filter(!(forced == 1 & att_bases_advanced == 1)  &  att_bases_advanced > 0)
dont_advance_play_positions <- ball_lands_positions %>% filter(att_bases_advanced == 0)

force_play_positions <- force_play_positions %>% mutate(base_field_dist = case_when(final_base == 2  ~  sqrt(field_x_fielder^2 + (field_y_fielder - 126.22)^2),
                                                                                    final_base == 3  ~  sqrt((field_x_fielder + 63.11)^2 + (field_y_fielder - 63.11)^2),
                                                                                    final_base == 4  ~  sqrt(field_x_fielder^2 + (field_y_fielder - 0.71)^2))) %>%
                                                 relocate(base_field_dist, .after = run_field_dist)


ggplot(not_forced_play_positions %>% filter(run_field_dist <= 15), aes(x = final_base_dist, y = run_field_dist, color = safe_est)) + 
       geom_point() + scale_color_gradient2(high = "green", low = "red", mid = "white", midpoint = 0.5)


ball_lands_positions <- ball_lands_positions %>% group_by(play_key, player_id_br) %>% mutate(runner_within_3 = 0) %>%
                                         group_modify(~{
                                           for(i in 2:nrow(.x)) {
                                              
                                              if(.x$final_base_dist[i] <= 3  |  .x$runner_within_3[i-1] == 1) {
                                                .x$runner_within_3[i] = 1
                                              }
                                           }
                                           
                                           .x
                                         })


ball_lands_positions <- ball_lands_positions %>% mutate(pos_safe_est = ifelse(sum(runner_within_3 == 0  &  run_field_dist < 4, na.rm = TRUE) > 0, 0, 1))

ggplot(ball_lands_positions %>% filter(run_field_dist <= 15), aes(x = final_base_dist, y = run_field_dist, color = pos_safe_est)) + 
       geom_point() + scale_color_gradient2(high = "green", low = "red", mid = "white", midpoint = 0.5)


check <- ball_lands_positions %>% filter(safe_est != pos_safe_est, safe_est != 0.5, final_base_dist < 30)

ball_lands_positions <- ball_lands_positions %>% mutate(final_safe = ifelse(safe_est == 0.5, pos_safe_est, safe_est),
                                                        final_safe = ifelse(play_key == "y1_d153.5_RME_ARN97"  &  player_id_br == 12, 1, pos_safe_est))

####################################################################################################################################################################

ball_lands_results <- ball_lands_positions %>% distinct(play_key, game_string, play_per_game, player_id_br, final_base, att_bases_advanced, forced, final_safe)

ball_lands_results <- ball_lands_results %>% mutate(original_base = player_id_br - 10,
                                                    succ_bases_advanced = att_bases_advanced - ifelse(final_safe == 1, 0, 1)) %>% 
                                             relocate(original_base, .after = player_id_br)

write.csv(ball_lands_results, "ball_lands_results.csv", row.names = FALSE)


