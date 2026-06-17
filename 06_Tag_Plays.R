
ball_positions_2 <- ball_positions %>% mutate(play_key = paste0(game_string, play_per_game))
player_positions_2 <- player_positions %>% mutate(play_key = paste0(game_string, play_per_game))

tag_up_positions <- tagged_up %>% ungroup() %>% distinct(play_key, game_string, play_per_game, player_id_br, original_base)
tag_up_positions <- tag_up_positions %>% rename(next_base = original_base) %>% mutate(next_base = next_base + 1)

tag_up_positions <- tag_up_positions %>% left_join(baserunner_positions[,c(3:6,11)], by = c("play_key", "player_id_br" = "player_id"))
tag_up_positions <- tag_up_positions %>% left_join(ball_events[,1:5], by = c("game_string", "play_per_game", "timestamp"))

tag_up_positions <- tag_up_positions %>% group_by(play_key, player_id_br) %>% mutate(ball_possessed = 0, 
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


tag_up_positions <- tag_up_positions %>% left_join(player_positions_2[,c(3:6,11)], by = c("play_key", "player_id", "timestamp"), suffix = c("_runner", "_fielder"))

tag_up_positions <- tag_up_positions %>% mutate(next_base_dist = case_when(next_base == 2  ~  sqrt(field_x_runner^2 + (field_y_runner - 126.22)^2),
                                                                           next_base == 3  ~  sqrt((field_x_runner + 63.11)^2 + (field_y_runner - 63.11)^2),
                                                                           next_base == 4  ~  sqrt(field_x_runner^2 + (field_y_runner - 0.71)^2)))
tag_up_positions <- tag_up_positions %>% mutate(run_field_dist = sqrt((field_x_runner - field_x_fielder)^2 + (field_y_runner - field_y_fielder)^2) ) 


baserunners <- baserunner_positions %>% distinct(play_key, player_id) %>% group_by(play_key) %>%
                                        summarise(first = sum(player_id == 11),
                                                  second = sum(player_id == 12), 
                                                  third = sum(player_id == 13))

tag_up_positions <- tag_up_positions %>% mutate(play_key_next = paste0(game_string, play_per_game+1)) %>% 
                                         left_join(baserunners, by = c("play_key_next" = "play_key"))

tag_up_positions <- tag_up_positions %>% mutate(safe_est = case_when(next_base == 2  ~  second,
                                                                     next_base == 3  ~  third,
                                                                     next_base == 4  ~  0.5))


ggplot(tag_up_positions %>% filter(run_field_dist <= 15), aes(x = next_base_dist, y = run_field_dist, color = safe_est)) + 
       geom_point() + scale_color_gradient2(high = "green", low = "red", mid = "white", midpoint = 0.5)


tag_up_positions <- tag_up_positions %>% group_by(play_key, player_id_br) %>% mutate(runner_within_3 = 0) %>%
                                         group_modify(~{
                                           for(i in 2:nrow(.x)) {
                                              
                                              if(.x$next_base_dist[i] <= 3  |  .x$runner_within_3[i-1] == 1) {
                                                .x$runner_within_3[i] = 1
                                              }
                                           }
                                           
                                           .x
                                         })


tag_up_positions <- tag_up_positions %>% mutate(pos_safe_est = ifelse(sum(runner_within_3 == 0  &  run_field_dist < 4, na.rm = TRUE) > 0, 0, 1))

ggplot(tag_up_positions %>% filter(run_field_dist <= 15), aes(x = next_base_dist, y = run_field_dist, color = pos_safe_est)) + 
       geom_point() + scale_color_gradient2(high = "green", low = "red", mid = "white", midpoint = 0.5)


################################################################################################################################################################################################


pot_tag_results <- less_2_outs_caught %>% ungroup() %>% distinct(play_key, game_string, play_per_game, player_id_br)

pot_tag_results <- pot_tag_results %>% left_join(tag_up_positions %>% distinct(play_key, player_id_br, pos_safe_est),
                                                 by = c("play_key", "player_id_br"))

pot_tag_results <- pot_tag_results %>% rename(safe_tag = pos_safe_est) %>% 
                                       mutate(tagged_up = ifelse(is.na(safe_tag), 0, 1),
                                              successful_tag = ifelse(is.na(safe_tag), 0, safe_tag)) %>%
                                       relocate(safe_tag, .after = tagged_up)


write.csv(pot_tag_results, "pot_tag_results.csv", row.names = FALSE)

