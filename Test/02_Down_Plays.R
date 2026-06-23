
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

##############################################################################################################################################################################################


### baserunner positions
baserunners_down <- ball_down[,1:3] %>% left_join(player_positions[,1:6] %>% filter(player_id %in% c(11:13)),
                                                  by = c("game_string", "play_per_game"))
baserunners_down <- baserunners_down %>% filter(!is.na(timestamp))
baserunners_down <- baserunners_down %>% left_join(ball_events[,1:5], by = c("game_string", "play_per_game", "timestamp"),
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

















