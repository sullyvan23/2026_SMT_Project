library(dplyr)

OF_field_events <- ball_events %>% group_by(game_string, play_per_game) %>%
                                   filter(sum(ball_eventcode == 4) > 0   &   sum(player_id %in% c(7:9)) > 0 )

OF_field_plays <- OF_field_events %>% filter(player_id %in% c(7:9)   &   ball_eventcode == 2)
OF_field_plays <- OF_field_plays %>% group_by(game_string, play_per_game) %>% filter(row_number() == 1)
