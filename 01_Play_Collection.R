library(dplyr)

OF_field_events <- ball_events %>% group_by(game_string, play_per_game) %>%
                                   filter(sum(ball_eventcode == 4) > 0   &   sum(player_id %in% c(7:9)) > 0   &   !is.na(game_string))

OF_field_hit <- OF_field_events %>% filter(ball_eventcode == 4) %>% group_by(game_string, play_per_game) %>% filter(n() == 1)
OF_field_first_bounce_acq <- OF_field_events %>% filter(lag(ball_eventcode) == 4) %>% group_by(game_string, play_per_game) %>% filter(n() == 1)

OF_field_timing <- OF_field_hit %>% left_join(OF_field_first_bounce_acq[,1:5], by = c("game_string", "play_per_game"), suffix = c("_hit", "_down"))
OF_field_timing <- OF_field_timing %>% mutate(air_time = (timestamp_down - timestamp_hit) / 1000)
plot(OF_field_timing$air_time)

plot(OF_field_timing$air_time, OF_field_timing$player_id_down)

fly_balls <- OF_field_timing %>% filter(air_time >= 2) %>% select(game_string, play_per_game, player_id_down, ball_eventcode_down, timestamp_hit, timestamp_down)
