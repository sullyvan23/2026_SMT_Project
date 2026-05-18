library(dplyr)

OF_field_events <- ball_events %>% group_by(game_string, play_per_game) %>%
                                   filter(sum(ball_eventcode == 4) > 0   &   sum(player_id %in% c(7:9)) > 0 )

OF_field_hit <- OF_field_events %>% filter(ball_eventcode == 4)
OF_field_first_bounce_acq <- OF_field_events %>% filter(player_id %in% c(7:9,255)) %>% 
                                                 group_by(game_string, play_per_game) %>% 
                                                 filter(row_number() == 1)

OF_field_timing <- OF_field_hit %>% left_join(OF_field_first_bounce_acq[,1:4], by = c("game_string", "play_per_game"), suffix = c("_hit", "_down"))
OF_field_timing <- OF_field_timing %>% filter(!is.na(timestamp_down))
OF_field_timing <- OF_field_timing %>% mutate(air_time = (timestamp_down - timestamp_hit) / 1000)
plot(OF_field_timing$air_time)

OF_field_timing <- OF_field_timing %>% filter(air_time < 25   &   air_time > -25)
plot(OF_field_timing$air_time, OF_field_timing$player_id_down)

fly_balls <- OF_field_timing %>% filter(air_time >= 2) %>% select(game_string, play_per_game, player_id_down, timestamp_hit, timestamp_down)
fly_balls <- fly_balls %>% mutate(OF_hit_pos_key = paste0(game_string, play_per_game, "_", timestamp_hit),
                                  ball_pos_key = paste0(game_string, play_per_game))

OF_pos_fly_ball <- player_positions %>% filter(player_id %in% c(7:9)) %>% left_join(fly_balls[,c(1:2,4)], by = c("game_string", "play_per_game"))
OF_pos_fly_ball <- OF_pos_fly_ball %>% filter(!is.na(timestamp_hit)) %>% mutate(time_diff = abs(timestamp - timestamp_hit))
OF_pos_fly_ball <- OF_pos_fly_ball %>% group_by(game_string, play_per_game) %>% filter(time_diff == min(time_diff))

ball_pos_fly_ball <- ball_positions %>% mutate(ball_pos_key = paste0(game_string, play_per_game))
ball_pos_fly_ball <- ball_pos_fly_ball %>% filter(ball_pos_key %in% fly_balls$ball_pos_key)

