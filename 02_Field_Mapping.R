home_count <- lineups %>% group_by(home_team) %>% summarise(count = n())

ball_bounces <- ball_events %>% filter(ball_eventcode == 16)
ball_bounces_2 <- ball_bounces %>% left_join(ball_positions[,1:6], by = c("game_string", "play_per_game", "timestamp"))

ball_wall_hits <- ball_events %>% filter(ball_eventcode == 10)
