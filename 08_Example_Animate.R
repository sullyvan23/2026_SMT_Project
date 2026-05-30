ball_pos_fly_ball_animate <- ball_pos_fly_ball %>% mutate(play_key = paste0(game_string, play_per_game))

ex_ball_flight <- ball_pos_fly_ball_animate[,c(14,3,5:7)] %>% filter(play_key == "y1_d143_ADQ_ANI17")

ball_pos_fly_ball_ground <- ball_pos_fly_ball_2.1
names(ball_pos_fly_ball_ground) <- names(ex_ball_flight)

ex_ball_flight <- bind_rows(ex_ball_flight, ball_pos_fly_ball_ground %>% filter(play_key == "y1_d143_ADQ_ANI17"))
ex_ball_flight <- ex_ball_flight %>% arrange(timestamp)
ex_ball_flight <- ex_ball_flight %>% filter(timestamp <= max(ifelse(timestamp%%1 != 0, timestamp, 0)) ) 

write.csv(ex_ball_flight, "ex_ball_flight.csv", row.names = FALSE)
