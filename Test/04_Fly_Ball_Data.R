
fly_balls <- bind_rows(ball_caught %>% mutate(caught = 1), 
                       ball_down %>% mutate(caught = 0))

fly_balls <- fly_balls %>% rename(timestamp_done = timestamp) %>%
                           mutate(timestamp_hit = timestamp_done - (1000*time_air)) %>% relocate(timestamp_hit, .before = timestamp_done)

fly_balls <- fly_balls %>% group_by(home_team) %>%
                           mutate(caught_height = case_when(home_team == "ANI"  ~  ball_position_z - predict(ground_ANI_model, newdata = pick(everything())),
                                                            home_team == "ARN"  ~  ball_position_z - predict(ground_ARN_model, newdata = pick(everything())),
                                                            home_team == "PHD"  ~  ball_position_z - predict(ground_PHD_model, newdata = pick(everything())),
                                                            home_team == "VAS"  ~  ball_position_z - predict(ground_VAS_model, newdata = pick(everything()))),
                                  caught_height = ifelse(caught == 1, caught_height, NA)) %>% ungroup()


hist(fly_balls$caught_height, breaks = 100)
### centered at 6

############################################################################################################################################################################################

fly_balls_proj <- fly_balls[,c(1:2,4:5)] %>% left_join(ball_positions[,1:6], by = c("game_string", "play_per_game"))
fly_balls_proj <- fly_balls_positions %>% filter(timestamp > time)


