
all_plays_data_sum <- bind_rows(doubled_up_data_sum, tag_up_data_sum, advance_one_data_sum)
all_plays_data_sum <- all_plays_data_sum %>% distinct(game_string, play_per_game, player_id_br, timestamp, .keep_all = TRUE)


all_plays_data_sum <- all_plays_data_sum %>% mutate(og_base_x = case_when(player_id_br == 11  ~  x_1b,
                                                                          player_id_br == 12  ~  x_2b,
                                                                          player_id_br == 13  ~  x_3b),
                                                    og_base_y = case_when(player_id_br == 11  ~  y_1b,
                                                                          player_id_br == 12  ~  y_2b,
                                                                          player_id_br == 13  ~  y_3b))
all_plays_data_sum <- all_plays_data_sum %>% mutate(next_base_x = case_when(player_id_br == 11  ~  x_2b,
                                                                            player_id_br == 12  ~  x_3b,
                                                                            player_id_br == 13  ~  x_home),
                                                    next_base_y = case_when(player_id_br == 11  ~  y_2b,
                                                                            player_id_br == 12  ~  y_3b,
                                                                            player_id_br == 13  ~  y_home))
all_plays_data_sum <- all_plays_data_sum %>% mutate(next2_base_x = case_when(player_id_br == 11  ~  x_3b,
                                                                             player_id_br == 12  ~  x_home,
                                                                             player_id_br == 13  ~  NA),
                                                    next2_base_y = case_when(player_id_br == 11  ~  y_3b,
                                                                             player_id_br == 12  ~  y_home,
                                                                             player_id_br == 13  ~  NA))
all_plays_data_sum <- all_plays_data_sum %>% mutate(next3_base_x = case_when(player_id_br == 11  ~  x_home,
                                                                             TRUE  ~  NA),
                                                    next3_base_y = case_when(player_id_br == 11  ~  y_home,
                                                                             TRUE  ~  NA))
all_plays_data_sum <- all_plays_data_sum %>% mutate(OF_og_x_dist = pred_x_OF - og_base_x,
                                                    OF_og_y_dist = pred_y_OF - og_base_y,
                                                    OF_og_dist = sqrt(OF_og_x_dist^2 + OF_og_y_dist^2),
                                                    ground_og_dist = sqrt((ground_x - og_base_x)^2 + (ground_y - og_base_y)^2),          
                                                    OF_ground_og_dist = ((OF_og_x_dist * OF_ground_x_dist) + (OF_og_y_dist * OF_ground_y_dist)) / 
                                                                          OF_og_dist,
                                                    OF_ground_og_angle = acos(OF_ground_og_dist / OF_ground_dist))
all_plays_data_sum <- all_plays_data_sum %>% mutate(OF_next_x_dist = pred_x_OF - next_base_x,
                                                    OF_next_y_dist = pred_y_OF - next_base_y,
                                                    OF_next_dist = sqrt(OF_next_x_dist^2 + OF_next_y_dist^2),
                                                    ground_next_dist = sqrt((ground_x - next_base_x)^2 + (ground_y - next_base_y)^2),          
                                                    OF_ground_next_dist = ((OF_next_x_dist * OF_ground_x_dist) + (OF_next_y_dist * OF_ground_y_dist)) / 
                                                                          OF_next_dist,
                                                    OF_ground_next_angle = acos(OF_ground_next_dist / OF_ground_dist))
all_plays_data_sum <- all_plays_data_sum %>% mutate(OF_next2_x_dist = pred_x_OF - next2_base_x,
                                                    OF_next2_y_dist = pred_y_OF - next2_base_y,
                                                    OF_next2_dist = sqrt(OF_next2_x_dist^2 + OF_next2_y_dist^2),
                                                    ground_next2_dist = sqrt((ground_x - next2_base_x)^2 + (ground_y - next2_base_y)^2),          
                                                    OF_ground_next2_dist = ((OF_next2_x_dist * OF_ground_x_dist) + (OF_next2_y_dist * OF_ground_y_dist)) / 
                                                                          OF_next2_dist,
                                                    OF_ground_next2_angle = acos(OF_ground_next2_dist / OF_ground_dist))  
all_plays_data_sum <- all_plays_data_sum %>% mutate(OF_next3_x_dist = pred_x_OF - next3_base_x,
                                                    OF_next3_y_dist = pred_y_OF - next3_base_y,
                                                    OF_next3_dist = sqrt(OF_next3_x_dist^2 + OF_next3_y_dist^2),
                                                    ground_next3_dist = sqrt((ground_x - next3_base_x)^2 + (ground_y - next3_base_y)^2),          
                                                    OF_ground_next3_dist = ((OF_next3_x_dist * OF_ground_x_dist) + (OF_next3_y_dist * OF_ground_y_dist)) / 
                                                                          OF_next3_dist,
                                                    OF_ground_next3_angle = acos(OF_ground_next3_dist / OF_ground_dist))

### NEED
### time_to_ground, time_left_ground, basepath, og_basepath_dist, runner_basepath_velo
### ground_og_dist, ground_next_dist, ground_next2_dist, ground_next3_dist
### OF_ground_og_angle, OF_ground_next_angle, OF_ground_next2_angle, OF_ground_next3_angle, 
### speed_95_runner, speed_95_throw, caught_prob


all_plays_data_sum <- all_plays_data_sum %>% select(game_string, play_per_game, timestamp, player_id_br,
                                                    time_to_ground, time_left_ground, ground_x, ground_y, 
                                                    basepath, og_basepath_dist, runner_basepath_velo, ground_og_dist, ground_next_dist, ground_next2_dist, ground_next3_dist,
                                                    OF_ground_og_angle, OF_ground_next_angle, OF_ground_next2_angle, OF_ground_next3_angle,
                                                    speed_95_runner, speed_95_throw, caught_prob)
all_plays_data_sum <- all_plays_data_sum %>% ungroup() %>% mutate(proj_batter_base = round(predict(batter_bases_model, newdata = all_plays_data_sum)))

write.csv(all_plays_data_sum, "all_plays_data_sum.csv", row.names = FALSE)

##################################################################################################################################################################################

one_on_data_sum <- all_plays_data_sum %>% group_by(game_string, play_per_game, timestamp) %>% filter(n() == 1) %>% ungroup()

### dataset with run expectancies of situations
one_on_run_exps <- read_csv("one_on_run_exps.csv")
one_on_data_sum <- one_on_data_sum %>% left_join(one_on_run_exps, by = c("player_id_br", "proj_batter_base"))






