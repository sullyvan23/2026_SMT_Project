
### getting together data from all plays
all_plays_data_sum <- bind_rows(doubled_up_data_sum, tag_up_data_sum, advance_data_sum)
all_plays_data_sum <- all_plays_data_sum %>% distinct(game_string, play_per_game, player_id_br, timestamp, .keep_all = TRUE)

### calculating base positions
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

### calculating dists/velos/directions, ended up not being significant in models so not really used
all_plays_data_sum <- all_plays_data_sum %>% mutate(OF_og_x_dist = pred_x - og_base_x,
                                                    OF_og_y_dist = pred_y - og_base_y,
                                                    OF_og_dist = sqrt(OF_og_x_dist^2 + OF_og_y_dist^2),
                                                    ground_og_dist = sqrt((ground_x - og_base_x)^2 + (ground_y - og_base_y)^2),          
                                                    OF_ground_og_dist = ((OF_og_x_dist * OF_ground_x_dist) + (OF_og_y_dist * OF_ground_y_dist)) / 
                                                                          OF_og_dist,
                                                    OF_ground_og_angle = acos(OF_ground_og_dist / OF_ground_dist))
all_plays_data_sum <- all_plays_data_sum %>% mutate(OF_next_x_dist = pred_x - next_base_x,
                                                    OF_next_y_dist = pred_y - next_base_y,
                                                    OF_next_dist = sqrt(OF_next_x_dist^2 + OF_next_y_dist^2),
                                                    ground_next_dist = sqrt((ground_x - next_base_x)^2 + (ground_y - next_base_y)^2),          
                                                    OF_ground_next_dist = ((OF_next_x_dist * OF_ground_x_dist) + (OF_next_y_dist * OF_ground_y_dist)) / 
                                                                          OF_next_dist,
                                                    OF_ground_next_angle = acos(OF_ground_next_dist / OF_ground_dist))
all_plays_data_sum <- all_plays_data_sum %>% mutate(OF_next2_x_dist = pred_x - next2_base_x,
                                                    OF_next2_y_dist = pred_y - next2_base_y,
                                                    OF_next2_dist = sqrt(OF_next2_x_dist^2 + OF_next2_y_dist^2),
                                                    ground_next2_dist = sqrt((ground_x - next2_base_x)^2 + (ground_y - next2_base_y)^2),          
                                                    OF_ground_next2_dist = ((OF_next2_x_dist * OF_ground_x_dist) + (OF_next2_y_dist * OF_ground_y_dist)) / 
                                                                          OF_next2_dist,
                                                    OF_ground_next2_angle = acos(OF_ground_next2_dist / OF_ground_dist))  
all_plays_data_sum <- all_plays_data_sum %>% mutate(OF_next3_x_dist = pred_x - next3_base_x,
                                                    OF_next3_y_dist = pred_y - next3_base_y,
                                                    OF_next3_dist = sqrt(OF_next3_x_dist^2 + OF_next3_y_dist^2),
                                                    ground_next3_dist = sqrt((ground_x - next3_base_x)^2 + (ground_y - next3_base_y)^2),          
                                                    OF_ground_next3_dist = ((OF_next3_x_dist * OF_ground_x_dist) + (OF_next3_y_dist * OF_ground_y_dist)) / 
                                                                          OF_next3_dist,
                                                    OF_ground_next3_angle = acos(OF_ground_next3_dist / OF_ground_dist))

### adding liklihood of single, double, triple of batter for plays
all_plays_data_sum <- cbind(all_plays_data_sum, predict(batter_bases_model, newdata = all_plays_data_sum, type = "response"))
all_plays_data_sum <- all_plays_data_sum %>% rename(batter_1 = "...84",
                                                    batter_2 = "...85",
                                                    batter_3 = "...86")

### NEED
### time_to_ground, time_left_ground, basepath, og_basepath_dist, runner_basepath_velo, runner_baseball_accel
### ground_og_dist, ground_next_dist, ground_next2_dist, ground_next3_dist, wall_ground_dist,
### OF_ground_og_angle, OF_ground_next_angle, OF_ground_next2_angle, OF_ground_next3_angle, 
### speed_95_runner, speed_95_throw, caught_prob

### taking only necessary data
all_plays_data_sum <- all_plays_data_sum %>% select(game_string, play_per_game, timestamp, player_id_br, player_code_runner,
                                                    time_to_ground, time_left_ground, ground_x, ground_y, 
                                                    basepath, og_basepath_dist, runner_basepath_velo, runner_basepath_accel, ground_og_dist, ground_next_dist, ground_next2_dist, ground_next3_dist,
                                                    wall_ground_dist, OF_ground_og_angle, OF_ground_next_angle, OF_ground_next2_angle, OF_ground_next3_angle,
                                                    speed_95_runner, speed_95_throw, caught_prob, batter_1, batter_2, batter_3)
all_plays_data_sum <- all_plays_data_sum %>% group_by(game_string, play_per_game, player_id_br) %>%
                                             mutate(fps = time_left_ground - lead(time_left_ground),
                                                    fps = ifelse(is.na(fps), lag(fps), fps))

### calculating last acceleration before current to be used for estmating next acceleration
all_plays_data_sum <- all_plays_data_sum %>% mutate(runner_basepath_accel_2 = lag(runner_basepath_accel),
                                                    runner_basepath_accel_2 = ifelse(row_number() == 1, 
                                                                                   lead(runner_basepath_accel_2), 
                                                                                   runner_basepath_accel_2)) %>%
                                             relocate(runner_basepath_accel_2, .after = runner_basepath_accel)

write.csv(all_plays_data_sum, "all_plays_data_sum.csv", row.names = FALSE)

##################################################################################################################################################################################

### limiting to one person on, simplify model and not take into account other runners as well
one_on_data_sum <- all_plays_data_sum %>% group_by(game_string, play_per_game, timestamp) %>% filter(n() == 1) %>% ungroup()

### pivoting batter advance probabilities
one_on_data_sum <- one_on_data_sum %>% pivot_longer(cols = batter_1:batter_3,
                                                    names_to = "proj_batter_base",
                                                    values_to = "prob")

### dataset with run expectancies of situations I made in excel
one_on_run_exps <- read_csv("one_on_run_exps.csv")
one_on_data_sum <- one_on_data_sum %>% left_join(one_on_run_exps, by = c("player_id_br", "proj_batter_base"))

### making run expectancies weighted by how far batter might advance if dropped
one_on_data_sum <- one_on_data_sum %>% group_by(game_string, play_per_game, player_id_br, timestamp) %>% 
                                       mutate(across(c(d_sit_re:a3_sit_re), ~ weighted.mean(., prob))) %>%
                                       slice(1) %>% ungroup()
one_on_data_sum <- one_on_data_sum %>% select(-c(proj_batter_base, prob))

##################################################################################################################################################################################

### dataset of only plays with a runner with play code (from 4 main teams), have enough data to maybe be on leaderboard
plays_share <- one_on_data_sum %>% filter(!is.na(player_code_runner))

### taking only players who show up 5+ times
plays_num <- plays_share %>% group_by(game_string, play_per_game, player_id_br) %>% slice(n())
plays_num <- plays_num %>% group_by(player_code_runner) %>% summarise(count = n())
plays_num <- plays_num %>% filter(count >= 5)

plays_share <- plays_share %>% filter(player_code_runner %in% plays_num$player_code_runner)

write.csv(plays_share, "plays_share.csv", row.names = FALSE)







