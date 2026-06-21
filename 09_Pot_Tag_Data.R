
pot_tag_data <- pot_tag_results %>% left_join(catch_prob_all_time_pred_2, by = "play_key")
pot_tag_data <- pot_tag_data %>% left_join(baserunner_positions[,c(3:6,11)], by = c("play_key", "timestamp", "player_id_br" = "player_id"),
                                           suffix = c("", "_br"))

pot_tag_data <- pot_tag_data %>% mutate(og_base_x = case_when(player_id_br == 11  ~  63.11,
                                                              player_id_br == 12  ~  0,
                                                              player_id_br == 13  ~  -63.11),
                                        og_base_y = case_when(player_id_br == 11  ~  63.11,
                                                              player_id_br == 12  ~  126.22,
                                                              player_id_br == 13  ~  63.11),
                                        next_base_x = case_when(player_id_br == 11  ~  0,
                                                                player_id_br == 12  ~  -63.11,
                                                                player_id_br == 13  ~  0),
                                        next_base_y = case_when(player_id_br == 11  ~  126.22,
                                                                player_id_br == 12  ~  63.11,
                                                                player_id_br == 13  ~  0.71))


pot_tag_data <- pot_tag_data %>% mutate(ground_next_x_dist = ground_x - next_base_x,
                                        ground_next_y_dist = ground_y - next_base_y,
                                        ground_next_dist = sqrt(ground_next_x_dist^2 + ground_next_y_dist^2),
                                        br_og_x_dist = field_x_br - og_base_x,
                                        br_og_y_dist = field_y_br - og_base_y,
                                        br_og_dist = sqrt(br_og_x_dist^2 + br_og_y_dist^2))


pot_tag_data <- pot_tag_data %>% mutate(OF_ball_next_front_dist = -((OF_ball_x_dist * ground_next_x_dist) + (OF_ball_y_dist * ground_next_y_dist)) / 
                                                                  ground_next_dist)
pot_tag_data <- pot_tag_data %>% mutate(OF_ball_next_angle = acos(-OF_ball_next_front_dist / OF_ball_dist))
pot_tag_data <- pot_tag_data %>% mutate(unit_x = ground_next_x_dist / ground_next_dist,
                                        unit_y = ground_next_y_dist / ground_next_dist,
                                        OF_ball_next_side_dist = OF_ball_dist * sin(OF_ball_next_angle),
                                        pos_next_side_diff = abs( (field_x + (OF_ball_next_front_dist * -unit_x) + (OF_ball_next_side_dist * unit_y)) - ground_x ),
                                        neg_next_side_diff = abs( (field_x + (OF_ball_next_front_dist * -unit_x) + (OF_ball_next_side_dist * -unit_y)) - ground_x ),
                                        OF_ball_next_side_dist = ifelse(pos_next_side_diff < neg_next_side_diff,
                                                                   OF_ball_next_side_dist, -OF_ball_next_side_dist)) %>% 
                                 dplyr::select(-c(unit_x, unit_y, pos_next_side_diff, neg_next_side_diff)) %>%
                                 relocate(OF_ball_next_side_dist, .before = OF_ball_next_angle)
pot_tag_data <- pot_tag_data %>% mutate(OF_ball_next_angle = atan2(OF_ball_next_side_dist, OF_ball_next_front_dist))


pot_tag_data <- pot_tag_data %>% mutate(velo_next_towards = -((x_velo * ground_next_x_dist) + (y_velo * ground_next_y_dist)) / 
                                                             ground_next_dist)
pot_tag_data <- pot_tag_data %>% mutate(velo_next_angle = acos(velo_next_towards / speed))
pot_tag_data <- pot_tag_data %>% mutate(unit_x = ground_next_x_dist / ground_next_dist,
                                        unit_y = ground_next_y_dist / ground_next_dist,
                                        velo_next_side = speed * sin(velo_next_angle),
                                        pos_side_diff = abs( ((velo_next_towards * unit_x) + (velo_next_side * unit_y)) - x_velo ),
                                        neg_side_diff = abs( ((velo_next_towards * unit_x) + (velo_next_side * -unit_y)) - x_velo ),
                                        velo_next_side = ifelse(pos_side_diff < neg_side_diff,
                                                              velo_next_side, -velo_next_side)) %>% 
                                 dplyr::select(-c(unit_x, unit_y, pos_side_diff, neg_side_diff)) %>%
                                 relocate(velo_next_side, .before = velo_next_angle)
pot_tag_data <- pot_tag_data %>% mutate(velo_next_angle = atan2(velo_next_side, velo_next_towards))


pot_tag_data <- pot_tag_data %>% left_join(lineups_pivoted[,9:11], by = c("play_key", "player_id_br" = "player_id"))
pot_tag_data <- pot_tag_data %>% group_by(play_key, player_id_br, timestamp) %>% 
                                 mutate(player_code = ifelse(first(player_code) != last(player_code), NA, player_code)) %>%
                                 slice(1)

pot_tag_data <- pot_tag_data %>% left_join(lineups_pivoted[,9:11], by = c("play_key", "player_id_down" = "player_id"),
                                           suffix = c("_br", "_of"))
pot_tag_data <- pot_tag_data %>% group_by(play_key, player_id_br, timestamp) %>% 
                                 mutate(player_code_of = ifelse(first(player_code_of) != last(player_code_of), NA, player_code_of)) %>%
                                 slice(1)

pot_tag_data <- pot_tag_data %>% left_join(player_speed[,1:2], by = c("player_code_br" = "player_code"))
pot_tag_data <- pot_tag_data %>% mutate(speed_95 = ifelse(is.na(speed_95), mean(player_speed$speed_95), speed_95))

pot_tag_data <- pot_tag_data %>% left_join(throw_speed[,1:2], by = c("player_code_of" = "player_code"),
                                           suffix = c("_br", "_throw"))
pot_tag_data <- pot_tag_data %>% mutate(speed_95_throw = ifelse(is.na(speed_95_throw), mean(throw_speed$speed_95), speed_95_throw))













