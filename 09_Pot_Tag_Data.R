
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


