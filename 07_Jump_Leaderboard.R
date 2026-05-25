
lineups <- lineups %>% mutate(play_key = paste0(game_string, play_per_game))

jump_players <- jump_catch_prob_pred[,c(1:2,45:47)] %>% left_join(lineups[,c(22,3:5,15:17)], by = "play_key")
### some errors in lineup with play_per_game same on both sides of half inning
jump_players <- jump_players %>% group_by(play_key, player_id) %>% filter((sum(half_inning == "Top")/n()) == 0   |   (sum(half_inning == "Top")/n()) == 1 ) %>% 
                                 ungroup()

jump_players <- jump_players %>% mutate(field_team = ifelse(half_inning == "Top", home_team, away_team)) %>% dplyr::select(-c(half_inning:away_team))
jump_players <- jump_players %>% filter(field_team %in% c("ANI", "ARN", "PHD", "VAS"))
jump_players <- jump_players %>% mutate(OF = case_when(
                                             player_id == 7 ~ left_fielder,
                                             player_id == 8 ~ center_fielder,
                                             player_id == 9 ~ right_fielder))

jump_leaderboard <- jump_players %>% group_by(OF) %>% summarise(fly_balls = n(),
                                                                avg_initial_catch_prob = mean(catch_prob_initial),
                                                                avg_post_jump_catch_prob = mean(catch_prob),
                                                                avg_jump_increase = mean(catch_prob_increase))

plot(jump_leaderboard$fly_balls, jump_leaderboard$avg_jump_increase)



plot(jump_catch_prob_pred$catch_prob_initial, jump_catch_prob_pred$catch_prob_increase)

jump_players_10_90 <- jump_players %>% filter(catch_prob_initial >= 0.1   &   catch_prob_initial <= 0.9)

jump_leaderboard_10_90 <- jump_players_10_90 %>% group_by(OF) %>% summarise(fly_balls = n(),
                                                                            avg_initial_catch_prob = mean(catch_prob_initial),
                                                                            avg_post_jump_catch_prob = mean(catch_prob),
                                                                            avg_jump_increase = mean(catch_prob_increase))
plot(jump_leaderboard_10_90$fly_balls, jump_leaderboard_10_90$avg_jump_increase)


