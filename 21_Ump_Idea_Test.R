
ball_5ft_2nd_base <- ball_positions %>% filter( sqrt(ball_position_x^2 + (ball_position_y - 126.3953)^2) <= 5  ,  !is.na(game_string) )
ball_5ft_2nd_base <- ball_5ft_2nd_base %>% group_by(game_string, play_per_game) %>% mutate(y_pos_change = ball_position_y - lag(ball_position_y))
ball_5ft_2nd_base <- ball_5ft_2nd_base %>% filter(sum(y_pos_change < 0, na.rm = TRUE) > -1)

runner_5ft_2nd_base <- player_positions %>% filter(player_id %in% c(10:11), field_x >= 0, field_x <= 5, field_y > 100)

play_at_2nd <- ball_5ft_2nd_base[,1:6] %>% left_join(runner_5ft_2nd_base[,1:6], by = c("game_string", "play_per_game", "timestamp"))
play_at_2nd <- play_at_2nd %>% filter(!is.na(player_id))
play_at_2nd <- play_at_2nd %>% mutate(runner_2nd_dist = sqrt(field_x^2 + (field_y - 126.3953)^2))
play_at_2nd <- play_at_2nd %>% group_by(game_string, play_per_game, player_id) %>% filter(runner_2nd_dist == min(runner_2nd_dist))

play_at_2nd_ump <- play_at_2nd %>% left_join(player_positions[,1:6] %>% filter(player_id == 16), 
                                             by = c("game_string", "play_per_game", "timestamp"), suffix = c("", "_ump"))
play_at_2nd_ump <- play_at_2nd_ump %>% filter(!is.na(player_id_ump))

plot(play_at_2nd_ump$field_x_ump, play_at_2nd_ump$field_y_ump)

################################################################################################################################################################################################

ball_5ft_2nd_base <- ball_positions %>% filter( sqrt(ball_position_x^2 + (ball_position_y - 126.3953)^2) <= 5  ,  !is.na(game_string) )
ball_5ft_2nd_base <- ball_5ft_2nd_base %>% group_by(game_string, play_per_game) %>% mutate(y_pos_change = ball_position_y - lag(ball_position_y))
ball_5ft_2nd_base <- ball_5ft_2nd_base %>% filter(sum(y_pos_change < 0, na.rm = TRUE) > -1)

runner_5ft_2nd_base <- player_positions %>% group_by(game_string, play_per_game) %>% 
                                            filter(sum(player_id %in% c(11:13)) == 0, player_id == 10, field_x >= 0, field_x <= 5, field_y > 100) %>% ungroup()

play_at_2nd <- ball_5ft_2nd_base[,1:6] %>% left_join(runner_5ft_2nd_base[,1:6], by = c("game_string", "play_per_game", "timestamp"))
play_at_2nd <- play_at_2nd %>% filter(!is.na(player_id))
play_at_2nd <- play_at_2nd %>% mutate(runner_2nd_dist = sqrt(field_x^2 + (field_y - 126.3953)^2))
play_at_2nd <- play_at_2nd %>% group_by(game_string, play_per_game, player_id) %>% filter(runner_2nd_dist == min(runner_2nd_dist))

play_at_2nd_ump <- play_at_2nd %>% left_join(player_positions[,1:6] %>% filter(player_id %in% c(15:17)), 
                                             by = c("game_string", "play_per_game", "timestamp"), suffix = c("", "_ump"))
play_at_2nd_ump <- play_at_2nd_ump %>% filter(!is.na(player_id_ump))

plot(play_at_2nd_ump$field_x_ump, play_at_2nd_ump$field_y_ump)


