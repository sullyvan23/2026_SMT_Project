sum(is.na(ball_events$game_string))
### 2253
sum(is.na(ball_positions$game_string))
### 17236
sum(is.na(player_positions$game_string))
### 704562

sum(is.na(ball_events[,6:9]))
### 0
sum(is.na(ball_positions[,7:10]))
### 0
sum(is.na(player_positions[,7:10]))
### 0

ball_events <- ball_events %>% mutate(game_string = ifelse(is.na(game_string), 
                                                           paste0(year, "_", day, "_", away_team, "_", home_team), 
                                                           game_string))
ball_positions <- ball_positions %>% mutate(game_string = ifelse(is.na(game_string), 
                                                                 paste0(year, "_", day, "_", away_team, "_", home_team), 
                                                                 game_string))
player_positions <- player_positions %>% mutate(game_string = ifelse(is.na(game_string), 
                                                                     paste0(year, "_", day, "_", away_team, "_", home_team), 
                                                                     game_string))

sum(is.na(ball_events$game_string))
### 0
sum(is.na(ball_positions$game_string))
### 0
sum(is.na(player_positions$game_string))
### 0



sum(is.na(ball_events$play_per_game))
### 2253
sum(is.na(ball_positions$play_per_game))
### 17236
sum(is.na(player_positions$play_per_game))
### 704562


sum(is.na(ball_events$timestamp))
### 2253
sum(is.na(ball_positions$timestamp))
### 17236
sum(is.na(player_positions$timestamp))
### 704562
