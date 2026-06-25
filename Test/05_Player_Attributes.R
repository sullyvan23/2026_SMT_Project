library(dplyr)
library(tidyr)

lineups_pivoted <- lineups %>% pivot_longer(cols = pitcher:on_3b,
                                            names_to = "player_id",
                                            values_to = "player_code")
lineups_pivoted <- lineups_pivoted %>% mutate(player_id = case_when(player_id == "pitcher"  ~  1,
                                                                    player_id == "catcher"  ~  2,
                                                                    player_id == "first_baseman"  ~  3,
                                                                    player_id == "second_baseman"  ~  4,
                                                                    player_id == "third_baseman"  ~  5,
                                                                    player_id == "shortstop"  ~  6,
                                                                    player_id == "left_fielder"  ~  7,
                                                                    player_id == "center_fielder"  ~  8,
                                                                    player_id == "right_fielder"  ~  9,
                                                                    player_id == "batter"  ~  10,    
                                                                    player_id == "on_1b"  ~  11,     
                                                                    player_id == "on_2b"  ~  12,     
                                                                    player_id == "on_3b"  ~  13))

lineups_players <- lineups_pivoted %>% mutate(player_team = substr(player_code, 1, 3))
lineups_players <- lineups_players %>% filter(player_team %in% c("ANI", "ARN", "PHD", "VAS"))

player_pos_speeds <- player_positions %>% left_join(lineups_players[,c(1,7,9:10)], by = c("game_string", "play_per_game", "player_id")) %>%
                                          filter(!is.na(player_code))
player_pos_speeds <- player_pos_speeds %>% group_by(game_string, play_per_game, player_id) %>%
                                           filter(sum(player_code == first(player_code), na.rm = TRUE) == n())
                                           

player_pos_speeds <- player_pos_speeds %>% mutate(x_velo = 0.68181818 * (field_x - lag(field_x)) / ((timestamp - lag(timestamp))/1000),
                                                  y_velo = 0.68181818 * (field_y - lag(field_y)) / ((timestamp - lag(timestamp))/1000),
                                                  speed = sqrt(x_velo^2 + y_velo^2))
player_pos_speeds <- player_pos_speeds %>% mutate(x_accel = 0.68181818 * (x_velo - lag(x_velo)) / ((timestamp - lag(timestamp))/1000),
                                                  y_accel = 0.68181818 * (y_velo - lag(y_velo)) / ((timestamp - lag(timestamp))/1000),
                                                  tang_accel = 0.68181818 * (speed - lag(speed)) / ((timestamp - lag(timestamp))/1000),
                                                  accel_mag = sqrt(x_accel^2 + y_accel^2))

player_pos_speeds <- player_pos_speeds %>% filter(speed < 25, accel_mag < 25)



player_speed <- player_pos_speeds %>% filter(speed >= 10) %>% group_by(player_code) %>%
                                     summarise(speed_95 = quantile(speed, probs = 0.95, na.rm = TRUE),
                                               count = n())

plot(player_speed$count, player_speed$speed_95)

player_speed <- player_speed %>% filter(count > 2500)


write.csv(player_speed, "player_speed.csv", row.names = FALSE)

################################################################################################################################################################################

ball_pos_throws <- ball_positions %>% left_join(ball_events[,1:5] %>% filter(ball_eventcode %in% c(3,8)), 
                                                  by = c("game_string", "play_per_game", "timestamp"))

ball_pos_throws <- ball_pos_throws %>% group_by(game_string, play_per_game) %>% mutate(ball_eventcode = ifelse(is.na(ball_eventcode), 999, ball_eventcode)) %>%
                                       mutate(ball_eventcode = ifelse(lag(ball_eventcode) == 3  |  lag(ball_eventcode,2) == 3, 3, ball_eventcode))

ball_pos_throws <- ball_pos_throws %>% left_join(lineups_players[,c(1,7,9:10)], by = c("game_string", "play_per_game", "player_id"))
ball_pos_throws <- ball_pos_throws %>% filter(sum(!is.na(player_code)) == 1)

ball_pos_throws <- ball_pos_throws %>% mutate(x_velo = 0.68181818 * (ball_position_x - lag(ball_position_x)) / ((timestamp - lag(timestamp))/1000),
                                              y_velo = 0.68181818 * (ball_position_y - lag(ball_position_y)) / ((timestamp - lag(timestamp))/1000),
                                              z_velo = 0.68181818 * (ball_position_z - lag(ball_position_z)) / ((timestamp - lag(timestamp))/1000),
                                              speed = sqrt(x_velo^2 + y_velo^2 + z_velo^2))

ball_pos_throws <- ball_pos_throws %>% mutate(player_id = first(player_id),
                                              player_code = first(player_code)) %>% slice(2)
ball_pos_throws <- ball_pos_throws %>% filter(speed >= 80, speed <= 106)


throw_speed <- ball_pos_throws %>% group_by(player_code) %>%
                                   summarise(speed_95 = quantile(speed, probs = 0.95, na.rm = TRUE),
                                             count = n())

throw_speed <- throw_speed %>% filter(count >= 3)

write.csv(throw_speed, "throw_speed.csv", row.names = FALSE)
