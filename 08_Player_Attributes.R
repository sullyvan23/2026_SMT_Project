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

player_pos_speeds <- player_positions_2 %>% left_join(lineups_pivoted[,9:11], by = c("play_key", "player_id"))
player_pos_speeds <- player_pos_speeds %>% arrange(play_key, player_id, timestamp) %>% 
                                           group_by(play_key, player_id) %>%
                                           filter(sum(player_code == first(player_code), na.rm = TRUE) == n())
                                           

player_pos_speeds <- player_pos_speeds %>% mutate(x_velo = 0.68181818 * (field_x - lag(field_x)) / ((timestamp - lag(timestamp))/1000),
                                                  y_velo = 0.68181818 * (field_y - lag(field_y)) / ((timestamp - lag(timestamp))/1000),
                                                  speed = sqrt(x_velo^2 + y_velo^2))
player_pos_speeds <- player_pos_speeds %>% mutate(x_accel = 0.68181818 * (x_velo - lag(x_velo)) / ((timestamp - lag(timestamp))/1000),
                                                  y_accel = 0.68181818 * (y_velo - lag(y_velo)) / ((timestamp - lag(timestamp))/1000),
                                                  tang_accel = 0.68181818 * (speed - lag(speed)) / ((timestamp - lag(timestamp))/1000),
                                                  accel_mag = sqrt(x_accel^2 + y_accel^2))







