ball_pos_fly_ball_animate <- ball_pos_fly_ball %>% mutate(play_key = paste0(game_string, play_per_game))

ex_ball_flight <- ball_pos_fly_ball_animate[,c(14,3,5:7)] %>% filter(play_key == "y1_d143_ADQ_ANI17")

ball_pos_fly_ball_ground <- ball_pos_fly_ball_2.1
names(ball_pos_fly_ball_ground) <- names(ex_ball_flight)

ex_ball_flight <- bind_rows(ex_ball_flight, ball_pos_fly_ball_ground %>% filter(play_key == "y1_d143_ADQ_ANI17"))
ex_ball_flight <- ex_ball_flight %>% arrange(timestamp)
ex_ball_flight <- ex_ball_flight %>% filter(timestamp <= max(ifelse(timestamp%%1 != 0, timestamp, 0)) ) 

write.csv(ex_ball_flight, "ex_ball_flight.csv", row.names = FALSE)

##########################################################################################################################################################################

test_mesh <-  expand.grid(ball_position_x = seq(0, 10, by = 1), ball_position_y = seq(0, 10, by = 1), ball_position_z = 0)
write.csv(test_mesh, "test_mesh.csv", row.names = FALSE)

ANI_field_mesh <- expand.grid(ball_position_x = seq(-300, 300, by = 1), ball_position_y = seq(-10, 450, by = 1)) %>%
                  filter(abs(ball_position_x) <= (ball_position_y + 10))

ANI_field_mesh <- ANI_field_mesh %>% mutate(home_dist = sqrt(ball_position_x^2 + ball_position_y^2),
                                            spray_angle = atan(ball_position_x/ball_position_y))

ANI_field_mesh <- ANI_field_mesh %>% mutate(wall_dist = predict(wall_ANI_model, newdata = ANI_field_mesh))
ANI_field_mesh <- ANI_field_mesh %>% mutate(wall_dist = ifelse(wall_dist < 280  |  is.na(wall_dist), 280, wall_dist))

ANI_field_mesh <- ANI_field_mesh %>% filter(home_dist <= wall_dist)

ANI_field_mesh <- ANI_field_mesh %>% mutate(ball_position_z = predict(ground_ANI_model, newdata = ANI_field_mesh))
ANI_field_mesh <- ANI_field_mesh %>% dplyr::select(ball_position_x, ball_position_y, ball_position_z)

write.csv(ANI_field_mesh, "ANI_field_mesh.csv", row.names = FALSE)

##########################################################################################################################################################################

ANI_foul_lines <- expand.grid(ball_position_x = seq(-300, 300, by = 1), ball_position_y = seq(0, 300, by = 1)) %>%
                  filter(abs(ball_position_x) == ball_position_y)

ANI_foul_lines_2 <- ANI_foul_lines %>% mutate(ball_position_y = ball_position_y + 0.4714)
ANI_foul_lines <- bind_rows(ANI_foul_lines, ANI_foul_lines_2)

ANI_foul_lines <- ANI_foul_lines %>% mutate(home_dist = sqrt(ball_position_x^2 + ball_position_y^2),
                                            spray_angle = atan(ball_position_x/ball_position_y))

ANI_foul_lines <- ANI_foul_lines %>% mutate(wall_dist = predict(wall_ANI_model, newdata = ANI_foul_lines))
ANI_foul_lines <- ANI_foul_lines %>% mutate(wall_dist = ifelse(is.na(wall_dist), 280, wall_dist))

ANI_foul_lines <- ANI_foul_lines %>% filter(home_dist <= wall_dist)

ANI_foul_lines <- ANI_foul_lines %>% mutate(ball_position_z = 0.01 + predict(ground_ANI_model, newdata = ANI_foul_lines))
ANI_foul_lines <- ANI_foul_lines %>% dplyr::select(ball_position_x, ball_position_y, ball_position_z)

write.csv(ANI_foul_lines, "ANI_foul_lines.csv", row.names = FALSE)

##########################################################################################################################################################################

plot(ANI_field_mesh$ball_position_x, ANI_field_mesh$ball_position_y)

ggplot(ANI_field_mesh, aes(x = ball_position_x, y = ball_position_y, color = ball_position_z)) + 
       geom_point() + scale_color_gradient2(high = "green", low = "red", mid = "white")

plot(ANI_foul_lines$ball_position_x, ANI_foul_lines$ball_position_y)
