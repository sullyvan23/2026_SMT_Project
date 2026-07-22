
angle <- ifelse(model_play$player_id_br[1] == 12, -pi/4, pi/4)

new_animate_positions <- animate_positions %>% mutate(new_field_x = (field_x * cos(angle)) - (field_y * sin(angle)),
                                                      new_field_y = (field_x * sin(angle)) + (field_y * cos(angle)),
                                                      field_x = new_field_x, field_y = new_field_y) %>%
                                               select(-c(new_field_x:new_field_y))

if(model_play$player_id_br[1] == 11) {
  new_animate_positions <- new_animate_positions %>% filter(between(field_x, -105.3, 15.3), between(field_y, 61, 115.9))
} else if(model_play$player_id_br[1] == 12) {
  new_animate_positions <- new_animate_positions %>% filter(between(field_x, -15.3, 105.3), between(field_y, 61, 115.9))
} else {
  new_animate_positions <- new_animate_positions %>% filter(between(field_x, -105.3, 15.3), between(field_y, -25.9, 29))
}


animate_basepath()

anim_save("example.gif", animation = last_animation())

############################################################################################################################################################################################

animate_basepath <- function() {

  ### random number for randomized fielder images
  rand_num <- sample(0:2, 1)
  
  # Set the specs for the gif we want to create (lower res to make it run quicker)
  options(gganimate.dev_args = list(width = 3, height = 3, units = 'in', res = 120))
  
  #' #ometimes the frames per second at different stadiums can vary (30 fps vs 50 fps)
  #' this finds an even rounding interval and calculates fps from the data explicitly
  fps <- animate_positions %>%
    # Double check columns are numeric
    mutate(across(c(timestamp, field_x, field_y, player_id),
                  as.numeric)) %>%
    # Filter for only Players
    filter(player_id < 14) %>%
    # Calculate Frames Per Second by player's position
    mutate(fps = timestamp - lag(timestamp), 
           .by = "player_id")  %>%
    # Calculate Frames Per Second and save as a vector
    count(fps) %>% slice_max(n) %>% pull(fps)
  fps <- fps
  
  # Find the time of the pitch
  time_of_pitch <- ball_events %>%
    filter(game_string == new_animate_positions$game_string[1] &
             play_per_game == new_animate_positions$play_per_game[1] &
             ball_eventcode == 1) %>%
    collect() %>%
    pull(timestamp)
  
  # Get the Tracking Data
  tracking_data <- new_animate_positions %>%
    collect() %>%
    ## Convert player_id to numeric
    mutate(player_id = as.numeric(player_id)) %>%
    ## Calculate type
    mutate(type = case_when(is.na(player_id)  ~  "ball",
                            (player_id%%1) == 0.5 ~  "computer_runner",
                            between(player_id, 10, 13)  ~  "actual_runner",
                            player_id <= 9 ~ "defense",
                            between(player_id, 14, 17) ~ "umpire",
                            player_id %in% c(18, 19) ~ "coach")) %>%
    ## Reorder and Rename Columns
    dplyr::select(game_string:caught_prob, player_id, type, field_x, field_y, field_z = ball_position_z, everything())
  
  # Combine all tracking data into 1 data frame
  tracking_data <- tracking_data %>% 
    ## Convert timestamps and positions to numeric
    mutate(across(c(timestamp, field_x, field_y, field_z), 
                  as.numeric)) %>%
    ## Order data chronologically
    arrange(timestamp) %>%
    ## Align timestamps to account for mechanical measurement error
    mutate(timestamp_adj = plyr::round_any(timestamp, fps)) %>%
    ## Start the animation to start when the pitch is thrown
    filter(timestamp >= time_of_pitch) %>%
    ## Create a frame_id for animation
    mutate(frame_id = match(timestamp_adj, unique(timestamp_adj)))

  tracking_data <- tracking_data %>% mutate(image = ifelse(type == "defense",
                                                           case_when(((player_id + rand_num) %% 3) == 0  ~  "fielder_1.png",
                                                                     ((player_id + rand_num) %% 3) == 1  ~  "fielder_2.png",
                                                                     ((player_id + rand_num) %% 3) == 2  ~  "fielder_3.png"),
                                                           NA))

  # Make Field and Plot Points
  if(model_play$player_id_br[1] == 11) {
     p <- first_base_line_plot()
  } else if(model_play$player_id_br[1] == 12) {
     p <- second_base_line_plot()
  } else if(model_play$player_id_br[1] == 13) {
     p <- third_base_line_plot()
  }
  
  p <- p +
    ### plotting defenders
    geom_image(data = tracking_data %>% filter(type == "defense"),
              aes(x = field_x, y = field_y, image = image),
              size = 0.15) +
    ### plotting actual runner
    geom_image(data = tracking_data %>% filter(type == "actual_runner"),
               aes(x = field_x, y = field_y, image = "actual_runner.png"),
               size = 0.15, alpha = 0.8) +
    ### plotting computer runner
    geom_image(data = tracking_data %>% filter(type == "computer_runner"),
               aes(x = field_x, y = field_y, image = "computer_runner.png"),
               size = 0.15, alpha = 0.8) +
    ## Plot the ball
    geom_point(data = tracking_data %>%
                 filter(type == "ball"),
               aes(x = field_x, y = field_y,
                   size = field_z),
               fill = "white",
               shape = 21,
               show.legend = F) +
    ## Specify colors for people
    scale_fill_manual(values = c("coach" = "#1A85FF",
                                 "umpire" = "black")) +
    ## Specify when to transition
    transition_time(frame_id)
  
  # Find the number of frames
  number_of_frames <-  max(tracking_data$frame_id)
  
  # Animate
  p2 <- animate(p, fps = fps/2, nframes = number_of_frames)
  
  return(p2)
}











