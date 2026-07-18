
if(!require("showtext")) {install.packages("showtext")}; library(showtext)
library(showtext)
font_add_google("Press Start 2P", "Press_Start_2P")
showtext_auto()

library(ggimage)


animate_positions <- model_play %>% select(game_string, play_per_game, timestamp, caught_prob, player_id_br, basepath)
animate_positions <- animate_positions %>% rename(player_id = player_id_br) %>%
                                           mutate(player_id = player_id + 0.5,
                                                  field_x = predict(basepath_x_model, newdata = animate_positions),
                                                  field_y = predict(basepath_y_model, newdata = animate_positions)) %>%
                                           select(-basepath)
animate_positions <- bind_rows(animate_positions,
                               player_positions %>% filter(game_string == model_play$game_string[1],
                                                           play_per_game == model_play$play_per_game[1])) %>% 
                      arrange(timestamp, player_id)
animate_positions <- animate_positions %>% group_by(timestamp) %>%
                                           mutate(caught_prob = ifelse(!is.na(caught_prob), 
                                                                       paste0(as.character( pmax(pmin(5*round(caught_prob*20), 95), 5) ),
                                                                                           "%"),
                                                                       "")) %>% 
                                            ungroup()


animate_model()

anim_save("example.gif", animation = last_animation())

###########################################################################################################################################################################################
### DON'T RE-ENTER
### doing new tests with this animation function and if you use this version instead of the one in the Rdata it won't work

animate_model <- function() {

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
  
  # Find the time of the pitch
  time_of_pitch <-  ball_events %>%
    filter(game_string == animate_positions$game_string[1] &
             play_per_game == animate_positions$play_per_game[1] &
             ball_eventcode == 1) %>%
    collect() %>%
    pull(timestamp)
  
  # Get the Ball Tracking Data
  ball_tracking_data <- ball_positions %>%
    ## Filter to correct game
    filter(game_string == animate_positions$game_string[1] &
             play_per_game == animate_positions$play_per_game[1]) %>%
    ## Collect from Arrow
    collect() %>%
    ## Add on a type and player_id column to match with player_tracking_data
    mutate(type = "ball", 
           player_id = NA) %>% 
    ## Reorder and Rename Columns
    dplyr::select(game_string:timestamp, player_id, type, position_x = ball_position_x,
           position_y = ball_position_y, position_z = ball_position_z, everything())
  
  # Get the Player Tracking Data
  player_tracking_data <- animate_positions %>%
    collect() %>%
    ## Convert player_id to numeric
    mutate(player_id = as.numeric(player_id)) %>%
    ## Calculate type and put position_z as NA
    mutate(type = case_when((player_id %% 1) == 0.5 ~  "computer_runner",
                            (player_id+0.5) %in% animate_positions$player_id  ~  "actual_runner",
                            player_id <= 9 ~ "defense",
                            between(player_id, 10, 13) ~ "offense",
                            between(player_id, 14, 17) ~ "umpire",
                            player_id %in% c(18, 19) ~ "coach"),
           position_z = NA
    ) %>%
    ## Reorder and Rename Columns
    dplyr::select(game_string:timestamp, player_id, type, position_x = field_x,
           position_y = field_y, position_z, everything())
  
  # Combine all tracking data into 1 data frame
  tracking_data <- bind_rows(player_tracking_data, ball_tracking_data) %>% 
    ## Convert timestamps and positions to numeric
    mutate(across(c(timestamp, position_x, position_y, position_z), 
                  as.numeric)) %>%
    ## Order data chronologically
    arrange(timestamp) %>%
    ## Align timestamps to account for mechanical measurement error
    mutate(timestamp_adj = plyr::round_any(timestamp, fps)) %>%
    ## Start the animation to start when the pitch is thrown
    filter(timestamp >= time_of_pitch) %>%
    ## Create a frame_id for animation
    mutate(frame_id = match(timestamp_adj, unique(timestamp_adj)))
  
  # Make Field and Plot Points
  if(animate_positions$home_team[1] == "ANI") {
     p <- ANI_background()
  } else if(animate_positions$home_team[1] == "ARN") {
    p <- ARN_background()
  } else if(animate_positions$home_team[1] == "PHD") {
    p <- PHD_background()
  } else if(animate_positions$home_team[1] == "VAS") {
    p <- VAS_background()
  }

  p <- p +
    ## plotting defenders
    geom_image(data = tracking_data %>% filter(type == "defense"  &  ((player_id + rand_num) %% 3) == 0),
              aes(x = position_x, y = position_y, image = "fielder_1.png"),
              size = 0.04) +
    geom_image(data = tracking_data %>% filter(type == "defense"  &  ((player_id + rand_num) %% 3) == 1),
              aes(x = position_x, y = position_y, image = "fielder_2.png"),
              size = 0.04) +
    geom_image(data = tracking_data %>% filter(type == "defense"  &  ((player_id + rand_num) %% 3) == 2),
              aes(x = position_x, y = position_y, image = "fielder_3.png"),
              size = 0.04) +
    ### plotting actual runner
    geom_image(data = tracking_data %>% filter(type == "actual_runner"),
               aes(x = position_x, y = position_y, image = "actual_runner.png"),
               size = 0.05, alpha = 0.8) +
    ### plotting computer runner
    geom_image(data = tracking_data %>% filter(type == "computer_runner"),
               aes(x = position_x, y = position_y, image = "computer_runner.png"),
               size = 0.05, alpha = 0.8) +
    ## Plot the ball
    geom_point(data = tracking_data %>%
                 filter(type == "ball"),
               aes(x = position_x, y = position_y,
                   size = position_z),
               fill = "white",
               shape = 21,
               show.legend = F) +
    ## show catch probability
    geom_text(data = tracking_data %>% filter(caught_prob != ""),
               aes(x = -150, y = 10,
                   label = paste0("Catch Prob: ", caught_prob)),
               color = "white", size = 3, show.legend = F,
               family = "Press_Start_2P") +
    ## Specify colors for people
    scale_fill_manual(values = c("offense" = "#005AB5",
                                 "defense" = "#FEFE62",
                                 "coach" = "#1A85FF",
                                 "umpire" = "black")) +
    ## Specify when to transition
    transition_time(frame_id) +
    ## Annotate with the Play and Game ID
    annotate("text", x = c(150, 0), y = c(10, 430), color = "white",
             label = c(paste("Play:", animate_positions$play_per_game[1]), 
                       paste("Game :", animate_positions$game_string[1])),
             family = "Press_Start_2P")
  
  # Find the number of frames
  number_of_frames <-  max(tracking_data$frame_id)
  
  # Animate
  p2 <- animate(p, fps = fps, nframes = number_of_frames)
  
  return(p2)
}
###########################################################################################################################################################################################
