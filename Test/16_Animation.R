### leading packages needed
if(!require("showtext")) {install.packages("showtext")}; library(showtext)
library(showtext)
font_add_google("Oswald", "Oswald")
showtext_auto()

library(ggimage)

if(!require("arrow")) {install.packages("arrow")}; library(arrow)
if(!require("tidyverse")) {install.packages("tidyverse")}; library(tidyverse)
if(!require("sportyR")) {install.packages("sportyR")}; library(sportyR)
if(!require("gganimate")) {install.packages("gganimate")}; library(gganimate)


### making motion of runner after ball lands/caught
after_model <- bind_rows(model_play[nrow(model_play),],
                         player_positions %>% filter(game_string == model_play$game_string[1],
                                                     play_per_game == model_play$play_per_game[1],
                                                     player_id == model_play$player_id[1],
                                                     timestamp > max(model_play$timestamp)) )

### go_back data for balls caught and not by base to tag, go forward otherwie
after_model <- go_back_final(after_model)
after_model <- after_model %>% mutate(player_id_br = first(player_id_br),
                                      basepath = og_basepath_dist + player_id_br - 10)

### joining together with model positions
animate_positions <- bind_rows(model_play, after_model[2:nrow(after_model),]) %>% 
                     select(game_string, play_per_game, timestamp, caught_prob, player_id_br, basepath)

### converting model runner basepaths to coordinates
animate_positions <- animate_positions %>% rename(player_id = player_id_br) %>%
                                           mutate(player_id = player_id + 0.5,
                                                  field_x = predict(basepath_x_model, newdata = animate_positions),
                                                  field_y = predict(basepath_y_model, newdata = animate_positions)) %>%
                                           select(-basepath)

### getting other player data and ball data
animate_positions <- bind_rows(animate_positions,
                               player_positions %>% filter(game_string == animate_positions$game_string[1],
                                                           play_per_game == animate_positions$play_per_game[1]),
                               ball_positions %>% filter(game_string == animate_positions$game_string[1],
                                                         play_per_game == animate_positions$play_per_game[1]) %>%
                                                  rename(field_x = ball_position_x,
                                                         field_y = ball_position_y))

### arranging by timestamp and adding catch probability to be used in animation
animate_positions <- animate_positions %>% group_by(timestamp) %>%
                                           mutate(caught_prob = ifelse(!is.na(caught_prob), 
                                                                       paste0(as.character( pmax(pmin(5*round(caught_prob*20), 95), 5) ),
                                                                                           "%"),
                                                                       ""),
                                                  home_team = substr(game_string, nchar(game_string)-2, nchar(game_string))) %>% 
                                            ungroup()
animate_positions <- animate_positions %>% arrange(timestamp)


### random number for randomized fielder images
rand_num <- sample(0:2, 1)


### animate model function
animate_model()


### save animations
anim_save("example.gif", animation = last_animation())

###########################################################################################################################################################################################

### animation function of whole field, largely based on the given animation function by SMT
animate_model <- function() {  
  
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
  time_of_pitch <- ball_events %>%
    filter(game_string == animate_positions$game_string[1] &
             play_per_game == animate_positions$play_per_game[1] &
             ball_eventcode == 1) %>%
    collect() %>%
    pull(timestamp)
  
  # Get the Tracking Data
  tracking_data <- animate_positions %>%
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
    ### plotting defenders
    geom_image(data = tracking_data %>% filter(type == "defense"),
              aes(x = field_x, y = field_y, image = image),
              size = 0.05) +
    ### plotting actual runner
    geom_image(data = tracking_data %>% filter(type == "actual_runner"),
               aes(x = field_x, y = field_y, image = "actual_runner.png"),
               size = 0.05, alpha = 0.8) +
    ### plotting computer runner
    geom_image(data = tracking_data %>% filter(type == "computer_runner"),
               aes(x = field_x, y = field_y, image = "computer_runner.png"),
               size = 0.05, alpha = 0.8) +
    ## Plot the ball
    geom_point(data = tracking_data %>%
                 filter(type == "ball"),
               aes(x = field_x, y = field_y,
                   size = field_z),
               fill = "white",
               shape = 21,
               show.legend = F) +
    ## show catch probability
    geom_text(data = tracking_data %>% filter(caught_prob != ""),
               aes(x = -150, y = 10,
                   label = paste("Catch Prob:", caught_prob)),
               color = "white", size = 5, show.legend = F,
               family = "Oswald", fontface = "bold") +
    ## Specify colors for people
    scale_fill_manual(values = c("coach" = "#1A85FF",
                                 "umpire" = "black")) +
    ## Specify when to transition
    transition_time(frame_id) +
    ## Annotate with the Play and Game ID
    annotate("text", x = c(150, 0), y = c(10, 430), color = "white",
             label = c(paste("Play:", animate_positions$play_per_game[1]), 
                       paste("Game :", animate_positions$game_string[1])),
             family = "Oswald", fontface = "bold", size = 5)
  
  # Find the number of frames
  number_of_frames <-  max(tracking_data$frame_id)
  
  # Animate
  p2 <- animate(p, fps = fps/2, nframes = number_of_frames)
  
  return(p2)
}
###########################################################################################################################################################################################


### go back function similar to one used for creating modeled basepath, but returns dataset
go_back_final <- function(input_data) {
  for(i in 2:nrow(input_data)) {
    ### close to minimum possible next acceleration
    input_data$runner_basepath_accel[i] <- round(input_data$runner_basepath_accel[i-1] + ((1-input_data$ellipse[i-1]^1) * (input_data$runner_basepath_accel[i-1] - input_data$runner_basepath_accel_2[i-1])) +
                                                  input_data$ellipse[i-1]^1 * ifelse(input_data$og_basepath_dist[i-1] <= 0.2  &  input_data$runner_basepath_velo[i-1] <= 0,
                                                                                     -(input_data$runner_basepath_velo[i-1]/20) - ((-0.2+input_data$og_basepath_dist[i-1])/20),
                                                                                     -(input_data$runner_basepath_velo[i-1]/80)) -
                                                  (ifelse(input_data$runner_basepath_velo[i-1] > 0, 0.0075, 0.005) * (fps/0.05)), 
                                                  3)

    input_data$runner_basepath_accel[i] <- ifelse(input_data$og_basepath_dist[i-1] == 0, 0, input_data$runner_basepath_accel[i])

    ### having velocity and position match acceleration
    input_data$runner_basepath_velo[i] <- input_data$runner_basepath_velo[i-1] + (input_data$runner_basepath_accel[i]*fps)
    input_data$og_basepath_dist[i] <- input_data$og_basepath_dist[i-1] + (input_data$runner_basepath_velo[i]*fps)
    if(input_data$og_basepath_dist[i] <= 0) {
      input_data$runner_basepath_velo[i] <- 0
      input_data$og_basepath_dist[i] <- 0
    }
    input_data$runner_basepath_accel_2[i] <- input_data$runner_basepath_accel[i-1]
    input_data$ellipse[i] <- (input_data$runner_basepath_velo[i]^2 / max_speed^2) +
                             ifelse(input_data$og_basepath_dist[i] <= 0.1  &  input_data$runner_basepath_velo[i] <= 0,
                                      max( ((input_data$og_basepath_dist[i]-0.1)^2 / 0.1^2), (input_data$runner_basepath_accel[i]^2 / max_accel^2) ),
                                            (input_data$runner_basepath_accel[i]^2 / max_accel^2))

    ### correcting for if it goes outside of the ellipse (mainly for going back and getting back towards a velocity of 0)
    while(input_data$ellipse[i] > 1) {
        if(input_data$og_basepath_dist[i] <= 0.1  &  input_data$runner_basepath_velo[i] < 0) {
          input_data$runner_basepath_velo[i] <- input_data$runner_basepath_velo[i] - (sign(input_data$runner_basepath_velo[i]) * 0.0005)
          if(input_data$og_basepath_dist[i] <= 0) {
            input_data$runner_basepath_velo[i] <- 0
            input_data$og_basepath_dist[i] <- 0
          }
        
        } else {
          if((input_data$runner_basepath_velo[i]^2 / max_speed^2) <= (input_data$runner_basepath_accel[i]^2 / max_accel^2)) {
            input_data$runner_basepath_accel[i] <- input_data$runner_basepath_accel[i] - (sign(input_data$runner_basepath_accel[i]) * 0.0005)
          } else {
            input_data$runner_basepath_velo[i] <- input_data$runner_basepath_velo[i] - (sign(input_data$runner_basepath_velo[i]) * 0.0005)
          }
          if(input_data$og_basepath_dist[i] <= 0) {
            input_data$runner_basepath_velo[i] <- 0
            input_data$og_basepath_dist[i] <- 0
          }  
        
      }
      
      input_data$ellipse[i] <- (input_data$runner_basepath_velo[i]^2 / max_speed^2) +
                               ifelse(input_data$og_basepath_dist[i] <= 0.1  &  input_data$runner_basepath_velo[i] <= 0,
                                        max( ((input_data$og_basepath_dist[i]-0.1)^2 / 0.1^2), (input_data$runner_basepath_accel[i]^2 / max_accel^2) ),
                                              (input_data$runner_basepath_accel[i]^2 / max_accel^2))
    }
    
  }

  ### returning final
  return(input_data)
}



### go forward function similar to one used for creating modeled basepath, but returns dataset
go_forward_final <- function(input_data) {
  for(i in 2:nrow(input_data)) {
    ### close to minimum possible next acceleration
    input_data$runner_basepath_accel[i] <- round(input_data$runner_basepath_accel[i-1] + ((1-input_data$ellipse[i-1]^1) * (input_data$runner_basepath_accel[i-1] - input_data$runner_basepath_accel_2[i-1])) +
                                                  input_data$ellipse[i-1]^1 * ifelse(input_data$og_basepath_dist[i-1] <= 0.2  &  input_data$runner_basepath_velo[i-1] <= 0,
                                                                                     -(input_data$runner_basepath_velo[i-1]/20) - ((-0.2+input_data$og_basepath_dist[i-1])/20),
                                                                                     -(input_data$runner_basepath_velo[i-1]/80)) +
                                                  (ifelse(input_data$runner_basepath_velo[i-1] > 0, 0.0075, 0.005) * (fps/0.05)), 
                                                  3)

    ### having velocity and position match acceleration
    input_data$runner_basepath_velo[i] <- input_data$runner_basepath_velo[i-1] + (input_data$runner_basepath_accel[i]*fps)
    input_data$og_basepath_dist[i] <- input_data$og_basepath_dist[i-1] + (input_data$runner_basepath_velo[i]*fps)
    input_data$runner_basepath_accel_2[i] <- input_data$runner_basepath_accel[i-1]
    input_data$ellipse[i] <- (input_data$runner_basepath_velo[i]^2 / max_speed^2) +
                                 ifelse(input_data$og_basepath_dist[i] <= 0.2  &  input_data$runner_basepath_velo[i] <= 0,
                                          max( ((input_data$og_basepath_dist[i]-0.224)^2 / 0.2^2), (input_data$runner_basepath_accel[i]^2 / max_accel^2) ),
                                                (input_data$runner_basepath_accel[i]^2 / max_accel^2))

    ### correcting for if it goes outside of the ellipse (mainly for going back and getting back towards a velocity of 0)
    if(input_data$og_basepath_dist[i] >= (-input_data$player_id_br[i] + 14)) {
      input_data$og_basepath_dist[i] <- (-input_data$player_id_br[i] + 14)
      input_data$runner_basepath_velo[i] <- 0
    } else {
      while(input_data$ellipse[i] > 1) {
        if(input_data$og_basepath_dist[i] <= 0.2  &  input_data$runner_basepath_velo[i] < 0) {
          input_data$runner_basepath_velo[i] <- input_data$runner_basepath_velo[i] - (sign(input_data$runner_basepath_velo[i]) * 0.0005)
          if(input_data$og_basepath_dist[i] <= 0.025) {
            input_data$runner_basepath_velo[i] <- 0
            input_data$og_basepath_dist[i] <- 0.025
          }
        
        } else {
          if((input_data$runner_basepath_velo[i]^2 / max_speed^2) <= (input_data$runner_basepath_accel[i]^2 / max_accel^2)) {
            input_data$runner_basepath_accel[i] <- input_data$runner_basepath_accel[i] - (sign(input_data$runner_basepath_accel[i]) * 0.0005)
          } else {
            input_data$runner_basepath_accel[i] <- input_data$runner_basepath_accel[i] - (sign(input_data$runner_basepath_velo[i]) * 0.0005)
          }
          input_data$runner_basepath_velo[i] <- input_data$runner_basepath_velo[i-1] + (input_data$runner_basepath_accel[i]*fps)
          input_data$og_basepath_dist[i] <- input_data$og_basepath_dist[i-1] + (input_data$runner_basepath_velo[i]*fps)
          if(input_data$og_basepath_dist[i] <= 0.025) {
            input_data$runner_basepath_velo[i] <- 0
            input_data$og_basepath_dist[i] <- 0.025
          }  
        
        }  
      
        input_data$ellipse[i] <- (input_data$runner_basepath_velo[i]^2 / max_speed^2) +
                                 ifelse(input_data$og_basepath_dist[i] <= 0.2  &  input_data$runner_basepath_velo[i] <= 0,
                                          max( ((input_data$og_basepath_dist[i]-0.224)^2 / 0.2^2), (input_data$runner_basepath_accel[i]^2 / max_accel^2) ),
                                                (input_data$runner_basepath_accel[i]^2 / max_accel^2))
      }
    }
    
  }

  ### returning final
  return(input_data)
}

