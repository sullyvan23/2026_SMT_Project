library(dplyr)
library(mgcv)

### NEED
### one_on_data_sum
### advance_first_model
### advance_second_model
### advance_third_model
### final_doubled_model
### tag_end_model

#####################################################################################################################################################################################

go_back_function <- function(input_data) {
  for(i in 2:nrow(input_data)) {
    ### close to minimum possible next acceleration
    input_data$runner_basepath_accel[i] <- round(input_data$runner_basepath_accel[i-1] + ((1-input_data$ellipse[i-1]^1) * (input_data$runner_basepath_accel[i-1] - input_data$runner_basepath_accel_2[i-1])) +
                                                  input_data$ellipse[i-1]^1 * ifelse(input_data$og_basepath_dist[i-1] <= 0.2  &  input_data$runner_basepath_velo[i-1] <= 0,
                                                                                     -(input_data$runner_basepath_velo[i-1]/10) + ((-0.2+input_data$og_basepath_dist[i-1]) * 
                                                                                                                                    input_data$runner_basepath_accel[i-1]/10),
                                                                                     -(input_data$runner_basepath_velo[i-1]/80)) -
                                                  (ifelse(input_data$runner_basepath_velo[i-1] > 0, 0.0045, 0.0035) * (fps/0.05)), 
                                                  3)

    ### having velocity and position match acceleration
    input_data$runner_basepath_velo[i] <- input_data$runner_basepath_velo[i-1] + (input_data$runner_basepath_accel[i]*fps)
    input_data$og_basepath_dist[i] <- input_data$og_basepath_dist[i-1] + (input_data$runner_basepath_velo[i]*fps)
    input_data$og_basepath_dist[i] <- ifelse(input_data$og_basepath_dist[i] < 0.025, 0.025, input_data$og_basepath_dist[i])
    input_data$runner_basepath_accel_2[i] = input_data$runner_basepath_accel[i-1]
    input_data$ellipse[i] <- (input_data$runner_basepath_velo[i]^2 / max_speed^2) +
                              (input_data$runner_basepath_accel[i]^2 / max_accel^2) +
                              ifelse(input_data$og_basepath_dist[i] <= 0.2  &  input_data$runner_basepath_velo[i] <= 0,
                                     (input_data$og_basepath_dist[i]-0.225)^2 / 0.2^2,
                                      0)

    ### correcting for if it goes outside of the ellipse (mainly for going back and getting back towards a velocity of 0)
    while(input_data$ellipse[i] > 1) {
      input_data$runner_basepath_velo[i] = ifelse(input_data$og_basepath_dist[i] == 0.025, 0, input_data$runner_basepath_velo[i])
      ifelse((input_data$runner_basepath_velo[i]^2 / max_speed^2) <= (input_data$runner_basepath_accel[i]^2 / max_accel^2),
         input_data$runner_basepath_accel[i] <- input_data$runner_basepath_accel[i] - (sign(input_data$runner_basepath_accel[i]) * 0.0005),
         input_data$runner_basepath_velo[i] <- input_data$runner_basepath_velo[i] - (sign(input_data$runner_basepath_velo[i]) * 0.0005))
      
      input_data$ellipse[i] <- (input_data$runner_basepath_velo[i]^2 / max_speed^2) +
                                (input_data$runner_basepath_accel[i]^2 / max_accel^2) +
                                ifelse(input_data$og_basepath_dist[i] <= 0.2  &  input_data$runner_basepath_velo[i] <= 0,
                                       (input_data$og_basepath_dist[i]-0.225)^2 / 0.2^2,
                                        0)
    }
    
  }

  ### returning final position and velocity
  return(list(input_data$og_basepath_dist[nrow(input_data)], 
              input_data$runner_basepath_velo[nrow(input_data)]))
}


### adding probabilities of advancing certain bases if ball drops
add_advance_probs <- function(input_dataset) {
  if(test_play$player_id_br[1] == 11) {
    advance_probs <- data.frame(predict(advance_first_model, newdata = input_dataset, type = "response"))[,-1]
  } else if(test_play$player_id_br[1] == 12) {
    advance_probs <- data.frame(predict(advance_second_model, newdata = input_dataset, type = "response"))[,-1]
    advance_probs[,3] <- NA
  } else if(test_play$player_id_br[1] == 13) {
    advance_probs <- data.frame(predict(advance_third_model, newdata = input_dataset, type = "response"))
    advance_probs[,2:3] <- NA
  }
  advance_probs <- advance_probs %>% rename(advance_1_prob = 1, advance_2_prob = 2, advance_3_prob = 3)
  input_dataset <- cbind(input_dataset, advance_probs)

  return(input_dataset)
}


#####################################################################################################################################################################################

### some plays I've looked at to potentially choose from
test_play <- one_on_data_sum %>% filter(game_string == "y1_d182_LRQ_ARN", play_per_game == 160)  ### 1st, doubled up
test_play <- one_on_data_sum %>% filter(game_string == "y1_d061_VKA_PHD", play_per_game == 91) ### 1st, succ tag, 60% caught most of time
test_play <- one_on_data_sum %>% filter(game_string == "y1_d211_QHX_ANI", play_per_game == 40) ### tag, decently high caught prob whole time
test_play <- one_on_data_sum %>% filter(game_string == "y1_d073_XPO_PHD", play_per_game == 379) ### 3rd, easy tag
test_play <- one_on_data_sum %>% filter(game_string == "y1_d199_TES_ARN", play_per_game == 197) ### 2nd, tag
test_play <- one_on_data_sum %>% filter(game_string == "y1_d166_FNQ_PHD", play_per_game == 80) ### not caught easily, stealing before
test_play <- one_on_data_sum %>% filter(game_string == "y1_d120_MKS_ARN", play_per_game == 230) ### not caught easily
test_play <- one_on_data_sum %>% filter(game_string == "y1_d168_BTL_ARN", play_per_game == 123) ### not caught easily
test_play <- one_on_data_sum %>% filter(game_string == "y1_d178_AVV_ARN", play_per_game == 137) ### infield fly
test_play <- one_on_data_sum %>% filter(game_string == "y1_d073_XPO_PHD", play_per_game == 95) ### 1st, rlly high catch prob
test_play <- one_on_data_sum %>% filter(game_string == "y1_d063_VKA_PHD", play_per_game == 166) ### 1st, up and down catch prob
test_play <- one_on_data_sum %>% filter(game_string == "y1_d202_PHD_VAS", play_per_game == 9)  ### 1st, doubled up, short fly
test_play <- one_on_data_sum %>% filter(game_string == "y1_d169_MPC_PHD", play_per_game == 79)  ### third tag, prob goes a bit too far
test_play <- one_on_data_sum %>% filter(game_string == "y1_d061_VKA_PHD", play_per_game == 22)  ### 2nd, catch prob drops
test_play <- one_on_data_sum %>% filter(game_string == "y1_d125_MEX_ANI", play_per_game == 228)  ### 1st, high catch prob short, drops



### play used
test_play <- one_on_data_sum %>% filter(game_string == "y1_d211_QHX_ANI", play_per_game == 145) ### 2nd, go really far

### needed variables
max_speed <- max(test_play$runner_basepath_velo, test_play$speed_95_runner[1]/0.681818/95)
max_accel <- max(max_speed*(95/140), test_play$speed_95_runner[1]/0.681818/110)
fps <- test_play$fps[1]

### ellipse for keeping motion within normal parameters
test_play <- test_play %>% mutate(back_basepath = NA,
                                  back_velo = NA,
                                  ellipse = (runner_basepath_velo^2 / max_speed^2) +
                                            (runner_basepath_accel^2 / max_accel^2) +
                                            ifelse(og_basepath_dist <= 0.2  &  runner_basepath_velo <= 0,
                                                   (og_basepath_dist-0.225)^2 / 0.2^2,
                                                    0))
### calculating roughly closest runner can get to original base by time ball is caught if they decide to go bacl
for(j in 1:(nrow(test_play)-1)) {
  back_results <- go_back_function(test_play[j:nrow(test_play),])
  test_play$back_basepath[j] <- back_results[[1]]
  test_play$back_velo[j] <- back_results[[2]]
  print(j/nrow(test_play))
}
test_play$back_basepath[nrow(test_play)] <- test_play$og_basepath_dist[nrow(test_play)]
test_play$back_velo[nrow(test_play)] <- test_play$runner_basepath_velo[nrow(test_play)]

### calculating necessary probabilities
test_play <- test_play %>% mutate(doubled_up_prob = 1 - predict(final_doubled_model, newdata = test_play, type = "response"),
                                  tag_up_prob = predict(tag_end_model, newdata = test_play, type = "response"))
test_play <- add_advance_probs(test_play)
test_play <- test_play %>% mutate(doubled = doubled_up_prob * caught_prob,
                                  stay = (1 - doubled_up_prob - tag_up_prob) * caught_prob,
                                  tag = tag_up_prob * caught_prob,
                                  advance_0 = (1 - rowSums(across(c(advance_1_prob:advance_3_prob)), na.rm = TRUE)) * (1 - caught_prob),
                                  advance_1 = advance_1_prob * (1 - caught_prob),
                                  advance_2 = advance_2_prob * (1 - caught_prob),
                                  advance_3 = advance_3_prob * (1 - caught_prob))

### calculating max potential speeds and accelerations, and run expectancy every timestamp
test_play <- test_play %>% mutate(run_exp = rowSums(across(doubled:advance_3) * across(d_sit_re:a3_sit_re), na.rm = TRUE))

#####################################################################################################################################################################################

### leaving first four rows as reaction that early is unrealistic
model_play <- test_play[1:4,] %>% mutate(runner_basepath_velo = round(runner_basepath_velo, 3))

for(i in 5:nrow(test_play)) {
  ### giving range of next possible acclerations
  accels <- seq(round(model_play$runner_basepath_accel[i-1] + ((1-model_play$ellipse[i-1]^1) * (model_play$runner_basepath_accel[i-1] - model_play$runner_basepath_accel_2[i-1])) +
                      model_play$ellipse[i-1]^1 * ifelse(model_play$og_basepath_dist[i-1] <= 0.2  &  model_play$runner_basepath_velo[i-1] <= 0,
                                                         -(model_play$runner_basepath_velo[i-1]/10) + ((-0.2+model_play$og_basepath_dist[i-1]) * 
                                                                                                        model_play$runner_basepath_accel[i-1]/10),
                                                         -(model_play$runner_basepath_velo[i-1]/80)) -
                      (ifelse(model_play$runner_basepath_velo[i-1] > 0, 0.005, 0.004) * (fps/0.05)), 
                      3),
                round(model_play$runner_basepath_accel[i-1] + ((1-model_play$ellipse[i-1]^1) * (model_play$runner_basepath_accel[i-1] - model_play$runner_basepath_accel_2[i-1])) +
                      model_play$ellipse[i-1]^1 * ifelse(model_play$og_basepath_dist[i-1] <= 0.2  &  model_play$runner_basepath_velo[i-1] <= 0,
                                                         -(model_play$runner_basepath_velo[i-1]/10) + ((-0.2+model_play$og_basepath_dist[i-1]) * 
                                                                                                        model_play$runner_basepath_accel[i-1]/10),
                                                         -(model_play$runner_basepath_velo[i-1]/80)) +
                      (ifelse(model_play$runner_basepath_velo[i-1] < 0, 0.005, 0.004) * (fps/0.05)), 
                      3),
                by = 0.0005)

  ### new dataset of all potential next accelerations with next timestamps other numbers, changing position and velo to account for acceleration
  next_time_check <- test_play[i,] %>% slice(rep(1,length(accels)))
  next_time_check$runner_basepath_accel <- accels
  next_time_check <- next_time_check %>% mutate(runner_basepath_velo = model_play$runner_basepath_velo[i-1] + (runner_basepath_accel*fps),
                                                og_basepath_dist = model_play$og_basepath_dist[i-1] + (runner_basepath_velo*fps),
                                                og_basepath_dist = ifelse(og_basepath_dist < 0.025, 0.025, og_basepath_dist),
                                                basepath = og_basepath_dist + player_id_br - 10,
                                                runner_basepath_accel_2 = model_play$runner_basepath_accel[i-1],
                                                ellipse = (runner_basepath_velo^2 / max_speed^2) +
                                                          (runner_basepath_accel^2 / max_accel^2) +
                                                          ifelse(og_basepath_dist <= 0.2  &  runner_basepath_velo <= 0,
                                                                 (og_basepath_dist-0.225)^2 / 0.2^2,
                                                                  0))

  ### correcting for if it goes outside of the ellipse (mainly for going back and getting back towards a velocity of 0)
  while(min(next_time_check$ellipse) > 1) {
    next_time_check <- next_time_check %>% slice_min(ellipse) %>%
                                           mutate(runner_basepath_velo = ifelse(og_basepath_dist == 0.025, 0, runner_basepath_velo))
    ifelse((next_time_check$runner_basepath_velo[1]^2 / max_speed^2) <= (next_time_check$runner_basepath_accel[1]^2 / max_accel^2),
           next_time_check <- next_time_check %>% mutate(runner_basepath_accel = runner_basepath_accel - (sign(runner_basepath_accel) * 0.001)),
           next_time_check <- next_time_check %>% mutate(runner_basepath_velo = runner_basepath_velo - (sign(runner_basepath_velo) * 0.001)))
    
    next_time_check <- next_time_check %>% mutate(ellipse = (runner_basepath_velo^2 / max_speed^2) +
                                                            (runner_basepath_accel^2 / max_accel^2) +
                                                            ifelse(og_basepath_dist <= 0.2  &  runner_basepath_velo <= 0,
                                                                  (og_basepath_dist-0.225)^2 / 0.2^2,
                                                                   0))
  }

  ### filtering for only possible motion
  next_time_check <- next_time_check %>% filter(ellipse <= 1) %>%
                                         mutate(back_basepath = NA,
                                                back_velo = NA)

  ### getting final possible positions if they decide to go back (for calcuating tag probability)
  if(i == nrow(test_play)) {
    next_time_check$back_basepath <- next_time_check$og_basepath_dist
    next_time_check$back_velo <- next_time_check$runner_basepath_velo
  } else {
    for(j in 1:nrow(next_time_check)) {
      go_back_check <- bind_rows(next_time_check[j,],
                                 test_play[(i+1):nrow(test_play),]) %>% select(og_basepath_dist, runner_basepath_velo, runner_basepath_accel, runner_basepath_accel_2, ellipse)
      back_results <- go_back_function(go_back_check)
      next_time_check$back_basepath[j] <- back_results[[1]]
      next_time_check$back_velo[j] <- back_results[[2]]
    }
  }

  ### calculating needed probabilities
  next_time_check <- next_time_check %>% mutate(doubled_up_prob = 1 - predict(final_doubled_model, newdata = next_time_check, type = "response"),
                                                tag_up_prob = predict(tag_end_model, newdata = next_time_check, type = "response"))
  next_time_check <- add_advance_probs(next_time_check %>% select(-c(advance_1_prob:advance_3_prob)))
  next_time_check <- next_time_check %>% mutate(doubled = doubled_up_prob * caught_prob,
                                                stay = (1 - doubled_up_prob - tag_up_prob) * caught_prob,
                                                tag = tag_up_prob * caught_prob,
                                                advance_0 = (1 - rowSums(across(c(advance_1_prob:advance_3_prob)), na.rm = TRUE)) * (1 - caught_prob),
                                                advance_1 = advance_1_prob * (1 - caught_prob),
                                                advance_2 = advance_2_prob * (1 - caught_prob),
                                                advance_3 = advance_3_prob * (1 - caught_prob))

  ### calculating run expectancy and sorting by it
  next_time_check <- next_time_check %>% mutate(run_exp = rowSums(across(doubled:advance_3) * across(d_sit_re:a3_sit_re), na.rm = TRUE),
                                                jerk_now = row_number() / nrow(next_time_check)) %>%
                                        arrange(desc(run_exp))

  ### taking best one and using
  model_play <- bind_rows(model_play, next_time_check[1,])
  print(i/nrow(test_play))
}

##################################################################################################################################################################

### plot of position over time (actual = black, model = red)
plot(-test_play$time_left_ground, test_play$basepath, col = "black", ylim = c(min(test_play$basepath,model_play$basepath), max(test_play$basepath,model_play$basepath)))
points(-model_play$time_left_ground, model_play$basepath, col = "red")

### plot of run expectancy over time (actual = black, model = red)
plot(-test_play$time_left_ground, test_play$run_exp, col = "black", ylim = c(min(test_play$run_exp,model_play$run_exp), max(test_play$run_exp,model_play$run_exp)))
points(-model_play$time_left_ground, model_play$run_exp, col = "red")

### avg run expectancy model is better by over play
mean(model_play$run_exp - test_play$run_exp)

### percent runs of model
sum(test_play$run_exp) / sum(model_play$run_exp)

##################################################################################################################################################################

save(one_on_data_sum, go_back_function, add_advance_probs, advance_first_model, advance_second_model, advance_third_model, final_doubled_model, tag_end_model,
     basepath_x_model, basepath_y_model, animate_model,
     file = "share.Rdata")











