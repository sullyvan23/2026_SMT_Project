
plays_share_data <- plays_share %>% mutate(runner_basepath_accel = ifelse(abs(runner_basepath_accel) > 0.3,
                                                                         0.3 * sign(runner_basepath_accel),
                                                                         runner_basepath_accel),
                                          runner_basepath_accel_2 = ifelse(abs(runner_basepath_accel_2) > 0.3,
                                                                           0.3 * sign(runner_basepath_accel_2),
                                                                           runner_basepath_accel_2),
                                          back_basepath = NA,
                                          back_velo = NA,
                                          forward_basepath = NA,
                                          forward_velo = NA,
                                          ellipse = (runner_basepath_velo^2 / max_speed^2) +
                                                    (runner_basepath_accel^2 / max_accel^2) +
                                                    ifelse(og_basepath_dist <= 0.2  &  runner_basepath_velo <= 0,
                                                           (og_basepath_dist-0.225)^2 / 0.2^2,
                                                            0))

group <- 0
plays_share_data <- plays_share_data %>% group_by(game_string, play_per_game, player_code_runner) %>%
                   group_modify(~{group <<- group + 1
                                message(group / sum(plays_num$count))

                                max_speed <- max(.x$runner_basepath_velo, .x$speed_95_runner[1]/0.681818/95)
                                max_accel <- max(max_speed*(95/125), .x$speed_95_runner[1]/0.681818/125)
                                fps <- .x$fps[1]
  
                                for(j in 1:(nrow(.x)-1)) {
                                  back_results <- go_back_function(.x[j:nrow(.x),])
                                  forward_results <- go_forward_function(.x[j:nrow(.x),])
                                  .x$back_basepath[j] <- back_results[[1]]
                                  .x$back_velo[j] <- back_results[[2]]
                                  .x$forward_basepath[j] <- forward_results[[1]]
                                  .x$forward_velo[j] <- forward_results[[2]]
                                  print(j/nrow(.x))
                                }
                                .x$back_basepath[nrow(.x)] <- .x$og_basepath_dist[nrow(.x)]
                                .x$back_velo[nrow(.x)] <- .x$runner_basepath_velo[nrow(.x)]
                                .x$forward_basepath[nrow(.x)] <- .x$og_basepath_dist[nrow(.x)]
                                .x$forward_velo[nrow(.x)] <- .x$runner_basepath_velo[nrow(.x)]

                                .x})


### calculating necessary probabilities
plays_share_data <- plays_share_data %>% ungroup() %>% 
                                         mutate(doubled_up_prob = 1 - predict(final_doubled_model, newdata = plays_share_data, type = "response"),
                                                tag_up_prob = predict(tag_end_model, newdata = plays_share_data, type = "response"))

plays_share_data <- bind_rows(add_advance_probs(plays_share_data %>% filter(player_id_br == 11)),
                              add_advance_probs(plays_share_data %>% filter(player_id_br == 12)),
                              add_advance_probs(plays_share_data %>% filter(player_id_br == 13)))

plays_share_data <- plays_share_data %>% mutate(doubled = doubled_up_prob * caught_prob,
                                                stay = (1 - doubled_up_prob - tag_up_prob) * caught_prob,
                                                tag = tag_up_prob * caught_prob,
                                                advance_0 = (1 - rowSums(across(c(advance_1_prob:advance_3_prob)), na.rm = TRUE)) * (1 - caught_prob),
                                                advance_1 = advance_1_prob * (1 - caught_prob),
                                                advance_2 = advance_2_prob * (1 - caught_prob),
                                                advance_3 = advance_3_prob * (1 - caught_prob))

### calculating max potential speeds and accelerations, and run expectancy every timestamp
plays_share_data <- plays_share_data %>% mutate(run_exp = rowSums(across(doubled:advance_3) * across(d_sit_re:a3_sit_re), na.rm = TRUE))

plays_share_data <- plays_share_data %>% group_by(game_string, play_per_game, player_id_br) %>% mutate(group = cur_group_id())

write.csv(plays_share_data, "plays_share_data.csv", row.names = FALSE)

##########################################################################################################################################################################################

max(plays_share_data$group)
### 440


### modeled_play_data <- data.frame()

for(g in 1:max(plays_share_data$group)) {
  test_play <- plays_share_data %>% ungroup() %>% filter(group == g)

  model_play <- test_play[1:4,] %>% mutate(runner_basepath_velo = round(runner_basepath_velo, 3))
  for(i in 5:nrow(test_play)) {
    ### giving range of next possible acclerations
    accels <- seq(round(model_play$runner_basepath_accel[i-1] + ((1-model_play$ellipse[i-1]^1) * (model_play$runner_basepath_accel[i-1] - model_play$runner_basepath_accel_2[i-1])) +
                        model_play$ellipse[i-1]^1 * ifelse(model_play$og_basepath_dist[i-1] <= 0.2  &  model_play$runner_basepath_velo[i-1] <= 0,
                                                           -(model_play$runner_basepath_velo[i-1]/10) - ((-0.2+model_play$og_basepath_dist[i-1])/10),
                                                           -(model_play$runner_basepath_velo[i-1]/80)) -
                        (ifelse(model_play$runner_basepath_velo[i-1] > 0, 0.0075, 0.005) * (fps/0.05)), 
                        3),
                  round(model_play$runner_basepath_accel[i-1] + ((1-model_play$ellipse[i-1]^1) * (model_play$runner_basepath_accel[i-1] - model_play$runner_basepath_accel_2[i-1])) +
                        model_play$ellipse[i-1]^1 * ifelse(model_play$og_basepath_dist[i-1] <= 0.2  &  model_play$runner_basepath_velo[i-1] <= 0,
                                                           -(model_play$runner_basepath_velo[i-1]/10) - ((-0.2+model_play$og_basepath_dist[i-1])/10),
                                                           -(model_play$runner_basepath_velo[i-1]/80)) +
                        (ifelse(model_play$runner_basepath_velo[i-1] < 0, 0.0075, 0.005) * (fps/0.05)), 
                        3),
                  by = 0.001)
  
    ### new dataset of all potential next accelerations with next timestamps other numbers, changing position and velo to account for acceleration
    next_time_check <- test_play[i,] %>% slice(rep(1,length(accels)))
    next_time_check$runner_basepath_accel <- accels
    next_time_check <- next_time_check %>% mutate(runner_basepath_velo = model_play$runner_basepath_velo[i-1] + (runner_basepath_accel*fps),
                                                  og_basepath_dist = model_play$og_basepath_dist[i-1] + (runner_basepath_velo*fps),
                                                  og_basepath_dist = ifelse(og_basepath_dist < 0.025, 0.025, og_basepath_dist),
                                                  basepath = og_basepath_dist + player_id_br - 10,
                                                  runner_basepath_accel_2 = model_play$runner_basepath_accel[i-1],
                                                  ellipse = (runner_basepath_velo^2 / max_speed^2) +
                                                            ifelse(og_basepath_dist <= 0.2  &  runner_basepath_velo <= 0,
                                                                   max( (runner_basepath_accel^2 / max_accel^2) , ((og_basepath_dist-0.225)^2 / 0.2^2) ),
                                                                     (runner_basepath_accel^2 / max_accel^2) ))
  
    ### correcting for if it goes outside of the ellipse (mainly for going back and getting back towards a velocity of 0)
    while(min(next_time_check$ellipse) > 1) {
      next_time_check <- next_time_check %>% slice_min(ellipse)
      
      ifelse((next_time_check$runner_basepath_velo[1]^2 / max_speed^2) <= (next_time_check$runner_basepath_accel[1]^2 / max_accel^2),
             next_time_check <- next_time_check %>% mutate(runner_basepath_accel = runner_basepath_accel - (sign(runner_basepath_accel) * 0.0005)),
             next_time_check <- next_time_check %>% mutate(runner_basepath_velo = runner_basepath_velo - (sign(runner_basepath_velo) * 0.0005)))
  
      next_time_check <- next_time_check %>% mutate(og_basepath_dist = model_play$og_basepath_dist[i-1] + (runner_basepath_velo*fps),
                                                    og_basepath_dist = ifelse(og_basepath_dist < 0.025, 0.025, og_basepath_dist),
                                                    runner_basepath_velo = ifelse(og_basepath_dist == 0.025, 0, runner_basepath_velo))
      
      next_time_check <- next_time_check %>% mutate(ellipse = (runner_basepath_velo^2 / max_speed^2) +
                                                              ifelse(og_basepath_dist <= 0.2  &  runner_basepath_velo <= 0,
                                                                     max( (runner_basepath_accel^2 / max_accel^2) , ((og_basepath_dist-0.225)^2 / 0.2^2) ),
                                                                       (runner_basepath_accel^2 / max_accel^2) ))
    }
  
    ### filtering for only possible motion
    next_time_check <- next_time_check %>% filter(ellipse <= 1) %>%
                                           mutate(back_basepath = NA, back_velo = NA, forward_basepath = NA, forward_velo = NA)
  
    ### getting final possible positions if they decide to go back (for calcuating tag probability)
    if(i == nrow(test_play)) {
      next_time_check$back_basepath <- next_time_check$og_basepath_dist
      next_time_check$back_velo <- next_time_check$runner_basepath_velo
      next_time_check$forward_basepath <- next_time_check$og_basepath_dist
      next_time_check$forward_velo <- next_time_check$runner_basepath_velo
    } else {
      for(j in 1:nrow(next_time_check)) {
        future_check <- bind_rows(next_time_check[j,],
                        test_play[(i+1):nrow(test_play),]) %>% select(og_basepath_dist, runner_basepath_velo, runner_basepath_accel, runner_basepath_accel_2, player_id_br, ellipse)
        back_results <- go_back_function(future_check)
        forward_results <- go_forward_function(future_check)
        next_time_check$back_basepath[j] <- back_results[[1]]
        next_time_check$back_velo[j] <- back_results[[2]]
        next_time_check$forward_basepath[j] <- forward_results[[1]]
        next_time_check$forward_velo[j] <- forward_results[[2]]
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
    
    print( g - 1 + (i/nrow(test_play)) )
  }

  modeled_play_data <- bind_rows(modeled_play_data, model_play)
}

write.csv(modeled_play_data, "modeled_play_data.csv", row.names = FALSE)
save.image("new.Rdata")

##########################################################################################################################################################################################

play_vs_model_data <- plays_share_data[,c(1:5,10,52)] %>% left_join(modeled_play_data[,c(1:2,4:5,10,52)],
                                                                 by = c("game_string", "play_per_game", "player_id_br", "timestamp"),
                                                                 suffix = c("_play", "_model"))

play_vs_model_summarise <- play_vs_model_data %>% group_by(game_string, play_per_game, player_id_br, player_code_runner) %>%
                                                  summarise(run_exp_play = mean(run_exp_play),
                                                            run_exp_model = mean(run_exp_model),
                                                            basepath_diff = last(basepath_play) - last(basepath_model)) %>%
                                                  mutate(percent = run_exp_play / run_exp_model,
                                                         exp_runs_lost = run_exp_model - run_exp_play)


hist(play_vs_model_summarise$percent, breaks = 50)



plays_leaderboard <- play_vs_model_summarise %>% filter(percent > 0.9, basepath_diff < 0.5)
plays_leaderboard <- plays_leaderboard %>% group_by(player_code_runner) %>%
                                           summarise(avg_percent = mean(percent),
                                                     avg_exp_runs_lost = mean(exp_runs_lost),
                                                     count = n())
plays_leaderboard <- plays_leaderboard %>% filter(count >= 5)


hist(plays_leaderboard$avg_percent, breaks = 5)
hist(plays_leaderboard$avg_exp_runs_lost, breaks = 5)


