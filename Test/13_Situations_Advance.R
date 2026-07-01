
advance_situations <- advance_data %>% ungroup() %>% select(game_string:succ_bases_advanced) %>% filter(safe_advance == 1) %>%
                                       group_by(game_string, play_per_game, player_id_br) %>% slice(1) %>% select(-c(final_base:att_bases_advanced))


advance_situations <- advance_situations %>% left_join(baserunners, by = c("game_string", "play_per_game"))

advance_situations <- advance_situations %>% mutate(next_play = play_per_game + 1)
advance_situations <- advance_situations %>% left_join(baserunners, by = c("game_string", "next_play" = "play_per_game"), suffix = c("", "_next"))
advance_situations <- advance_situations %>% filter(!is.na(first_next))


advance_situations <- advance_situations %>% group_by(succ_bases_advanced, first, second, third) %>%
                                             summarise(first_next = mean(first_next),
                                                       second_next = mean(second_next),
                                                       third_next = mean(third_next),
                                                       count = n())

