
advance_one_base <- ball_lands_results %>% mutate(advance_1 = ifelse(succ_bases_advanced > 0, 1, 0))
advance_two_bases <- ball_lands_results %>% mutate(advance_2 = ifelse(succ_bases_advanced > 1, 1, 0)) %>%
                                            filter(original_base < 3)
advance_three_base <- ball_lands_results %>% mutate(advance_3 = ifelse(succ_bases_advanced > 2, 1, 0)) %>%
                                            filter(original_base < 2)
