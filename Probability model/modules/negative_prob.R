#---------------------------------------#
# Negative stats for FPL Probability Model
# Written by: ncfisher
# Last updated: July 11 2025
#---------------------------------------#

# Probabilities of penalty misses and own goals

## 1) Penalty misses

### Penalty misses per 90 by player product w/ playing probability
probs_neg <- combined_data %>%
  group_by(name, position, team) %>%
  summarize(minutes = sum(minutes, na.rm = T),
            penalties_missed = sum(penalties_missed, na.rm = T)) %>%
  ungroup() %>%
  mutate(pen_miss_per_90 = ifelse(minutes > 0, penalties_missed/(minutes/90), 0)) %>%
  left_join(probs_time) %>%
  mutate(prob_pen_miss = pen_miss_per_90 * Prob_played)

## 2) Own goals

### Own goals per 90 by player product w/ playing probability
probs_neg <- probs_neg %>%
  left_join(
    combined_data %>%
      group_by(name, position, team) %>%
      summarize(minutes = sum(minutes, na.rm = T),
                own_goals = sum(own_goals, na.rm = T)) %>%
      ungroup() %>%
      mutate(og_per_90 = ifelse(minutes > 0, own_goals/(minutes/90), 0)) %>%
      left_join(probs_time) %>%
      mutate(prob_own_goal = og_per_90 * Prob_played)
  )
  

objects <- ls()
keep <- objects[grep('combined_data|test|fixture|team|current_players|probs|understat|weight', objects)]
rm(list=setdiff(objects, keep))
gc()
