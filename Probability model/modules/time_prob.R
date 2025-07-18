#---------------------------------------#
# Time probabilities script for FPL Probability Model
# Written by: ncfisher
# Last updated: July 11 2025
#---------------------------------------#

## In this module, we set up the probabilities that a certain player will play for their team in the upcoming matches
### This will be a weighting exercise - want to have higher weights towards the more recent seasons

probs_time <- combined_data %>%
  mutate(games=1,
         played = ifelse(minutes > 0, 1, 0),
         played60 = ifelse(minutes >=60, 1, 0)) %>%
  group_by(name, season) %>%
  summarize(played = sum(played, na.rm = T),
            played60 = sum(played60, na.rm = T),
            games = sum(games, na.rm = T)) %>%
  ungroup() %>%
  mutate(Prob_played = played/games,
         Prob_played60 = played60/games) %>%
  group_by(name) %>%
  mutate(max_season = max(season),
         difference = max_season - season,
         weight = ifelse(difference > 1, time_weight_2, 0),
         weight = ifelse(difference > 2, time_weight_3, weight),
         weight = ifelse(difference==0, time_weight_1, weight)) %>%
  ungroup() %>%
  mutate(Prob_played = Prob_played * weight,
         Prob_played60 = Prob_played60 * weight) %>%
  group_by(name, season) %>%
  summarize(Prob_played = sum(Prob_played, na.rm = T),
            Prob_played60 = sum(Prob_played60, na.rm = T)) %>%
  ungroup() %>%
  select(name, season, everything())

objects <- ls()
keep <- objects[grep('combined_data|test|fixture|team|current_players|probs|understat|weight', objects)]
rm(list=setdiff(objects, keep))
gc()
