#---------------------------------------#
# Assists for FPL Probability Model
# Written by: ncfisher
# Last updated: July 11 2025
#---------------------------------------#

## In this module we want to calculate the probability of scoring
### Weighted xA calculation

#### By player - overall avg xA per match played
player <- combined_data %>%
  mutate(games_played = ifelse(minutes > 0, 1, 0)) %>%
  group_by(name) %>%
  summarize(xA_player = mean(expected_assists, na.rm = T)) %>%
  ungroup()

#### By opponent - avg per match xA played against by player
opponent <- combined_data %>%
  mutate(games_played = ifelse(minutes > 0, 1, 0)) %>%
  group_by(name, opponent_name) %>%
  summarize(xA_opp = mean(expected_assists, na.rm = T)) %>%
  ungroup()

#### By team - avg per match xA against by team
team <- combined_data %>%
  mutate(games_played = ifelse(minutes > 0, 1, 0)) %>%
  group_by(team, opponent_name) %>%
  summarize(xA_team = mean(expected_assists, na.rm = T)) %>%
  ungroup()

#### By home/away - avg per match xA against by player by home or away
ha <- combined_data %>%
  mutate(games_played = ifelse(minutes > 0, 1, 0)) %>%
  group_by(name, was_home) %>%
  summarize(xA_ha = mean(expected_assists, na.rm = T)) %>%
  ungroup()

#### By position - avg per match xA against a team by position
position <- combined_data %>%
  mutate(games_played = ifelse(minutes > 0, 1, 0)) %>%
  group_by(position, opponent_name) %>%
  summarize(xA_pos = mean(expected_assists, na.rm = T)) %>%
  ungroup()

### Do a weighted exercise experiment with the following weights:
assist_probs <- combined_data %>%
  select(name, season, team, opponent_name, position, was_home) %>%
  left_join(player) %>%
  left_join(opponent, by=c('name', 'opponent_name')) %>%
  left_join(team, by = c('team', 'opponent_name')) %>%
  left_join(ha) %>%
  left_join(position, by=c('position', 'opponent_name')) %>%
  mutate(
    xA_calculated = (xA_player * player_assist_weight) +
      (xA_opp * opponent_assist_weight) +
      (xA_team * team_assist_weight) +
      (xA_ha * ha_assist_weight) +
      (xA_pos * position_assist_weight)
  )

objects <- ls()
keep <- objects[grep('combined_data|test|fixture|team|current_players|probs|understat|weight', objects)]
rm(list=setdiff(objects, keep))
gc()
