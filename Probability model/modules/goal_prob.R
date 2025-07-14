#---------------------------------------#
# Goal scoring for FPL Probability Model
# Written by: ncfisher
# Last updated: July 11 2025
#---------------------------------------#

## In this module we want to calculate the probability of scoring
### Weighted xG calculation

#### By player - overall avg xG per match played
player <- combined_data %>%
  mutate(games_played = ifelse(minutes > 0, 1, 0)) %>%
  group_by(name) %>%
  summarize(xG_player = mean(expected_goals, na.rm = T)) %>%
  ungroup()

#### By opponent - avg per match xG played against by player
opponent <- combined_data %>%
  mutate(games_played = ifelse(minutes > 0, 1, 0)) %>%
  group_by(name, opponent_name) %>%
  summarize(xG_opp = mean(expected_goals, na.rm = T)) %>%
  ungroup()

#### By team - avg per match xG against by team
team <- combined_data %>%
  mutate(games_played = ifelse(minutes > 0, 1, 0)) %>%
  group_by(team, opponent_name) %>%
  summarize(xG_team = mean(expected_goals, na.rm = T)) %>%
  ungroup()

#### By home/away - avg per match xG against by player by home or away
ha <- combined_data %>%
  mutate(games_played = ifelse(minutes > 0, 1, 0)) %>%
  group_by(name, was_home) %>%
  summarize(xG_ha = mean(expected_goals, na.rm = T)) %>%
  ungroup()

#### By position - avg per match xG against a team by position
position <- combined_data %>%
  mutate(games_played = ifelse(minutes > 0, 1, 0)) %>%
  group_by(position, opponent_name) %>%
  summarize(xG_pos = mean(expected_goals, na.rm = T)) %>%
  ungroup()

### Do a weighted exercise experiment with the parameter weights:
goal_probs <- combined_data %>%
  select(name, season, team, opponent_name, position, was_home) %>%
  left_join(player) %>%
  left_join(opponent, by=c('name', 'opponent_name')) %>%
  left_join(team, by = c('team', 'opponent_name')) %>%
  left_join(ha) %>%
  left_join(position, by=c('position', 'opponent_name')) %>%
  mutate(
    xG_calculated = (xG_player * player_goal_weight) +
      (xG_opp * opponent_goal_weight) + 
      (xG_team * team_goal_weight) +
      (xG_ha * ha_goal_weight) +
      (xG_pos * position_goal_weight)
  )

objects <- ls()
keep <- objects[grep('combined_data|test|fixture|team|current_players|probs|understat|weight', objects)]
rm(list=setdiff(objects, keep))
gc()
