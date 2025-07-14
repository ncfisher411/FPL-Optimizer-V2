#---------------------------------------#
# Cards for FPL Probability Model
# Written by: ncfisher
# Last updated: July 11 2025
#---------------------------------------#


## In this module we want to calculate the probability of scoring
### Weighted xA calculation

#### By player - overall avg careds per match played
player <- combined_data %>%
  mutate(games_played = ifelse(minutes > 0, 1, 0)) %>%
  group_by(name) %>%
  summarize(yellow_cards_player = mean(yellow_cards, na.rm = T),
            red_cards_player = mean(yellow_cards, na.rm = T)) %>%
  ungroup()

#### By opponent - avg per match xA played against by player
opponent <- combined_data %>%
  mutate(games_played = ifelse(minutes > 0, 1, 0)) %>%
  group_by(name, opponent_name) %>%
  summarize(yellow_cards_opp = mean(yellow_cards, na.rm = T),
            red_cards_opp = mean(red_cards, na.rm = T)) %>%
  ungroup()

#### By team - avg per match xA against by team
team <- combined_data %>%
  mutate(games_played = ifelse(minutes > 0, 1, 0)) %>%
  group_by(team, opponent_name) %>%
  summarize(yellow_cards_team = mean(yellow_cards, na.rm = T),
            red_cards_team= mean(red_cards, na.rm = T)) %>%
  ungroup()

#### By home/away - avg per match xA against by player by home or away
ha <- combined_data %>%
  mutate(games_played = ifelse(minutes > 0, 1, 0)) %>%
  group_by(name, was_home) %>%
  summarize(yellow_cards_ha = mean(yellow_cards, na.rm = T),
            red_cards_ha = mean(red_cards, na.rm = T)) %>%
  ungroup()

#### By position - avg per match xA against a team by position
position <- combined_data %>%
  mutate(games_played = ifelse(minutes > 0, 1, 0)) %>%
  group_by(position, opponent_name) %>%
  summarize(yellow_cards_position = mean(yellow_cards, na.rm = T),
            red_cards_position = mean(red_cards, na.rm = T)) %>%
  ungroup()

### Do a weighted exercise experiment with weights from parameters:
yellow_cards_probs <- combined_data %>%
  select(name, season, team, opponent_name, position, was_home) %>%
  left_join(player) %>%
  left_join(opponent, by=c('name', 'opponent_name')) %>%
  left_join(team, by = c('team', 'opponent_name')) %>%
  left_join(ha) %>%
  left_join(position, by=c('position', 'opponent_name')) %>%
  mutate(
    yellow_cards_calculated = (yellow_cards_player * player_card_weight) +
      (yellow_cards_opp * opponent_card_weight) +
      (yellow_cards_team * team_card_weight) +
      (yellow_cards_ha * ha_card_weight) +
      (yellow_cards_position * position_card_weight)
  )

red_cards_probs <- combined_data %>%
  select(name, season, team, opponent_name, position, was_home) %>%
  left_join(player) %>%
  left_join(opponent, by=c('name', 'opponent_name')) %>%
  left_join(team, by = c('team', 'opponent_name')) %>%
  left_join(ha) %>%
  left_join(position, by=c('position', 'opponent_name')) %>%
  mutate(
    red_cards_calculated = (red_cards_player * player_card_weight) +
      (red_cards_opp * opponent_card_weight) +
      (red_cards_team * team_card_weight) +
      (red_cards_ha * ha_card_weight) +
      (red_cards_position * position_card_weight)
  )

objects <- ls()
keep <- objects[grep('combined_data|test|fixture|team|current_players|probs|understat|weight', objects)]
rm(list=setdiff(objects, keep))
gc()