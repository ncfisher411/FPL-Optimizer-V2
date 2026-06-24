#---------------------------------------#
# Cards for FPL Probability Model
# Written by: ncfisher
# Last updated: June 23 2026
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
  ungroup() %>%
  filter(!is.na(opponent_name))

#### By team - avg per match xA against by team
team <- combined_data %>%
  mutate(games_played = ifelse(minutes > 0, 1, 0)) %>%
  group_by(team, opponent_name) %>%
  summarize(yellow_cards_team = mean(yellow_cards, na.rm = T),
            red_cards_team= mean(red_cards, na.rm = T)) %>%
  ungroup() %>%
  filter(!is.na(opponent_name))

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
  ungroup() %>%
  filter(!is.na(opponent_name))

### Do a weighted exercise experiment with weights from parameters:
yellow_cards_probs <- combined_data %>%
  select(name, season, team, opponent_name, position, was_home) %>%
  left_join(player) %>%
  left_join(opponent) %>%
  left_join(team) %>%
  left_join(ha) %>%
  left_join(position) %>%
  mutate(
    yellow_cards_calculated = (yellow_cards_player * player_card_weight) +
      (yellow_cards_opp * opponent_card_weight) +
      (yellow_cards_team * team_card_weight) +
      (yellow_cards_ha * ha_card_weight) +
      (yellow_cards_position * position_card_weight),
    yellow_cards_calculated = ifelse(is.nan(yellow_cards_calculated), yellow_cards_team * (1-team_card_weight), yellow_cards_calculated),
    yellow_cards_calculated = ifelse(is.nan(yellow_cards_calculated), yellow_cards_position * (1 - position_card_weight), yellow_cards_calculated),
    yellow_cards_calculated = ifelse(is.nan(yellow_cards_calculated), 0, yellow_cards_calculated),
    across(starts_with('yellow'), ~ifelse(is.na(.), 0, .))
  )

red_cards_probs <- combined_data %>%
  select(name, season, team, opponent_name, position, was_home) %>%
  left_join(player) %>%
  left_join(opponent) %>%
  left_join(team) %>%
  left_join(ha) %>%
  left_join(position) %>%
  mutate(
    red_cards_calculated = (red_cards_player * player_card_weight) +
      (red_cards_opp * opponent_card_weight) +
      (red_cards_team * team_card_weight) +
      (red_cards_ha * ha_card_weight) +
      (red_cards_position * position_card_weight),
    red_cards_calculated = ifelse(is.nan(red_cards_calculated), red_cards_team * (1-team_card_weight), red_cards_calculated),
    red_cards_calculated = ifelse(is.nan(red_cards_calculated), red_cards_position * (1 - position_card_weight), red_cards_calculated),
    red_cards_calculated = ifelse(is.nan(red_cards_calculated), 0, red_cards_calculated),
    across(starts_with('red'), ~ifelse(is.na(.), 0, .))
  )

### Adding a step that will work for promoted teams without previous stats - really just Sunderland - if needed
status <- grepl('TRUE', unique(fixtures$finished))

if(status=='FALSE'){
  
  teams <- c('Burnley', 'Leeds', 'Ipswich', 'Leicester', 'Luton', 'Sheffield Utd', 'Southampton',
             'Hull City', 'Coventry City')
  
  ### Get the team data
  temp <- yellow_cards_probs %>% filter(team %in% teams) %>%
    group_by(opponent_name, was_home) %>% 
    summarize(across(starts_with('yellow_cards') | starts_with('red_cards'), ~mean(., na.rm = T))) %>%
    ungroup() %>%
    mutate(team = 'Sunderland')
  
  temp <- yellow_cards_probs %>% filter(team=='Sunderland') %>%
    select(name, season, team, opponent_name, position, was_home) %>%
    left_join(temp)
  
  yellow_cards_probs <- yellow_cards_probs %>% filter(team!='Sunderland') %>%
    rbind(temp)
  
  temp <- red_cards_probs %>% filter(team %in% teams) %>%
    group_by(opponent_name, was_home) %>% 
    summarize(across(starts_with('yellow_cards') | starts_with('red_cards'), ~mean(., na.rm = T))) %>%
    ungroup() %>%
    mutate(team = 'Sunderland')
  
  temp <- red_cards_probs %>% filter(team=='Sunderland') %>%
    select(name, season, team, opponent_name, position, was_home) %>%
    left_join(temp)
  
  red_cards_probs <- red_cards_probs %>% filter(team!='Sunderland') %>%
    rbind(temp)
  
  ### Get the opponent data
  temp <- yellow_cards_probs %>% filter(opponent_name %in% teams) %>%
    group_by(team, was_home) %>% 
    summarize(across(starts_with('yellow_cards') | starts_with('red_cards'), ~mean(., na.rm = T))) %>%
    ungroup() %>%
    mutate(opponent_name = 'Sunderland')
  
  temp <- yellow_cards_probs %>% filter(opponent_name=='Sunderland') %>%
    select(name, season, team, opponent_name, position, was_home) %>%
    left_join(temp)
  
  yellow_cards_probs <- yellow_cards_probs %>% filter(opponent_name!='Sunderland') %>%
    rbind(temp)
  
  temp <- red_cards_probs %>% filter(opponent_name %in% teams) %>%
    group_by(team, was_home) %>% 
    summarize(across(starts_with('yellow_cards') | starts_with('red_cards'), ~mean(., na.rm = T))) %>%
    ungroup() %>%
    mutate(opponent_name = 'Sunderland')
  
  temp <- red_cards_probs %>% filter(opponent_name=='Sunderland') %>%
    select(name, season, team, opponent_name, position, was_home) %>%
    left_join(temp)
  
  red_cards_probs <- red_cards_probs %>% filter(opponent_name!='Sunderland') %>%
    rbind(temp)
}

objects <- ls()
keep <- objects[grep('combined_data|test|fixture|team|current_players|probs|understat|weight', objects)]
rm(list=setdiff(objects, keep))
gc()
