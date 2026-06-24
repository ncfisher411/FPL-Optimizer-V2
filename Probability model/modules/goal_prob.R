#---------------------------------------#
# Goal scoring for FPL Probability Model
# Written by: ncfisher
# Last updated: June 23 2026
#---------------------------------------#

## In this module we want to calculate the probability of scoring
### Weighted xG calculation

#### By player - overall avg xG per match played
player <- combined_data %>%
  mutate(games_played = ifelse(as.numeric(minutes) > 0, 1, 0)) %>%
  group_by(name) %>%
  summarize(xG_player = mean(as.numeric(expected_goals), na.rm = T)) %>%
  ungroup()

#### By opponent - avg per match xG played against by player
opponent <- combined_data %>%
  mutate(games_played = ifelse(as.numeric(minutes) > 0, 1, 0)) %>%
  group_by(name, opponent_name) %>%
  summarize(xG_opp = mean(as.numeric(expected_goals), na.rm = T)) %>%
  ungroup() %>%
  filter(!is.na(opponent_name))

#### By team - avg per match xG against by team
team <- combined_data %>%
  mutate(games_played = ifelse(as.numeric(minutes) > 0, 1, 0)) %>%
  group_by(team, opponent_name) %>%
  summarize(xG_team = mean(as.numeric(expected_goals), na.rm = T)) %>%
  ungroup() %>%
  filter(!is.na(opponent_name))

#### By home/away - avg per match xG against by player by home or away
ha <- combined_data %>%
  mutate(games_played = ifelse(as.numeric(minutes) > 0, 1, 0)) %>%
  group_by(name, was_home) %>%
  summarize(xG_ha = mean(as.numeric(expected_goals), na.rm = T)) %>%
  ungroup()

#### By position - avg per match xG against a team by position
position <- combined_data %>%
  mutate(games_played = ifelse(as.numeric(minutes) > 0, 1, 0)) %>%
  group_by(position, opponent_name) %>%
  summarize(xG_pos = mean(as.numeric(expected_goals), na.rm = T)) %>%
  ungroup() %>%
  filter(!is.na(opponent_name))

### Do a weighted exercise experiment with the parameter weights:
goal_probs <- combined_data %>%
  select(name, season, team, opponent_name, position, was_home) %>%
  left_join(player) %>%
  left_join(opponent) %>%
  left_join(team) %>%
  left_join(ha) %>%
  left_join(position) %>%
  group_by(name) %>%
  mutate(across(where(is.numeric), ~ifelse(is.nan(.) | is.na(.), mean(., na.rm = T), .))) %>%
  ungroup() %>%
  mutate(
    xG_calculated = (xG_player * player_goal_weight) +
      (xG_opp * opponent_goal_weight) + 
      (xG_team * team_goal_weight) +
      (xG_ha * ha_goal_weight) +
      (xG_pos * position_goal_weight),
    xG_calculated = ifelse(is.nan(xG_calculated), xG_team * (1-team_goal_weight), xG_calculated),
    xG_calculated = ifelse(is.nan(xG_calculated), xG_pos * (1 - position_goal_weight), xG_calculated),
    xG_calculated = ifelse(is.nan(xG_calculated), 0, xG_calculated)
  )

### Adding a step that will work for promoted teams without previous stats
status <- grepl('TRUE', unique(fixtures$finished))

if(status=='FALSE'){
  
  teams <- c('Burnley', 'Leeds', 'Ipswich', 'Leicester', 'Luton', 'Sheffield Utd', 'Southampton',
             'Hull City', 'Coventry City')
  
  ### Get the team data
  temp <- goal_probs %>% filter(team %in% teams) %>%
    group_by(opponent_team, was_home) %>% 
    summarize(across(starts_with('xG'), ~mean(., na.rm = T))) %>%
    ungroup() %>%
    mutate(team = 'Sunderland')
  
  temp <- goal_probs %>% filter(team=='Sunderland') %>%
    select(name, season, team, opponent_team, position, was_home) %>%
    left_join(temp)
  
  goal_probs <- goal_probs %>% filter(team!='Sunderland') %>%
    rbind(temp)
  
  ### Get the opponent data
  temp <- goal_probs %>% filter(opponent_team %in% teams) %>%
    group_by(team, was_home) %>% 
    summarize(across(starts_with('xG'), ~mean(., na.rm = T))) %>%
    ungroup() %>%
    mutate(opponent_team = 'Sunderland')
  
  temp <- goal_probs %>% filter(opponent_team=='Sunderland') %>%
    select(name, season, team, opponent_team, position, was_home) %>%
    left_join(temp)
  
  goal_probs <- goal_probs %>% filter(opponent_team!='Sunderland') %>%
    rbind(temp)
}

objects <- ls()
keep <- objects[grep('combined_data|test|fixture|team|current_players|probs|understat|weight', objects)]
rm(list=setdiff(objects, keep))
gc()
