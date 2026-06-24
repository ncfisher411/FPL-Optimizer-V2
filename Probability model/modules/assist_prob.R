#---------------------------------------#
# Assists for FPL Probability Model
# Written by: ncfisher
# Last updated: June 23 2026
#---------------------------------------#

## In this module we want to calculate the probability of scoring
### Weighted xA calculation

#### By player - overall avg xA per match played
player <- combined_data %>%
  mutate(games_played = ifelse(as.numeric(minutes) > 0, 1, 0)) %>%
  group_by(name) %>%
  summarize(xA_player = mean(as.numeric(assists), na.rm = T)) %>%
  ungroup()

#### By opponent - avg per match xA played against by player
opponent <- combined_data %>%
  mutate(games_played = ifelse(as.numeric(minutes) > 0, 1, 0)) %>%
  group_by(name, opponent_name) %>%
  summarize(xA_opp = mean(as.numeric(assists), na.rm = T)) %>%
  ungroup() %>%
  filter(!is.na(opponent_name))

#### By team - avg per match xA against by team
team <- combined_data %>%
  mutate(games_played = ifelse(as.numeric(minutes) > 0, 1, 0)) %>%
  group_by(team, opponent_name) %>%
  summarize(xA_team = mean(as.numeric(assists), na.rm = T)) %>%
  ungroup() %>%
  filter(!is.na(opponent_name))

#### By home/away - avg per match xA against by player by home or away
ha <- combined_data %>%
  mutate(games_played = ifelse(as.numeric(minutes) > 0, 1, 0)) %>%
  group_by(name, was_home) %>%
  summarize(xA_ha = mean(as.numeric(assists), na.rm = T)) %>%
  ungroup()

#### By position - avg per match xA against a team by position
position <- combined_data %>%
  mutate(games_played = ifelse(as.numeric(minutes) > 0, 1, 0)) %>%
  group_by(position, opponent_name) %>%
  summarize(xA_pos = mean(as.numeric(assists), na.rm = T)) %>%
  ungroup() %>%
  filter(!is.na(opponent_name))

### Do a weighted exercise experiment with the following weights:
assist_probs <- combined_data %>%
  select(name, season, team, opponent_name, position, was_home) %>%
  left_join(player) %>%
  left_join(opponent) %>%
  left_join(team) %>%
  left_join(ha) %>%
  left_join(position) %>%
  mutate(
    xA_calculated = (xA_player * player_assist_weight) +
      (xA_opp * opponent_assist_weight) +
      (xA_team * team_assist_weight) +
      (xA_ha * ha_assist_weight) +
      (xA_pos * position_assist_weight),
    xA_calculated = ifelse(is.nan(xA_calculated), xA_team * (1-team_assist_weight), xA_calculated),
    xA_calculated = ifelse(is.nan(xA_calculated), xA_pos * (1 - position_assist_weight), xA_calculated),
    xA_calculated = ifelse(is.nan(xA_calculated), 0, xA_calculated),
    across(starts_with('xA'), ~ifelse(is.na(.), 0, .))
  )

### Adding a step that will work for promoted teams without previous stats - really just Sunderland - if needed
status <- grepl('TRUE', unique(fixtures$finished))

if(status=='FALSE'){
  
  teams <- c('Burnley', 'Leeds', 'Ipswich', 'Leicester', 'Luton', 'Sheffield Utd', 'Southampton',
             'Hull City', 'Coventry City')
  
  ### Get the team data
  temp <- assist_probs %>% filter(team %in% teams) %>%
    group_by(opponent_name, was_home) %>% 
    summarize(across(starts_with('xA'), ~mean(., na.rm = T))) %>%
    ungroup() %>%
    mutate(team = 'Sunderland')
  
  temp <- assist_probs %>% filter(team=='Sunderland') %>%
    select(name, season, team, opponent_name, position, was_home) %>%
    left_join(temp)
  
  assist_probs <- assist_probs %>% filter(team!='Sunderland') %>%
    rbind(temp)
  
  ### Get the opponent data
  temp <- assist_probs %>% filter(opponent_name %in% teams) %>%
    group_by(team, was_home) %>% 
    summarize(across(starts_with('xA'), ~mean(., na.rm = T))) %>%
    ungroup() %>%
    mutate(opponent_name = 'Sunderland')
  
  temp <- assist_probs %>% filter(opponent_name=='Sunderland') %>%
    select(name, season, team, opponent_name, position, was_home) %>%
    left_join(temp)
  
  assist_probs <- assist_probs %>% filter(opponent_name!='Sunderland') %>%
    rbind(temp)
}

objects <- ls()
keep <- objects[grep('combined_data|test|fixture|team|current_players|probs|understat|weight', objects)]
rm(list=setdiff(objects, keep))
gc()
