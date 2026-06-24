#---------------------------------------#
# Clean sheet and goals conceded script for FPL Probability Model
# Written by: ncfisher
# Last updated: June 23 2026
#---------------------------------------#

# 1) Get the probability by team vs opponent that they will get a clean sheet
team <- combined_data %>%
  mutate(team_score = ifelse(was_home=='True', team_h_score, team_a_score),
         opponent_score = ifelse(was_home=='True', team_a_score, team_h_score)) %>%
  distinct(season, team, opponent_name, team_score, opponent_score) %>%
  group_by(team, opponent_name) %>%
  ungroup() %>%
  arrange(opponent_score) %>%
  filter(!is.na(team_score) & !is.na(opponent_score)) %>%
  mutate(cat = paste0('goals_conceded_', opponent_score),
         count = 1) %>%
  pivot_wider(names_from = cat, values_from = count) %>%
  select(-season, -contains('score')) %>%
  group_by(team, opponent_name) %>%
  summarize(across(starts_with('goal'), ~sum(., na.rm = T))) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(count = sum(c_across(starts_with('goals')))) %>%
  ungroup() %>%
  mutate(across(starts_with('goals'), ~./count)) %>%
  select(-count) %>%
  pivot_longer(cols = -c('team', 'opponent_name'), names_to = 'cat', values_to = 'probabilities') %>%
  filter(cat=='goals_conceded_0' | cat=='goals_conceded_2' | cat=='goals_conceded_4' | cat=='goals_conceded_6' | cat=='goals_conceded_8') %>%
  mutate(cat = paste0(cat, '_team')) %>%
  pivot_wider(names_from = cat, values_from = probabilities)

# 2) Get the probability by team of home vs away
ha <- combined_data %>%
  mutate(team_score = ifelse(was_home=='True', team_h_score, team_a_score),
         opponent_score = ifelse(was_home=='True', team_a_score, team_h_score)) %>%
  distinct(season, team, opponent_name, team_score, opponent_score, .keep_all = T) %>%
  select(season, team, opponent_name, team_score, opponent_score, was_home) %>%
  group_by(team, was_home) %>%
  ungroup() %>%
  arrange(opponent_score) %>%
  filter(!is.na(team_score) & !is.na(opponent_score)) %>%
  mutate(cat = paste0('goals_conceded_', opponent_score),
         count = 1,
         seasons = length(unique(season))) %>%
  pivot_wider(names_from = cat, values_from = count) %>%
  select(-season, -contains('score')) %>%
  group_by(team, was_home) %>%
  summarize(across(starts_with('goal'), ~sum(., na.rm = T))) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(count = sum(c_across(starts_with('goals')))) %>%
  ungroup() %>%
  mutate(across(starts_with('goals'), ~./count)) %>%
  select(-count) %>%
  pivot_longer(cols = -c('team', 'was_home'), names_to = 'cat', values_to = 'probabilities') %>%
  filter(cat=='goals_conceded_0' | cat=='goals_conceded_2' | cat=='goals_conceded_4' | cat=='goals_conceded_6' | cat=='goals_conceded_8') %>%
  pivot_wider(names_from = cat, values_from = probabilities)

cs_probs <- team %>% left_join(ha)

# 3) For promoted teams that don't have a PL record - replace with the avg of teams that have most recently been relegated if necessary
## For 2025-26, this is just Sunderland
status <- grepl('TRUE', unique(fixtures$finished))

if(status=='FALSE'){
  
  teams <- c('Burnley', 'Leeds', 'Ipswich', 'Leicester', 'Luton', 'Sheffield Utd', 'Southampton',
             'Hull City', 'Coventry City')
  
  ### Get the team data
  temp <- cs_probs %>% filter(team %in% teams) %>%
    group_by(opponent_name, was_home) %>% 
    summarize(across(starts_with('goals_conceded'), ~mean(., na.rm = T))) %>%
    ungroup() %>%
    mutate(team = 'Sunderland') %>%
    filter(opponent_name!='Ipswich') %>%
    filter(opponent_name!='Leicester') %>%
    filter(opponent_name!='Luton') %>%
    filter(opponent_name!='Sheffield Utd') %>%
    filter(opponent_name!='Southampton')
  
  cs_probs <- cs_probs %>% filter(team!='Sunderland') %>%
    rbind(temp)
  
  ### Get the opponent data
  temp <- cs_probs %>% filter(opponent_name %in% teams) %>%
    group_by(team, was_home) %>% 
    summarize(across(starts_with('goals_conceded'), ~mean(., na.rm = T))) %>%
    ungroup() %>%
    mutate(opponent_name = 'Sunderland')
  
  cs_probs <- cs_probs %>% filter(opponent_name!='Sunderland') %>%
    rbind(temp)
}

objects <- ls()
keep <- objects[grep('combined_data|test|fixture|team|current_players|probs|understat|weight', objects)]
rm(list=setdiff(objects, keep))
gc()

