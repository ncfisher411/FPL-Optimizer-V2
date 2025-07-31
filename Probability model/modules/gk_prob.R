#---------------------------------------#
# Goalkeeping for FPL Probability Model
# Written by: ncfisher
# Last updated: July 30 2025
#---------------------------------------#

### Probability of saving shots by player, team, based on how many shots faced

#### 1) estimate a basic model - how does the difference between xG against and
#### actual goals against translate to goal saves?
df <- combined_data %>% filter(position=='GK' & minutes > 0) %>%
  mutate(xGA_per_90 = expected_goals_conceded/(minutes/90),
         ga_per_90 = goals_conceded/(minutes/90),
         xG_conceded_difference_per_90 = xGA_per_90 - ga_per_90)

midpoint <- ceiling(nrow(df)/2)

df_train <- df[1:midpoint, ]
df_test <- df[(midpoint+1):nrow(df), ]

model <- lm(saves ~ xG_conceded_difference_per_90 + xGA_per_90, data = df_train)
summary(model)
prediction <- df_test %>%
  select(name, team, xG_conceded_difference_per_90, saves) %>%
  cbind(predict(model, newdata = df_test) %>%
          data.frame() %>%
          rename(saves_modeled=1)) %>%
  mutate(validation = saves - saves_modeled) %>%
  summarize(saves = sum(saves, na.rm = T),
            saves_modeled = sum(saves_modeled, na.rm = T),
            validation = sum(validation, na.rm = T)) %>%
  mutate(pct_diff = (saves_modeled-saves)/saves)
    #### Less than 2% difference in the test data - relatively strong predictor for goalkeepers

#### 2) set up the xG parameters to do an average by player and team
status <- grepl('TRUE', unique(fixtures$finished))

if(status=='FALSE'){
  
  xg_against_team <- combined_data %>%
    filter(position=='GK') %>%
    filter(season!=max(season)) %>%
    group_by(team) %>%
    summarize(xGA_per_90_team = mean(expected_goals_conceded/(minutes/90), na.rm = T),
              ga_per_90_team = mean(goals_conceded/(minutes/90), na.rm = T)) %>%
    ungroup() %>%
    mutate(xG_conceded_difference_team = xGA_per_90_team - ga_per_90_team) %>%
    mutate(across(where(is.numeric), ~ifelse(is.nan(.), 0, .)))
  
  xg_against_player <- combined_data %>%
    filter(position=='GK') %>%
    filter(season!=max(season)) %>%
    group_by(name) %>%
    summarize(xGA_per_90_player = mean(expected_goals_conceded/(minutes/90), na.rm = T),
              ga_per_90_player = mean(goals_conceded/(minutes/90), na.rm = T)) %>%
    ungroup() %>%
    mutate(xG_conceded_difference_player = xGA_per_90_player - ga_per_90_player) %>%
    mutate(across(where(is.numeric), ~ifelse(is.nan(.), 0, .)))
  
  xg_opponent <- combined_data %>%
    filter(position!='GK' & minutes > 0) %>%
    filter(season!=max(season)) %>%
    group_by(team) %>%
    summarize(xG_per_90_opponent = mean(expected_goals/(minutes/90), na.rm = T),
              g_per_90_opponent = mean(goals_scored/(minutes/90), na.rm = T)) %>%
    ungroup() %>%
    mutate(xG_difference_per_90_opponent = xG_per_90_opponent - g_per_90_opponent) %>%
    rename(opponent_team = team) %>%
    mutate(across(where(is.numeric), ~ifelse(is.nan(.), 0, .)))
  
  df2 <- current_players %>%
    mutate(position = ifelse(position=='GKP', 'GK', position)) %>%
    filter(position=='GK') %>%
    rename(team = team_name) %>%
    left_join(fixtures) %>%
    rename(opponent_team = opponent) %>%
    select(name, position, team, opponent_team) %>%
    left_join(xg_against_player) %>%
    left_join(xg_against_team) %>%
    left_join(xg_opponent) %>%
    left_join(probs_time) %>%
    mutate(across(where(is.numeric), ~ifelse(is.na(.), 0, .))) %>%
    mutate(xGA_per_90 = (player_xGA_gk_weight * xGA_per_90_player) +
             (opponent_xGA_gk_weight * xG_per_90_opponent) +
             (team_xGA_gk_weight*xGA_per_90_team),
           ga_per_90 = (player_ga_gk_weight * ga_per_90_player) +
             (opponent_ga_gk_weight * g_per_90_opponent) +
             (team_ga_gk_weight * ga_per_90_team),
           xG_conceded_difference_per_90 = xGA_per_90 - ga_per_90)
  
  ### Get the promoted team data - for 2025-26, just Sunderland
  
  teams <- c('Burnley', 'Leeds', 'Ipswich', 'Leicester', 'Luton', 'Sheffield Utd', 'Southampton')
  
  #### Get for team
  temp <- df2 %>% filter(team %in% teams) %>%
    group_by(opponent_team, position) %>% 
    summarize(across(contains('xG'), ~mean(., na.rm = T)),
              across(contains('ga'), ~mean(., na.rm = T)),
              across(contains('Prob'), ~mean(., na.rm = T)),
              across(contains('per_90'), ~mean(., na.rm = T)),
              across(contains('played'), ~mean(., na.rm = T))) %>%
    ungroup() %>%
    mutate(team = 'Sunderland')
  
  temp <- current_players %>%
    mutate(position = ifelse(position=='GKP', 'GK', position)) %>%
    rename(team = team_name) %>%
    filter(team=='Sunderland' & position=='GK') %>%
    select(name, team, position) %>%
    left_join(temp)
  
  df2 <- df2 %>% filter(team!='Sunderland') %>%
    rbind(temp) %>%
    filter(team!=opponent_team)
  
} else if(status=='TRUE'){
  
  xg_against_team <- combined_data %>%
    filter(position=='GK') %>%
    group_by(team) %>%
    summarize(xGA_per_90_team = mean(expected_goals_conceded/(minutes/90), na.rm = T),
              ga_per_90_team = mean(goals_conceded/(minutes/90), na.rm = T)) %>%
    ungroup() %>%
    mutate(xG_conceded_difference_team = xGA_per_90_team - ga_per_90_team) %>%
    mutate(across(where(is.numeric), ~ifelse(is.nan(.), 0, .)))
  
  xg_against_player <- combined_data %>%
    filter(position=='GK') %>%
    group_by(name) %>%
    summarize(xGA_per_90_player = mean(expected_goals_conceded/(minutes/90), na.rm = T),
              ga_per_90_player = mean(goals_conceded/(minutes/90), na.rm = T)) %>%
    ungroup() %>%
    mutate(xG_conceded_difference_player = xGA_per_90_player - ga_per_90_player) %>%
    mutate(across(where(is.numeric), ~ifelse(is.nan(.), 0, .)))
  
  xg_opponent <- combined_data %>%
    filter(position!='GK' & minutes > 0) %>%
    group_by(team) %>%
    summarize(xG_per_90_opponent = mean(expected_goals/(minutes/90), na.rm = T),
              g_per_90_opponent = mean(goals_scored/(minutes/90), na.rm = T)) %>%
    ungroup() %>%
    mutate(xG_difference_per_90_opponent = xG_per_90_opponent - g_per_90_opponent) %>%
    rename(opponent_team = team) %>%
    mutate(across(where(is.numeric), ~ifelse(is.nan(.), 0, .)))
  
  df2 <- current_players %>%
    mutate(position = ifelse(position=='GKP', 'GK', position)) %>%
    filter(position=='GK') %>%
    rename(team = team_name) %>%
    left_join(fixtures) %>%
    rename(opponent_team = opponent) %>%
    select(name, position, team, opponent_team) %>%
    left_join(xg_against_player) %>%
    left_join(xg_against_team) %>%
    left_join(xg_opponent) %>%
    left_join(probs_time) %>%
    mutate(across(where(is.numeric), ~ifelse(is.na(.), 0, .))) %>%
    mutate(xGA_per_90 = (player_xGA_gk_weight * xGA_per_90_player) +
             (opponent_xGA_gk_weight * xG_per_90_opponent) +
             (team_xGA_gk_weight*xGA_per_90_team),
           ga_per_90 = (player_ga_gk_weight * ga_per_90_player) +
             (opponent_ga_gk_weight * g_per_90_opponent) +
             (team_ga_gk_weight * ga_per_90_team),
           xG_conceded_difference_per_90 = xGA_per_90 - ga_per_90)
  
}


#### 3) run through the model
saves_probs <- df2 %>%
  cbind(predict(model, newdata = df2) %>%
          data.frame() %>%
          rename(saves=1)) %>%
  mutate(saves = ifelse(saves < 0, 0, saves),
         saves = saves * Prob_played) %>%
  group_by(name, team, opponent_team) %>%
  summarize(saves = mean(saves, na.rm = T)) %>%
  ungroup()

#### 4) what is the probability a certain team giving up or getting a penalty - use the understat data
df2 <- understat_data %>%
  select(season, match_id, date, home_team, away_team, h_a, situation, result) %>%
  mutate(team = ifelse(h_a=='h', away_team, home_team),
         penalty_against = ifelse(situation=='Penalty', 1, 0),
         saved_penalty = ifelse(result=='SavedShot' & situation=='Penalty', 1, 0),
         date = as.character(substr(date, 0, 10))) %>%
  group_by(team, match_id, season, date) %>%
  summarize(penalty_against = sum(penalty_against, na.rm = T),
            saved_penalty = sum(saved_penalty, na.rm = T)) %>%
  ungroup() %>%
  left_join(understat_data %>%
              select(season, match_id, date, home_team, away_team, h_a, situation, result) %>%
              mutate(team = ifelse(h_a=='a', away_team, home_team),
                     penalty_for = ifelse(situation=='Penalty', 1, 0),
                     date = as.character(substr(date, 0, 10))) %>%
              group_by(team, match_id, season, date) %>%
              summarize(penalty_for = sum(penalty_for, na.rm = T)) %>%
              ungroup()) %>%
  mutate(games = 1) %>%
  group_by(team) %>%
  mutate(penalty_against_total = sum(penalty_against, na.rm = T),
         penalty_for_total = sum(penalty_for, na.rm = T),
         saved_penalty_total = sum(saved_penalty, na.rm = T),
         games = sum(games, na.rm = T)) %>%
  ungroup()

#### 5) combined with player probability for saves
##### Want to do intersectional probability - given that a penalty is awarded, what is the probability of the player saving it
if(status=='FALSE'){
  
  df3 <- combined_data %>%
    filter(position=='GK') %>%
    filter(season != max(season)) %>%
    select(name, position, team, opponent_team, round, kickoff_time, season, was_home, penalties_saved) %>%
    mutate(date =as.character(substr(kickoff_time, 0, 10))) %>%
    left_join(df2) %>%
    left_join(temp <- df2 %>%
                group_by(team) %>%
                summarize(games = mean(games, na.rm = T),
                          penalty_against_total = mean(penalty_against_total, na.rm = T),
                          penalty_for_total = mean(penalty_for_total, na.rm = T),) %>%
                ungroup() %>%
                mutate(pen_against_prob = penalty_against_total/games,
                       pen_for_prob = penalty_for_total/games) %>%
                select(team, contains('prob'))) %>%
    group_by(name, position) %>%
    summarize(penalty_against = sum(penalty_against, na.rm = T),
              penalty_for = sum(penalty_for, na.rm = T),
              penalties_saved = sum(penalties_saved, na.rm = T),
              across(contains('prob'), ~mean(., na.rm = T))) %>%
    ungroup() %>%
    mutate(pen_prob_mean = (pen_against_prob + pen_for_prob)/2,
           pen_save_prob = ifelse(penalty_against > 0, penalties_saved/penalty_against, 0),
           pen_save_prob = pen_save_prob * pen_prob_mean)
  
  #### Account for a team not being present - Sunderland in this case
  temp <- combined_data %>%
    filter(position=='GK') %>%
    filter(season != max(season)) %>%
    filter(team %in% teams) %>%
    select(name, position, team, opponent_team, round, kickoff_time, season, was_home, penalties_saved) %>%
    mutate(date =as.character(substr(kickoff_time, 0, 10))) %>%
    left_join(df2) %>%
    left_join(temp <- df2 %>%
                group_by(team) %>%
                summarize(games = mean(games, na.rm = T),
                          penalty_against_total = mean(penalty_against_total, na.rm = T),
                          penalty_for_total = mean(penalty_for_total, na.rm = T),) %>%
                ungroup() %>%
                mutate(pen_against_prob = penalty_against_total/games,
                       pen_for_prob = penalty_for_total/games) %>%
                select(team, contains('prob'))) %>%
    group_by(position) %>%
    summarize(penalty_against_temp = sum(penalty_against, na.rm = T),
              penalty_against = mean(penalty_against, na.rm = T),
              penalty_for_temp = sum(penalty_for, na.rm = T),
              penalty_for = mean(penalty_for, na.rm = T),
              penalties_saved_temp = sum(penalties_saved, na.rm = T),
              penalties_saved = mean(penalties_saved, na.rm = T),
              across(contains('prob'), ~mean(., na.rm = T))) %>%
    ungroup() %>%
    mutate(pen_prob_mean = (pen_against_prob + pen_for_prob)/2,
           pen_save_prob = ifelse(penalty_against_temp > 0, penalties_saved_temp/penalty_against_temp, 0),
           pen_save_prob = pen_save_prob * pen_prob_mean) %>%
    select(-contains('temp'))
  
  temp <- current_players %>%
    mutate(position = ifelse(position=='GKP', 'GK', position)) %>%
    filter(position=='GK') %>%
    filter(team_name=='Sunderland' | team_name=='Leeds' | team_name=='Burnley') %>%
    select(name, position) %>%
    left_join(temp)
  
  df3 <- rbind(df3, temp)
  
  saves_probs <- saves_probs %>%
    left_join(df3 %>% select(name, pen_save_prob)) %>%
    mutate(pen_save_prob = ifelse(is.na(pen_save_prob), 0, pen_save_prob))
  
}

objects <- ls()
keep <- objects[grep('combined_data|test|fixture|team|current_players|probs|understat|weight', objects)]
rm(list=setdiff(objects, keep))
gc()
