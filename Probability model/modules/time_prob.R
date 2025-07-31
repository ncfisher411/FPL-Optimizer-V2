#---------------------------------------#
# Time probabilities script for FPL Probability Model
# Written by: ncfisher
# Last updated: July 30 2025
#---------------------------------------#

## In this module, we set up the probabilities that a certain player will play for their team in the upcoming matches
### This will be a weighting exercise - want to have higher weights towards the more recent seasons
status <- grepl('TRUE', unique(fixtures$finished))

#### And will want to make sure we are only using past data - no current data - to inform
if(status=='FALSE'){
  
  probs_time <- combined_data %>%
    filter(season != max(season)) %>%
    mutate(games=1,
           played = ifelse(minutes > 0, 1, 0),
           played60 = ifelse(minutes >=60, 1, 0)) %>%
    group_by(name) %>%
    summarize(played = sum(played, na.rm = T),
              played60 = sum(played60, na.rm = T),
              games = sum(games, na.rm = T)) %>%
    ungroup() %>%
    mutate(Prob_played = played/games,
           Prob_played60 = played60/games)
  
} else if(status=='TRUE'){
  
  probs_time <- combined_data %>%
    mutate(games=1,
           played = ifelse(minutes > 0, 1, 0),
           played60 = ifelse(minutes >=60, 1, 0),
           season = ifelse(season < max(season), 'old', 'current')) %>%
    group_by(name, season) %>%
    summarize(played = sum(played, na.rm = T),
              played60 = sum(played60, na.rm = T),
              games = sum(games, na.rm = T)) %>%
    ungroup() %>%
    mutate(weight = ifelse(season=='current', time_weight, 1 - time_weight),
           Prob_played = (played/games) * weight,
           Prob_played60 = (played60/games) * weight) %>%
    group_by(name) %>%
    summarize(Prob_played = sum(Prob_played, na.rm = T),
              Prob_played60 = sum(Prob_played60, na.rm = T)) %>%
    ungroup()
  
}

## 2) Create a step to get promoted team data if they don't have previous PL data - temporary placeholder


if(status=='FALSE'){
  
  teams <- c('Burnley', 'Leeds', 'Ipswich', 'Leicester', 'Luton', 'Sheffield Utd', 'Southampton')
  
  ### Get by team
  temp <- probs_time %>%
    left_join(combined_data %>% select(name, team, position, opponent_team, season, minutes)) %>%
    group_by(name, season) %>%
    mutate(minutes_sum = sum(minutes, na.rm = T)) %>%
    ungroup() %>%
    filter(team %in% teams & minutes_sum >= 1000) %>%
    group_by(opponent_team, position) %>% 
    summarize(across(starts_with('Prob'), ~mean(., na.rm = T))) %>%
    ungroup() %>%
    mutate(team = 'Sunderland',
           position = ifelse(position=='GK', 'GKP', position))
  
  temp2 <- current_players %>%
    rename(team = team_name) %>%
    select(name, season, position, team) %>%
    left_join(fixtures) %>%
    rename(opponent_team = opponent) %>%
    filter(team %in% temp$team) %>%
    left_join(temp) %>%
    group_by(name, season) %>%
    summarize(across(starts_with('Prob'), ~mean(., na.rm = T))) %>%
    ungroup() %>%
    mutate(games = 38,
           played = Prob_played * games,
           played60 = Prob_played60 * games) %>%
    select(-season)
  
  probs_time <- probs_time %>%
    filter(!(name %in% temp2$name)) %>%
    rbind(temp2)
  
}

objects <- ls()
keep <- objects[grep('combined_data|test|fixture|team|current_players|probs|understat|weight', objects)]
rm(list=setdiff(objects, keep))
gc()
