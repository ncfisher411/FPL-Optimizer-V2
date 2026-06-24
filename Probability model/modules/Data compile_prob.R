#---------------------------------------#
# Data compile script for FPL Probability Model
# Written by: ncfisher
# Last updated: June 23 2026
#---------------------------------------#

## Start by compiling data sources - need the following measures conditional on being home/away:
### Goals - calculate the npxG
### Assists - calculate the xA
### Yellow/red cards
### Minutes played (convert to played or played 60 probabilities)
### Clean sheets
### Goals conceded
### Penalty saves
### Penalty misses
### Own goals
### Bonus points

## Data sources:
### vaastav Github CSVs - Github user that compiles data by year for each match week - use this for X season through 2024/25 season
##### Link: https://github.com/vaastav/Fantasy-Premier-League/tree/master
### FPL API call to get the current year data when it is available

## Step 1) Use the vaastav datasets to get data from 2021-22 through most recent season
#### THIS STEP ONLY NEEDS TO BE UNCOMMENTED AND RUN WHEN UPDATING THE COMBINED_DATA FILE WITH NEW SEASON DATA
# 
# seasons <- c('2022-23', '2023-24', '2024-25')
# matches <- c(1:38)
# teams_by_week <- data.frame()
# combined_data <- data.frame()
# 
# for(i in seasons){
#   for(j in matches){
#     temp2 <- read.csv(paste0('https://raw.githubusercontent.com/vaastav/Fantasy-Premier-League/refs/heads/master/data/', i, '/teams.csv')) %>%
#       select(id, name) %>%
#       rename(opponent_name=name)
#     temp <- read.csv(paste0('https://raw.githubusercontent.com/vaastav/Fantasy-Premier-League/refs/heads/master/data/', i, '/gws/gw', j, '.csv'), encoding = 'UTF-8') %>%
#       mutate(season = as.numeric(str_sub(i, start = 1, end = 4))) %>%
#       left_join(temp2, by = c('opponent_team'='id')) %>%
#       select(name, position, team, season, fixture, opponent_name)
#     teams_by_week <- rbind(teams_by_week, temp)
#   }
# }
# 
# for(i in seasons){
#   players <- read.csv(paste0('https://raw.githubusercontent.com/vaastav/Fantasy-Premier-League/refs/heads/master/data/', i, '/player_idlist.csv'), encoding = 'UTF-8') %>%
#     mutate(combined = paste0(first_name, '_', second_name, '_', id))
#   for(j in players$combined){
#     temp <- players %>%
#       filter(combined==j) %>%
#       mutate(name2 = paste0(first_name, ' ', second_name),
#              combined = ifelse(grepl(' ', combined), gsub(' ', '%20', combined), combined))
#     data <- read.csv(paste0('https://raw.githubusercontent.com/vaastav/Fantasy-Premier-League/refs/heads/master/data/', i, '/players/', temp$combined, '/gw.csv'), encoding = 'UTF-8') %>%
#       mutate(name = temp$name2,
#              season = as.numeric(str_sub(i, start = 1, end = 4))) %>%
#       left_join(teams_by_week) %>%
#       select(name, position, team, season, assists, bonus, bps, clean_sheets, creativity, element, expected_assists,
#              expected_goal_involvements, expected_goals, expected_goals_conceded, fixture, goals_conceded,
#              goals_scored, ict_index, influence, kickoff_time, minutes, opponent_team, opponent_name, own_goals,
#              penalties_missed, penalties_saved, red_cards, round, saves, selected, starts, team_a_score,
#              team_h_score, threat, total_points, transfers_balance, transfers_in, transfers_out,
#              value, was_home, yellow_cards)
#     combined_data <- rbind(combined_data, data)
#   }
# }
# 
# write.csv(combined_data, 'data/Combined_data.csv', row.names = F)

combined_data <- read.csv('data/Combined_data.csv') %>%
  # select(-fixture) %>%
  filter(position!='AM') %>%
  mutate(name = stri_trans_general(name, 'LATIN-ASCII'),
         name = ifelse(grepl('Becker', name), 'Alisson Becker', name),
         name = ifelse(grepl('Antony', name), 'Antony dos Santos', name),
         defensive_contribution = 0)

## Step 2) Compile the current season data

### Get current season gameweeks if available
url <- 'https://fantasy.premierleague.com/api/bootstrap-static/'
json <- GET(url)
json <- content(json, 'text')
ls <- fromJSON(json)
teams <- ls$teams

url <- 'https://fantasy.premierleague.com/api/fixtures/'
json <- GET(url)
json <- content(json, 'text')
fixtures <- fromJSON(json) %>%
  rename(GW=event) %>%
  select(GW, id, finished, kickoff_time, team_a, team_h, team_a_score, team_h_score) %>%
  left_join(teams %>% select(name, id, strength), by=c('team_a'='id')) %>%
  rename(opponent=name,
         difficulty=strength,
         GW_id=id) %>%
  left_join(teams %>% select(name, id, strength), by=c('team_h'='id')) %>%
  mutate(team=name,
         h_a='h',
         season=as.numeric(substr(kickoff_time, start = 1, stop = 4)),
         season=min(season, na.rm = T)) %>%
  select(GW, GW_id, season, finished, kickoff_time, team, h_a, strength,
         opponent, difficulty)

fixtures <- fixtures %>%
  rbind(fixtures %>% rename(opponent=team, team=opponent, strength=difficulty, difficulty=strength) %>%
          mutate(h_a='a')) %>% arrange(GW, GW_id)

### Now get the max season - do not need to do an extraction if the data already exists
test <- max(fixtures$season)==max(combined_data$season)

if(test==FALSE){
  url <- 'https://fantasy.premierleague.com/api/bootstrap-static/'
  json <- GET(url)
  json <- content(json, 'text')
  ls <- fromJSON(json)
  
  ids <- ls$elements %>%
    mutate(position=ifelse(element_type==1, 'GKP', NA),
           position=ifelse(element_type==2, 'DEF', position),
           position=ifelse(element_type==3, 'MID', position),
           position=ifelse(element_type==4, 'FWD', position),
           name=paste0(first_name,' ', second_name)) %>%
    filter(status!='u') %>%
    left_join(teams %>% rename(team_name=name) %>% select(-position), by=c('team'='id')) %>%
    mutate(team=team_name, value=value_season) %>%
    select(id, web_name, position, name, status, value, team)
  
  df2 <- data.frame()
  max <- fixtures %>% filter(finished==F)
  max <- max(max$GW)
  
  for (i in ids$id) {
    url <- paste0('https://fantasy.premierleague.com/api/element-summary/', i, '/')
    json <- GET(url)
    json <- content(json, 'text')
    ls <- fromJSON(json)
    d <- ls$history
    if(length(d)!=0) {
      
      d <- d  %>%
        mutate(season=min(fixtures$season)) %>%
        left_join(ids, by=c('element'='id')) %>%
        left_join(teams %>% select(id, name) %>% rename(opponent_name = name), by=c('opponent_team'='id')) %>%
        mutate(h_a=ifelse(was_home=='TRUE', 'h', 'a'),
               team_score=ifelse(h_a=='h', team_h_score, team_a_score),
               opponent_score=ifelse(h_a=='h', team_a_score, team_h_score),
               kickoff_time=substr(kickoff_time, start = 1, stop = 10),
               value=value.x)
      
      d2 <- fixtures %>% filter(team==d$team[[1]]) %>%
        mutate(kickoff_time=substr(kickoff_time, start = 1, stop = 10)) %>%
        left_join(d) %>%
        filter(finished=='TRUE') %>%
        mutate(name=d$name[[1]],
               name = stri_trans_general(name, 'LATIN-ASCII'),
               web_name=d$web_name[[1]],
               position=d$position[[1]],
               id=d$element[[1]]) %>%
        select(name, position, team, season, bonus, bps, clean_sheets, creativity, element, expected_assists,
               expected_goal_involvements, expected_goals, expected_goals_conceded, goals_conceded, defensive_contribution,
               goals_scored, assists, ict_index, influence, kickoff_time, minutes, opponent_name, own_goals,
               penalties_missed, penalties_saved, red_cards, round, saves, selected, starts, team_a_score,
               team_h_score, threat, total_points, transfers_balance, transfers_in, transfers_out,
               value, was_home, yellow_cards)
      
    } else if(length(d)==0) {
      
      d2 <- fixtures %>%
        left_join(ids %>% filter(id==i)) %>%
        mutate(total_points=NA, goals_scored=NA, assists = NA, expected_goals=NA, expected_assists=NA, expected_goal_involvements = NA,
               expected_goals_conceded = NA, ict_index=NA, own_goals=NA, assists=NA, penalties_missed=NA, team_a_score=NA, team_h_score=NA,
               bonus=NA, minutes=NA, yellow_cards=NA, red_cards=NA, goals_conceded=NA, saves=NA, 
               penalties_saved=NA, team_score=NA, opponent_score=NA, bps = NA, clean_sheets = NA, 
               creativity = NA, element = NA, influence = NA, minutes = NA, saves = NA, selected = NA,
               starts = NA, threat = NA, transfers_balance = NA, transfers_in = NA, transfers_out = NA) %>%
        rename(round = GW, opponent_team = opponent) %>%
        mutate(was_home=ifelse(h_a=='h', 'TRUE', 'FALSE'),
               name = stri_trans_general(name, 'LATIN-ASCII')) %>%
        select(name, position, team, season, bonus, bps, clean_sheets, creativity, element, expected_assists,
               expected_goal_involvements, expected_goals, expected_goals_conceded, goals_conceded, defensive_contribution,
               goals_scored, assists, ict_index, influence, kickoff_time, minutes, opponent_team, own_goals,
               penalties_missed, penalties_saved, red_cards, round, saves, selected, starts, team_a_score,
               team_h_score, threat, total_points, transfers_balance, transfers_in, transfers_out,
               value, was_home, yellow_cards) %>% 
        filter(round <= max) %>% filter(!is.na(name)) %>%
        left_join(teams %>% select(id, name) %>% rename(opponent_name = name),
                  by = c('opponent_team' = 'id')) %>%
        select(-opponent_team)

    }
    
    df2 <- rbind(df2, d2)
  }
  
  if(max(df2$season)==max(combined_data$season)){
    
    combined_data <- combined_data %>% 
      select(-opponent_team) %>%
      rbind(df2)
  } else {
    
    combined_data <- combined_data %>% 
      select(-opponent_team) %>%
      rbind(df2)
    
    write.csv(combined_data, 'data/Combined_data.csv')
    
  }
  
}



url <- 'https://fantasy.premierleague.com/api/bootstrap-static/'
json <- GET(url)
json <- content(json, 'text')
temp <- fixtures %>%
  # mutate(finished=ifelse(GW==38, 'FALSE', finished)) %>%
  filter(finished=='FALSE') %>%
  filter(!is.na(GW))

ls <- fromJSON(json)
current_players <- ls$elements %>%
  mutate(position=ifelse(element_type==1, 'GKP', NA),
         position=ifelse(element_type==2, 'DEF', position),
         position=ifelse(element_type==3, 'MID', position),
         position=ifelse(element_type==4, 'FWD', position),
         name=paste0(first_name,' ', second_name),
         season = as.numeric(str_sub(ls$events$deadline_time[1], start = 1, end = 4))) %>%
  filter(status!='u') %>%
  left_join(teams %>% select(name, id) %>% rename(team_name = name), by=c('team'='id')) %>%
  select(name, season, team_name, position, status, web_name, chance_of_playing_this_round) %>%
  filter(!is.na(position)) %>%
  mutate(chance_of_playing_this_round = ifelse(is.na(chance_of_playing_this_round) & status=='a', 100, 0),
         name = stri_trans_general(name, 'LATIN-ASCII'))

### Understat data to use for modeling saves
understat_data <- read.csv('data/understat_data.csv')

understat_data <- understat_data %>%
  mutate(across(contains('_team'), ~ifelse(grepl('Manchester', .), gsub('Manchester', 'Man', .), .)),
         across(contains('_team'), ~ifelse(grepl('Wolverhampton', .), 'Wolves', .)),
         across(contains('_team'), ~ifelse(grepl('Tottenham', .), 'Spurs', .)),
         across(contains('_team'), ~ifelse(grepl('United', .), gsub('United', 'Utd', .), .)),
         across(contains('_team'), ~ifelse(grepl('Newcastle', .), 'Newcastle', .)),
         across(contains('_team'), ~ifelse(grepl('Forest', .), "Nott'm Forest", .)),
         player = stri_trans_general(player, 'LATIN-ASCII'))

objects <- ls()
keep <- objects[grep('combined_data|test|fixture|team|current_players|understat|weight', objects)]
rm(list=setdiff(objects, keep))
gc()
