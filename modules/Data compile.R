# Use this script to call the FPL API to get updated data
# First compile the vaastav and understat data
## To start load understat data for past seasons
### This step takes a long time to run so will just save this as a CSV and call it from the directory
# years <- c(2021:2024)
#
# df <- data.frame()
#
# for (i in years) {
#   temp <- understat_league_season_shots('EPL', i) %>%
#     mutate(goal=ifelse(result=='Goal',1,0),
#            team=ifelse(h_a=='h', home_team, away_team),
#            opponent=ifelse(h_a=='h', away_team, home_team),
#            team_goals=ifelse(h_a=='h', home_goals, away_goals),
#            opponent_goals=ifelse(h_a=='h', away_goals, home_goals),
#            date=substr(date, start = 1, stop = 10)) %>%
#     select(player, player_id, season, team, match_id, date, h_a, opponent, team_goals, opponent_goals, goal, xG) %>%
#     group_by(player, player_id, season, team, match_id, date, h_a, opponent) %>%
#     summarize(team_goals=mean(team_goals, na.rm = T),
#               opponent_goals=mean(opponent_goals, na.rm = T),
#               goals=sum(goal, na.rm = T), xG=sum(xG, na.rm = T)) %>%
#     ungroup()
#   df <- rbind(df, temp)
# }
#
# write.csv(df, 'data/understat_xg_by_match.csv', row.names = F)
# temp <- read.csv('https://raw.githubusercontent.com/vaastav/Fantasy-Premier-League/master/data/2021-22/id_dict.csv',
#                  encoding = 'UTF-8') %>% mutate(season=2021) %>%
#   rbind(read.csv('https://raw.githubusercontent.com/vaastav/Fantasy-Premier-League/master/data/2022-23/id_dict.csv',
#                  encoding = 'UTF-8') %>% mutate(season=2022))
#
# df <- read.csv('https://raw.githubusercontent.com/vaastav/Fantasy-Premier-League/master/data/2021-22/gws/merged_gw.csv',
#                   encoding = 'UTF-8') %>%
#   mutate(h_a=ifelse(was_home=='True', 'h', 'a')) %>%
#   select(name, position, team, assists, ict_index, influence, kickoff_time, opponent_team,
#          team_a_score, team_h_score, h_a, GW) %>%
#     left_join(temp, by=c('name'='FPL_Name')) %>%
#   filter(!is.na(season)) %>%
#   left_join(read.csv('https://raw.githubusercontent.com/vaastav/Fantasy-Premier-League/master/data/2021-22/teams.csv',
#                      encoding = 'UTF-8') %>% select(id, name) %>%
#               rename(opponent=name),
#             by = c('opponent_team'='id')) %>%
#   select(-opponent_team)
#
# ## Temporarily cloned repo, but combined into full csv file and saved into data folder
# dir <- paste0(getwd(), '/data/temp/Fantasy-Premier-League/data/2021-22/understat')
#
# files <- list.files(dir) %>% data.frame() %>%
#   filter(!grepl('understat', .))
#
# df2 <- data.frame()
#
# for (i in files$.) {
#   data <- read.csv(paste0(dir, '/', i)) %>%
#     mutate(name=i)
#   name_split <- strsplit(as.character(i), '_')
#   data <- data %>%
#     mutate(name=gsub('_', ' ', name),
#            name=gsub('1|2|3|4|5|6|7|8|9|0', '', name),
#            name=gsub('.csv', '', name))  %>%
#     mutate(name=ifelse(grepl(' NA', name), gsub(' NA', '', name), name),
#            season=2021) %>%
#     filter(grepl('2021|2022', date))
#
#   df2 <- rbind(df2, data)
# }
#
# dir <- paste0(getwd(), '/data/temp/Fantasy-Premier-League/data/2022-23/understat')
#
# files <- list.files(dir) %>% data.frame() %>%
#   filter(!grepl('understat', .))
#
# for (i in files$.) {
#   data <- read.csv(paste0(dir, '/', i)) %>%
#     mutate(name=i)
#   data <- data %>%
#     mutate(name=gsub('_', ' ', name),
#            name=gsub('1|2|3|4|5|6|7|8|9|0', '', name),
#            name=gsub('.csv', '', name)) %>%
#     mutate(name=ifelse(grepl(' NA', name), gsub(' NA', '', name), name),
#            season=2022) %>%
#     filter(grepl('2022|2023', date))
#
#   df2 <- rbind(df2, data)
# }
#
# remove_trailing_spaces <- function(x) {
#   if(substr(x, nchar(x), nchar(x)) == ' '){
#     return(substring(x, 1, nchar(x)-1))
#   } else{
#     return(x)
#   }
# }
#
# df2$name <- sapply(df2$name, remove_trailing_spaces)
#
# df3 <- df2 %>% left_join(temp, by=c('name'='Understat_Name', 'season')) %>%
#   filter(!is.na(FPL_Name)) %>%
#   select(FPL_Name, xA, key_passes, date, season)
# ## Save this here - fpl data for 2023/24 has xA so don't need to crosswalk with understat
# write.csv(df3, 'data/xA_understat.csv', row.names = F)

xg <- read.csv('data/understat_xg_by_match.csv') %>%
  arrange(player, season, date) %>%
  select(player, season, date, xG) %>%
  filter(season < 2023) %>%
  left_join(
    rbind(
      read.csv('https://raw.githubusercontent.com/vaastav/Fantasy-Premier-League/master/data/2021-22/id_dict.csv',
               encoding = 'UTF-8') %>%
        mutate(season=2021),
      read.csv('https://raw.githubusercontent.com/vaastav/Fantasy-Premier-League/master/data/2022-23/id_dict.csv',
               encoding = 'UTF-8') %>%
        mutate(season=2022)
    ), by=c('player'='Understat_Name', 'season')
  ) %>% mutate(player=FPL_Name) %>%
  select(-Understat_ID, -FPL_ID, -FPL_Name) %>%
  distinct(player, date, .keep_all = T)

xA <- read.csv('data/xA_understat.csv') %>%
  filter(season < 2023) %>%
  select(-key_passes)

df <- rbind(
  read.csv('https://raw.githubusercontent.com/vaastav/Fantasy-Premier-League/master/data/2021-22/gws/merged_gw.csv',
           encoding = 'UTF-8') %>%
    select(name, team, position, kickoff_time, ict_index, goals_scored, assists, own_goals,
           penalties_missed, round, element, opponent_team, team_a_score, team_h_score, was_home,
           bonus, minutes, yellow_cards, red_cards, goals_conceded, saves, penalties_saved,
           total_points, value
    ) %>%
    rename(GW=round, id=element, goals=goals_scored, h_a=was_home) %>%
    mutate(h_a=ifelse(h_a=='True', 'h', 'a'),
           team_score=ifelse(h_a=='h', team_h_score, team_a_score),
           opponent_score=ifelse(h_a=='h', team_a_score, team_h_score),
           kickoff_time=substr(kickoff_time, start = 1, stop = 10),
           season=as.numeric(substr(kickoff_time, start = 1, stop = 4)),
           season=min(season)) %>%
    left_join(
      read.csv('https://raw.githubusercontent.com/vaastav/Fantasy-Premier-League/master/data/2021-22/teams.csv',
               encoding='UTF-8') %>%
        select(id, name, strength) %>%
        rename(opponent=name, difficulty=strength), by=c('opponent_team' = 'id')
    ) %>%
    left_join(
      read.csv('https://raw.githubusercontent.com/vaastav/Fantasy-Premier-League/master/data/2021-22/teams.csv',
               encoding='UTF-8') %>%
        select(name, strength), by=c('team' = 'name')
    ),
  read.csv('https://raw.githubusercontent.com/vaastav/Fantasy-Premier-League/master/data/2022-23/gws/merged_gw.csv',
           encoding = 'UTF-8') %>%
    select(name, team, position, kickoff_time, ict_index, goals_scored, assists, own_goals,
           penalties_missed, round, element, opponent_team, team_a_score, team_h_score, was_home,
           bonus, minutes, yellow_cards, red_cards, goals_conceded, saves, penalties_saved,
           total_points, value
    ) %>%
    rename(GW=round, id=element, goals=goals_scored, h_a=was_home) %>%
    mutate(h_a=ifelse(h_a=='True', 'h', 'a'),
           team_score=ifelse(h_a=='h', team_h_score, team_a_score),
           opponent_score=ifelse(h_a=='h', team_a_score, team_h_score),
           kickoff_time=substr(kickoff_time, start = 1, stop = 10),
           season=as.numeric(substr(kickoff_time, start = 1, stop = 4)),
           season=min(season)) %>%
    left_join(
      read.csv('https://raw.githubusercontent.com/vaastav/Fantasy-Premier-League/master/data/2022-23/teams.csv',
               encoding='UTF-8') %>%
        select(id, name, strength) %>%
        rename(opponent=name, difficulty=strength), by=c('opponent_team' = 'id')
    ) %>%
    left_join(
      read.csv('https://raw.githubusercontent.com/vaastav/Fantasy-Premier-League/master/data/2022-23/teams.csv',
               encoding='UTF-8') %>%
        select(name, strength), by=c('team' = 'name')
    )
) %>% left_join(
  xg, by=c('name'='player', 'season', 'kickoff_time'='date')
) %>% left_join(
  xA, by=c('name'='FPL_Name', 'season', 'kickoff_time'='date')
) %>% rbind(
  read.csv('https://raw.githubusercontent.com/vaastav/Fantasy-Premier-League/master/data/2023-24/gws/merged_gw.csv',
           encoding = 'UTF-8') %>%
    select(name, team, position, kickoff_time, ict_index, goals_scored, assists,own_goals,
           penalties_missed, round, element, opponent_team, team_a_score, team_h_score, was_home,
           bonus, minutes, yellow_cards, red_cards, goals_conceded, saves, penalties_saved,
           expected_goals, expected_assists, total_points, value
    ) %>%
    rename(GW=round, id=element, goals=goals_scored, h_a=was_home,
           xG=expected_goals, xA=expected_assists) %>%
    mutate(h_a=ifelse(h_a=='True', 'h', 'a'),
           team_score=ifelse(h_a=='h', team_h_score, team_a_score),
           opponent_score=ifelse(h_a=='h', team_a_score, team_h_score),
           kickoff_time=substr(kickoff_time, start = 1, stop = 10),
           season=as.numeric(substr(kickoff_time, start = 1, stop = 4)),
           season=min(season)) %>%
    left_join(
      read.csv('https://raw.githubusercontent.com/vaastav/Fantasy-Premier-League/master/data/2023-24/teams.csv',
               encoding='UTF-8') %>%
        select(id, name, strength) %>%
        rename(opponent=name, difficulty=strength), by=c('opponent_team' = 'id')
    ) %>%
    left_join(
      read.csv('https://raw.githubusercontent.com/vaastav/Fantasy-Premier-League/master/data/2023-24/teams.csv',
               encoding='UTF-8') %>%
        select(name, strength), by=c('team' = 'name')
    )
) %>% #distinct(name, season, GW, .keep_all = T) %>%
  mutate(opponent=ifelse(opponent=='Manchester City', 'Man City', opponent),
         opponent=ifelse(opponent=='Manchester United', 'Man Utd', opponent),
         opponent=ifelse(opponent=='Newcastle United', 'Newcastle', opponent),
         opponent=ifelse(opponent=='Wolverhampton Wanderers', 'Wolves', opponent),
         opponent=ifelse(opponent=='Spurs', 'Tottenham', opponent),
         opponent=ifelse(grepl('Nott', opponent), 'Forest', opponent),
         opponent=ifelse(grepl('Sheffield', opponent), 'Sheffield', opponent),
         team=ifelse(team=='Manchester City', 'Man City', team),
         team=ifelse(team=='Manchester United', 'Man Utd', team),
         team=ifelse(team=='Newcastle United', 'Newcastle', team),
         team=ifelse(team=='Wolverhampton Wanderers', 'Wolves', team),
         team=ifelse(team=='Spurs', 'Tottenham', team),
         team=ifelse(grepl('Nott', team), 'Forest', team),
         team=ifelse(grepl('Sheffield', team), 'Sheffield', team),
         position=ifelse(grepl('GK', position), 'GKP', position),
         xG=ifelse(is.na(xG), 0, xG),
         xA=ifelse(is.na(xA), 0, xA),
         finished='TRUE') %>%
  select(-opponent_team)

# temp <- df %>% filter(season==2023) %>%
#   group_by(name, team, position, season) %>%
#   summarize(total_points=sum(total_points, na.rm = T),
#             goals=sum(goals, na.rm = T),
#             assists=sum(assists, na.rm = T),
#             own_goals=sum(own_goals, na.rm = T),
#             penalties_missed=sum(penalties_missed, na.rm = T),
#             bonus=sum(bonus, na.rm = T),
#             minutes=sum(minutes, na.rm = T),
#             yellow_cards=sum(yellow_cards, na.rm = T),
#             red_cards=sum(red_cards, na.rm = T),
#             goals_conceded=sum(goals_conceded, na.rm = T),
#             saves=sum(saves, na.rm = T),
#             penalties_saved=sum(penalties_saved, na.rm = T),
#             xG=sum(xG, na.rm = T),
#             xA=sum(xA, na.rm = T))
# 
# write.xlsx(temp, 'results_2324.xlsx')

## Call API for current matches
url <- 'https://fantasy.premierleague.com/api/bootstrap-static/'
json <- GET(url)
json <- content(json, 'text')
ls <- fromJSON(json)
teams <- ls$teams %>%
  select(name, id, strength) %>%
  mutate(name=ifelse(name=='Manchester City', 'Man City', name),
         name=ifelse(name=='Manchester United', 'Man Utd', name),
         name=ifelse(name=='Newcastle United', 'Newcastle', name),
         name=ifelse(name=='Wolverhampton Wanderers', 'Wolves', name),
         name=ifelse(name=='Spurs', 'Tottenham', name),
         name=ifelse(grepl('Nott', name), 'Forest', name),
         name=ifelse(grepl('Sheffield', name), 'Sheffield', name))

url <- 'https://fantasy.premierleague.com/api/fixtures/'
json <- GET(url)
json <- content(json, 'text')
fixtures <- fromJSON(json) %>%
  rename(GW=event) %>%
  select(GW, id, finished, kickoff_time, team_a, team_h, team_a_score, team_h_score) %>%
  left_join(teams, by=c('team_a'='id')) %>%
  rename(opponent=name,
         difficulty=strength,
         GW_id=id) %>%
  left_join(teams, by=c('team_h'='id')) %>%
  mutate(team=name,
         h_a='h',
         season=as.numeric(substr(kickoff_time, start = 1, stop = 4)),
         season=min(season)) %>%
  select(GW, GW_id, season, finished, kickoff_time, team, h_a, strength,
         opponent, difficulty)

fixtures <- fixtures %>%
  rbind(fixtures %>% rename(opponent=team, team=opponent, strength=difficulty, difficulty=strength) %>%
          mutate(h_a='a')) %>% arrange(GW, GW_id)

## Call API for current players
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
  left_join(teams %>% rename(team_name=name), by=c('team'='id')) %>%
  mutate(team=team_name, value=value_season) %>%
  select(id, web_name, position, name, status, value, team)

df2 <- data.frame()
max <- fixtures %>% filter(finished==T)
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
             web_name=d$web_name[[1]],
             position=d$position[[1]],
             id=d$element[[1]]) %>%
      rename(goals=goals_scored, xG=expected_goals, xA=expected_assists) %>%
      select(name, web_name, team, position, GW, kickoff_time, ict_index, goals, own_goals,
             assists, penalties_missed, id, team_a_score, team_h_score, h_a,
             bonus, minutes, yellow_cards, red_cards, goals_conceded, saves,
             penalties_saved, team_score, opponent_score, season, opponent, difficulty,
             strength, xG, xA, total_points, value, finished)
    
    temp <- c('name', 'web_name', 'team', 'position', 'kickoff_time', 'h_a', 'opponent', 'finished')
    
    d2 <- d2 %>%
      mutate_at(vars(-one_of(temp)), as.numeric)
    
  } else if(length(d)==0) {
    
    d2 <- fixtures %>%
      left_join(ids %>% filter(id==i)) %>%
      mutate(total_points=NA, goals=NA, xG=NA, xA=NA, ict_index=NA, own_goals=NA, assists=NA, penalties_missed=NA,
             team_a_score=NA, team_h_score=NA, bonus=NA, minutes=NA, yellow_cards=NA, red_cards=NA,
             goals_conceded=NA, saves=NA, penalties_saved=NA, team_score=NA, opponent_score=NA) %>%
      select(name, web_name, team, position, GW, kickoff_time, ict_index, goals, own_goals,
             assists, penalties_missed, id, opponent, team_a_score, team_h_score, h_a,
             bonus, minutes, yellow_cards, red_cards, goals_conceded, saves,
             penalties_saved, team_score, opponent_score, season, difficulty,
             strength, xG, xA, total_points, value, finished) %>% 
      filter(GW <= max) %>% filter(!is.na(name))
    
    temp <- c('name', 'web_name', 'team', 'position', 'kickoff_time', 'h_a', 'opponent', 'finished')
    
    d2 <- d2 %>%
      mutate_at(vars(-one_of(temp)), as.numeric)
    

  }
  
  df2 <- rbind(df2, d2)
}

## Deal with transfers here
url <- tm_league_team_urls(country_name = 'England', start_year = max(df2$season))
dict <- player_dictionary_mapping()

### Transfers by team
transfers <- data.frame()
for(i in url) {
  temp <- tm_team_transfers(team_url = i, transfer_window = 'all')
  transfers <- rbind(transfers, temp)
}

### Get arrivals data
arrivals <- transfers %>% filter(transfer_type=='Arrivals')
dict2 <- dict %>% filter(UrlTmarkt %in% arrivals$player_url)

### Get a market value for each league based on TM player valuations; use as imperfect proxy for level of competition
###### This step takes a long time to run; comment and save a csv for easy loading; push changes to github at beginning of each season
# comps <- read.csv('https://raw.githubusercontent.com/JaseZiv/worldfootballR_data/master/raw-data/transfermarkt_leagues/main_comp_seasons.csv', encoding = 'UTF-8') %>%
#   group_by(comp_name) %>%
#   filter(season_start_year==max(season_start_year)) %>%
#   ungroup()
#
# count <- c(1:nrow(comps))
# values <- data.frame()
#
# for(c in count) {
#   temp <- tm_player_market_values(start_year = comps$season_start_year[[c]], league_url = comps$comp_url[[c]]) %>%
#     mutate(n=1) %>%
#     group_by(country, comp_name) %>%
#     summarize(total_players=sum(n, na.rm = T),
#               mean_value_euros=mean(player_market_value_euro, na.rm = T),
#               total_value_euros=sum(player_market_value_euro, na.rm = T)) %>%
#     ungroup()
#
#   values <- rbind(values, temp)
#
# }
#
# write.csv(values, 'data/league_values.csv', row.names = F)

values <- read.csv('data/league_values.csv') %>% arrange(-mean_value_euros) %>%
  mutate(percentile=percent_rank(mean_value_euros),
         coef=ifelse(percentile >= 0.75, 0.9, 0),
         coef=ifelse(percentile <= 0.75, 0.7, coef),
         coef=ifelse(percentile <= 0.5, 0.5, coef),
         coef=ifelse(percentile <= 0.25, 0.3, coef),
         coef=ifelse(comp_name=='Premier League', 1, coef)) %>%
  select(country, comp_name, coef)

##### See variation in stats based on difficulty using "df"
difficulty_matrix <- df %>%
  group_by(difficulty) %>%
  summarize(goals=mean(goals, na.rm = T),
            own_goals=mean(own_goals, na.rm = T),
            assists=mean(assists, na.rm = T),
            penalties_missed=mean(penalties_missed, na.rm = T),
            yellow_cards=mean(yellow_cards),
            red_cards=mean(red_cards),
            saves=mean(saves, na.rm = T),
            penalties_saved=mean(penalties_saved, na.rm = T),
            xG=mean(xG, na.rm = T),
            xA=mean(xA, na.rm = T),
            goals_conceded = mean(goals_conceded, na.rm = T),
            bonus=mean(bonus, na.rm = T)) %>%
  ungroup() %>%
  mutate(
    goals_scale=goals/goals[[2]],
    own_goals_scale=own_goals/own_goals[[2]],
    assists_scale=assists/assists[[2]],
    penalties_missed_scale=penalties_missed/penalties_missed[[2]],
    yellow_cards_scale=yellow_cards/yellow_cards[[2]],
    red_cards_scale=red_cards/red_cards[[2]],
    saves_scale=saves/saves[[2]],
    penalties_saved_scale=penalties_saved/penalties_saved[[2]],
    xG_scale=xG/xG[[2]],
    xA_scale=xA/xA[[2]],
    goals_conceded_scale=goals_conceded/goals_conceded[[2]],
    bonus_scale=bonus/bonus[[2]]
  ) %>%
  select(difficulty, contains('scale'))

# Separate the estimation and validation data
est_data <- df %>%
  rbind(df2 %>% select(-web_name) %>% filter(finished=='TRUE')) %>%
  mutate(played = ifelse(minutes > 0, 1, 0),
         played60 = ifelse(minutes > 59, 1, 0),
         clean_sheet=ifelse(played60==1 & goals_conceded==0, 1, 0))

temp <- est_data %>%
  rename(team=opponent, opponent=team) %>%
  group_by(team, season, GW) %>%
  summarize(xG_opponent=sum(xG, na.rm = T),
            ict_index_opponent=mean(ict_index, na.rm = T)) %>% 
  ungroup()

est_data <- est_data %>% left_join(temp) %>%
  mutate(xG_opponent=ifelse(is.na(xG_opponent), median(xG_opponent), xG_opponent),
         xG_opponent=xG_opponent*(minutes/90),
         ict_index_opponent=ifelse(is.na(ict_index_opponent), median(ict_index_opponent), ict_index_opponent),
         ict_index_opponent=ict_index_opponent*(minutes/90))

val_data <- df2 %>% mutate_all(., replace_na, 0) %>%
  filter(finished=='TRUE') %>%
  mutate(played = ifelse(minutes > 0, 1, 0),
         played60 = ifelse(minutes > 59, 1, 0),
         clean_sheet=ifelse(played60==1 & goals_conceded==0, 1, 0))

temp <- val_data %>%
  rename(team=opponent, opponent=team) %>%
  group_by(team, season, GW) %>%
  summarize(xG_opponent=sum(xG, na.rm = T),
            ict_index_opponent=mean(ict_index, na.rm = T)) %>% 
  ungroup()

val_data <- val_data %>% left_join(temp) %>%
  mutate(xG_opponent=ifelse(is.na(xG_opponent), median(xG_opponent), xG_opponent),
         xG_opponent=xG_opponent*(minutes/90),
         ict_index_opponent=ifelse(is.na(ict_index_opponent), median(ict_index_opponent), ict_index_opponent),
         ict_index_opponent=ict_index_opponent*(minutes/90))

## Get est_data averages for transfers and apply league coefficients to integrate for initial estimates
transfer_data <- est_data %>%
  group_by(position, h_a, strength, difficulty) %>%
  summarize(ict_index=mean(ict_index, na.rm = T),
            goals=mean(goals, na.rm = T),
            assists=mean(assists, na.rm = T),
            own_goals=mean(own_goals, na.rm = T),
            penalties_missed=mean(penalties_missed, na.rm = T),
            bonus=mean(bonus, na.rm = T),
            minutes=mean(minutes, na.rm = T),
            yellow_cards=mean(yellow_cards, na.rm = T),
            red_cards=mean(red_cards, na.rm = T),
            goals_conceded=mean(goals_conceded, na.rm = T),
            saves=mean(saves, na.rm = T),
            penalties_saved=mean(penalties_saved, na.rm = T),
            xG=mean(xG, na.rm = T),
            xA=mean(xA, na.rm = T),
            played=mean(played, na.rm = T),
            played60=mean(played60, na.rm = T),
            clean_sheet=mean(clean_sheet, na.rm = T),
            total_points=mean(total_points, na.rm = T)) %>%
  ungroup()

df3 <- arrivals %>% filter(!grepl('End of loan', transfer_notes)) %>%
  select(player_name, league_2) %>%
  rename(name=player_name) %>%
  stringdist_inner_join(val_data, by='name') %>%
  rename(name=name.x) %>%
  select(name, web_name, league_2, position, team, kickoff_time, GW, id, 
         team_h_score, h_a, team_a_score, team_score, opponent_score, season, opponent,
         difficulty, strength, difficulty, value, finished) %>%
  left_join(transfer_data) %>%
  rename(comp_name=league_2) %>%
  left_join(values, by='comp_name') %>%
  mutate(coef=ifelse(is.na(coef), 0.3, coef))

temp <- c('goals', 'assists', 'ict_index', 'own_goals', 'penalties_missed', 'bonus',
          'minutes', 'yellow_cards', 'red_cards', 'goals_conceded', 'saves', 'penalties_saved',
          'xG', 'xA', 'total_points', 'played', 'played60', 'clean_sheet')

df3 <- df3 %>%
  mutate(across(all_of(temp), ~.x*coef)) %>%
  select(intersect(colnames(df3), colnames(est_data)))

temp <- df3 %>%
  rename(team=opponent, opponent=team) %>%
  group_by(team, season, GW) %>%
  summarize(xG_opponent=sum(xG, na.rm = T),
            ict_index_opponent=mean(ict_index, na.rm = T)) %>% 
  ungroup()

df3 <- df3 %>% left_join(temp) %>%
  mutate(xG_opponent=ifelse(is.na(xG_opponent), median(xG_opponent), xG_opponent),
         xG_opponent=xG_opponent*(minutes/90),
         ict_index_opponent=ifelse(is.na(ict_index_opponent), median(ict_index_opponent), ict_index_opponent),
         ict_index_opponent=ict_index_opponent*(minutes/90)) %>%
  mutate(season=max(val_data$season)-1)
  
for (i in unique(df3$name)) {
  
  if (!(i %in% unique(est_data$name))) {
    
    temp <- df3 %>% filter(name==i)
    
    est_data <- est_data %>% rbind(temp)
    
  }
}

### Get ranking of minutes and avg goal differneces
temp <- est_data %>%
  mutate(goal_difference=team_score-opponent_score) %>%
  group_by(name, position, season, team) %>%
  summarize(total_minutes=sum(minutes, na.rm = T),
            goal_difference=mean(goal_difference, na.rm = T)) %>%
  ungroup() %>%
  group_by(team, position, season) %>%
  mutate(rank_minutes=rank(-total_minutes/90)) %>%
  ungroup()

est_data <- est_data %>% left_join(temp) %>%
  mutate_if(is.numeric, replace_na, 0)

temp <- val_data %>%
  mutate(goal_difference=team_score-opponent_score) %>%
  group_by(name, position, season, team) %>%
  summarize(total_minutes=sum(minutes, na.rm = T),
            goal_difference=mean(goal_difference, na.rm = T)) %>%
  group_by(team, position, season) %>%
  mutate(rank_minutes=rank(-total_minutes)) %>%
  ungroup()

val_data <- val_data %>% left_join(temp) %>%
  mutate_if(is.numeric, replace_na, 0)

## Get the data averages prepared

##### Get correlations
cor <- cor(est_data %>% mutate(position=as.numeric(as.factor(position))) %>% select(where(is.numeric))) %>%
  as.data.frame()

#### In the goals model we are estimating goals, own goals, and penalties missed

##### Goals
cor2 <- cor %>% select(goals) %>% arrange(-goals)
    #### est vars = xG, ict_index, xA, value, played, played60, position, strength, difficulty, h_a

##### Own goals
cor2 <- cor %>% select(own_goals) %>% arrange(-own_goals)
    #### est vars = ict_index_opponent, xG_opponent, played, played60, position, strength, difficulty, h_a

##### Penalties missed
cor2 <- cor %>% select(penalties_missed) %>% arrange(-penalties_missed)
    #### est vars = xG, ict_index, value, position, strength, difficulty, h_a

### In the assists model we are estimating assists

##### Assists
cor2 <- cor %>% select(assists) %>% arrange(-assists)
    #### est vars = xA, ict_index, xG, value, played60, played, position, strength, difficulty, h_a

### In the bonus model we are estimating bonus points
cor2 <- cor %>% select(bonus) %>% arrange(-bonus)
    #### est vars = ict_index, xG, xA, clean_sheet, played, played60, ict_index_opponent, position, strength, difficulty, h_a

### In the cards model we are estimating yellow and red cards probability

###### Yellow cards
cor2 <- cor %>% select(yellow_cards) %>% arrange(-yellow_cards)
    #### est vars = played60, played, ict_index_opponent, xG_opponent, goals_conceded, position, strength, difficulty, h_a

###### Red cards
cor2 <- cor %>% select(red_cards) %>% arrange(-red_cards)
    #### est vars = goals_conceded, played, xG_opponent, ict_index_opponent, position, strengthm difficulty, h_a

### In the saves model we are estimating saves, penalties saved, and goals conceded

##### Saves
cor2 <- cor %>% select(saves) %>% arrange(-saves)
    #### est vars = ict_index_opponent, xG_opponent, played60, goals_conceded, played, strength, difficulty, h_a

##### Pens saved
cor2 <- cor %>% select(penalties_saved) %>% arrange(-penalties_saved)
    #### est vars = saves, xG_opponent, ict_index_opponent, played60, position, strength, difficulty, h_a

##### Goals conceded
cor2 <- cor %>% select(goals_conceded) %>% arrange(-goals_conceded)
    #### est vars = xG_opponent, ict_index_opponent, played60, played, ict_index, position, strength, difficulty, h_a

### In the time model we are estimating the probability of playing, playing 60 minutes, and getting a clean sheet

#### Played
cor2 <- cor %>% select(played) %>% arrange(-played)
    #### est_data = ict_index_opponent, xG_opponent, ict_index, goals_conceded, xA, xG, value, position, strength, difficulty h_a

#### Played 60 minutes
cor2 <- cor %>% select(played60) %>% arrange(-played60)
    #### est vars = ict_index_opponent, xG_opponent, ict_index, goals_conceded, xA, xG, value, strength, difficulty, h_a

#### Clean sheet
cor2 <- cor %>% select(clean_sheet) %>% arrange(-clean_sheet)
    #### est vars = played60, played, ict_index, ict_index_opponent, xG_opponent, strength, difficulty, h_a

# Now set up the data for predictions

### Average the independent variables by difficulty for each player
avg_team <- est_data %>%
  group_by(name, strength, difficulty) %>%
  summarize(
    xG_opp=mean(xG, na.rm = T),
    xA_opp=mean(xA, na.rm = T),
    ict_index_opp=mean(ict_index, na.rm = T),
    played_opp=mean(played, na.rm = T),
    played60_opp=mean(played60, na.rm = T),
    clean_sheet_opp=mean(clean_sheet, na.rm = T),
    ict_index_opponent_opp=mean(ict_index_opponent, na.rm = T),
    xG_opponent_opp=mean(xG_opponent, na.rm = T),
    saves_opp=mean(saves, na.rm = T),
    goals_conceded_opp=mean(goals_conceded, na.rm = T)
    ) %>% ungroup()

avg_season <- val_data %>%
  group_by(name, position, season) %>%
  summarize(
    xG_season=mean(xG, na.rm = T),
    xA_season=mean(xA, na.rm = T),
    ict_index_season=mean(ict_index, na.rm = T),
    played_season=mean(played, na.rm = T),
    played60_season=mean(played60, na.rm = T),
    clean_sheet_season=mean(clean_sheet, na.rm = T),
    ict_index_opponent_season=mean(ict_index_opponent, na.rm = T),
    xG_opponent_season=mean(xG_opponent, na.rm = T),
    saves_season=mean(saves, na.rm = T),
    goals_conceded_season=mean(goals_conceded, na.rm = T)
    ) %>%
  ungroup()

if(max(avg_season$season)==max(val_data$season)) {
  avg_season <- avg_season %>% filter(season==max(season))
} else if (max(avg_season$season)!=max(val_data$season)) {
  avg_season <- avg_season %>%
    filter(season==max(season)) %>%
    mutate(season=max(val_data$season))
  
  temp <- avg_season %>%
    group_by(position) %>%
    summarize(
      xG_season=mean(xG, na.rm = T),
      xA_season=mean(xA, na.rm = T),
      ict_index_season=mean(ict_index, na.rm = T),
      played_season=mean(played, na.rm = T),
      played60_season=mean(played60, na.rm = T),
      clean_sheet_season=mean(clean_sheet, na.rm = T),
      ict_index_opponent_season=mean(ict_index_opponent, na.rm = T),
      xG_opponent_season=mean(xG_opponent, na.rm = T),
      saves_season=mean(saves, na.rm = T),
      goals_conceded_season=mean(goals_conceded, na.rm = T)
    ) %>%
    ungroup()
  
  avg_season2 <- val_data %>%
    distinct(name, team, position, season) %>%
    filter(team %in% new_teams) %>%
    mutate(season=max(val_data$season)) %>%
    left_join(temp)
  
  avg_season <- avg_season %>%
    filter(team %in% val_data$team) %>%
    rbind(avg_season2) %>%
    group_by(name, position, season) %>%
    summarize(
      xG_season=mean(xG, na.rm = T),
      xA_season=mean(xA, na.rm = T),
      ict_index_season=mean(ict_index, na.rm = T),
      played_season=mean(played, na.rm = T),
      played60_season=mean(played60, na.rm = T),
      clean_sheet_season=mean(clean_sheet, na.rm = T),
      ict_index_opponent_season=mean(ict_index_opponent, na.rm = T),
      xG_opponent_season=mean(xG_opponent, na.rm = T),
      saves_season=mean(saves, na.rm = T),
      goals_conceded_season=mean(goals_conceded, na.rm = T)
    ) %>%
    ungroup()
}

  avg_h_a <- est_data %>%
    group_by(name, h_a) %>%
    summarize(
      xG_h_a=mean(xG, na.rm = T),
      xA_h_a=mean(xA, na.rm = T),
      ict_index_h_a=mean(ict_index, na.rm = T),
      played_h_a=mean(played, na.rm = T),
      played60_h_a=mean(played60, na.rm = T),
      clean_sheet_h_a=mean(clean_sheet, na.rm = T),
      ict_index_opponent_h_a=mean(ict_index_opponent, na.rm = T),
      xG_opponent_h_a=mean(xG_opponent, na.rm = T),
      saves_h_a=mean(saves, na.rm = T),
      goals_conceded_h_a=mean(goals_conceded, na.rm = T)
    ) %>% ungroup()
  
predict_data <- ids %>%
  select(name, web_name, team, position, value) %>%
  left_join(fixtures) %>%
  left_join(avg_team) %>%
  left_join(avg_h_a) %>%
  left_join(avg_season) %>% ### get weighted avgs here: 70% season, 15% h_a, 15% strength/difficulty
  mutate(
    xG= 0.7*xG_season + 0.15*xG_h_a + 0.15*xG_season,
    xA= 0.7*xA_season + 0.15*xA_h_a + 0.15*xA_season,
    ict_index= 0.7*ict_index_season + 0.15*ict_index_h_a + 0.15*ict_index_season,
    played= 0.7*played_season + 0.15*played_h_a + 0.15*played_season,
    played60= 0.7*played60_season + 0.15*played60_h_a + 0.15*played60_season,
    clean_sheet= 0.7*clean_sheet_season + 0.15*clean_sheet_h_a + 0.15*clean_sheet_season,
    ict_index_opponent= 0.7*ict_index_opponent_season + 0.15*ict_index_opponent_h_a + 0.15*ict_index_opponent_opp,
    xG_opponent= 0.7*xG_opponent_season + 0.15*xG_opponent_h_a + 0.15*xG_opponent_opp,
    saves= 0.7*saves_season + 0.15*saves_h_a + 0.15*saves_opp,
    goals_conceded= 0.7*goals_conceded_season + 0.15*goals_conceded_h_a + 0.15*goals_conceded_opp
  ) %>%
  select(name, web_name, team, position, season, value, GW, opponent, h_a, strength, difficulty, xG, xA, ict_index, played, played60, clean_sheet, ict_index_opponent,
         xG_opponent, saves, goals_conceded)


objects <- ls()
keep <- objects[grep('results|comp|data|fixtures|ids', objects)]
rm(list=setdiff(objects, keep))
gc()
