#---------------------------------------#
# Defense contributions for FPL Probability Model
# Written by: ncfisher
# Last updated: July 30 2025
#---------------------------------------#

##### THIS METHODOLOGY IS LIKELY TEMPORARY AND SHOULD BE UPDATED USING UPCOMING FPL API STATS IF AVAILABLE

### Pull the data from the regression model
pos_player <- read.xlsx('../Regression model/data/fbref_possession.xlsx', sheet = 'player')
pos_team <- read.xlsx('../Regression model/data/fbref_possession.xlsx', sheet = 'team')
def_player <- read.xlsx('../Regression model/data/fbref_defense.xlsx', sheet = 'player')
misc_player <- read.xlsx('../Regression model/data/fbref_misc.xlsx', sheet = 'player')

df <- def_player %>% 
  select(Player, Squad, season, `90s`, Pos, Sh, Clr, `Tkl+Int`) %>%
  left_join(misc_player %>%
              select(Player, Squad, season, Recov)) %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.), 0, .)),
         Pos = str_sub(Pos, 1, 2),
         Pos = ifelse(Pos=='MF', 'MID', Pos),
         Pos = ifelse(Pos=='FW', 'FWD', Pos),
         Pos = ifelse(Pos=='DF', 'DEF', Pos),
         Pos = ifelse(Pos=='GK', 'GKP', Pos),
         def_actions = ifelse(Pos=='DEF', Sh + Clr + `Tkl+Int`, 0),
         def_actions = ifelse(Pos=='GKP', 0, def_actions),
         def_actions = ifelse(Pos=='MID' | Pos=='FWD', Sh + Clr + `Tkl+Int` + Recov , def_actions),
         def_actions_per_90 = ifelse(`90s` > 0, def_actions/`90s`, 0),
         minutes = `90s` * 90,
         `60s` = minutes/60,
         def_actions_per_60 = ifelse(`60s` > 0, def_actions/`60s`, 0)) %>%
  group_by(Squad, Pos) %>%
  summarize(def_actions_per_90 = mean(def_actions_per_90, na.rm = T),
            def_actions_per_60 = mean(def_actions_per_60, na.rm = T)) %>%
  ungroup()

df2 <- df %>%
  mutate(Squad = ifelse(grepl('Leeds', Squad), 'Leeds', Squad),
         Squad = ifelse(grepl('Manchester Utd', Squad), 'Man Utd', Squad),
         Squad = ifelse(grepl('Manchester City', Squad), 'Man City', Squad),
         Squad = ifelse(grepl('Newcastle', Squad), 'Newcastle', Squad),
         Squad = ifelse(grepl('Forest', Squad), "Nott'm Forest", Squad),
         Squad = ifelse(grepl('Tottenham', Squad), 'Spurs', Squad)) %>%
  filter(Squad %in% unique(current_players$team_name))

### Get the stats for promoted teams that have not been in the league - just Sunderland now
status <- grepl('TRUE', unique(fixtures$finished))

teams <- c('Burnley', 'Ipswich Town', 'Leeds United', 'Luton Town', 'Norwich City', 'Sheffield Utd', 'Southampton', 'Watford',
           'Hull City', 'Coventry City')
  
temp <- df %>%
  filter(Squad %in% teams) %>%
  group_by(Pos) %>%
  summarize(def_actions_per_90 = mean(def_actions_per_90, na.rm = T),
            def_actions_per_60 = mean(def_actions_per_60, na.rm = T)) %>%
  ungroup() %>%
  mutate(Squad = 'Sunderland') %>%
  rename(team = Squad, position = Pos)
  
def_probs <- df2 %>% 
  rename(position = Pos, team = Squad) %>%
  rbind(temp)

objects <- ls()
keep <- objects[grep('combined_data|test|fixture|team|current_players|probs|understat|weight', objects)]
rm(list=setdiff(objects, keep))
gc()
