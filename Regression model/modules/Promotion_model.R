#---------------------------------------#
# Promotion effects for the FPL Lineup Optimizer
# Written by: ncfisher
# Last updated: July 23 2025
#---------------------------------------#

promoted_teams <- c('Sunderland', 'Leeds', 'Burnley')
temp <- predict_data %>% filter(team %in% promoted_teams)

if(is.nan(mean(temp$xG))){

## 1) Do an estimation of ict index on xG, xA
ict_model <- lm(ict_index ~ xG + xA, data = est_data)
summary(ict_model)
ict_model2 <- lm(ict_index_opponent ~ xG_opponent, data = est_data)
summary(ict_model2)
### should work for estimating the ict index based on xG and xA from championship

## 2) Filter the est_data to relevant teams
promoted_data <- est_data %>%
  filter(season > 2021) %>%
  filter(season==2023 & team=='Burnley' | season==2023 & team=='Luton' | season==2023 & team=='Sheffield' |
         season==2024 & team=='Leicester' | season==2024 & team=='Ipswich' | season==2024 & team=='Southampton') %>%
  group_by(name, position, season, strength, difficulty) %>%
  summarize(xG = mean(xG, na.rm = T),
            xA = mean(xA, na.rm = T),
            ict_index = mean(ict_index, na.rm = T),
            played = sum(played, na.rm = T),
            played60 = sum(played60, na.rm = T),
            clean_sheet = sum(clean_sheet, na.rm = T),
            ict_index_opponent = mean(ict_index_opponent, na.rm = T),
            xG_opponent = mean(xG_opponent, na.rm = T),
            saves = mean(saves, na.rm = T),
            goals_conceded = mean(goals_conceded, na.rm = T),
            def_actions_per_90 = mean(def_actions_per_90, na.rm = T),
            def_actions_per_60 = mean(def_actions_per_60, na.rm = T)) %>%
  ungroup() %>%
  group_by(name, season) %>%
  mutate(played_prob = sum(played, na.rm = T)/38,
         played60_prob = sum(played60, na.rm = T)/38) %>%
  ungroup() %>%
  mutate(prob_played_range = ifelse(played_prob <= 0.25, 0.25, 0),
         prob_played_range = ifelse(played_prob > 0.25, 0.5, prob_played_range),
         prob_played_range = ifelse(played_prob > 0.5, 0.75, prob_played_range),
         prob_played_range = ifelse(played_prob > 0.75, 1, prob_played_range),
         prob_played60_range = ifelse(played60_prob <= 0.25, 0.25, 0),
         prob_played60_range = ifelse(played60_prob > 0.25, 0.5, prob_played60_range),
         prob_played60_range = ifelse(played60_prob > 0.5, 0.75, prob_played60_range),
         prob_played60_range = ifelse(played60_prob > 0.75, 1, prob_played60_range))

## 3) Load relevant FBref data from championship
df <- read.xlsx('data/championship_data.xlsx', sheet = 'standard') %>%
  select(Player, season, Pos, Squad, MP, `90s`, Min, xG) %>%
  left_join(
    read.xlsx('data/championship_data.xlsx', sheet = 'passing') %>%
      select(Player, season, Pos, Squad, xA)
  ) %>%
  left_join(
    read.xlsx('data/championship_data.xlsx', sheet = 'time') %>%
      select(Player, season, Pos, Squad, onGA, onxGA)
  ) %>%
  left_join(
    read.xlsx('data/championship_data.xlsx', sheet = 'gkp') %>%
      select(Player, season, Pos, Squad, Saves)
  ) %>%
  left_join(
    read.xlsx('data/championship_data.xlsx', sheet = 'def') %>%
      select(Player, season, Pos, Squad, `Tkl+Int`, Clr, Sh)
  ) %>%
  left_join(
    read.xlsx('data/championship_data.xlsx', sheet = 'misc') %>%
      select(Player, season, Pos, Squad, Recov)
  ) %>% mutate(Pos = str_sub(Pos, 1, 2),
               Pos = ifelse(Pos=='MF', 'MID', Pos),
               Pos = ifelse(Pos=='DF', 'DEF', Pos),
               Pos = ifelse(Pos=='GK', 'GKP', Pos),
               Pos = ifelse(Pos=='FW', 'FWD', Pos),
               played = MP/46,
               played60 = `90s`/46,
               Saves = ifelse(is.na(Saves), 0, Saves),
               def_actions = ifelse(Pos=='DEF', Sh + Clr + `Tkl+Int`, 0),
               def_actions = ifelse(Pos=='MID' | Pos=='FWD', Sh + Clr + `Tkl+Int` + Recov , 0))

### Apply the ict index model
df <- df %>%
  cbind(predict(ict_model, df) %>% data.frame() %>% rename(ict_index = 1))

### Mutate all to avg per match
df <- df %>%
  mutate(xG = xG/MP,
         xA = xA/MP,
         onGA = onGA/MP,
         onxGA = onxGA/MP,
         ict_index = ict_index/MP,
         Saves = ifelse(!is.na(Saves), Saves/MP, Saves),
         def_actions_per_90 = def_actions/`90s`,
         `60s` = Min/60,
         def_actions_per_60 = def_actions/`60s`)

## 4) Filter to just teams that were promoted and then join to the PL stats to examine changes
df2 <- df %>% 
  filter(season==2022 & Squad=='Luton Town' | season==2022 & Squad=='Burnley' | season==2022 & Squad=='Sheffield Utd' |
         season==2023 & Squad=='Ipswich Town' | season==2023 & Squad=='Leicester City' | season==2023 & Squad=='Southampton') %>%
  mutate(Squad = ifelse(Squad=='Luton Town', 'Luton', Squad),
         Squad = ifelse(Squad=='Sheffield Utd', 'Sheffield', Squad),
         Squad = ifelse(Squad=='Leicester City', 'Leicester', Squad),
         Squad = ifelse(Squad=='Ipswich Town', 'Ipswich', Squad),
         prob_played_range = ifelse(played <= 0.25, 0.25, 0),
         prob_played_range = ifelse(played > 0.25, 0.5, prob_played_range),
         prob_played_range = ifelse(played > 0.5, 0.75, prob_played_range),
         prob_played_range = ifelse(played > 0.75, 1, prob_played_range),
         prob_played60_range = ifelse(played60 <= 0.25, 0.25, 0),
         prob_played60_range = ifelse(played60 > 0.25, 0.5, prob_played60_range),
         prob_played60_range = ifelse(played60 > 0.5, 0.75, prob_played60_range),
         prob_played60_range = ifelse(played60 > 0.75, 1, prob_played60_range)) %>%
  group_by(prob_played_range, Pos) %>%
  summarize(
    xG = mean(xG, na.rm = T),
    xA = mean(xA, na.rm = T),
    goals_conceded = mean(onGA, na.rm = T),
    xG_opponent = mean(onxGA, na.rm = T),
    played60 = mean(played60, na.rm = T),
    ict_index = mean(ict_index, na.rm = T),
    Saves = mean(Saves, na.rm = T),
    def_actions_per_90 = mean(def_actions_per_90, na.rm = T),
    def_actions_per_60 = mean(def_actions_per_60, na.rm = T)
  ) %>% ungroup() %>%
  rename(position = Pos) %>%
  arrange(position, prob_played_range)

temp <- df2 %>% filter(position=='GKP') %>%
  rbind(df2 %>% filter(position=='GKP' & prob_played_range==0.25) %>% mutate(prob_played_range = 0.5),
        df2 %>% filter(position=='GKP' & prob_played_range==0.25) %>% mutate(prob_played_range = 0.75)) %>%
  pivot_longer(cols = -c('prob_played_range', 'position'), names_to = 'stat', values_to = 'value') %>%
  arrange(stat, prob_played_range) %>%
  mutate(value = ifelse(prob_played_range==0.5, lag(value) + ((lead(value, 2) - lag(value))/(4-1)) * (2-1), value),
         value = ifelse(prob_played_range==0.75, lag(value, 2) + ((lead(value) - lag(value, 2))/(4-1)) * (3-1), value)) %>%
  pivot_wider(names_from = stat, values_from = value)

df2 <- df2 %>% filter(position!='GKP') %>%
  rbind(temp) %>%
  arrange(position, prob_played_range)

df3 <- promoted_data %>%
  mutate(season = season - 1) %>%
  group_by(position, prob_played_range) %>%
  summarize(xG_prem = mean(xG, na.rm = T),
            xA_prem = mean(xA, na.rm = T),
            goals_conceded_prem = mean(goals_conceded, na.rm = T),
            xG_opponent_prem = mean(xG_opponent, na.rm = T),
            played60_prem = mean(played60_prob, na.rm = T),
            ict_index_prem = mean(ict_index, na.rm = T),
            saves_prem = mean(saves, na.rm = T),
            def_actions_per_90_prem = mean(def_actions_per_90, na.rm = T),
            def_actions_per_60_prem = mean(def_actions_per_60, na.rm = T)) %>%
  ungroup() %>%
  arrange(position, prob_played_range)

### This gives avg rate of change by probability that a player plays, and the position, between leagues
comp <- df2 %>% 
  left_join(df3) %>%
  mutate(xG_chg = (xG_prem-xG)/xG,
         xA_chg = (xA_prem-xA)/xA,
         goals_conceded_chg = (goals_conceded_prem-goals_conceded)/goals_conceded,
         xG_opponent_chg = (xG_opponent_prem-xG_opponent)/xG_opponent,
         played60_chg = (played60_prem-played60)/played60,
         ict_index_chg = (ict_index_prem-ict_index)/ict_index,
         Saves_chg = (saves_prem-Saves)/Saves,
         def_actions_per_90_chg = (def_actions_per_90_prem-def_actions_per_90)/def_actions_per_90,
         def_actions_per_60_chg = (def_actions_per_60_prem-def_actions_per_60)/def_actions_per_60) %>%
  rename(saves = Saves) %>%
  select(prob_played_range, position, contains('chg')) %>%
  mutate(across(where(is.numeric), ~ifelse(is.infinite(.) | is.nan(.), 0, .)))

## 5) Apply the rates to promoted teams
df4 <- predict_data %>%
  filter(team=='Sunderland' | team=='Leeds' | team=='Burnley') %>%
  select(name, web_name, team, position, season, value, GW, opponent, h_a, strength, difficulty, contains('rating')) %>%
  stringdist_inner_join(
    df %>%
      filter(season==max(season)) %>%
      mutate(season=unique(predict_data$season),
             team = ifelse(grepl('Leeds', Squad), 'Leeds', NA),
             team = ifelse(grepl('Sunderland', Squad), 'Sunderland', team),
             team = ifelse(grepl('Burnley', Squad), 'Burnley', team),
             clean_sheet = 0.1,
             prob_played_range = ifelse(played <= 0.25, 0.25, 0),
             prob_played_range = ifelse(played > 0.25, 0.5, prob_played_range),
             prob_played_range = ifelse(played > 0.5, 0.75, prob_played_range),
             prob_played_range = ifelse(played > 0.75, 1, prob_played_range),
             prob_played60_range = ifelse(played60 <= 0.25, 0.25, 0),
             prob_played60_range = ifelse(played60 > 0.25, 0.5, prob_played60_range),
             prob_played60_range = ifelse(played60 > 0.5, 0.75, prob_played60_range),
             prob_played60_range = ifelse(played60 > 0.75, 1, prob_played60_range)) %>%
      rename(goals_conceded = onGA, xG_opponent = onxGA, saves = Saves) %>%
      filter(team=='Leeds' | team=='Sunderland' | team=='Burnley') %>%
      select(Player, team, xG, xA, ict_index, played, played60, goals_conceded, xG_opponent, saves, 
             def_actions_per_90, def_actions_per_60, contains('prob_played'), clean_sheet),
    by = c('name' = 'Player', 'team')
  ) %>% select(-team.y) %>% rename(team = team.x) %>%
  left_join(comp) %>%
  mutate(xG_promoted = xG + (xG * xG_chg),
         xA_promoted = xA + (xA * xA_chg),
         ict_index_promoted = ict_index + (ict_index * ict_index_chg),
         played60_promoted = played60 + (played60 * played60_chg),
         goals_conceded_promoted = goals_conceded + (goals_conceded * goals_conceded_chg),
         xG_opponent_promoted = xG_opponent + (xG_opponent * xG_opponent_chg),
         saves_promoted = saves + (saves * Saves_chg),
         def_actions_per_90_promoted = def_actions_per_90 + (def_actions_per_90 * def_actions_per_90_chg),
         def_actions_per_60_promoted = def_actions_per_60 + (def_actions_per_60 * def_actions_per_60_chg))

df4 <- df4 %>%
  cbind(predict(ict_model2, df4) %>% data.frame() %>% rename(ict_index_opponent_promoted=1)) %>%
  rename(played_promoted = played, clean_sheet_promoted = clean_sheet) %>%
  select(name, web_name, team, position, season, value, GW, opponent, h_a, strength, difficulty,
         contains('promoted')) %>%
  distinct(name, GW, .keep_all = T)

## 6) Filter the predict data then bind the promoted teams or average
df5 <- predict_data %>% 
  filter(team=='Leeds' | team=='Sunderland' | team=='Burnley') %>%
  left_join(df4) %>%
  mutate(
    xG = ifelse(is.nan(xG) | is.na(xG) | is.infinite(xG), xG_promoted, (xG + xG_promoted)/2),
    xA = ifelse(is.nan(xA) | is.na(xA) | is.infinite(xA), xA_promoted, (xA + xA_promoted)/2),
    ict_index = ifelse(is.nan(ict_index) | is.na(ict_index) | is.infinite(ict_index), ict_index_promoted, (ict_index + ict_index_promoted)/2),
    played = ifelse(is.nan(played) | is.na(played) | is.infinite(played), played_promoted, (played + played_promoted)/2),
    played60 = ifelse(is.nan(played60) | is.na(played60) | is.infinite(played60), played60_promoted, (played60 + played60_promoted)/2),
    goals_conceded = ifelse(is.nan(goals_conceded) | is.na(goals_conceded) | is.infinite(goals_conceded), goals_conceded_promoted, (goals_conceded + goals_conceded_promoted)/2),
    xG_opponent = ifelse(is.nan(xG_opponent) | is.na(xG_opponent) | is.infinite(xG_opponent), xG_opponent_promoted, (xG_opponent + xG_opponent_promoted)/2),
    ict_index_opponent = ifelse(is.nan(ict_index_opponent) | is.na(ict_index_opponent) | is.infinite(ict_index_opponent), ict_index_opponent_promoted, (ict_index_opponent + ict_index_opponent_promoted)/2),
    saves = ifelse(is.nan(saves) | is.na(saves) | is.infinite(saves), saves_promoted, (saves + saves_promoted)/2),
    def_actions_per_90 = ifelse(is.nan(def_actions_per_90) | is.na(def_actions_per_90) | is.infinite(def_actions_per_90), def_actions_per_90_promoted, (def_actions_per_90 + def_actions_per_90_promoted)/2),
    def_actions_per_60 = ifelse(is.nan(def_actions_per_60) | is.na(def_actions_per_60) | is.infinite(def_actions_per_60), def_actions_per_60_promoted, (def_actions_per_60 + def_actions_per_60_promoted)/2),
    clean_sheet = ifelse(is.nan(clean_sheet) | is.na(clean_sheet) | is.infinite(clean_sheet), clean_sheet_promoted, (clean_sheet + clean_sheet_promoted)/2)
  ) %>% select(names(predict_data)) %>%
  group_by(team, position) %>% ### Last resort - just replace with averages
  mutate(xG = ifelse(is.na(xG), mean(xG, na.rm = T), xG),
         xA = ifelse(is.na(xA), mean(xA, na.rm = T), xA),
         ict_index = ifelse(is.na(ict_index), mean(ict_index, na.rm = T), ict_index),
         played = ifelse(is.na(played), mean(played, na.rm = T), played),
         played60 = ifelse(is.na(played60), mean(played60, na.rm = T), played60),
         goals_conceded = ifelse(is.na(goals_conceded), mean(goals_conceded, na.rm = T), goals_conceded),
         xG_opponent = ifelse(is.na(xG_opponent), mean(xG_opponent, na.rm = T), xG_opponent),
         ict_index_opponent = ifelse(is.na(ict_index_opponent), mean(ict_index_opponent, na.rm = T), ict_index_opponent),
         saves = ifelse(is.na(saves), mean(saves, na.rm = T), saves),
         def_actions_per_90 = ifelse(is.na(def_actions_per_90), mean(def_actions_per_90, na.rm = T), def_actions_per_90),
         def_actions_per_60 = ifelse(is.na(def_actions_per_60), mean(def_actions_per_60, na.rm = T), def_actions_per_60),
         clean_sheet = ifelse( is.na(clean_sheet), mean(clean_sheet, na.rm = T), clean_sheet)) %>%
  ungroup() %>%
  select(names(predict_data))

predict_data <- predict_data %>%
  filter(team!='Leeds') %>%
  filter(team!='Sunderland') %>%
  filter(team!='Burnley') %>%
  rbind(df5) %>%
  distinct(name, team, GW, .keep_all = T)

}

objects <- ls()
keep <- objects[grep('results|comp|data|fixtures|ids|dev|teams|avg', objects)]
rm(list=setdiff(objects, keep))
gc()