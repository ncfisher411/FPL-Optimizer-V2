#---------------------------------------#
# Bous points model for the FPL Lineup Optimizer
# Written by: ncfisher
# Last updated: May 10 2024
#---------------------------------------#

# Add in the model for bonus points here since we have all the data on-hand

## Model set up
linear_bonus_model <- lm(bonus ~ ict_index + xG + xA + difficulty + strength + position + h_a, data = est_data)
logit_bonus_model <- glm(bonus ~ ict_index + xG + xA + difficulty + strength + position + h_a, data = est_data)
rf_bonus_model <- randomForest(bonus ~ ict_index + xG + xA + difficulty + strength + position + h_a, data = est_data)

## We will use the averages to build the estimate dataframe
### Take an avg xG and ICT index from these measures depending on the schedule
### Need to load the remaining games - vaastav combined data for 2023/24 has xG
### Now get remaining games
## Get an avg xG, ICT, for each player by difficulty
avg_team <- est_data %>%
  group_by(name, position, team, difficulty, season) %>%
  summarize(xG_opp=mean(xG, na.rm = T),
            xA_opp=mean(xA, na.rm = T),
            ict_index_opp=mean(ict_index, na.rm = T)) %>%
  ungroup()

## Build out for new teams at beginning of season - new teams get the average value
if(max(avg_team$season==max(val_data$season))) {
  avg_team <- avg_team %>% filter(season==max(season))
} else if(max(avg_team$season)!=max(val_data$season)) {
  avg_team <- avg_team %>%
    filter(season==max(season)) %>%
    mutate(season=max(val_data$season))
  
  new_teams <- setdiff(unique(val_data$team), unique(avg_team$team))
  temp <- avg_team %>%
    group_by(position, difficulty) %>%
    summarize(xG_opp=mean(xG_opp, na.rm =T ),
              xA_opp=mean(xA_opp, na.rm =T ),
              ict_index_opp=mean(ict_index_opp, na.rm = T)) %>%
    ungroup()
  
  avg_team2 <- val_data %>%
    distinct(name, position, team) %>%
    filter(team %in% new_teams) %>%
    mutate(season=max(val_data$season)) %>%
    left_join(temp)
  
  avg_team <- avg_team %>% 
    filter(team %in% val_data$team) %>% 
    rbind(avg_team2)
}

## Get an avg for each player by home/away
avg_h_a <- est_data %>%
  group_by(name, team, position, h_a, season) %>%
  summarize(xG=mean(xG, na.rm = T),
            xA=mean(xA, na.rm = T),
            ict_index=mean(ict_index, na.rm = T)) %>%
  ungroup() %>%
  pivot_wider(names_from = h_a, values_from = c(xG, xA, ict_index))

if(max(avg_h_a$season)==max(val_data$season)) {
  avg_h_a <- avg_h_a %>% filter(season==max(season))
} else if(max(avg_h_a$season)!=max(val_data$season)) {
  avg_h_a <- avg_h_a %>%
    filter(season==max(season)) %>%
    mutate(season=max(val_data$season))
  
  temp <- avg_h_a %>%
    group_by(position) %>%
    summarize(xG_a=mean(xG_a, na.rm =T ),
              xG_h=mean(xG_h, na.rm =T ),
              xA_a=mean(xA_a, na.rm =T ),
              xA_h=mean(xA_h, na.rm =T ),
              ict_index_a=mean(ict_index_a, na.rm = T),
              ict_index_h=mean(ict_index_h, na.rm = T)) %>%
    ungroup()
  
  avg_h_a2 <- val_data %>% 
    distinct(name, position, team) %>%
    filter(team %in% new_teams) %>%
    mutate(season=max(val_data$season)) %>%
    left_join(temp)
  
  avg_h_a <- avg_h_a %>%
    filter(team %in% val_data$team) %>%
    rbind(avg_h_a2) %>%
    group_by(name, position, season) %>%
    summarize(xG_a=mean(xG_a, na.rm =T ),
              xG_h=mean(xG_h, na.rm =T ),
              xA_a=mean(xA_a, na.rm =T ),
              xA_h=mean(xA_h, na.rm =T ),
              ict_index_a=mean(ict_index_a, na.rm = T),
              ict_index_h=mean(ict_index_h, na.rm = T)) %>%
    ungroup()
}

## Get an avg for the season by player
avg_season <- est_data %>%
  group_by(name, position, team, season) %>%
  summarize(xG_season=mean(xG, na.rm = T),
            xA_season=mean(xA, na.rm = T),
            ict_index_season=mean(ict_index, na.rm = T)) %>%
  ungroup()

if(max(avg_season$season)==max(val_data$season)) {
  avg_season <- avg_season %>% filter(season==max(season))
} else if (max(avg_season$season)!=max(val_data$season)) {
  avg_season <- avg_season %>%
    filter(season==max(season)) %>%
    mutate(season=max(val_data$season))
  
  temp <- avg_season %>%
    group_by(position) %>%
    summarize(xG_season=mean(xG_season, na.rm = T),
              xA_season=mean(xA_season, na.rm = T),
              ict_index_season=mean(ict_index_season, na.rm = T)) %>%
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
    summarize(xG_season=mean(xG_season, na.rm = T),
              xA_season=mean(xA_season, na.rm = T),
              ict_index_season=mean(ict_index_season, na.m = T))
}

temp <- est_data %>%
  filter(season==max(season))

## Get the actual numbers

temp <- val_data %>% filter(finished!='TRUE')

if (nrow(temp)==0) {
  
  df2 <- val_data %>% 
    select(name, web_name, team, position, season, GW, kickoff_time, h_a, opponent, strength, difficulty, value) %>%
    left_join(avg_season) %>%
    left_join(avg_h_a) %>%
    left_join(avg_team) %>%
    mutate(
      xG=ifelse(h_a=='h', (xG_season*0.5) + (xG_h*0.3) + (xG_opp*0.2),
                             (xG_season*0.5) + (xG_a*0.3) + (xG_opp*0.2)),
      xA=ifelse(h_a=='h', (xA_season*0.5) + (xA_h*0.3) + (xA_opp*0.2),
                (xA_season*0.5) + (xA_a*0.3) + (xA_opp*0.2)),
      ict_index=ifelse(h_a=='h', (ict_index_season*0.5) + (ict_index_h)*0.3 + (ict_index_opp*0.2),
                                (ict_index_season*0.5) + (ict_index_a*0.3) + (ict_index_opp*0.2))
    ) %>% select(name, web_name, team, position, season, GW, kickoff_time, h_a, opponent, strength, difficulty, xG, ict_index, xA, value) %>%
    distinct(name, GW, .keep_all = T) %>%
    # mutate_if(is.numeric, function(x) ifelse(is.na(x), 0, x)) %>%
    group_by(team, h_a, strength, difficulty, position) %>%
    mutate(xG=ifelse(is.na(xG), mean(xG, na.rm = T), xG),
           xA=ifelse(is.na(xA), mean(xA, na.rm = T), xA),
           ict_index=ifelse(is.na(ict_index), mean(ict_index, na.rm = T), ict_index)) %>%
    ungroup()
  
} else if (nrow(temp) > 0) {
  
  temp2 <- temp %>%
    select(name, web_name, team, position, season, GW, kickoff_time, h_a, opponent, strength, difficulty, value) %>%
    left_join(avg_season) %>%
    left_join(avg_h_a) %>%
    left_join(avg_team) %>%
    mutate(
      xG=ifelse(h_a=='h', (xG_season*0.5) + (xG_h*0.3) + (xG_opp*0.2),
                (xG_season*0.5) + (xG_a*0.3) + (xG_opp*0.2)),
      xA=ifelse(h_a=='h', (xA_season*0.5) + (xA_h*0.3) + (xA_opp*0.2),
                (xA_season*0.5) + (xA_a*0.3) + (xA_opp*0.2)),
      ict_index=ifelse(h_a=='h', (ict_index_season*0.5) + (ict_index_h)*0.3 + (ict_index_opp*0.2),
                       (ict_index_season*0.5) + (ict_index_a*0.3) + (ict_index_opp*0.2))
    ) %>% select(name, web_name, team, position, season, GW, kickoff_time, h_a, opponent, strength, difficulty, xG, ict_index, xA, value) %>%
    distinct(name, GW, .keep_all = T) %>%
    # mutate_if(is.numeric, function(x) ifelse(is.na(x), 0, x)) %>%
    group_by(team, h_a, strength, difficulty, position) %>%
    mutate(xG=ifelse(is.na(xG), mean(xG, na.rm = T), xG),
           xA=ifelse(is.na(xA), mean(xA, na.rm = T), xA),
           ict_index=ifelse(is.na(ict_index), mean(ict_index, na.rm = T), ict_index)) %>%
    ungroup()
  
  df2 <- val_data %>%
    filter(!(GW %in% temp$GW)) %>%
    select(name, web_name, team, position, season, GW, kickoff_time, h_a, opponent, strength, difficulty, xG, ict_index, xA, value) %>%
    rbind(temp2) %>%
    distinct(name, GW, .keep_all = T) %>%
    # mutate_if(is.numeric, function(x) ifelse(is.na(x), 0, x))
    group_by(team, h_a, strength, difficulty, position) %>%
    mutate(xG=ifelse(is.na(xG), mean(xG, na.rm = T), xG),
           xA=ifelse(is.na(xA), mean(xA, na.rm = T), xA),
           ict_index=ifelse(is.na(ict_index), mean(ict_index, na.rm = T), ict_index)) %>%
    ungroup()
  
} else {
  
  print('Error in assigning played vs not played games')
  
}

## Run the prediction with each model
linear_predictions <- predict(linear_bonus_model, df2) %>% data.frame() %>%
  rename(Predicted_bonus_linear=1) %>%
  cbind(df2) %>%
  select(name, position, team, season, GW, opponent, h_a, Predicted_bonus_linear) %>%
  left_join(val_data %>% select(name, position, team, GW, bonus), by=c('name', 'position', 'team', 'GW')) %>%
  mutate(Predicted_bonus_linear=ifelse(Predicted_bonus_linear<0, 0, Predicted_bonus_linear),
         Predicted_bonus_linear=ifelse(Predicted_bonus_linear>3, 3, Predicted_bonus_linear),
         linear_bonus_validation=bonus-Predicted_bonus_linear) %>%
  select(-bonus)

logit_predictions <- predict(logit_bonus_model, df2) %>% data.frame() %>%
  rename(Predicted_bonus_logit=1) %>%
  cbind(df2) %>%
  select(name, position, team, season, GW, opponent, h_a, Predicted_bonus_logit) %>%
  left_join(val_data %>% select(name, position, team, GW, bonus), by=c('name', 'position', 'team', 'GW')) %>%
  mutate(Predicted_bonus_logit=ifelse(Predicted_bonus_logit<0, 0, Predicted_bonus_logit),
         Predicted_bonus_logit=ifelse(Predicted_bonus_logit>3, 3, Predicted_bonus_logit),
         logit_bonus_validation=bonus-Predicted_bonus_logit) %>%
  select(-bonus)

rf_predictions <- predict(rf_bonus_model, df2) %>% data.frame() %>%
  rename(Predicted_bonus_rf=1) %>%
  cbind(df2) %>%
  select(name, position, team, season, GW, opponent, h_a, Predicted_bonus_rf) %>%
  left_join(val_data %>% select(name, position, team, GW, bonus), by=c('name', 'position', 'team', 'GW')) %>%
  mutate(Predicted_bonus_rf=ifelse(Predicted_bonus_rf<0, 0, Predicted_bonus_rf),
         Predicted_bonus_rf=ifelse(Predicted_bonus_rf>3, 3, Predicted_bonus_rf),
         rf_bonus_validation=bonus-Predicted_bonus_rf) %>%
  select(-bonus)

## Get metrics
comp_bonus <- data.frame(Stat='Bonus points',
                         Model=c('Linear', 'Logit'),
                         RMSE=c(sqrt(mean(resid(linear_bonus_model)^2)),
                                sqrt(mean(resid(logit_bonus_model)^2))),
                         R2=c(summary(linear_bonus_model)$r.squared,
                              summary(logit_bonus_model)$r.squared),
                         Validation=c(mean(linear_predictions$linear_bonus_validation),
                                      mean(logit_predictions$logit_bonus_validation))
) %>%
  rbind(
    data.frame(Stat='Bonus points',
               Model='Random Forest',
               RMSE=sqrt(mean(rf_bonus_model$mse)),
               R2=mean(rf_bonus_model$rsq),
               Validation=c(mean(rf_predictions$rf_bonus_validation)))
  )

bonus_results <- linear_predictions %>%
  distinct(name, GW, .keep_all = T) %>%
  left_join(logit_predictions %>%
              distinct(name, GW, .keep_all = T)) %>%
  left_join(rf_predictions %>%
              distinct(name, GW, .keep_all = T)) %>%
  left_join(temp2) %>%
  left_join(df2 %>% select(name, position, team, GW, opponent, h_a, season)) %>%
  select(name, position, team, GW, opponent, h_a, everything(), -contains('validation')) %>%
  rename(Player=name, Position=position, Gameweek=GW, Opponent=opponent,
         `Home/Away`=h_a, Team=team, Season=season) %>%
  mutate(`Home/Away`=ifelse(`Home/Away`=='h', 'Home', 'Away'),
  ) %>%
  distinct(Player, Gameweek, .keep_all = T)

objects <- ls()
keep <- objects[grep('results|comp|data|fixtures|ids', objects)]
rm(list=setdiff(objects, keep))
gc()
