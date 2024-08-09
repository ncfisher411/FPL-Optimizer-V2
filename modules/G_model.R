#---------------------------------------#
# Goals model for the FPL Lineup Optimizer
# Written by: ncfisher
# Last updated: May 25 2024
#---------------------------------------#

# This script is used for modeling match xG for the FPL optimizer

## Now test model variations for predicting goals using xG and ICT index
### Dependent variable = goals
### Independent variable = xG, ICT, position, team, home/away, opponent
df <- est_data %>% select(goals, own_goals, penalties_missed, xG, ict_index, position, h_a, strength, difficulty)

linear_xg_model <- lm(goals ~ xG + ict_index + position + h_a + strength + difficulty, data = df)
logit_xg_model <- glm(goals ~ xG + ict_index + position + h_a + strength + difficulty, data = df)
rf_xg_model <- randomForest(goals ~ xG + ict_index + position + h_a + strength + difficulty, data = df)
summary(linear_xg_model)
summary(logit_xg_model)
gc()
linear_og_model <- lm(own_goals ~ position + strength + difficulty + h_a, data = df)
logit_og_model <- glm(own_goals ~ position + strength + difficulty + h_a, data = df)
rf_og_model <- randomForest(own_goals ~ position + strength + difficulty + h_a, data = df)
gc()
summary(linear_og_model)
summary(logit_og_model)
linear_pen_model <- lm(penalties_missed ~ xG + position + h_a + strength + difficulty, data = df)
logit_pen_model <- lm(penalties_missed ~ xG + position + h_a + strength + difficulty, data = df)
rf_pen_model <- randomForest(penalties_missed ~ xG + position + h_a + strength + difficulty, data = df)
gc()
summary(linear_pen_model)
summary(logit_pen_model)

## Now set up the prediction for goals

## We will use the averages to build the estimate dataframe
### Take an avg xG and ICT index from these measures depending on the schedule
### Need to load the remaining games - vaastav combined data for 2023/24 has xG
### Now get remaining games
## Get an avg xG, ICT, for each player by difficulty
avg_team <- est_data %>%
  group_by(name, position, team, difficulty, season) %>%
  summarize(xG_opp=mean(xG, na.rm = T),
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
  group_by(name, position, team, h_a, season) %>%
  summarize(xG=mean(xG, na.rm = T),
            ict_index=mean(ict_index, na.rm = T)) %>%
  ungroup() %>%
  pivot_wider(names_from = h_a, values_from = c(xG, ict_index))

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
              ict_index_a=mean(ict_index_a, na.rm = T),
              ict_index_h=mean(ict_index_h, na.rm = T))%>%
    ungroup()
}

## Get an avg for the season by player
avg_season <- est_data %>%
  group_by(name, position, team, season) %>%
  summarize(xG_season=mean(xG, na.rm = T),
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
              ict_index_season=mean(ict_index_season, na.m = T)) %>%
    ungroup()
}

temp <- est_data %>%
  filter(season==max(season))

## Get the actual numbers

temp <- val_data %>% filter(finished!='TRUE')

if (nrow(temp)==0) {
  
  df2 <- val_data %>% 
    select(name, web_name, team, position, season, GW, kickoff_time, h_a, opponent, strength, difficulty) %>%
    left_join(avg_season) %>%
    left_join(avg_h_a) %>%
    left_join(avg_team) %>%
    mutate(
      xG=ifelse(h_a=='h', (xG_season*0.5) + (xG_h*0.3) + (xG_opp*0.2),
                (xG_season*0.5) + (xG_a*0.3) + (xG_opp*0.2)),
      ict_index=ifelse(h_a=='h', (ict_index_season*0.5) + (ict_index_h)*0.3 + (ict_index_opp*0.2),
                       (ict_index_season*0.5) + (ict_index_a*0.3) + (ict_index_opp*0.2))
    ) %>% select(name, web_name, team, position, season, GW, kickoff_time, h_a, opponent, strength, difficulty, xG, ict_index) %>%
    distinct(name, GW, .keep_all = T) %>%
    # mutate_if(is.numeric, function(x) ifelse(is.na(x), 0, x)) %>%
    group_by(team, h_a, strength, difficulty, position) %>%
    mutate(xG=ifelse(is.na(xG), mean(xG, na.rm = T), xG),
           ict_index=ifelse(is.na(ict_index), mean(ict_index, na.rm = T), ict_index)) %>%
    ungroup()
  
} else if (nrow(temp) > 0) {
  
  temp2 <- temp %>%
    select(name, web_name, team, position, season, GW, kickoff_time, h_a, opponent, strength, difficulty) %>%
    left_join(avg_season) %>%
    left_join(avg_h_a) %>%
    left_join(avg_team) %>%
    mutate(
      xG=ifelse(h_a=='h', (xG_season*0.5) + (xG_h*0.3) + (xG_opp*0.2),
                (xG_season*0.5) + (xG_a*0.3) + (xG_opp*0.2)),
      ict_index=ifelse(h_a=='h', (ict_index_season*0.5) + (ict_index_h)*0.3 + (ict_index_opp*0.2),
                       (ict_index_season*0.5) + (ict_index_a*0.3) + (ict_index_opp*0.2))
    ) %>% select(name, web_name, team, position, season, GW, kickoff_time, h_a, opponent, strength, difficulty, xG, ict_index) %>%
    distinct(name, GW, .keep_all = T) %>%
    # mutate_if(is.numeric, function(x) ifelse(is.na(x), 0, x)) %>%
    group_by(team, h_a, strength, difficulty, position) %>%
    mutate(xG=ifelse(is.na(xG), mean(xG, na.rm = T), xG),
           ict_index=ifelse(is.na(ict_index), mean(ict_index, na.rm = T), ict_index)) %>%
    ungroup()
  
  df2 <- val_data %>%
    filter(!(GW %in% temp$GW)) %>%
    select(name, web_name, team, position, season, GW, kickoff_time, h_a, opponent, strength, difficulty, xG, ict_index) %>%
    rbind(temp2) %>%
    distinct(name, GW, .keep_all = T) %>%
    # mutate_if(is.numeric, function(x) ifelse(is.na(x), 0, x))
    group_by(team, h_a, strength, difficulty, position) %>%
    mutate(xG=ifelse(is.na(xG), mean(xG, na.rm = T), xG),
           ict_index=ifelse(is.na(ict_index), mean(ict_index, na.rm = T), ict_index)) %>%
    ungroup()
  
} else {
  
  print('Error in assigning played vs not played games')
  
}

## Run the prediction with each model
linear_predictions <- predict(linear_xg_model, df2) %>% data.frame() %>%
  rename(Predicted_goals_linear=1) %>%
  cbind(df2) %>%
  select(name, position, team, season, GW, opponent, h_a, Predicted_goals_linear) %>%
  left_join(val_data %>% select(name, position, team, GW, goals), by=c('name', 'position', 'team', 'GW')) %>%
  mutate(Predicted_goals_linear=ifelse(Predicted_goals_linear<0, 0, Predicted_goals_linear),
         linear_goals_validation=goals-Predicted_goals_linear) %>%
  select(-goals)

logit_predictions <- predict(logit_xg_model, df2) %>% data.frame() %>%
  rename(Predicted_goals_logit=1) %>%
  cbind(df2) %>%
  select(name, position, team, season, GW, opponent, h_a, Predicted_goals_logit) %>%
  left_join(val_data %>% select(name, position, team, GW, goals), by=c('name', 'position', 'team', 'GW')) %>%
  mutate(Predicted_goals_logit=ifelse(Predicted_goals_logit<0, 0, Predicted_goals_logit),
         logit_goals_validation=goals-Predicted_goals_logit) %>%
  select(-goals)

rf_predictions <- predict(rf_xg_model, df2) %>% data.frame() %>%
  rename(Predicted_goals_rf=1) %>%
  cbind(df2) %>%
  select(name, position, team, season, GW, opponent, h_a, Predicted_goals_rf) %>%
  left_join(val_data %>% select(name, position, team, GW, goals), by=c('name', 'position', 'team', 'GW')) %>%
  mutate(Predicted_goals_rf=ifelse(Predicted_goals_rf<0, 0, Predicted_goals_rf),
         rf_goals_validation=goals-Predicted_goals_rf) %>%
  select(-goals)

linear_og_predictions <- predict(linear_og_model, df2) %>%
  data.frame() %>%
  rename(Predicted_og_linear=1) %>%
  cbind(df2) %>%
  select(name, position, team, season, GW, opponent, h_a, Predicted_og_linear) %>%
  left_join(val_data %>% select(name, position, team, GW, own_goals), by=c('name', 'position', 'team', 'GW')) %>%
  mutate(Predicted_og_linear=ifelse(Predicted_og_linear < 0, 0, Predicted_og_linear),
         linear_og_validation=own_goals-Predicted_og_linear) %>%
  select(-own_goals)

linear_pen_predictions <- predict(linear_pen_model, df2) %>%
  data.frame() %>%
  rename(Predicted_pen_linear=1) %>%
  cbind(df2) %>%
  select(name, position, team, season, GW, opponent, h_a, Predicted_pen_linear) %>%
  left_join(val_data %>% select(name, position, team, GW, penalties_missed), by=c('name', 'position', 'team', 'GW')) %>%
  mutate(Predicted_pen_linear=ifelse(Predicted_pen_linear < 0, 0, Predicted_pen_linear),
         linear_pen_validation=penalties_missed-Predicted_pen_linear) %>%
  select(-penalties_missed)

logit_pen_predictions <- predict(logit_pen_model, df2) %>%
  data.frame() %>%
  rename(Predicted_pen_logit=1) %>%
  cbind(df2) %>%
  select(name, position, team, season, GW, opponent, h_a, Predicted_pen_logit) %>%
  left_join(val_data %>% select(name, position, team, GW, penalties_missed), by=c('name', 'position', 'team', 'GW')) %>%
  mutate(Predicted_pen_logit=ifelse(Predicted_pen_logit < 0, 0, Predicted_pen_logit),
         logit_pen_validation=penalties_missed-Predicted_pen_logit) %>%
  select(-penalties_missed)

rf_pen_predictions <- predict(rf_pen_model, df2) %>%
  data.frame() %>%
  rename(Predicted_pen_rf=1) %>%
  cbind(df2) %>%
  select(name, position, team, season, GW, opponent, h_a, Predicted_pen_rf) %>%
  left_join(val_data %>% select(name, position, team, GW, penalties_missed), by=c('name', 'position', 'team', 'GW')) %>%
  mutate(Predicted_pen_rf=ifelse(Predicted_pen_rf < 0, 0, Predicted_pen_rf),
         rf_pen_validation=penalties_missed-Predicted_pen_rf) %>%
  select(-penalties_missed)

logit_og_predictions <- predict(logit_og_model, df2) %>%
  data.frame() %>%
  rename(Predicted_og_logit=1) %>%
  cbind(df2) %>%
  select(name, position, team, season, GW, opponent, h_a, Predicted_og_logit) %>%
  left_join(val_data %>% select(name, position, team, GW, own_goals), by=c('name', 'position', 'team', 'GW')) %>%
  mutate(Predicted_og_logit=ifelse(Predicted_og_logit < 0, 0, Predicted_og_logit),
         logit_og_validation=own_goals-Predicted_og_logit) %>%
  select(-own_goals)

rf_og_predictions <- predict(rf_og_model, df2) %>%
  data.frame() %>%
  rename(Predicted_og_rf=1) %>%
  cbind(df2) %>%
  select(name, position, team, season, GW, opponent, h_a, Predicted_og_rf) %>%
  left_join(val_data %>% select(name, position, team, GW, own_goals), by=c('name', 'position', 'team', 'GW')) %>%
  mutate(Predicted_og_rf=ifelse(Predicted_og_rf < 0, 0, Predicted_og_rf),
         rf_og_validation=own_goals-Predicted_og_rf) %>%
  select(-own_goals)


## Metrics summary for model choice justification
comp_goals <- data.frame(Stat='Goals',
  Model=c('Linear', 'Logit'),
  RMSE=c(sqrt(mean(resid(linear_xg_model)^2)),
         sqrt(mean(resid(logit_xg_model)^2))),
  R2=c(summary(linear_xg_model)$r.squared,
       summary(logit_xg_model)$r.squared),
  Validation=c(mean(linear_predictions$linear_goals_validation),
               mean(logit_predictions$logit_goals_validation))
) %>%
  rbind(
    data.frame(Stat='Goals',
               Model='Random Forest',
               RMSE=sqrt(mean(rf_xg_model$mse)),
               R2=mean(rf_xg_model$rsq),
               Validation=mean(rf_predictions$rf_goals_validation))
    ) %>%
  rbind(
    data.frame(
      Stat = 'Own goals',
      Model=c('Linear', 'Logit'),
      RMSE=c(sqrt(mean(resid(linear_og_model)^2)),
             sqrt(mean(resid(logit_og_model)^2))),
      R2=c(summary(linear_og_model)$r.squared,
           summary(logit_xg_model)$r.squared),
      Validation=c(mean(linear_og_predictions$linear_og_validation),
                   mean(logit_og_predictions$logit_og_validation))
    ),
    data.frame(
      Stat='Own goals',
      Model='Random Forest',
      RMSE=sqrt(mean(rf_og_model$mse)),
      R2=mean(rf_og_model$rsq),
      Validation=c(mean(rf_og_predictions$rf_og_validation))
    ),
    data.frame(
      Stat='Penalties missed',
      Model=c('Linear', 'Logit'),
      RMSE=c(sqrt(mean(resid(linear_pen_model)^2)),
             sqrt(mean(resid(logit_pen_model)^2))),
      R2=c(summary(linear_pen_model)$r.squared,
           summary(logit_pen_model)$r.squared),
      Validation=c(mean(linear_pen_predictions$linear_pen_validation),
                   mean(logit_pen_predictions$logit_pen_validation))
    ),
    data.frame(
      Stat='Penalties missed',
      Model='Random Forest',
      RMSE=sqrt(mean(rf_pen_model$mse)),
      R2=mean(rf_pen_model$rsq),
      Validation=mean(rf_pen_predictions$rf_pen_validation)
    )
  )

## Use random forest model - best metrics (?)
goals_results <- linear_predictions %>%
  distinct(name, GW, .keep_all = T) %>%
  left_join(logit_predictions %>%
              distinct(name, GW, .keep_all = T)) %>%
  left_join(rf_predictions %>%
              distinct(name, GW, .keep_all = T)) %>%
  left_join(linear_og_predictions %>%
              distinct(name, GW, .keep_all = T)) %>%
  left_join(logit_og_predictions %>%
              distinct(name, GW, .keep_all = T)) %>%
  left_join(rf_og_predictions %>%
              distinct(name, GW, .keep_all = T)) %>%
  left_join(linear_pen_predictions %>%
              distinct(name, GW, .keep_all = T)) %>%
  left_join(logit_pen_predictions %>%
              distinct(name, GW, .keep_all = T)) %>%
  left_join(rf_pen_predictions %>%
              distinct(name, GW, .keep_all = T)) %>%
  left_join(temp2) %>%
  left_join(df2 %>% select(name, position, team, GW, opponent, h_a, season)) %>%
  select(name, position, team, GW, opponent, h_a, everything(), -contains('validation')) %>%
  rename(Player=name, Position=position, Gameweek=GW, Opponent=opponent,
         `Home/Away`=h_a, Team=team, Season=season) %>%
  mutate(`Home/Away`=ifelse(`Home/Away`=='h', 'Home', 'Away'),
         ) %>%
  distinct(Player, Gameweek, .keep_all = T)


## Validation: want to check for overfitting 
### Using k-fold cross validation  
# set.seed(123)
# temp <- trainControl(method = 'cv', number = 5)
# train_linear <- train(goals ~ xG + ict_index + position + h_a + team + opponent,
#                       data = df2, method ='lm', trControl = temp)
# print(train_linear$results$RMSE)
# 
# train_logit <- train(goals ~ xG + ict_index + position + h_a + team + opponent,
#                      data = df2, method='glm', trControl = temp)
# print(train_logit$results$RMSE)

# train_rf <- train(goals ~ xG + ict_index + position + h_a + team + opponent,
#                      data = df2, method='rf', trControl = temp)
# print(train_rf$results$RMSE)

objects <- ls()
keep <- objects[grep('results|comp|data|fixtures|ids', objects)]
rm(list=setdiff(objects, keep))
gc()
