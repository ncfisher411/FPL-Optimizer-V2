#---------------------------------------#
# Assist model for the FPL Lineup Optimizer
# Written by: ncfisher
# Last updated: May 25 2024
#---------------------------------------#

## Now test the models: Linear, logit, random forest
### Dependent variable: # of assists
### Independent variables: xA, ict_index, home/away, position (other vars tested and not important)
linear_assist_model <- lm(assists ~ xA + ict_index + h_a + position + strength + difficulty, data = est_data)
summary(linear_assist_model)
logit_assist_model <- glm(assists ~ xA + ict_index + h_a + position + strength + difficulty, data = est_data)
summary(logit_assist_model)
rf_assist_model <- randomForest(assists ~ xA + ict_index + h_a + position + strength + difficulty, data = est_data)

## Get an avg xA, ict_index for each player by opponent
avg_team <- est_data %>% 
  group_by(name, team, position, season, difficulty) %>%
  summarize(xA_opp=mean(xA, na.rm = T),
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
    summarize(xA_opp=mean(xA_opp, na.rm =T ),
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

## Get an avg xA, ict_index for each player by home or away
avg_h_a <- est_data %>% 
  group_by(name, team, position, h_a, season) %>%
  summarize(xA=mean(xA, na.rm = T),
            ict_index=mean(ict_index, na.rm = T)) %>%
  ungroup() %>%
  pivot_wider(names_from = h_a, values_from = c(xA, ict_index))

if(max(avg_h_a$season)==max(val_data$season)) {
  avg_h_a <- avg_h_a %>% filter(season==max(season))
} else if(max(avg_h_a$season)!=max(val_data$season)) {
  avg_h_a <- avg_h_a %>%
    filter(season==max(season)) %>%
    mutate(season=max(val_data$season))
  
  temp <- avg_h_a %>%
    group_by(position) %>%
    summarize(xA_a=mean(xA_a, na.rm =T ),
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
    summarize(xA_a=mean(xA_a, na.rm =T ),
              xA_h=mean(xA_h, na.rm =T ),
              ict_index_a=mean(ict_index_a, na.rm = T),
              ict_index_h=mean(ict_index_h, na.rm = T))%>%
    ungroup()
}

## Get an avg for the season by player
avg_season <- est_data %>%
  group_by(name, team, position, season) %>%
  summarize(xA_season=mean(xA, na.rm = T),
            ict_index_season = mean(ict_index, na.rm = T)) %>%
  filter(!is.na(xA_season))

if(max(avg_season$season)==max(val_data$season)) {
  avg_season <- avg_season %>% filter(season==max(season))
} else if (max(avg_season$season)!=max(val_data$season)) {
  avg_season <- avg_season %>%
    filter(season==max(season)) %>%
    mutate(season=max(val_data$season))
  
  temp <- avg_season %>%
    group_by(position) %>%
    summarize(xA_season=mean(xA_season, na.rm = T),
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
    summarize(xA_season=mean(xA_season, na.rm = T),
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
      xA=ifelse(h_a=='h', (xA_season*0.5) + (xA_h*0.3) + (xA_opp*0.2),
                (xA_season*0.5) + (xA_a*0.3) + (xA_opp*0.2)),
      ict_index=ifelse(h_a=='h', (ict_index_season*0.5) + (ict_index_h)*0.3 + (ict_index_opp*0.2),
                       (ict_index_season*0.5) + (ict_index_a*0.3) + (ict_index_opp*0.2))
    ) %>% select(name, web_name, team, position, season, GW, kickoff_time, h_a, opponent, strength, difficulty, xA, ict_index) %>%
    distinct(name, GW, .keep_all = T) %>%
    # mutate_if(is.numeric, function(x) ifelse(is.na(x), 0, x)) %>%
    group_by(team, h_a, strength, difficulty, position) %>%
    mutate(xA=ifelse(is.na(xA), mean(xA, na.rm = T), xA),
           ict_index=ifelse(is.na(ict_index), mean(ict_index, na.rm = T), ict_index)) %>%
    ungroup()
  
} else if (nrow(temp) > 0) {
  
  temp2 <- temp %>%
    select(name, web_name, team, position, season, GW, kickoff_time, h_a, opponent, strength, difficulty) %>%
    left_join(avg_season) %>%
    left_join(avg_h_a) %>%
    left_join(avg_team) %>%
    mutate(
      xA=ifelse(h_a=='h', (xA_season*0.5) + (xA_h*0.3) + (xA_opp*0.2),
                (xA_season*0.5) + (xA_a*0.3) + (xA_opp*0.2)),
      ict_index=ifelse(h_a=='h', (ict_index_season*0.5) + (ict_index_h)*0.3 + (ict_index_opp*0.2),
                       (ict_index_season*0.5) + (ict_index_a*0.3) + (ict_index_opp*0.2))
    ) %>% select(name, web_name, team, position, season, GW, kickoff_time, h_a, opponent, strength, difficulty, xA, ict_index) %>%
    distinct(name, GW, .keep_all = T) %>%
    # mutate_if(is.numeric, function(x) ifelse(is.na(x), 0, x)) %>%
    group_by(team, h_a, strength, difficulty, position) %>%
    mutate(xA=ifelse(is.na(xA), mean(xA, na.rm = T), xA),
           ict_index=ifelse(is.na(ict_index), mean(ict_index, na.rm = T), ict_index)) %>%
    ungroup()
  
  df2 <- val_data %>%
    filter(!(GW %in% temp$GW)) %>%
    select(name, web_name, team, position, season, GW, kickoff_time, h_a, opponent, strength, difficulty, xA, ict_index) %>%
    rbind(temp2) %>%
    distinct(name, GW, .keep_all = T) %>%
    # mutate_if(is.numeric, function(x) ifelse(is.na(x), 0, x))
    group_by(team, h_a, strength, difficulty, position) %>%
    mutate(xA=ifelse(is.na(xA), mean(xA, na.rm = T), xA),
           ict_index=ifelse(is.na(ict_index), mean(ict_index, na.rm = T), ict_index)) %>%
    ungroup()
  
} else {
  
  print('Error in assigning played vs not played games')
  
}

## Run the predictions of each model
linear_predictions <- predict(linear_assist_model, df2) %>% data.frame() %>%
  rename(Predicted_assists_linear=1) %>%
  cbind(df2) %>%
  select(name, position, team, opponent, h_a, GW, season, Predicted_assists_linear) %>%
  left_join(val_data %>% select(name, position, team, GW, assists), by=c('name', 'position', 'team', 'GW')) %>%
  mutate(Predicted_assists_linear=ifelse(Predicted_assists_linear<0, 0, Predicted_assists_linear),
         linear_assists_validation=assists-Predicted_assists_linear) %>%
  select(-assists)

logit_predictions <- predict(logit_assist_model, df2) %>% data.frame() %>%
  rename(Predicted_assists_logit=1) %>%
  cbind(df2) %>%
  select(name, position, team, opponent, h_a, GW, season, Predicted_assists_logit) %>%
  left_join(val_data %>% select(name, position, team, GW, assists), by=c('name', 'position', 'team', 'GW')) %>%
  mutate(Predicted_assists_logit=ifelse(Predicted_assists_logit<0, 0, Predicted_assists_logit),
         logit_assists_validation=assists-Predicted_assists_logit) %>%
  select(-assists)

rf_predictions <- predict(rf_assist_model, df2) %>% data.frame() %>%
  rename(Predicted_assists_rf=1) %>%
  cbind(df2) %>%
  select(name, position, team, opponent, h_a, GW, season, Predicted_assists_rf) %>%
  left_join(val_data %>% select(name, position, team, GW, assists), by=c('name', 'position', 'team', 'GW')) %>%
  mutate(Predicted_assists_rf=ifelse(Predicted_assists_rf<0, 0, Predicted_assists_rf),
         rf_assists_validation=assists-Predicted_assists_rf) %>%
  select(-assists)

## Metrics summary for model choice justification
comp_assists <- data.frame(
  Stat='Assists',
  Model=c('Linear', 'Logit'),
  RMSE=c(sqrt(mean(resid(linear_assist_model)^2)),
         sqrt(mean(resid(logit_assist_model)^2))),
  R2=c(summary(linear_assist_model)$r.squared,
       summary(logit_assist_model)$r.squared),
  Validation=c(mean(linear_predictions$linear_assists_validation),
               mean(logit_predictions$logit_assists_validation))
) %>%
  rbind(
    data.frame(Stat='Assists',
               Model='Random Forest',
               RMSE=sqrt(mean(rf_assist_model$mse)),
               R2=mean(rf_assist_model$rsq),
               Validation=mean(rf_predictions$rf_assists_validation))
  )

assists_results <- linear_predictions %>%
  distinct(name, GW, .keep_all = T) %>%
  left_join(logit_predictions %>% distinct(name, GW, .keep_all = T)) %>%
  left_join(rf_predictions %>% distinct(name, GW, .keep_all = T)) %>%
  left_join(temp2) %>%
  left_join(df2 %>% select(name, position, team, GW, opponent, h_a, season)) %>%
  select(name, position, team, GW, opponent, h_a, everything(), -contains('validation')) %>%
  rename(Player=name, Position=position, Gameweek=GW, Opponent=opponent,
         `Home/Away`=h_a, Team=team, Season=season) %>%
  mutate(`Home/Away`=ifelse(`Home/Away`=='h', 'Home', 'Away')) %>%
  distinct(Player, Gameweek, .keep_all = T)

objects <- ls()
keep <- objects[grep('results|comp|data|fixtures|ids', objects)]
rm(list=setdiff(objects, keep))
gc()

## Validation: want to check for overfitting after adding overperformance
### Using k-fold cross validation  
# set.seed(123)
# temp <- trainControl(method = 'cv', number = 5)
# train_linear <- train(assists ~ xA + ict_index + h_a + position + team + opponent,
#                       data = df2, method ='lm', trControl = temp)
# print(train_linear$results$RMSE)
# 
# train_logit <- train(assists ~ xA + ict_index + h_a + position + team + opponent,
#                      data = df2, method='glm', trControl = temp)
# print(train_logit$results$RMSE)
# 
# train_rf <- train(assists ~ xA + ict_index + h_a + position + team + opponent,
#                   data = df2, method='rf', trControl = temp)
# print(train_rf$results$RMSE)
