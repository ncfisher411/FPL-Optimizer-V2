#---------------------------------------#
# Assist model for the FPL Lineup Optimizer
# Written by: ncfisher
# Last updated: June 19 2026
#---------------------------------------#

## Now test the models: Linear, logit, random forest
### Dependent variable: # of assists
### Independent variables: xA, ict_index, home/away, position (other vars tested and not important)
##### In compile FPL script, decided to use logit model
if(assists_model=='linear'){
  linear_assist <- lm(assists ~ xA + ict_index + xG  + played60 + played + h_a + position + strength + difficulty, data = est_data, na.action = na.omit)
  
  ### predictions
  assist_predictions <- predict(linear_assist, predict_data) %>% data.frame() %>%
    rename(Predicted_assists=1) %>%
    cbind(predict_data) %>%
    select(name, position, team, opponent, h_a, GW, season, Predicted_assists) %>%
    left_join(val_data %>% select(name, position, team, GW, assists), by=c('name', 'position', 'team', 'GW')) %>%
    mutate(Predicted_assists = ifelse(Predicted_assists < 0, 0, Predicted_assists),
           Assists_validation=assists-Predicted_assists) %>%
    select(-assists)
  
  ## Save summary of model output
  table = linear_assist %>% tidy()
  write.table(table, 'Model performance summaries/assists.txt', sep = '\t', row.names = F)
  
} else if(assists_model=='logit'){
  logit_assist <- glm(assists ~ xA + ict_index + xG + played60 + played + h_a + position + strength + difficulty, data = est_data)
  
  ### predictions
  assist_predictions <- predict(logit_assist, predict_data) %>% data.frame() %>%
    rename(Predicted_assists=1) %>%
    cbind(predict_data) %>%
    select(name, position, team, opponent, h_a, GW, season, Predicted_assists) %>%
    left_join(val_data %>% select(name, position, team, GW, assists), by=c('name', 'position', 'team', 'GW')) %>%
    mutate(Predicted_assists = ifelse(Predicted_assists < 0, 0, Predicted_assists),
           Assists_validation=assists-Predicted_assists) %>%
    select(-assists)
  
  ## Save summary of model output
  table = logit_assist %>% tidy()
  write.table(table, 'Model performance summaries/assists.txt', sep = '\t', row.names = F)
  
} else if(assists_model=='random forest'){
  rf_assist <- randomForest(assists ~ xA + ict_index + xG + played60 + played + h_a + position + strength + difficulty, data = est_data)
  
  ### predictions
  assist_predictions <- predict(rf_assist, predict_data) %>% data.frame() %>%
    rename(Predicted_assists=1) %>%
    cbind(predict_data) %>%
    select(name, position, team, opponent, h_a, GW, season, Predicted_assists) %>%
    left_join(val_data %>% select(name, position, team, GW, assists), by=c('name', 'position', 'team', 'GW')) %>%
    mutate(Predicted_assists = ifelse(Predicted_assists < 0, 0, Predicted_assists),
           Assists_validation=assists-Predicted_assists) %>%
    select(-assists)
}
gc()

assists_results <- assist_predictions %>% distinct(name, GW, .keep_all = T) %>%
  left_join(predict_data %>% select(name, position, team, GW, opponent, h_a, season)) %>%
  select(name, position, team, GW, opponent, h_a, everything(), -contains('validation')) %>%
  rename(Player=name, Position=position, Gameweek=GW, Opponent=opponent,
         `Home/Away`=h_a, Team=team, Season=season) %>%
  mutate(`Home/Away`=ifelse(`Home/Away`=='h', 'Home', 'Away')) %>%
  distinct(Player, Gameweek, .keep_all = T)
  
objects <- ls()
keep <- objects[grep('results|data|fixtures|ids|model|teams', objects)]
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
