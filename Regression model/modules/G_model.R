#---------------------------------------#
# Goals model for the FPL Lineup Optimizer
# Written by: ncfisher
# Last updated: August 31 2024
#---------------------------------------#

# This script is used for modeling match xG for the FPL optimizer

## Now test model variations for predicting goals using xG and ICT index
### Dependent variable = goals
### Independent variable = xG, ICT, position, team, home/away, opponent
if(goals_model=='linear'){
  linear_goals <- lm(goals ~ xG + xA + ict_index  + played + played60 + position + h_a + strength + difficulty, data = est_data)
  
  ## predictions
  goal_predictions <- predict(linear_goals, predict_data) %>% data.frame() %>%
    rename(Predicted_goals=1) %>%
    cbind(predict_data) %>%
    select(name, position, team, season, GW, opponent, h_a, Predicted_goals) %>%
    left_join(val_data %>% select(name, position, team, GW, goals), by=c('name', 'position', 'team', 'GW')) %>%
    mutate(Predicted_goals=ifelse(Predicted_goals<0, 0, Predicted_goals),
           Goals_validation=goals-Predicted_goals) %>%
    select(-goals)
  
  ## Save summary of model output
  table_goals = linear_goals %>% tidy()
  write.table(table_goals, 'Model performance summaries/goals.txt', sep = '\t', row.names = F)
  
} else if(goals_model=='logit') {
  logit_goals <- glm(goals ~ xG + xA + ict_index + played + played60 + position + h_a + strength + difficulty, data = est_data)
  
  ### predictions
  goal_predictions <- predict(logit_goals, predict_data) %>% data.frame() %>%
    rename(Predicted_goals=1) %>%
    cbind(predict_data) %>%
    select(name, position, team, season, GW, opponent, h_a, Predicted_goals) %>%
    left_join(val_data %>% select(name, position, team, GW, goals), by=c('name', 'position', 'team', 'GW')) %>%
    mutate(Predicted_goals=ifelse(Predicted_goals<0, 0, Predicted_goals),
           Goals_validation=goals-Predicted_goals) %>%
    select(-goals)
  
  ## Save summary of model output
  table_goals = logit_goals %>% tidy()
  write.table(table_goals, 'Model performance summaries/goals.txt', sep = '\t', row.names = F)
  
} else if(goals_model=='random forest'){
  rf_goals <- randomForest(goals ~ xG + ict_index + played + played60 + position + h_a + strength + difficulty, data = est_data %>% filter(!is.na(goals)))
  
  ### predictions
  goal_predictions <- predict(rf_goals, predict_data) %>% data.frame() %>%
    rename(Predicted_goals=1) %>%
    cbind(predict_data) %>%
    select(name, position, team, season, GW, opponent, h_a, Predicted_goals) %>%
    left_join(val_data %>% select(name, position, team, GW, goals), by=c('name', 'position', 'team', 'GW')) %>%
    mutate(Predicted_goals=ifelse(Predicted_goals<0, 0, Predicted_goals),
           Goals_validation=goals-Predicted_goals) %>%
    select(-goals)
}
gc()

if(own_goals_model=='linear'){
  linear_og <- lm(own_goals ~ ict_index_opponent + xG_opponent + played + played60 + position + strength + difficulty + h_a, data = est_data)
  
  ### predictions
  og_predictions <- predict(linear_og, predict_data) %>%
    data.frame() %>%
    rename(Predicted_og=1) %>%
    cbind(predict_data) %>%
    select(name, position, team, season, GW, opponent, h_a, Predicted_og) %>%
    left_join(val_data %>% select(name, position, team, GW, own_goals), by=c('name', 'position', 'team', 'GW')) %>%
    mutate(Predicted_og=ifelse(Predicted_og < 0, 0, Predicted_og),
           Predicted_og_validation=own_goals-Predicted_og) %>%
    select(-own_goals)
  
  ## Save summary of model output
  table_og = linear_og %>% tidy()
  write.table(table_og, 'Model performance summaries/own_goals.txt', sep = '\t', row.names = F)
  
} else if(own_goals_model=='logit'){
  logit_og <- glm(own_goals ~ ict_index_opponent + xG_opponent + played + played60 + position + strength + difficulty + h_a, data = est_data)
  
  ### predictions
  og_predictions <- predict(logit_og, predict_data) %>%
    data.frame() %>%
    rename(Predicted_og=1) %>%
    cbind(predict_data) %>%
    select(name, position, team, season, GW, opponent, h_a, Predicted_og) %>%
    left_join(val_data %>% select(name, position, team, GW, own_goals), by=c('name', 'position', 'team', 'GW')) %>%
    mutate(Predicted_og=ifelse(Predicted_og < 0, 0, Predicted_og),
           Predicted_og_validation=own_goals-Predicted_og) %>%
    select(-own_goals)
  
  ## Save summary of model output
  table_og = logit_og %>% tidy()
  write.table(table_og, 'Model performance summaries/own_goals.txt', sep = '\t', row.names = F)
  
} else if(own_goals_model=='random forest'){
  rf_og <- randomForest(own_goals ~ ict_index_opponent + xG_opponent + played + played60 + position + strength + difficulty + h_a, data = est_data)
  
  ### predictions
  og_predictions <- predict(rf_og, predict_data) %>%
    data.frame() %>%
    rename(Predicted_og=1) %>%
    cbind(predict_data) %>%
    select(name, position, team, season, GW, opponent, h_a, Predicted_og) %>%
    left_join(val_data %>% select(name, position, team, GW, own_goals), by=c('name', 'position', 'team', 'GW')) %>%
    mutate(Predicted_og=ifelse(Predicted_og < 0, 0, Predicted_og),
           Predicted_og_validation=own_goals-Predicted_og) %>%
    select(-own_goals)
}
gc()

if(penalties_missed_model=='linear'){
  linear_pen <- lm(penalties_missed ~ xG + ict_index + position + h_a + strength + difficulty, data = est_data)
  
  ### predictions
  pen_predictions <- predict(linear_pen, predict_data) %>%
    data.frame() %>%
    rename(Predicted_pen_missed=1) %>%
    cbind(predict_data) %>%
    select(name, position, team, season, GW, opponent, h_a, Predicted_pen_missed) %>%
    left_join(val_data %>% select(name, position, team, GW, penalties_missed), by=c('name', 'position', 'team', 'GW')) %>%
    mutate(Predicted_pen_missed=ifelse(Predicted_pen_missed < 0, 0, Predicted_pen_missed),
           Pen_missed_validation=penalties_missed-Predicted_pen_missed) %>%
    select(-penalties_missed)
  
  ## Save summary of model output
  table_pen = linear_pen %>% tidy()
  write.table(table_pen, 'Model performance summaries/penalties_missed.txt', sep = '\t', row.names = F)
  
} else if(penalties_missed_model=='logit'){
  logit_pen <- glm(penalties_missed ~ xG + ict_index + position + h_a + strength + difficulty, data = est_data)
  
  pen_predictions <- predict(logit_pen, predict_data) %>%
    data.frame() %>%
    rename(Predicted_pen_missed=1) %>%
    cbind(predict_data) %>%
    select(name, position, team, season, GW, opponent, h_a, Predicted_pen_missed) %>%
    left_join(val_data %>% select(name, position, team, GW, penalties_missed), by=c('name', 'position', 'team', 'GW')) %>%
    mutate(Predicted_pen_missed=ifelse(Predicted_pen_missed < 0, 0, Predicted_pen_missed),
           Pen_missed_validation=penalties_missed-Predicted_pen_missed) %>%
    select(-penalties_missed)
  
  ## Save summary of model output
  table_pen = logit_pen %>% tidy()
  write.table(table_pen, 'Model performance summaries/penalties_missed.txt', sep = '\t', row.names = F)
  
} else if(penalties_missed_model=='random forest'){
  rf_pen <- randomForest(penalties_missed ~ xG + ict_index + position + h_a + strength + difficulty, data = est_data)
  
  pen_predictions <- predict(rf_pen, predict_data) %>%
    data.frame() %>%
    rename(Predicted_pen_missed=1) %>%
    cbind(predict_data) %>%
    select(name, position, team, season, GW, opponent, h_a, Predicted_pen_missed) %>%
    left_join(val_data %>% select(name, position, team, GW, penalties_missed), by=c('name', 'position', 'team', 'GW')) %>%
    mutate(Predicted_pen_missed=ifelse(Predicted_pen_missed < 0, 0, Predicted_pen_missed),
           Pen_missed_validation=penalties_missed-Predicted_pen_missed) %>%
    select(-penalties_missed)
}
gc()

goals_results <- goal_predictions %>% distinct(name, GW, .keep_all = T) %>%
  left_join(og_predictions %>% distinct(name, GW, .keep_all = T)) %>%
  left_join(pen_predictions %>% distinct(name, GW, .keep_all = T)) %>%
  left_join(predict_data %>% select(name, position, team, GW, opponent, h_a, season, value)) %>%
  select(name, position, team, value, GW, opponent, h_a, everything(), -contains('validation')) %>%
  rename(Player=name, Position=position, Gameweek=GW, Opponent=opponent,
         `Home/Away`=h_a, Team=team, Season=season) %>%
  mutate(`Home/Away`=ifelse(`Home/Away`=='h', 'Home', 'Away')) %>%
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
keep <- objects[grep('results|data|fixtures|ids|model|teams', objects)]
rm(list=setdiff(objects, keep))
gc()
