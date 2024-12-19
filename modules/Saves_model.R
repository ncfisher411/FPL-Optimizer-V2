#---------------------------------------#
# Saves model for the FPL Lineup Optimizer
# Written by: ncfisher
# Last updated: August 31 2024
#---------------------------------------#

## Build a model that predicts the number of saves a keeper will get in a match
### Will need to figure out how to build this so that we're showing the number of shots a keeper faced in a match
### Can cross-reference with vaastav data and which keeper was playing using FPL dictionary

## Now set up the model for saves
if(saves_model=='linear'){
  linear_saves <- lm(saves ~ xG_opponent  + ict_index_opponent + played60 + played + goals_conceded +  strength + difficulty + h_a, data = est_data)
  
  ### predictions
  save_predictions <- predict(linear_saves, predict_data) %>%
    data.frame() %>% 
    rename(Predicted_saves=1) %>%
    cbind(predict_data %>% select(-saves)) %>%
    select(name, position, team, opponent, h_a, GW, season, Predicted_saves) %>%
    left_join(val_data %>% select(name, position, team, GW, saves), by=c('name', 'position', 'team', 'GW')) %>%
    mutate(Predicted_saves=ifelse(Predicted_saves < 0, 0, Predicted_saves),
           Saves_validation=saves-Predicted_saves) %>%
    select(-saves)
  
} else if(saves_model=='logit'){
  logit_saves <- glm(saves ~ xG_opponent  + ict_index_opponent + played60 + played + goals_conceded +  strength + difficulty + h_a, data = est_data)
  
  ### predictions
  save_predictions <- predict(logit_saves, predict_data) %>%
    data.frame() %>% 
    rename(Predicted_saves=1) %>%
    cbind(predict_data %>% select(-saves)) %>%
    select(name, position, team, opponent, h_a, GW, season, Predicted_saves) %>%
    left_join(val_data %>% select(name, position, team, GW, saves), by=c('name', 'position', 'team', 'GW')) %>%
    mutate(Predicted_saves=ifelse(Predicted_saves < 0, 0, Predicted_saves),
           Saves_validation=saves-Predicted_saves) %>%
    select(-saves)
  
} else if(saves_model=='random forest'){
  rf_saves <- randomForest(saves ~ xG_opponent  + ict_index_opponent + played60 + played + goals_conceded +  strength + difficulty + h_a, data = est_data)
  
  ### predictions
  save_predictions <- predict(rf_saves, predict_data) %>%
    data.frame() %>% 
    rename(Predicted_saves=1) %>%
    cbind(predict_data %>% select(-saves)) %>%
    select(name, position, team, opponent, h_a, GW, season, Predicted_saves) %>%
    left_join(val_data %>% select(name, position, team, GW, saves), by=c('name', 'position', 'team', 'GW')) %>%
    mutate(Predicted_saves=ifelse(Predicted_saves < 0, 0, Predicted_saves),
           Saves_validation=saves-Predicted_saves) %>%
    select(-saves)
}
gc()

if(penalties_saved_model=='linear'){
  linear_pen <- lm(penalties_saved ~ saves + xG_opponent  + ict_index_opponent + played60 +  strength + difficulty + h_a, data = est_data)
  
  ### predictions
  pen_predictions <- predict(linear_pen, predict_data) %>%
    data.frame() %>%
    rename(Predicted_pen_saves=1) %>%
    cbind(predict_data) %>%
    select(name, position, team, opponent, h_a, GW, season, Predicted_pen_saves) %>%
    left_join(val_data %>% select(name, position, team, GW, penalties_saved), by=c('name', 'position', 'team', 'GW')) %>%
    mutate(Predicted_pen_saves=ifelse(Predicted_pen_saves < 0, 0, Predicted_pen_saves),
           Pen_saves_validation=penalties_saved-Predicted_pen_saves) %>%
    select(-penalties_saved)
  
} else if(penalties_saved_model=='logit'){
  logit_pen <- glm(penalties_saved ~ saves + xG_opponent  + ict_index_opponent + played60 +  strength + difficulty + h_a, data = est_data)
  
  ### predictions
  pen_predictions <- predict(logit_pen, predict_data) %>%
    data.frame() %>%
    rename(Predicted_pen_saves=1) %>%
    cbind(predict_data) %>%
    select(name, position, team, opponent, h_a, GW, season, Predicted_pen_saves) %>%
    left_join(val_data %>% select(name, position, team, GW, penalties_saved), by=c('name', 'position', 'team', 'GW')) %>%
    mutate(Predicted_pen_saves=ifelse(Predicted_pen_saves < 0, 0, Predicted_pen_saves),
           Pen_saves_validation=penalties_saved-Predicted_pen_saves) %>%
    select(-penalties_saved)
  
} else if(penalties_saved_model=='random forest'){
  rf_pen <- randomForest(penalties_saved ~ saves + xG_opponent  + ict_index_opponent + played60 +  strength + difficulty + h_a, data = est_data)
  
  ### predictions
  pen_predictions <- predict(rf_pen, predict_data) %>%
    data.frame() %>%
    rename(Predicted_pen_saves=1) %>%
    cbind(predict_data) %>%
    select(name, position, team, opponent, h_a, GW, season, Predicted_pen_saves) %>%
    left_join(val_data %>% select(name, position, team, GW, penalties_saved), by=c('name', 'position', 'team', 'GW')) %>%
    mutate(Predicted_pen_saves=ifelse(Predicted_pen_saves < 0, 0, Predicted_pen_saves),
           Pen_saves_validation=penalties_saved-Predicted_pen_saves) %>%
    select(-penalties_saved)
}
gc()

if(goals_conceded_model=='linear'){
  linear_goals_conceded <- lm(goals_conceded ~ xG_opponent  + ict_index_opponent + played60 + played + ict_index +  strength + difficulty + h_a, data = est_data)
  
  ### predictions
  goals_conceded_prediction <- predict(linear_goals_conceded, predict_data) %>%
    data.frame() %>%
    rename(Predicted_goals_conceded=1) %>%
    cbind(predict_data %>% select(-goals_conceded)) %>% 
    select(name, position, team, opponent, h_a, GW, season, Predicted_goals_conceded) %>%
    left_join(val_data %>% select(name, position, team, GW, goals_conceded), by=c('name', 'position', 'team', 'GW')) %>%
    mutate(Predicted_goals_conceded=ifelse(Predicted_goals_conceded < 0, 0, Predicted_goals_conceded),
           Goals_conceded_validation=goals_conceded-Predicted_goals_conceded) %>%
    select(-goals_conceded)
  
} else if(goals_conceded_model=='logit'){
  logit_goals_conceded <- glm(goals_conceded ~ xG_opponent  + ict_index_opponent + played60 + played + ict_index +  strength + difficulty + h_a, data = est_data)
  
  ### predictions
  goals_conceded_prediction <- predict(logit_goals_conceded, predict_data) %>%
    data.frame() %>%
    rename(Predicted_goals_conceded=1) %>%
    cbind(predict_data %>% select(-goals_conceded)) %>% 
    select(name, position, team, opponent, h_a, GW, season, Predicted_goals_conceded) %>%
    left_join(val_data %>% select(name, position, team, GW, goals_conceded), by=c('name', 'position', 'team', 'GW')) %>%
    mutate(Predicted_goals_conceded=ifelse(Predicted_goals_conceded < 0, 0, Predicted_goals_conceded),
           Goals_conceded_validation=goals_conceded-Predicted_goals_conceded) %>%
    select(-goals_conceded)
  
} else if(goals_conceded_model=='random forest'){
  rf_goals_conceded <- randomForest(goals_conceded ~ xG_opponent  + ict_index_opponent + played60 + played + ict_index +  strength + difficulty + h_a, data = est_data)
  
  ### predictions
  goals_conceded_prediction <- predict(rf_goals_conceded, predict_data) %>%
    data.frame() %>%
    rename(Predicted_goals_conceded=1) %>%
    cbind(predict_data %>% select(-goals_conceded)) %>% 
    select(name, position, team, opponent, h_a, GW, season, Predicted_goals_conceded) %>%
    left_join(val_data %>% select(name, position, team, GW, goals_conceded), by=c('name', 'position', 'team', 'GW')) %>%
    mutate(Predicted_goals_conceded=ifelse(Predicted_goals_conceded < 0, 0, Predicted_goals_conceded),
           Goals_conceded_validation=goals_conceded-Predicted_goals_conceded) %>%
    select(-goals_conceded)
  
}
gc()

save_results <- save_predictions %>% distinct(name, GW, .keep_all = T) %>%
  left_join(pen_predictions %>% distinct(name, GW, .keep_all = T)) %>%
  left_join(goals_conceded_prediction %>% distinct(name, GW, .keep_all = T)) %>%
  select(name, position, team, GW, opponent, h_a, everything(), -contains('validation')) %>%
  rename(Player=name, Position=position, Gameweek=GW, Opponent=opponent,
         `Home/Away`=h_a, Team=team, Season=season) %>%
  mutate(`Home/Away`=ifelse(`Home/Away`=='h', 'Home', 'Away')) %>%
  distinct(Player, Gameweek, .keep_all = T)

objects <- ls()
keep <- objects[grep('results|model|data|fixtures|ids', objects)]
rm(list=setdiff(objects, keep))
gc()
