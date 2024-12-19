#---------------------------------------#
# Bous points model for the FPL Lineup Optimizer
# Written by: ncfisher
# Last updated: August 31 2024
#---------------------------------------#

# Add in the model for bonus points here since we have all the data on-hand

## Model set up
if(bonus_model=='linear'){
  linear_bonus <- lm(bonus ~ ict_index + xG + xA + clean_sheet + played + played60 + ict_index_opponent + difficulty + strength + position + h_a, data = est_data)
  
  ### predictions
  bonus_predictions <- predict(linear_bonus, predict_data) %>% data.frame() %>%
    rename(Predicted_bonus=1) %>%
    cbind(predict_data) %>%
    select(name, position, team, season, GW, opponent, h_a, Predicted_bonus) %>%
    left_join(val_data %>% select(name, position, team, GW, bonus), by=c('name', 'position', 'team', 'GW')) %>%
    mutate(Predicted_bonus=ifelse(Predicted_bonus<0, 0, Predicted_bonus),
           Predicted_bonus=ifelse(Predicted_bonus > 3, 3, Predicted_bonus),
           Bonus_validation=bonus-Predicted_bonus) %>%
    select(-bonus)
  
} else if(bonus_model=='logit'){
  logit_bonus <- glm(bonus ~ ict_index + xG + xA + clean_sheet + played + played60 + ict_index_opponent + difficulty + strength + position + h_a, data = est_data)
  
  ### predictions
  bonus_predictions <- predict(logit_bonus, predict_data) %>% data.frame() %>%
    rename(Predicted_bonus=1) %>%
    cbind(predict_data) %>%
    select(name, position, team, season, GW, opponent, h_a, Predicted_bonus) %>%
    left_join(val_data %>% select(name, position, team, GW, bonus), by=c('name', 'position', 'team', 'GW')) %>%
    mutate(Predicted_bonus=ifelse(Predicted_bonus<0, 0, Predicted_bonus),
           Predicted_bonus=ifelse(Predicted_bonus > 3, 3, Predicted_bonus),
           Bonus_validation=bonus-Predicted_bonus) %>%
    select(-bonus)
  
} else if(bonus_model=='random forest'){
  rf_bonus <- randomForest(bonus ~ ict_index + xG + xA + clean_sheet + played + played60 + ict_index_opponent + difficulty + strength + position + h_a, data = est_data)
  
  ### predictions
  bonus_predictions <- predict(rf_bonus, predict_data) %>% data.frame() %>%
    rename(Predicted_bonus=1) %>%
    cbind(predict_data) %>%
    select(name, position, team, season, GW, opponent, h_a, Predicted_bonus) %>%
    left_join(val_data %>% select(name, position, team, GW, bonus), by=c('name', 'position', 'team', 'GW')) %>%
    mutate(Predicted_bonus=ifelse(Predicted_bonus<0, 0, Predicted_bonus),
           Predicted_bonus=ifelse(Predicted_bonus > 3, 3, Predicted_bonus),
           Bonus_validation=bonus-Predicted_bonus) %>%
    select(-bonus)
  
}
gc()

bonus_results <- bonus_predictions %>% distinct(name, GW, .keep_all = T) %>%
  left_join(predict_data %>% select(name, position, team, GW, opponent, h_a, season)) %>%
  select(name, position, team, GW, opponent, h_a, everything(), -contains('validation')) %>%
  rename(Player=name, Position=position, Gameweek=GW, Opponent=opponent,
         `Home/Away`=h_a, Team=team, Season=season) %>%
  mutate(`Home/Away`=ifelse(`Home/Away`=='h', 'Home', 'Away')) %>%
  distinct(Player, Gameweek, .keep_all = T)

objects <- ls()
keep <- objects[grep('results|model|data|fixtures|ids', objects)]
rm(list=setdiff(objects, keep))
gc()
