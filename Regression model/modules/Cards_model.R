#---------------------------------------#
# Yellow and Red Cards model for the FPL Lineup Optimizer
# Written by: ncfisher
# Last updated: August 31 2024
#---------------------------------------#

## Need to predict number of red cards, yellow cards, based on controls, ICT Index, goals conceded

## Set up the models
if(yellow_cards_model=='linear'){
  linear_yc <- lm(yellow_cards ~ ict_index_opponent + played + played60 + xG_opponent + goals_conceded + position + h_a + strength + difficulty, data = est_data)
  
  ### predictions
  yc_predictions <- predict(linear_yc, predict_data) %>%
    data.frame() %>%
    rename(Predicted_yellow_cards=1) %>%
    cbind(predict_data) %>%
    select(name, position, team, opponent, h_a, GW, season, Predicted_yellow_cards) %>%
    left_join(val_data %>% select(name, position, team, GW, yellow_cards), by=c('name', 'position', 'team', 'GW')) %>%
    mutate(Predicted_yellow_cards=ifelse(Predicted_yellow_cards < 0, 0, Predicted_yellow_cards),
           Yellow_cards_validation=yellow_cards-Predicted_yellow_cards) %>%
    select(-yellow_cards)
  
} else if(yellow_cards_model=='logit'){
  logit_yc <- glm(yellow_cards ~ ict_index_opponent + played + played60 + xG_opponent + goals_conceded + position + h_a + strength + difficulty, data = est_data)
  
  ### predictions
  yc_predictions <- predict(logit_yc, predict_data) %>%
    data.frame() %>%
    rename(Predicted_yellow_cards=1) %>%
    cbind(predict_data) %>%
    select(name, position, team, opponent, h_a, GW, season, Predicted_yellow_cards) %>%
    left_join(val_data %>% select(name, position, team, GW, yellow_cards), by=c('name', 'position', 'team', 'GW')) %>%
    mutate(Predicted_yellow_cards=ifelse(Predicted_yellow_cards < 0, 0, Predicted_yellow_cards),
           Yellow_cards_validation=yellow_cards-Predicted_yellow_cards) %>%
    select(-yellow_cards)
  
} else if(yellow_cards_model=='random forest'){
  rf_yc <- randomForest(yellow_cards ~ ict_index_opponent + played + played60 + xG_opponent + goals_conceded + position + h_a + strength + difficulty, data = est_data)
  
  ### predictions
  yc_predictions <- predict(rf_yc, predict_data) %>%
    data.frame() %>%
    rename(Predicted_yellow_cards=1) %>%
    cbind(predict_data) %>%
    select(name, position, team, opponent, h_a, GW, season, Predicted_yellow_cards) %>%
    left_join(val_data %>% select(name, position, team, GW, yellow_cards), by=c('name', 'position', 'team', 'GW')) %>%
    mutate(Predicted_yellow_cards=ifelse(Predicted_yellow_cards < 0, 0, Predicted_yellow_cards),
           Yellow_cards_validation=yellow_cards-Predicted_yellow_cards) %>%
    select(-yellow_cards)
  
}
gc()

if(red_cards_model=='linear'){
  linear_rc <- lm(red_cards ~ ict_index_opponent + xG_opponent + played + goals_conceded + position + h_a + strength + difficulty, data = est_data)
  
  ### predictions
  rc_predictions <- predict(linear_rc, predict_data) %>%
    data.frame() %>%
    rename(Predicted_red_cards=1) %>%
    cbind(predict_data) %>%
    select(name, position, team, opponent, h_a, GW, season, Predicted_red_cards) %>%
    left_join(val_data %>% select(name, position, team, GW, red_cards), by=c('name', 'position', 'team', 'GW')) %>%
    mutate(Predicted_red_cards=ifelse(Predicted_red_cards < 0, 0, Predicted_red_cards),
           Red_cards_validation=red_cards-Predicted_red_cards) %>%
    select(-red_cards)
  
} else if(red_cards_model=='logit'){
  logit_rc <- lm(red_cards ~ ict_index_opponent + xG_opponent + played + goals_conceded + position + h_a + strength + difficulty, data = est_data)
  
  ### predictions
  rc_predictions <- predict(logit_rc, predict_data) %>%
    data.frame() %>%
    rename(Predicted_red_cards=1) %>%
    cbind(predict_data) %>%
    select(name, position, team, opponent, h_a, GW, season, Predicted_red_cards) %>%
    left_join(val_data %>% select(name, position, team, GW, red_cards), by=c('name', 'position', 'team', 'GW')) %>%
    mutate(Predicted_red_cards=ifelse(Predicted_red_cards < 0, 0, Predicted_red_cards),
           Red_cards_validation=red_cards-Predicted_red_cards) %>%
    select(-red_cards)
  
} else if(red_cards_model=='random forest'){
  rf_rc <- randomForest(red_cards ~ ict_index_opponent + xG_opponent + played + goals_conceded + position + h_a + strength + difficulty, data = est_data)
  
  ### predictions
  rc_predictions <- predict(rf_rc, predict_data) %>%
    data.frame() %>%
    rename(Predicted_red_cards=1) %>%
    cbind(predict_data) %>%
    select(name, position, team, opponent, h_a, GW, season, Predicted_red_cards) %>%
    left_join(val_data %>% select(name, position, team, GW, red_cards), by=c('name', 'position', 'team', 'GW')) %>%
    mutate(Predicted_red_cards=ifelse(Predicted_red_cards < 0, 0, Predicted_red_cards),
           Red_cards_validation=red_cards-Predicted_red_cards) %>%
    select(-red_cards)
  
}
gc()

cards_results <- yc_predictions %>% distinct(name, GW, .keep_all = T) %>%
  left_join(rc_predictions %>% distinct(name, GW, .keep_all = T)) %>%
  select(name, position, team, GW, opponent, h_a, everything(), -contains('validation')) %>%
  rename(Player=name, Position=position, Gameweek=GW, Opponent=opponent,
         `Home/Away`=h_a, Team=team, Season=season) %>%
  mutate(`Home/Away`=ifelse(`Home/Away`=='h', 'Home', 'Away')) %>%
  distinct(Player, Gameweek, .keep_all = T)


objects <- ls()
keep <- objects[grep('results|model|data|fixtures|ids', objects)]
rm(list=setdiff(objects, keep))
gc()
