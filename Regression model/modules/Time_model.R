#---------------------------------------#
# Time played model for the FPL Lineup Optimizer
# Written by: ncfisher
# Last updated: August 31 2024
#---------------------------------------#

# Model set up
if(played_model=='linear'){
   linear_played <- lm(played ~ ict_index_opponent + xG_opponent + ict_index + goals_conceded + xG + xA + position + strength + difficulty + h_a, data = est_data)
   
   ### predictions
   played_predictions <- predict(linear_played, predict_data) %>%
      data.frame() %>%
      rename(Probability_played=1) %>%
      cbind(predict_data) %>%
      select(name, position, team, opponent, h_a, GW, season, Probability_played) %>%
      left_join(val_data %>% select(name, position, team, GW, minutes), by=c('name', 'position', 'team', 'GW')) %>%
      distinct(name, GW, .keep_all = T) %>%
      mutate(Probability_played=ifelse(Probability_played < 0, 0, Probability_played),
             Probability_played=ifelse(Probability_played > 1, 1, Probability_played),
             played=ifelse(minutes > 0, 1, 0),
             played_validation=played-Probability_played) %>%
      select(-played, -minutes)
   
} else if(played_model=='logit'){
   logit_played <- glm(played ~ ict_index_opponent + xG_opponent + ict_index + goals_conceded + xG + xA + position + strength + difficulty + h_a, data = est_data)  
   
   ### predictions
   played_predictions <- predict(logit_played, predict_data) %>%
      data.frame() %>%
      rename(Probability_played=1) %>%
      cbind(predict_data) %>%
      select(name, position, team, opponent, h_a, GW, season, Probability_played) %>%
      left_join(val_data %>% select(name, position, team, GW, minutes), by=c('name', 'position', 'team', 'GW')) %>%
      distinct(name, GW, .keep_all = T) %>%
      mutate(Probability_played=ifelse(Probability_played < 0, 0, Probability_played),
             Probability_played=ifelse(Probability_played > 1, 1, Probability_played),
             played=ifelse(minutes > 0, 1, 0),
             played_validation=played-Probability_played) %>%
      select(-played, -minutes)
   
} else if(played_model=='random forest'){
   rf_played <- randomForest(played ~ ict_index_opponent + xG_opponent + ict_index + goals_conceded + xG + xA + position + strength + difficulty + h_a, data = est_data)
   
   ### predictions
   played_predictions <- predict(rf_played, predict_data) %>%
      data.frame() %>%
      rename(Probability_played=1) %>%
      cbind(predict_data) %>%
      select(name, position, team, opponent, h_a, GW, season, Probability_played) %>%
      left_join(val_data %>% select(name, position, team, GW, minutes), by=c('name', 'position', 'team', 'GW')) %>%
      distinct(name, GW, .keep_all = T) %>%
      mutate(Probability_played=ifelse(Probability_played < 0, 0, Probability_played),
             Probability_played=ifelse(Probability_played > 1, 1, Probability_played),
             played=ifelse(minutes > 0, 1, 0),
             played_validation=played-Probability_played) %>%
      select(-played, -minutes)
   
}
gc()

if(played60_model=='linear'){
   linear_played60 <- lm(played60 ~ ict_index_opponent + xG_opponent + ict_index + goals_conceded + xG + xA +  position + strength + difficulty + h_a, data = est_data)
   
   ### predictions
   played60_predictions <- predict(linear_played60, predict_data) %>%
      data.frame() %>%
      rename(Probability_played60=1) %>%
      cbind(predict_data) %>%
      select(name, position, team, opponent, h_a, GW, season, Probability_played60) %>%
      left_join(val_data %>% select(name, position, team, GW, minutes), by=c('name', 'position', 'team', 'GW')) %>%
      distinct(name, GW, .keep_all = T) %>%
      mutate(Probability_played60=ifelse(Probability_played60 < 0, 0, Probability_played60),
             Probability_played60=ifelse(Probability_played60 > 1, 1, Probability_played60),
             played60=ifelse(minutes > 59, 1, 0),
             played60_validation=played60-Probability_played60) %>%
      select(-minutes)
   
} else if(played60_model=='logit'){
   logit_played60 <- glm(played60 ~ ict_index_opponent + xG_opponent + ict_index + goals_conceded + xG + xA +  position + strength + difficulty + h_a, data = est_data)
   
   ### predictions
   played60_predictions <- predict(logit_played60, predict_data) %>%
      data.frame() %>%
      rename(Probability_played60=1) %>%
      cbind(predict_data) %>%
      select(name, position, team, opponent, h_a, GW, season, Probability_played60) %>%
      left_join(val_data %>% select(name, position, team, GW, minutes), by=c('name', 'position', 'team', 'GW')) %>%
      distinct(name, GW, .keep_all = T) %>%
      mutate(Probability_played60=ifelse(Probability_played60 < 0, 0, Probability_played60),
             Probability_played60=ifelse(Probability_played60 > 1, 1, Probability_played60),
             played60=ifelse(minutes > 59, 1, 0),
             played60_validation=played60-Probability_played60) %>%
      select(-minutes)
   
} else if(played60_model=='random forest'){
   rf_played60 <- randomForest(played60 ~ ict_index_opponent + xG_opponent + ict_index + goals_conceded + xG + xA +  position + strength + difficulty + h_a, data = est_data)
   
   ### predictions
   played60_predictions <- predict(rf_played60, predict_data) %>%
      data.frame() %>%
      rename(Probability_played60=1) %>%
      cbind(predict_data) %>%
      select(name, position, team, opponent, h_a, GW, season, Probability_played60) %>%
      left_join(val_data %>% select(name, position, team, GW, minutes), by=c('name', 'position', 'team', 'GW')) %>%
      distinct(name, GW, .keep_all = T) %>%
      mutate(Probability_played60=ifelse(Probability_played60 < 0, 0, Probability_played60),
             Probability_played60=ifelse(Probability_played60 > 1, 1, Probability_played60),
             played60=ifelse(minutes > 59, 1, 0),
             played60_validation=played60-Probability_played60) %>%
      select(-minutes)
   
}
gc()

if(cs_model=='linear'){
   linear_cs <- lm(clean_sheet ~ played + played60 + ict_index + ict_index_opponent + xG_opponent + position + strength + difficulty + h_a, data = est_data)
   
   ### predictions
   cs_prediction <- predict(linear_cs, predict_data %>%
                               left_join(played60_predictions %>%
                                            select(name, team, position, GW, Probability_played60) %>%
                                            rename(played60=Probability_played60))
                            ) %>%
      data.frame() %>%
      rename(Probability_cs=1) %>%
      cbind(predict_data) %>%
      select(name, position, team, opponent, h_a, GW, season, Probability_cs) %>%
      left_join(val_data %>% select(name, position, team, GW, minutes, goals_conceded), by=c('name', 'position', 'team', 'GW')) %>%
      distinct(name, GW, .keep_all = T) %>%
      mutate(Probability_cs=ifelse(Probability_cs < 0, 0, Probability_cs),
             Probability_cs=ifelse(Probability_cs > 1, 1, Probability_cs))
   
} else if(cs_model=='logit'){
   logit_cs <- glm(clean_sheet ~ played + played60 + ict_index + ict_index_opponent + xG_opponent + position + strength + difficulty + h_a, data = est_data)
   
   ### predictions
   cs_prediction <- predict(logit_cs, predict_data %>%
                               left_join(played60_predictions %>%
                                            select(name, team, position, GW, Probability_played60) %>%
                                            rename(played60=Probability_played60))
   ) %>%
      data.frame() %>%
      rename(Probability_cs=1) %>%
      cbind(predict_data) %>%
      select(name, position, team, opponent, h_a, GW, season, Probability_cs) %>%
      left_join(val_data %>% select(name, position, team, GW, minutes, goals_conceded), by=c('name', 'position', 'team', 'GW')) %>%
      distinct(name, GW, .keep_all = T) %>%
      mutate(Probability_cs=ifelse(Probability_cs < 0, 0, Probability_cs),
             Probability_cs=ifelse(Probability_cs > 1, 1, Probability_cs))
   
}  else if(cs_model=='random forest'){
   rf_cs <- randomForest(clean_sheet ~ played + played60 + ict_index + ict_index_opponent + xG_opponent + position + strength + difficulty + h_a, data = est_data)
   
   ### predictions
   cs_prediction <- predict(rf_cs, predict_data %>%
                               left_join(played60_predictions %>%
                                            select(name, team, position, GW, Probability_played60) %>%
                                            rename(played60=Probability_played60))
   ) %>%
      data.frame() %>%
      rename(Probability_cs=1) %>%
      cbind(predict_data) %>%
      select(name, position, team, opponent, h_a, GW, season, Probability_cs) %>%
      left_join(val_data %>% select(name, position, team, GW, minutes, goals_conceded), by=c('name', 'position', 'team', 'GW')) %>%
      distinct(name, GW, .keep_all = T) %>%
      mutate(Probability_cs=ifelse(Probability_cs < 0, 0, Probability_cs),
             Probability_cs=ifelse(Probability_cs > 1, 1, Probability_cs))
}
gc()

 ## 6/21: Add manual correction for the CS model: if the player has less than 25% chance of playing 60 minutes,
 ## then probability of clean sheet = 0
time_results <- played_predictions %>% distinct(name, GW, .keep_all = T) %>%
   left_join(played60_predictions %>% distinct(name, GW, .keep_all = T)) %>%
   left_join(cs_prediction %>% distinct(name, GW, .keep_all = T)) %>%
   select(name, position, team, GW, opponent, h_a, everything(), -contains('validation')) %>%
   rename(Player=name, Position=position, Gameweek=GW, Opponent=opponent,
          `Home/Away`=h_a, Team=team, Season=season) %>%
   mutate(`Home/Away`=ifelse(`Home/Away`=='h', 'Home', 'Away'),
          Probability_cs=ifelse(Probability_played60<0.25, 0, Probability_cs)) %>%
   distinct(Player, Gameweek, .keep_all = T)
 
 objects <- ls()
 keep <- objects[grep('results|model|data|fixtures|ids|teams', objects)]
 rm(list=setdiff(objects, keep))
 gc()
 