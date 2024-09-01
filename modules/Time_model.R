#---------------------------------------#
# Time played model for the FPL Lineup Optimizer
# Written by: ncfisher
# Last updated: August 31 2024
#---------------------------------------#

# Model set up
linear_played_model <- lm(played ~ ict_index_opponent + xG_opponent + ict_index + goals_conceded + xG + xA + position + strength + difficulty + h_a, data = est_data)
summary(linear_played_model)
logit_played_model <- glm(played ~ ict_index_opponent + xG_opponent + ict_index + goals_conceded + xG + xA + position + strength + difficulty + h_a, data = est_data)
summary(logit_played_model)
rf_played_model <- randomForest(played ~ ict_index_opponent + xG_opponent + ict_index + goals_conceded + xG + xA + position + strength + difficulty + h_a, data = est_data)
gc()
linear_played60_model <- lm(played60 ~ ict_index_opponent + xG_opponent + ict_index + goals_conceded + xG + xA +  position + strength + difficulty + h_a, data = est_data)
summary(linear_played60_model)
logit_played60_model <- glm(played60 ~ ict_index_opponent + xG_opponent + ict_index + goals_conceded + xG + xA +  position + strength + difficulty + h_a, data = est_data)
summary(logit_played60_model)
rf_played60_model <- randomForest(played60 ~ ict_index_opponent + xG_opponent + ict_index + goals_conceded + xG + xA +  position + strength + difficulty + h_a, data = est_data)
gc()
linear_cs_model <- lm(clean_sheet ~ played + played60 + ict_index + ict_index_opponent + xG_opponent + position + strength + difficulty + h_a, data = est_data)
summary(linear_cs_model)
logit_cs_model <- glm(clean_sheet ~ played + played60 + ict_index + ict_index_opponent + xG_opponent + position + strength + difficulty + h_a, data = est_data)
summary(logit_cs_model)
rf_cs_model <- randomForest(clean_sheet ~ played + played60 + ict_index + ict_index_opponent + xG_opponent + position + strength + difficulty + h_a, data = est_data)
gc()
 
 #### Predictions
 linear_played_predictions <- predict(linear_played_model, predict_data) %>%
   data.frame() %>%
   rename(Probability_played_linear=1) %>%
   cbind(predict_data) %>%
   left_join(val_data %>% select(name, position, team, GW, minutes), by=c('name', 'position', 'team', 'GW')) %>%
   distinct(name, GW, .keep_all = T) %>%
   mutate(Probability_played_linear=ifelse(Probability_played_linear < 0, 0, Probability_played_linear),
          Probability_played_linear=ifelse(Probability_played_linear > 1, 1, Probability_played_linear),
          played=ifelse(minutes > 0, 1, 0),
          linear_played_validation=played-Probability_played_linear) %>%
   select(-played, -minutes)
 
 logit_played_predictions <- predict(logit_played_model, predict_data) %>%
   data.frame() %>%
   cbind(predict_data) %>%
   left_join(val_data %>% select(name, position, team, GW, minutes), by=c('name', 'position', 'team', 'GW')) %>%
   distinct(name, GW, .keep_all = T) %>%
   rename(Probability_played_logit=1) %>%
   mutate(Probability_played_logit=ifelse(Probability_played_logit < 0, 0, Probability_played_logit),
          Probability_played_logit=ifelse(Probability_played_logit > 1, 1, Probability_played_logit),
          played=ifelse(minutes > 0, 1, 0),
          logit_played_validation=played-Probability_played_logit) %>%
   select(-played, -minutes)
 
 rf_played_predictions <- predict(rf_played_model, predict_data) %>%
   data.frame() %>%
   cbind(predict_data) %>%
   left_join(val_data %>% select(name, position, team, GW, minutes), by=c('name', 'position', 'team', 'GW')) %>%
   distinct(name, GW, .keep_all = T) %>%
   rename(Probability_played_rf=1) %>%
   mutate(Probability_played_rf=ifelse(Probability_played_rf < 0, 0, Probability_played_rf),
          Probability_played_rf=ifelse(Probability_played_rf > 1, 1, Probability_played_rf),
          played=ifelse(minutes > 0, 1, 0),
          rf_played_validation=played-Probability_played_rf) %>%
   select(-minutes, -played)
 
 linear_played60_predictions <- predict(linear_played60_model, predict_data) %>%
   data.frame() %>%
   cbind(predict_data) %>%
   left_join(val_data %>% select(name, position, team, GW, minutes), by=c('name', 'position', 'team', 'GW')) %>%
   distinct(name, GW, .keep_all = T) %>%
   rename(Probability_played60_linear=1) %>%
   mutate(Probability_played60_linear=ifelse(Probability_played60_linear > 1, 1, Probability_played60_linear),
          Probability_played60_linear=ifelse(Probability_played60_linear < 0, 0, Probability_played60_linear),
          played60=ifelse(minutes > 59, 1, 0),
          linear_played60_validation=played60-Probability_played60_linear) %>%
   select(-minutes, -played60)
 
 logit_played60_predictions <- predict(logit_played60_model, predict_data) %>%
   data.frame() %>%
   rename(Probability_played60_logit=1) %>%
   cbind(predict_data) %>%
   left_join(val_data %>% select(name, position, team, GW, minutes), by=c('name', 'position', 'team', 'GW')) %>%
   distinct(name, GW, .keep_all = T) %>%
   mutate(Probability_played60_logit=ifelse(Probability_played60_logit > 1, 1, Probability_played60_logit),
          Probability_played60_logit=ifelse(Probability_played60_logit < 0, 0, Probability_played60_logit),
          played60=ifelse(minutes > 59, 1, 0),
          logit_played60_validation=played60-Probability_played60_logit) %>%
   select(-played60, -minutes)
 
 rf_played60_predictions <- predict(rf_played60_model, predict_data) %>%
   data.frame() %>%
   cbind(predict_data) %>%
   left_join(val_data %>% select(name, position, team, GW, minutes), by=c('name', 'position', 'team', 'GW')) %>%
   distinct(name, GW, .keep_all = T) %>%
   rename(Probability_played60_rf=1) %>%
   mutate(Probability_played60_rf=ifelse(Probability_played60_rf > 1, 1, Probability_played60_rf),
          Probability_played60_rf=ifelse(Probability_played60_rf < 0, 0, Probability_played60_rf),
          played60=ifelse(minutes > 59, 1, 0),
          rf_played60_validation=played60-Probability_played60_rf) %>%
   select(-played60, -minutes)
 
 linear_cs_prediction <- predict(linear_cs_model, predict_data %>%
                                   left_join(linear_played60_predictions %>%
                                               select(name, team, position, GW, Probability_played60_linear) %>%
                                               rename(played60=Probability_played60_linear))
                                 ) %>%
   data.frame() %>%
   cbind(predict_data) %>%
   left_join(val_data %>% select(name, position, team, GW, minutes, goals_conceded), by=c('name', 'position', 'team', 'GW')) %>%
   distinct(name, GW, .keep_all = T) %>%
   rename(Probability_cs_linear=1) %>%
   mutate(Probability_cs_linear=ifelse(Probability_cs_linear < 0, 0, Probability_cs_linear),
          Probability_cs_linear=ifelse(Probability_cs_linear > 1, 1, Probability_cs_linear))
 
 logit_cs_prediction <- predict(logit_cs_model, predict_data %>%
                                  left_join(logit_played60_predictions %>%
                                              select(name, team, position, GW, Probability_played60_logit) %>%
                                              rename(played60=Probability_played60_logit))) %>%
   data.frame() %>%
   cbind(predict_data) %>%
   left_join(val_data %>% select(name, position, team, GW, minutes, goals_conceded), by=c('name', 'position', 'team', 'GW')) %>%
   distinct(name, GW, .keep_all = T) %>%
   rename(Probability_cs_logit=1) %>%
   mutate(Probability_cs_logit=ifelse(Probability_cs_logit < 0, 0, Probability_cs_logit),
          Probability_cs_logit=ifelse(Probability_cs_logit > 1, 1, Probability_cs_logit))
 
 rf_cs_prediction <- predict(rf_cs_model, predict_data %>%
                               left_join(rf_played60_predictions %>%
                                           select(name, team, position, GW, Probability_played60_rf) %>%
                                           rename(played60=Probability_played60_rf))) %>%
   data.frame() %>%
   cbind(predict_data) %>%
   left_join(val_data %>% select(name, position, team, GW, minutes, goals_conceded), by=c('name', 'position', 'team', 'GW')) %>%
   distinct(name, GW, .keep_all = T) %>%
   rename(Probability_cs_rf=1) %>%
   mutate(Probability_cs_rf=ifelse(Probability_cs_rf < 0, 0, Probability_cs_rf),
          Probability_cs_rf=ifelse(Probability_cs_rf > 1, 1, Probability_cs_rf)) 
 
 comp_time <- data.frame(
   Stat='Played > 0 Minutes',
   Model = c('Linear', 'Logit'),
   RMSE=c(sqrt(mean(resid(linear_played_model)^2)),
          sqrt(mean(resid(logit_played_model)^2))),
   R2=c(summary(linear_played_model)$r.squared,
        summary(logit_played_model)$r.squared),
   Validation=c(mean(linear_played_predictions$linear_played_validation),
                mean(logit_played_predictions$logit_played_validation))
 ) %>% rbind(
   data.frame(
     Stat='Played > 0 Minutes',
     Model='Random Forest',
     RMSE=sqrt(mean(rf_played_model$mse)),
     R2=mean(rf_played_model$rsq),
     Validation=mean(rf_played_predictions$rf_played_validation)
   )
 ) %>% rbind(
   data.frame(
     Stat='Played 60 Minutes',
     Model = c('Linear', 'Logit'),
     RMSE=c(sqrt(mean(resid(linear_played60_model)^2)),
            sqrt(mean(resid(logit_played60_model)^2))),
     R2=c(summary(linear_played60_model)$r.squared,
          summary(logit_played60_model)$r.squared),
     Validation=c(mean(linear_played60_predictions$linear_played60_validation),
                  mean(logit_played60_predictions$logit_played60_validation))
   )
 ) %>% rbind(
   data.frame(
     Stat='Played 60 Minutes',
     Model='Random Forest',
     RMSE=sqrt(mean(rf_played60_model$mse)),
     R2=mean(rf_played_model$rsq),
     Validation=mean(rf_played60_predictions$rf_played60_validation)
   )
 ) %>%
   rbind(data.frame(
     Stat='Clean Sheet',
     Model = c('Linear', 'Logit'),
     RMSE=c(sqrt(mean(resid(linear_cs_model)^2)),
            sqrt(mean(resid(logit_cs_model)^2))),
     R2=c(summary(linear_cs_model)$r.squared,
          summary(logit_cs_model)$r.squared),
     Validation=c(mean(linear_cs_prediction$linear_cs_validation),
                  mean(logit_cs_prediction$logit_cs_validation))
   )
   ) %>% rbind(
     data.frame(
       Stat='Clean Sheet',
       Model='Random Forest',
       RMSE=sqrt(mean(rf_cs_model$mse)),
       R2=mean(rf_cs_model$rsq),
       Validation=mean(rf_played60_predictions$rf_played60_validation)
     )
   )
 
 ## 6/21: Add manual correction for the CS model: if the player has less than 25% chance of playing 60 minutes,
 ## then probability of clean sheet = 0
 time_results <- linear_played_predictions %>%
   distinct(name, GW, .keep_all = T) %>%
   left_join(logit_played_predictions %>% distinct(name, GW, .keep_all = T)) %>%
   left_join(rf_played_predictions %>% distinct(name, GW, .keep_all = T)) %>%
   left_join(linear_played60_predictions %>% distinct(name, GW, .keep_all = T)) %>%
   left_join(logit_played60_predictions %>% distinct(name, GW, .keep_all = T)) %>%
   left_join(rf_played60_predictions %>% distinct(name, GW, .keep_all = T)) %>%
   left_join(linear_cs_prediction %>% distinct(name, GW, .keep_all = T)) %>%
   left_join(logit_cs_prediction %>% distinct(name, GW, .keep_all = T)) %>%
   left_join(rf_cs_prediction %>% distinct(name, GW, .keep_all = T)) %>%
   select(name, position, team, GW, opponent, h_a, everything(), -contains('validation')) %>%
   rename(Player=name, Position=position, Gameweek=GW, Opponent=opponent,
          `Home/Away`=h_a, Team=team, Season=season) %>%
   mutate(`Home/Away`=ifelse(`Home/Away`=='h', 'Home', 'Away'),
          Probability_cs_linear=ifelse(Probability_played60_linear<0.25, 0, Probability_cs_linear),
          Probability_cs_logit=ifelse(Probability_played60_logit<0.25, 0, Probability_cs_logit),
          Probability_cs_rf=ifelse(Probability_played60_rf<0.25, 0, Probability_cs_rf)) %>%
   distinct(Player, Gameweek, .keep_all = T)
 
 objects <- ls()
 keep <- objects[grep('results|comp|data|fixtures|ids', objects)]
 rm(list=setdiff(objects, keep))
 gc()
 
 ### Extra code below - ignore
 # df <- est_data %>% group_by(team, position, rank_minutes, strength, difficulty, h_a) %>%
 #   summarize(played=mean(played, na.rm = T),
 #             played60=mean(played60, na.rm = T),
 #             clean_sheet=mean(clean_sheet, na.rm = T)) %>%
 #   ungroup()
 # 
 # df2 <- df %>% mutate(rank_minutes=rank_minutes+0.5) %>%
 #   anti_join(df, by=c('position', 'rank_minutes', 'strength', 'difficulty', 'h_a'))
 # 
 # df <- df %>% rbind(df2) %>%
 #   distinct(position, rank_minutes, strength, difficulty, h_a, .keep_all = T)
 # 
 # time_results <- val_data %>% 
 #   select(where(is.character), where(is.logical), strength, difficulty, rank_minutes) %>%
 #   left_join(df)
 