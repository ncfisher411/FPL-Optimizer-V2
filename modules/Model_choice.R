# DEV SCRIPT FOR ASSESSING BEST MODELS
## updated 12/12/2024
## Run this after compiling data if assessing estimates or adding model strategies

### IF dev="Yes", then this will automatically run and assess all models for comparison
if(dev=='Yes'){

##-------------------------------------##
##            Goals models             ##
##-------------------------------------##
linear_goals_model <- lm(goals ~ xG + xA + ict_index  + played + played60 + position + h_a + strength + difficulty, data = est_data)
logit_goals_model <- glm(goals ~ xG + xA + ict_index + played + played60 + position + h_a + strength + difficulty, data = est_data)
rf_goals_model <- randomForest(goals ~ xG + ict_index + played + played60 + position + h_a + strength + difficulty, data = est_data)
summary(linear_goals_model)
summary(logit_goals_model)
gc()

linear_og_model <- lm(own_goals ~ ict_index_opponent + xG_opponent + played + played60 + position + strength + difficulty + h_a, data = est_data)
logit_og_model <- glm(own_goals ~ ict_index_opponent + xG_opponent + played + played60 + position + strength + difficulty + h_a, data = est_data)
rf_og_model <- randomForest(own_goals ~ ict_index_opponent + xG_opponent + played + played60 + position + strength + difficulty + h_a, data = est_data)
summary(linear_og_model)
summary(logit_og_model)
gc()

linear_pen_model <- lm(penalties_missed ~ xG + ict_index + position + h_a + strength + difficulty, data = est_data)
logit_pen_model <- glm(penalties_missed ~ xG + ict_index + position + h_a + strength + difficulty, data = est_data)
rf_pen_model <- randomForest(penalties_missed ~ xG + ict_index + position + h_a + strength + difficulty, data = est_data)
summary(linear_pen_model)
summary(logit_pen_model)
gc()

## Run the prediction with each model
linear_predictions <- predict(linear_goals_model, predict_data) %>% data.frame() %>%
  rename(Predicted_goals_linear=1) %>%
  cbind(predict_data) %>%
  select(name, position, team, season, GW, opponent, h_a, Predicted_goals_linear) %>%
  left_join(val_data %>% select(name, position, team, GW, goals), by=c('name', 'position', 'team', 'GW')) %>%
  mutate(Predicted_goals_linear=ifelse(Predicted_goals_linear<0, 0, Predicted_goals_linear),
         linear_goals_validation=goals-Predicted_goals_linear) %>%
  select(-goals)

logit_predictions <- predict(logit_goals_model, predict_data) %>% data.frame() %>%
  rename(Predicted_goals_logit=1) %>%
  cbind(predict_data) %>%
  select(name, position, team, season, GW, opponent, h_a, Predicted_goals_logit) %>%
  left_join(val_data %>% select(name, position, team, GW, goals), by=c('name', 'position', 'team', 'GW')) %>%
  mutate(Predicted_goals_logit=ifelse(Predicted_goals_logit<0, 0, Predicted_goals_logit),
         logit_goals_validation=goals-Predicted_goals_logit) %>%
  select(-goals)

rf_predictions <- predict(rf_goals_model, predict_data) %>% data.frame() %>%
  rename(Predicted_goals_rf=1) %>%
  cbind(predict_data) %>%
  select(name, position, team, season, GW, opponent, h_a, Predicted_goals_rf) %>%
  left_join(val_data %>% select(name, position, team, GW, goals), by=c('name', 'position', 'team', 'GW')) %>%
  mutate(Predicted_goals_rf=ifelse(Predicted_goals_rf<0, 0, Predicted_goals_rf),
         rf_goals_validation=goals-Predicted_goals_rf) %>%
  select(-goals)

linear_og_predictions <- predict(linear_og_model, predict_data) %>%
  data.frame() %>%
  rename(Predicted_og_linear=1) %>%
  cbind(predict_data) %>%
  select(name, position, team, season, GW, opponent, h_a, Predicted_og_linear) %>%
  left_join(val_data %>% select(name, position, team, GW, own_goals), by=c('name', 'position', 'team', 'GW')) %>%
  mutate(Predicted_og_linear=ifelse(Predicted_og_linear < 0, 0, Predicted_og_linear),
         linear_og_validation=own_goals-Predicted_og_linear) %>%
  select(-own_goals)

linear_pen_predictions <- predict(linear_pen_model, predict_data) %>%
  data.frame() %>%
  rename(Predicted_pen_linear=1) %>%
  cbind(predict_data) %>%
  select(name, position, team, season, GW, opponent, h_a, Predicted_pen_linear) %>%
  left_join(val_data %>% select(name, position, team, GW, penalties_missed), by=c('name', 'position', 'team', 'GW')) %>%
  mutate(Predicted_pen_linear=ifelse(Predicted_pen_linear < 0, 0, Predicted_pen_linear),
         linear_pen_validation=penalties_missed-Predicted_pen_linear) %>%
  select(-penalties_missed)

logit_pen_predictions <- predict(logit_pen_model, predict_data) %>%
  data.frame() %>%
  rename(Predicted_pen_logit=1) %>%
  cbind(predict_data) %>%
  select(name, position, team, season, GW, opponent, h_a, Predicted_pen_logit) %>%
  left_join(val_data %>% select(name, position, team, GW, penalties_missed), by=c('name', 'position', 'team', 'GW')) %>%
  mutate(Predicted_pen_logit=ifelse(Predicted_pen_logit < 0, 0, Predicted_pen_logit),
         logit_pen_validation=penalties_missed-Predicted_pen_logit) %>%
  select(-penalties_missed)

rf_pen_predictions <- predict(rf_pen_model, predict_data) %>%
  data.frame() %>%
  rename(Predicted_pen_rf=1) %>%
  cbind(predict_data) %>%
  select(name, position, team, season, GW, opponent, h_a, Predicted_pen_rf) %>%
  left_join(val_data %>% select(name, position, team, GW, penalties_missed), by=c('name', 'position', 'team', 'GW')) %>%
  mutate(Predicted_pen_rf=ifelse(Predicted_pen_rf < 0, 0, Predicted_pen_rf),
         rf_pen_validation=penalties_missed-Predicted_pen_rf) %>%
  select(-penalties_missed)

logit_og_predictions <- predict(logit_og_model, predict_data) %>%
  data.frame() %>%
  rename(Predicted_og_logit=1) %>%
  cbind(predict_data) %>%
  select(name, position, team, season, GW, opponent, h_a, Predicted_og_logit) %>%
  left_join(val_data %>% select(name, position, team, GW, own_goals), by=c('name', 'position', 'team', 'GW')) %>%
  mutate(Predicted_og_logit=ifelse(Predicted_og_logit < 0, 0, Predicted_og_logit),
         logit_og_validation=own_goals-Predicted_og_logit) %>%
  select(-own_goals)

rf_og_predictions <- predict(rf_og_model, predict_data) %>%
  data.frame() %>%
  rename(Predicted_og_rf=1) %>%
  cbind(predict_data) %>%
  select(name, position, team, season, GW, opponent, h_a, Predicted_og_rf) %>%
  left_join(val_data %>% select(name, position, team, GW, own_goals), by=c('name', 'position', 'team', 'GW')) %>%
  mutate(Predicted_og_rf=ifelse(Predicted_og_rf < 0, 0, Predicted_og_rf),
         rf_og_validation=own_goals-Predicted_og_rf) %>%
  select(-own_goals)


## Metrics summary for model choice justification
comp_goals <- data.frame(Stat='Goals',
                         Model=c('Linear', 'Logit'),
                         RMSE=c(sqrt(mean(resid(linear_goals_model)^2)),
                                sqrt(mean(resid(logit_goals_model)^2))),
                         R2=c(summary(linear_goals_model)$r.squared,
                              summary(logit_goals_model)$r.squared),
                         Validation=c(mean(linear_predictions$linear_goals_validation),
                                      mean(logit_predictions$logit_goals_validation))
) %>%
  rbind(
    data.frame(Stat='Goals',
               Model='Random Forest',
               RMSE=sqrt(mean(rf_goals_model$mse)),
               R2=mean(rf_goals_model$rsq),
               Validation=mean(rf_predictions$rf_goals_validation))
  ) %>%
  rbind(
    data.frame(
      Stat = 'Own goals',
      Model=c('Linear', 'Logit'),
      RMSE=c(sqrt(mean(resid(linear_og_model)^2)),
             sqrt(mean(resid(logit_og_model)^2))),
      R2=c(summary(linear_og_model)$r.squared,
           summary(logit_og_model)$r.squared),
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
keep <- objects[grep('results|comp|data|fixtures|ids', objects)]
rm(list=setdiff(objects, keep))
gc()

##-------------------------------------##
##            Assists models           ##
##-------------------------------------##
linear_assist_model <- lm(assists ~ xA + ict_index + xG  + played60 + played + h_a + position + strength + difficulty, data = est_data)
summary(linear_assist_model)
logit_assist_model <- glm(assists ~ xA + ict_index + xG + played60 + played + h_a + position + strength + difficulty, data = est_data)
summary(logit_assist_model)
rf_assist_model <- randomForest(assists ~ xA + ict_index + xG + played60 + played + h_a + position + strength + difficulty, data = est_data)

## Run the predictions of each model
linear_predictions <- predict(linear_assist_model, predict_data) %>% data.frame() %>%
  rename(Predicted_assists_linear=1) %>%
  cbind(predict_data) %>%
  select(name, position, team, opponent, h_a, GW, season, Predicted_assists_linear) %>%
  left_join(val_data %>% select(name, position, team, GW, assists), by=c('name', 'position', 'team', 'GW')) %>%
  mutate(Predicted_assists_linear=ifelse(Predicted_assists_linear<0, 0, Predicted_assists_linear),
         linear_assists_validation=assists-Predicted_assists_linear) %>%
  select(-assists)

logit_predictions <- predict(logit_assist_model, predict_data) %>% data.frame() %>%
  rename(Predicted_assists_logit=1) %>%
  cbind(predict_data) %>%
  select(name, position, team, opponent, h_a, GW, season, Predicted_assists_logit) %>%
  left_join(val_data %>% select(name, position, team, GW, assists), by=c('name', 'position', 'team', 'GW')) %>%
  mutate(Predicted_assists_logit=ifelse(Predicted_assists_logit<0, 0, Predicted_assists_logit),
         logit_assists_validation=assists-Predicted_assists_logit) %>%
  select(-assists)

rf_predictions <- predict(rf_assist_model, predict_data) %>% data.frame() %>%
  rename(Predicted_assists_rf=1) %>%
  cbind(predict_data) %>%
  select(name, position, team, opponent, h_a, GW, season, Predicted_assists_rf) %>%
  left_join(val_data %>% select(name, position, team, GW, assists), by=c('name', 'position', 'team', 'GW')) %>%
  mutate(Predicted_assists_rf=ifelse(Predicted_assists_rf<0, 0, Predicted_assists_rf),
         rf_assists_validation=assists-Predicted_assists_rf) %>%
  select(-assists)

## Metrics summary for model choice justification
comp_assists <- data.frame()
model_list <- grep('assist_model', names(.GlobalEnv), value=T)

#### Models list
for(i in model_list) {
  
  model <- get(i)
  type <- sub('_.*', '', i)
  name <- grep('prediction', names(.GlobalEnv), value = T)
  name <- grep(type, name, value = T)
  df <- get(name)
  col <- grep('validation', names(df), value = T)
  mean <- mean(df[[col]], na.rm = T)
  
  if(grepl('linear', i)) {
    data <- data.frame(
      Stat = 'Assists',
      Model = i,
      RMSE = sqrt(mean(model$residuals^2)),
      R2=summary(model)$r.squared,
      Validation=mean
    )
  } else if(grepl('logit', i)) {
    summary <- summary(model)
    
    data <- data.frame(
      Stat = 'Assists',
      Model = i,
      RMSE = sqrt(mean(model$residuals^2)),
      R2=1-(summary$deviance/summary$null.deviance),
      Validation=mean
    )
  } else if(grepl('rf', i)){
    data <- data.frame(
      Stat = 'Assists',
      Model = i,
      RMSE=sqrt(mean(model$mse)),
      R2=mean(model$rsq),
      Validation=mean
    ) 
  }
  
  comp_assists <- rbind(comp_assists, data)
}

assists_results <- linear_predictions %>%
  distinct(name, GW, .keep_all = T) %>%
  left_join(logit_predictions %>% distinct(name, GW, .keep_all = T)) %>%
  left_join(rf_predictions %>% distinct(name, GW, .keep_all = T)) %>%
  left_join(predict_data %>% select(name, position, team, GW, opponent, h_a, season)) %>%
  select(name, position, team, GW, opponent, h_a, everything(), -contains('validation')) %>%
  rename(Player=name, Position=position, Gameweek=GW, Opponent=opponent,
         `Home/Away`=h_a, Team=team, Season=season) %>%
  mutate(`Home/Away`=ifelse(`Home/Away`=='h', 'Home', 'Away')) %>%
  distinct(Player, Gameweek, .keep_all = T)

objects <- ls()
keep <- objects[grep('results|comp|data|fixtures|ids', objects)]
rm(list=setdiff(objects, keep))
gc()

##-------------------------------------##
##            Cards models             ##
##-------------------------------------##
## Set up the models
linear_yc_model <- lm(yellow_cards ~ ict_index_opponent + played + played60 + xG_opponent + goals_conceded + position + h_a + strength + difficulty, data = est_data)
linear_rc_model <- lm(red_cards ~ ict_index_opponent + xG_opponent + played + goals_conceded + position + h_a + strength + difficulty, data = est_data)
logit_yc_model <- glm(yellow_cards ~ ict_index_opponent + played + played60 + xG_opponent + goals_conceded + position + h_a + strength + difficulty, data = est_data)
logit_rc_model <- lm(red_cards ~ ict_index_opponent + xG_opponent + played + goals_conceded + position + h_a + strength + difficulty, data = est_data)
summary(linear_yc_model)
summary(logit_yc_model)
summary(linear_rc_model)
summary(logit_rc_model)
rf_yc_model <- randomForest(yellow_cards ~ ict_index_opponent + played + played60 + xG_opponent + goals_conceded + position + h_a + strength + difficulty, data = est_data)
rf_rc_model <- randomForest(red_cards ~ ict_index_opponent + xG_opponent + played + goals_conceded + position + h_a + strength + difficulty, data = est_data)
gc()

## Run the predictions for yellow cards
linear_yc_predictions <- predict(linear_yc_model, predict_data) %>%
  data.frame() %>%
  rename(Predicted_yellow_cards_linear=1) %>%
  cbind(predict_data) %>%
  left_join(val_data %>% select(name, position, team, GW, yellow_cards), by=c('name', 'position', 'team', 'GW')) %>%
  mutate(Predicted_yellow_cards_linear=ifelse(Predicted_yellow_cards_linear < 0, 0, Predicted_yellow_cards_linear),
         linear_yellow_cards_validation=yellow_cards-Predicted_yellow_cards_linear) %>%
  select(-yellow_cards)

logit_yc_predictions <- predict(logit_yc_model, predict_data) %>%
  data.frame() %>%
  rename(Predicted_yellow_cards_logit=1) %>%
  cbind(predict_data) %>%
  left_join(val_data %>% select(name, position, team, GW, yellow_cards), by=c('name', 'position', 'team', 'GW')) %>%
  mutate(Predicted_yellow_cards_logit=ifelse(Predicted_yellow_cards_logit < 0, 0, Predicted_yellow_cards_logit),
         logit_yellow_cards_validation=yellow_cards-Predicted_yellow_cards_logit) %>%
  select(-yellow_cards)

rf_yc_predictions <- predict(rf_yc_model, predict_data) %>%
  data.frame() %>%
  rename(Predicted_yellow_cards_rf=1) %>%
  cbind(predict_data) %>%
  left_join(val_data %>% select(name, position, team, GW, yellow_cards), by=c('name', 'position', 'team', 'GW')) %>%
  mutate(Predicted_yellow_cards_rf=ifelse(Predicted_yellow_cards_rf < 0, 0, Predicted_yellow_cards_rf),
         rf_yellow_cards_validation=yellow_cards-Predicted_yellow_cards_rf) %>%
  select(-yellow_cards)

## Run the predictions for red cards
linear_rc_predictions <- predict(linear_rc_model, predict_data) %>%
  data.frame() %>%
  rename(Predicted_red_cards_linear=1) %>%
  cbind(predict_data) %>%
  left_join(val_data %>% select(name, position, team, GW, red_cards), by=c('name', 'position', 'team', 'GW')) %>%
  mutate(Predicted_red_cards_linear=ifelse(Predicted_red_cards_linear < 0, 0, Predicted_red_cards_linear),
         linear_red_cards_validation=red_cards-Predicted_red_cards_linear) %>%
  select(-red_cards)

logit_rc_predictions <- predict(logit_rc_model, predict_data) %>%
  data.frame() %>%
  rename(Predicted_red_cards_logit=1) %>%
  cbind(predict_data) %>%
  left_join(val_data %>% select(name, position, team, GW, red_cards), by=c('name', 'position', 'team', 'GW')) %>%
  mutate(Predicted_red_cards_logit=ifelse(Predicted_red_cards_logit < 0, 0, Predicted_red_cards_logit),
         logit_red_cards_validation=red_cards-Predicted_red_cards_logit) %>%
  select(-red_cards)

rf_rc_predictions <- predict(rf_rc_model, predict_data) %>%
  data.frame() %>% 
  rename(Predicted_red_cards_rf=1) %>%
  cbind(predict_data) %>%
  left_join(val_data %>% select(name, position, team, GW, red_cards), by=c('name', 'position', 'team', 'GW')) %>%
  mutate(Predicted_red_cards_rf=ifelse(Predicted_red_cards_rf < 0, 0, Predicted_red_cards_rf),
         rf_red_cards_validation=red_cards-Predicted_red_cards_rf) %>%
  select(-red_cards)

cards_results <- linear_yc_predictions %>%
  distinct(name, GW, .keep_all = T) %>%
  left_join(logit_yc_predictions %>%
              distinct(name, GW, .keep_all = T)) %>%
  left_join(rf_yc_predictions %>%
              distinct(name, GW, .keep_all = T)) %>%
  left_join(linear_rc_predictions %>%
              distinct(name, GW, .keep_all = T)) %>%
  left_join(logit_rc_predictions %>%
              distinct(name, GW, .keep_all = T)) %>%
  select(name, position, team, GW, opponent, h_a, everything(), -contains('validation')) %>%
  rename(Player=name, Position=position, Gameweek=GW, Opponent=opponent,
         `Home/Away`=h_a, Team=team, Season=season) %>%
  mutate(`Home/Away`=ifelse(`Home/Away`=='h', 'Home', 'Away')) %>%
  distinct(Player, Gameweek, .keep_all = T)

## Get metrics

comp_cards <- data.frame(Stat='Yellow cards',
                         Model=c('Linear', 'Logit'),
                         RMSE=c(sqrt(mean(resid(linear_yc_model)^2)),
                                sqrt(mean(resid(logit_yc_model)^2))),
                         R2=c(summary(linear_yc_model)$r.squared,
                              summary(logit_yc_model)$r.squared),
                         Validation=c(mean(linear_yc_predictions$linear_yellow_cards_validation),
                                      mean(logit_yc_predictions$logit_yellow_cards_validation))
) %>%
  rbind(
    data.frame(Stat='Yellow cards',
               Model='Random Forest',
               RMSE=sqrt(mean(rf_yc_model$mse)),
               R2=mean(rf_yc_model$rsq),
               Validation=mean(rf_yc_predictions$rf_yellow_cards_validation)),
    data.frame(Stat='Red cards',
               Model=c('Linear', 'Logit'),
               RMSE=c(sqrt(mean(resid(linear_rc_model)^2)),
                      sqrt(mean(resid(logit_rc_model)^2))),
               R2=c(summary(linear_rc_model)$r.squared,
                    summary(logit_rc_model)$r.squared),
               Validation=c(mean(linear_rc_predictions$linear_red_cards_validation),
                            mean(logit_rc_predictions$logit_red_cards_validation))),
    data.frame(Stat='Red cards',
               Model='Random Forest',
               RMSE=sqrt(mean(rf_rc_model$mse)),
               R2=mean(rf_rc_model$rsq),
               Validation=mean(rf_rc_predictions$rf_red_cards_validation))
    
  )

objects <- ls()
keep <- objects[grep('results|comp|data|fixtures|ids', objects)]
rm(list=setdiff(objects, keep))
gc()

##-------------------------------------##
##            Saves models             ##
##-------------------------------------##
## Now set up the model for saves
linear_saves_model <- lm(saves ~ xG_opponent  + ict_index_opponent + played60 + played + goals_conceded +  strength + difficulty + h_a, data = est_data)
logit_saves_model <- glm(saves ~ xG_opponent  + ict_index_opponent + played60 + played + goals_conceded +  strength + difficulty + h_a, data = est_data)
summary(linear_saves_model)
summary(logit_saves_model)
rf_saves_model <- randomForest(saves ~ xG_opponent  + ict_index_opponent + played60 + played + goals_conceded +  strength + difficulty + h_a, data = est_data)
gc()
linear_pen_model <- lm(penalties_saved ~ saves + xG_opponent  + ict_index_opponent + played60 +  strength + difficulty + h_a, data = est_data)
logit_pen_model <- glm(penalties_saved ~ saves + xG_opponent  + ict_index_opponent + played60 +  strength + difficulty + h_a, data = est_data)
rf_pen_model <- randomForest(penalties_saved ~ saves + xG_opponent  + ict_index_opponent + played60 +  strength + difficulty + h_a, data = est_data)
gc()
summary(linear_pen_model)
summary(logit_pen_model)
linear_goals_conceded_model <- lm(goals_conceded ~ xG_opponent  + ict_index_opponent + played60 + played + ict_index +  strength + difficulty + h_a, data = est_data)
logit_goals_conceded_model <- glm(goals_conceded ~ xG_opponent  + ict_index_opponent + played60 + played + ict_index +  strength + difficulty + h_a, data = est_data)
summary(linear_goals_conceded_model)
summary(logit_goals_conceded_model)
rf_goals_conceded_model <- randomForest(goals_conceded ~ xG_opponent  + ict_index_opponent + played60 + played + ict_index +  strength + difficulty + h_a, data = est_data)
gc()

## Run the predictions
linear_save_predictions <- predict(linear_saves_model, predict_data) %>%
  data.frame() %>% 
  rename(Predicted_saves_linear=1) %>%
  cbind(predict_data %>% select(-saves)) %>%
  left_join(val_data %>% select(name, position, team, GW, saves), by=c('name', 'position', 'team', 'GW')) %>%
  mutate(Predicted_saves_linear=ifelse(Predicted_saves_linear < 0, 0, Predicted_saves_linear),
         linear_saves_validation=saves-Predicted_saves_linear) %>%
  select(-saves)

linear_pen_predictions <- predict(linear_pen_model, predict_data) %>%
  data.frame() %>%
  rename(Predicted_pen_saves_linear=1) %>%
  cbind(predict_data) %>%
  left_join(val_data %>% select(name, position, team, GW, penalties_saved), by=c('name', 'position', 'team', 'GW')) %>%
  mutate(Predicted_pen_saves_linear=ifelse(Predicted_pen_saves_linear < 0, 0, Predicted_pen_saves_linear),
         linear_pen_saves_validation=penalties_saved-Predicted_pen_saves_linear) %>%
  select(-penalties_saved)

logit_save_predictions <- predict(logit_saves_model, predict_data) %>%
  data.frame() %>%
  rename(Predicted_saves_logit=1) %>%
  cbind(predict_data %>% select(-saves)) %>% 
  left_join(val_data %>% select(name, position, team, GW, saves), by=c('name', 'position', 'team', 'GW')) %>%
  mutate(Predicted_saves_logit=ifelse(Predicted_saves_logit < 0, 0, Predicted_saves_logit),
         logit_saves_validation=saves-Predicted_saves_logit) %>%
  select(-saves)

logit_pen_predictions <- predict(logit_saves_model, predict_data) %>%
  data.frame() %>%
  rename(Predicted_pen_saves_logit=1) %>%
  cbind(predict_data) %>%
  left_join(val_data %>% select(name, position, team, GW, penalties_saved), by=c('name', 'position', 'team', 'GW')) %>%
  mutate(Predicted_pen_saves_logit=ifelse(Predicted_pen_saves_logit < 0, 0, Predicted_pen_saves_logit),
         logit_pen_saves_validation=penalties_saved-Predicted_pen_saves_logit) %>%
  select(-penalties_saved)

rf_save_predictions <- predict(rf_saves_model, predict_data) %>%
  data.frame() %>%
  rename(Predicted_saves_rf=1) %>%
  cbind(predict_data %>% select(-saves)) %>% 
  left_join(val_data %>% select(name, position, team, GW, saves), by=c('name', 'position', 'team', 'GW')) %>%
  mutate(Predicted_saves_rf=ifelse(Predicted_saves_rf < 0, 0, Predicted_saves_rf),
         rf_saves_validation=saves-Predicted_saves_rf) %>%
  select(-saves)

rf_pen_predictions <- predict(rf_saves_model, predict_data) %>%
  data.frame() %>% 
  rename(Predicted_pen_saves_rf=1) %>%
  cbind(predict_data) %>%
  left_join(val_data %>% select(name, position, team, GW, penalties_saved), by=c('name', 'position', 'team', 'GW')) %>%
  mutate(Predicted_pen_saves_rf=ifelse(Predicted_pen_saves_rf < 0, 0, Predicted_pen_saves_rf),
         rf_pen_saves_validation=penalties_saved-Predicted_pen_saves_rf) %>%
  select(-penalties_saved)

linear_goals_conceded_prediction <- predict(linear_goals_conceded_model, predict_data) %>%
  data.frame() %>%
  rename(Predicted_goals_conceded_linear=1) %>%
  cbind(predict_data %>% select(-goals_conceded)) %>% 
  left_join(val_data %>% select(name, position, team, GW, goals_conceded), by=c('name', 'position', 'team', 'GW')) %>%
  mutate(Predicted_goals_conceded_linear=ifelse(Predicted_goals_conceded_linear < 0, 0, Predicted_goals_conceded_linear),
         linear_goals_conceded_validation=goals_conceded-Predicted_goals_conceded_linear) %>%
  select(-goals_conceded)

logit_goals_conceded_prediction <- predict(logit_goals_conceded_model, predict_data) %>%
  data.frame() %>%
  rename(Predicted_goals_conceded_logit=1) %>%
  cbind(predict_data %>% select(-goals_conceded)) %>% 
  left_join(val_data %>% select(name, position, team, GW, goals_conceded), by=c('name', 'position', 'team', 'GW')) %>%
  mutate(Predicted_goals_conceded_logit=ifelse(Predicted_goals_conceded_logit < 0, 0, Predicted_goals_conceded_logit),
         logit_goals_conceded_validation=goals_conceded-Predicted_goals_conceded_logit) %>%
  select(-goals_conceded)

rf_goals_conceded_prediction <- predict(rf_goals_conceded_model, predict_data) %>%
  data.frame() %>% 
  rename(Predicted_goals_conceded_rf=1) %>%
  cbind(predict_data %>% select(-goals_conceded)) %>% 
  left_join(val_data %>% select(name, position, team, GW, goals_conceded), by=c('name', 'position', 'team', 'GW')) %>%
  mutate(Predicted_goals_conceded_rf=ifelse(Predicted_goals_conceded_rf < 0, 0, Predicted_goals_conceded_rf),
         rf_goals_conceded_validation=goals_conceded-Predicted_goals_conceded_rf) %>%
  select(-goals_conceded)

## Get the model metrics
comp_saves <- data.frame(Stat='Saves',
                         Model=c('Linear', 'Logit'),
                         RMSE=c(sqrt(mean(resid(linear_saves_model)^2)),
                                sqrt(mean(resid(logit_saves_model)^2))),
                         R2=c(summary(linear_saves_model)$r.squared,
                              summary(logit_saves_model)$r.squared),
                         Validation=c(mean(linear_save_predictions$linear_saves_validation),
                                      mean(logit_save_predictions$logit_saves_validation))
) %>%
  rbind(
    data.frame(Stat='Saves',
               Model='Random Forest',
               RMSE=sqrt(mean(rf_saves_model$mse)),
               R2=mean(rf_saves_model$rsq),
               Validation=mean(rf_save_predictions$rf_saves_validation))
  ) %>%
  rbind(
    data.frame(Stat='Penalty saves',
               Model=c('Linear', 'Logit'),
               RMSE=c(sqrt(mean(resid(linear_pen_model)^2)),
                      sqrt(mean(resid(logit_pen_model)^2))),
               R2=c(summary(linear_pen_model)$r.squared,
                    summary(logit_pen_model)$r.squared),
               Validation=c(mean(linear_pen_predictions$linear_pen_saves_validation),
                            mean(logit_pen_predictions$logit_pen_saves_validation)))
  ) %>%
  rbind(
    data.frame(Stat='Penalty saves',
               Model='Random Forest',
               RMSE=sqrt(mean(rf_pen_model$mse)),
               R2=mean(rf_pen_model$rsq),
               Validation=mean(rf_pen_predictions$rf_pen_saves_validation))
  ) %>%
  rbind(
    data.frame(Stat='Goals conceded',
               Model=c('Linear', 'Logit'),
               RMSE=c(sqrt(mean(resid(linear_goals_conceded_model)^2)),
                      sqrt(mean(resid(logit_goals_conceded_model)^2))),
               R2=c(summary(linear_goals_conceded_model)$r.squared,
                    summary(logit_goals_conceded_model)$r.squared),
               Validation=c(mean(linear_goals_conceded_prediction$linear_goals_conceded_validation),
                            mean(logit_goals_conceded_prediction$logit_goals_conceded_validation))),
    data.frame(Stat='Goals conceded',
               Model='Random Forest',
               RMSE=sqrt(mean(rf_goals_conceded_model$mse)),
               R2=mean(rf_goals_conceded_model$rsq),
               Validation=mean(rf_goals_conceded_prediction$rf_goals_conceded_validation))
  )

save_results <- rf_save_predictions %>%
  distinct(name, GW, .keep_all = T) %>%
  left_join(linear_save_predictions %>% distinct(name, GW, .keep_all = T)) %>%
  left_join(logit_save_predictions %>% distinct(name, GW, .keep_all = T)) %>%
  left_join(linear_pen_predictions %>% distinct(name, GW, .keep_all = T)) %>%
  left_join(logit_pen_predictions %>% distinct(name, GW, .keep_all = T)) %>%
  left_join(rf_pen_predictions %>% distinct(name, GW, .keep_all = T)) %>%
  left_join(linear_goals_conceded_prediction %>% distinct(name, GW, .keep_all = T)) %>%
  left_join(logit_goals_conceded_prediction %>% distinct(name, GW, .keep_all = T)) %>%
  left_join(rf_goals_conceded_prediction %>% distinct(name, GW, .keep_all = T)) %>%
  select(name, position, team, GW, opponent, h_a, everything(), -contains('validation')) %>%
  rename(Player=name, Position=position, Gameweek=GW, Opponent=opponent,
         `Home/Away`=h_a, Team=team, Season=season) %>%
  mutate(`Home/Away`=ifelse(`Home/Away`=='h', 'Home', 'Away')) %>%
  distinct(Player, Gameweek, .keep_all = T)

## Validation
# set.seed(123)
# temp <- trainControl(method = 'cv', number = 5)
# train_linear <- train(saves ~ xG_against + overperformance_against + ict_index_against + minutes + team + opponent + h_a,
#                       data = df3, method ='lm', trControl = temp)
# print(train_linear$results$RMSE)
# 
# train_logit <- train(saves ~ xG_against + overperformance_against + ict_index_against + minutes + team + opponent + h_a,
#                      data = df3, method='glm', trControl = temp)
# print(train_logit$results$RMSE)

# train_rf <- train(saves ~ xG_against + overperformance_against + ict_index_against + minutes + team + opponent + h_a,
#                   data = df2, method='rf', trControl = temp)
# print(train_rf$results$RMSE)

objects <- ls()
keep <- objects[grep('results|comp|data|fixtures|ids', objects)]
rm(list=setdiff(objects, keep))
gc()

##-------------------------------------##
##            Time models              ##
##-------------------------------------##

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

##-------------------------------------##
##            Bonus models             ##
##-------------------------------------##
## Model set up
linear_bonus_model <- lm(bonus ~ ict_index + xG + xA + clean_sheet + played + played60 + ict_index_opponent + difficulty + strength + position + h_a, data = est_data)
logit_bonus_model <- glm(bonus ~ ict_index + xG + xA + clean_sheet + played + played60 + ict_index_opponent + difficulty + strength + position + h_a, data = est_data)
rf_bonus_model <- randomForest(bonus ~ ict_index + xG + xA + clean_sheet + played + played60 + ict_index_opponent + difficulty + strength + position + h_a, data = est_data)

## Run the prediction with each model
linear_predictions <- predict(linear_bonus_model, predict_data) %>% data.frame() %>%
  rename(Predicted_bonus_linear=1) %>%
  cbind(predict_data) %>%
  select(name, position, team, season, GW, opponent, h_a, Predicted_bonus_linear) %>%
  left_join(val_data %>% select(name, position, team, GW, bonus), by=c('name', 'position', 'team', 'GW')) %>%
  mutate(Predicted_bonus_linear=ifelse(Predicted_bonus_linear<0, 0, Predicted_bonus_linear),
         Predicted_bonus_linear=ifelse(Predicted_bonus_linear>3, 3, Predicted_bonus_linear),
         linear_bonus_validation=bonus-Predicted_bonus_linear) %>%
  select(-bonus)

logit_predictions <- predict(logit_bonus_model, predict_data) %>% data.frame() %>%
  rename(Predicted_bonus_logit=1) %>%
  cbind(predict_data) %>%
  select(name, position, team, season, GW, opponent, h_a, Predicted_bonus_logit) %>%
  left_join(val_data %>% select(name, position, team, GW, bonus), by=c('name', 'position', 'team', 'GW')) %>%
  mutate(Predicted_bonus_logit=ifelse(Predicted_bonus_logit<0, 0, Predicted_bonus_logit),
         Predicted_bonus_logit=ifelse(Predicted_bonus_logit>3, 3, Predicted_bonus_logit),
         logit_bonus_validation=bonus-Predicted_bonus_logit) %>%
  select(-bonus)

rf_predictions <- predict(rf_bonus_model, predict_data) %>% data.frame() %>%
  rename(Predicted_bonus_rf=1) %>%
  cbind(predict_data) %>%
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
  left_join(predict_data %>% select(name, position, team, GW, opponent, h_a, season)) %>%
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

## Compile the metrics to make model choices
objects <- ls()
temp <- mget(objects[grep('comp', objects)])
metrics <- do.call(rbind, temp) %>%
  mutate(`User score`=RMSE/3+R2/3+(1-abs(Validation))/3) %>%
  data.frame()
rownames(metrics) <- NULL
metrics <- metrics %>% 
  arrange(Stat, -User.score) %>%
  rbind(data.frame(
    Stat='User Score = average value of RMSE, R2, and 1-Validation. Highest user score should represent the preferred model',
    Model=NA,
    RMSE=NA,
    R2=NA,
    Validation=NA,
    User.score=NA
  )) %>% rename(`User score`=User.score)

### write the metrics
write.csv(metrics, 'data/model_metrics.csv', row.names = F)

} else {
  metrics <- read.csv('data/model_metrics.csv')
}

### G_model.R choices
goals_model = 'random forest'
own_goals_model = 'logit'
penalties_missed_model = 'random forest'

### A_model.R choices
assists_model = 'logit'

### Cards_model.R choice
yellow_cards_model = 'logit'
red_cards_model = 'logit'

### Saves_model.R choice
saves_model = 'logit'
penalties_saved_model = 'logit'
goals_conceded_model = 'random forest'

### Time model choice
played_model = 'linear'
played60_model = 'linear'
cs_model = 'random forest'

### Bonus model choice
bonus_model = 'random forest'

objects <- ls()
keep <- objects[grep('results|data|fixtures|ids|dev|model', objects)]
rm(list=setdiff(objects, keep))
gc()
