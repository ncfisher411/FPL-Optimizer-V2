#---------------------------------------#
# Saves model for the FPL Lineup Optimizer
# Written by: ncfisher
# Last updated: August 31 2024
#---------------------------------------#

## Build a model that predicts the number of saves a keeper will get in a match
### Will need to figure out how to build this so that we're showing the number of shots a keeper faced in a match
### Can cross-reference with vaastav data and which keeper was playing using FPL dictionary

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
