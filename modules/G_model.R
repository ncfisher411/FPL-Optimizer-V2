#---------------------------------------#
# Goals model for the FPL Lineup Optimizer
# Written by: ncfisher
# Last updated: August 31 2024
#---------------------------------------#

# This script is used for modeling match xG for the FPL optimizer

## Now test model variations for predicting goals using xG and ICT index
### Dependent variable = goals
### Independent variable = xG, ICT, position, team, home/away, opponent

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
