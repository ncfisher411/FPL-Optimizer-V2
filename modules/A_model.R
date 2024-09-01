#---------------------------------------#
# Assist model for the FPL Lineup Optimizer
# Written by: ncfisher
# Last updated: August 31 2024
#---------------------------------------#

## Now test the models: Linear, logit, random forest
### Dependent variable: # of assists
### Independent variables: xA, ict_index, home/away, position (other vars tested and not important)
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
comp_assists <- data.frame(
  Stat='Assists',
  Model=c('Linear', 'Logit'),
  RMSE=c(sqrt(mean(resid(linear_assist_model)^2)),
         sqrt(mean(resid(logit_assist_model)^2))),
  R2=c(summary(linear_assist_model)$r.squared,
       summary(logit_assist_model)$r.squared),
  Validation=c(mean(linear_predictions$linear_assists_validation),
               mean(logit_predictions$logit_assists_validation))
) %>%
  rbind(
    data.frame(Stat='Assists',
               Model='Random Forest',
               RMSE=sqrt(mean(rf_assist_model$mse)),
               R2=mean(rf_assist_model$rsq),
               Validation=mean(rf_predictions$rf_assists_validation))
  )

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
