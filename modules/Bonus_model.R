#---------------------------------------#
# Bous points model for the FPL Lineup Optimizer
# Written by: ncfisher
# Last updated: August 31 2024
#---------------------------------------#

# Add in the model for bonus points here since we have all the data on-hand

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
