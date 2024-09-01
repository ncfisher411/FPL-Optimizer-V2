#---------------------------------------#
# Yellow and Red Cards model for the FPL Lineup Optimizer
# Written by: ncfisher
# Last updated: August 31 2024
#---------------------------------------#

## Need to predict number of red cards, yellow cards, based on controls, ICT Index, goals conceded

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
