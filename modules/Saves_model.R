#---------------------------------------#
# Saves model for the FPL Lineup Optimizer
# Written by: ncfisher
# Last updated: May 26 2024
#---------------------------------------#

## Build a model that predicts the number of saves a keeper will get in a match
### Will need to figure out how to build this so that we're showing the number of shots a keeper faced in a match
### Can cross-reference with vaastav data and which keeper was playing using FPL dictionary

## Now set up the model for saves
linear_saves_model <- lm(saves ~ xG_opponent  + ict_index_opponent + rank_minutes +  strength + difficulty + h_a, data = est_data)
logit_saves_model <- glm(saves ~ xG_opponent  + ict_index_opponent + rank_minutes +  strength + difficulty + h_a, data = est_data)
summary(linear_saves_model)
summary(logit_saves_model)
rf_saves_model <- randomForest(saves ~ xG_opponent  + ict_index_opponent + rank_minutes +  strength + difficulty + h_a, data = est_data %>% filter(!is.na(rank_minutes)))
gc()
linear_pen_model <- lm(penalties_saved ~ xG_opponent  + ict_index_opponent + rank_minutes +  strength + difficulty + h_a, data = est_data)
logit_pen_model <- glm(penalties_saved ~ xG_opponent  + ict_index_opponent + rank_minutes +  strength + difficulty + h_a, data = est_data)
rf_pen_model <- randomForest(penalties_saved ~ xG_opponent  + ict_index_opponent + rank_minutes +  strength + difficulty + h_a, data = est_data %>% filter(!is.na(rank_minutes)))
gc()
summary(linear_pen_model)
summary(logit_pen_model)
linear_goals_conceded_model <- lm(goals_conceded ~ xG_opponent  + ict_index_opponent + rank_minutes +  strength + difficulty + h_a, data = est_data)
logit_goals_conceded_model <- glm(goals_conceded ~ xG_opponent  + ict_index_opponent + rank_minutes +  strength + difficulty + h_a, data = est_data)
summary(linear_goals_conceded_model)
summary(logit_goals_conceded_model)
rf_goals_conceded_model <- randomForest(goals_conceded ~ xG_opponent  + ict_index_opponent + rank_minutes +  strength + difficulty + h_a, data = est_data %>% filter(!is.na(rank_minutes)))
gc()

## Get averages
avg_team <- est_data %>%
  group_by(name, position, team, difficulty, season) %>%
  summarize(xG_opponent_opp=mean(xG_opponent, na.rm = T),
            ict_index_opponent_opp=mean(ict_index_opponent, na.rm = T),
            rank_minutes_opp=mean(rank_minutes, na.rm = T)) %>%
  ungroup()

## Build out for new teams at beginning of season - new teams get the average value
if(max(avg_team$season==max(val_data$season))) {
  avg_team <- avg_team %>% filter(season==max(season))
} else if(max(avg_team$season)!=max(val_data$season)) {
  avg_team <- avg_team %>%
    filter(season==max(season)) %>%
    mutate(season=max(val_data$season))
  
  new_teams <- setdiff(unique(val_data$team), unique(avg_team$team))
  temp <- avg_team %>%
    group_by(position, difficulty) %>%
    summarize(xG_opponent_opp=mean(xG_opponent_opp, na.rm =T ),
              ict_index_opponent_opp=mean(ict_index_opponent_opp, na.rm = T),
              rank_minutes_opp=mean(rank_minutes_opp, na.rm = T)) %>%
    ungroup()
  
  avg_team2 <- val_data %>%
    distinct(name, position, team) %>%
    filter(team %in% new_teams) %>%
    mutate(season=max(val_data$season)) %>%
    left_join(temp)
  
  avg_team <- avg_team %>% 
    filter(team %in% val_data$team) %>% 
    rbind(avg_team2)
}

avg_h_a <- est_data %>%
  group_by(name, team, position, h_a, season) %>%
  summarize(xG_opponent=mean(xG_opponent, na.rm = T),
            ict_index_opponent=mean(ict_index_opponent, na.rm = T),
            rank_minutes=mean(rank_minutes, na.rm = T)) %>%
  ungroup() %>%
  pivot_wider(names_from = h_a, values_from = c(xG_opponent, ict_index_opponent, rank_minutes))

if(max(avg_h_a$season)==max(val_data$season)) {
  avg_h_a <- avg_h_a %>% filter(season==max(season))
} else if(max(avg_h_a$season)!=max(val_data$season)) {
  avg_h_a <- avg_h_a %>%
    filter(season==max(season)) %>%
    mutate(season=max(val_data$season))
  
  temp <- avg_h_a %>%
    group_by(position) %>%
    summarize(xG_opponent_a=mean(xG_opponent_a, na.rm =T ),
              xG_opponent_h=mean(xG_opponent_h, na.rm =T ),
              ict_index_opponent_a=mean(ict_index_opponent_a, na.rm = T),
              ict_index_opponent_h=mean(ict_index_opponent_h, na.rm = T),
              rank_minutes_a=mean(rank_minutes_a, na.rm = T),
              rank_minutes_h=mean(rank_minutes_h, na.rm = T)) %>%
    ungroup()
  
  avg_h_a2 <- val_data %>% 
    distinct(name, position, team) %>%
    filter(team %in% new_teams) %>%
    mutate(season=max(val_data$season)) %>%
    left_join(temp)
  
  avg_h_a <- avg_h_a %>%
    filter(team %in% val_data$team) %>%
    rbind(avg_h_a2) %>%
    group_by(name, position, season) %>%
    summarize(xG_opponent_a=mean(xG_opponent_a, na.rm =T ),
              xG_opponent_h=mean(xG_opponent_h, na.rm =T ),
              ict_index_opponent_a=mean(ict_index_opponent_a, na.rm = T),
              ict_index_opponent_h=mean(ict_index_opponent_h, na.rm = T),
              rank_minutes_a=mean(rank_minutes_a, na.rm = T),
              rank_minutes_h=mean(rank_minutes_h, na.rm = T)) %>%
    ungroup()
}

avg_season <- est_data %>%
  group_by(name, position, team, season) %>%
  summarize(xG_opponent_season=mean(xG_opponent, na.rm = T),
            ict_index_opponent_season=mean(ict_index_opponent, na.rm = T),
            rank_minutes_season=mean(rank_minutes, na.rm = T)) %>%
  ungroup()

if(max(avg_season$season)==max(val_data$season)) {
  avg_season <- avg_season %>% filter(season==max(season))
} else if (max(avg_season$season)!=max(val_data$season)) {
  avg_season <- avg_season %>%
    filter(season==max(season)) %>%
    mutate(season=max(val_data$season))
  
  temp <- avg_season %>%
    group_by(position) %>%
    summarize(xG_opponent_season=mean(xG_opponent_season, na.rm = T),
              ict_index_opponent_season=mean(ict_index_opponent_season, na.rm = T),
              rank_minutes_season=mean(rank_minutes_season, na.rm = T)) %>%
    ungroup()
  
  avg_season2 <- val_data %>%
    distinct(name, team, position, season) %>%
    filter(team %in% new_teams) %>%
    mutate(season=max(val_data$season)) %>%
    left_join(temp)
  
  avg_season <- avg_season %>%
    filter(team %in% val_data$team) %>%
    rbind(avg_season2) %>%
    group_by(name, position, season) %>%
    summarize(xG_opponent_season=mean(xG_opponent_season, na.rm = T),
              ict_index_opponent_season=mean(ict_index_opponent_season, na.rm = T),
              rank_minutes_season=mean(rank_minutes_season, na.rm = T)) %>%
    ungroup()
}

temp <- est_data %>%
  filter(season==max(season))

## Get the actual numbers

temp <- val_data %>% filter(finished!='TRUE')

if (nrow(temp)==0) {
  
  df2 <- val_data %>% 
    select(name, web_name, team, position, season, GW, kickoff_time, h_a, opponent, strength, difficulty) %>%
    left_join(avg_season) %>%
    left_join(avg_h_a) %>%
    left_join(avg_team) %>%
    mutate(
      xG_opponent=ifelse(h_a=='h', (xG_opponent_season*0.5) + (xG_opponent_h*0.3) + (xG_opponent_opp*0.2),
                             (xG_opponent_season*0.5) + (xG_opponent_a*0.3) + (xG_opponent_opp*0.2)),
      ict_index_opponent=ifelse(h_a=='h', (ict_index_opponent_season*0.5) + (ict_index_opponent_h)*0.3 + (ict_index_opponent_opp*0.2),
                                (ict_index_opponent_season*0.5) + (ict_index_opponent_a*0.3) + (ict_index_opponent_opp*0.2)),
      rank_minutes=ifelse(h_a=='h', (rank_minutes_season*0.5) + (rank_minutes_h)*0.3 + (rank_minutes_opp*0.2),
                                (rank_minutes_season*0.5) + (rank_minutes_a*0.3) + (rank_minutes_opp*0.2))
    ) %>% select(name, web_name, team, position, season, GW, kickoff_time, h_a, opponent, strength, difficulty, xG_opponent, ict_index_opponent, rank_minutes) %>%
    distinct(name, GW, .keep_all = T) %>%
    # mutate_if(is.numeric, function(x) ifelse(is.na(x), 0, x)) %>%
    group_by(team, h_a, strength, difficulty, position) %>%
    mutate(xG_opponent=ifelse(is.na(xG_opponent), mean(xG_opponent, na.rm = T), xG_opponent),
           ict_index_opponent=ifelse(is.na(ict_index_opponent), mean(ict_index_opponent, na.rm = T), ict_index_opponent),
           rank_minutes=ifelse(is.na(rank_minutes), mean(rank_minutes, na.rm = T), rank_minutes)) %>%
    ungroup()
  
} else if (nrow(temp) > 0) {
  
  temp2 <- temp %>%
    select(name, web_name, team, position, season, GW, kickoff_time, h_a, opponent, strength, difficulty) %>%
    left_join(avg_season) %>%
    left_join(avg_h_a) %>%
    left_join(avg_team) %>%
    mutate(
      xG_opponent=ifelse(h_a=='h', (xG_opponent_season*0.5) + (xG_opponent_h*0.3) + (xG_opponent_opp*0.2),
                         (xG_opponent_season*0.5) + (xG_opponent_a*0.3) + (xG_opponent_opp*0.2)),
      ict_index_opponent=ifelse(h_a=='h', (ict_index_opponent_season*0.5) + (ict_index_opponent_h)*0.3 + (ict_index_opponent_opp*0.2),
                                (ict_index_opponent_season*0.5) + (ict_index_opponent_a*0.3) + (ict_index_opponent_opp*0.2)),
      rank_minutes=ifelse(h_a=='h', (rank_minutes_season*0.5) + (rank_minutes_h)*0.3 + (rank_minutes_opp*0.2),
                          (rank_minutes_season*0.5) + (rank_minutes_a*0.3) + (rank_minutes_opp*0.2))
    ) %>% select(name, web_name, team, position, season, GW, kickoff_time, h_a, opponent, strength, difficulty, xG_opponent, ict_index_opponent, rank_minutes) %>%
    distinct(name, GW, .keep_all = T) %>%
    # mutate_if(is.numeric, function(x) ifelse(is.na(x), 0, x))
    group_by(team, h_a, strength, difficulty, position) %>%
    mutate(xG_opponent=ifelse(is.na(xG_opponent), mean(xG_opponent, na.rm = T), xG_opponent),
           ict_index_opponent=ifelse(is.na(ict_index_opponent), mean(ict_index_opponent, na.rm = T), ict_index_opponent),
           rank_minutes=ifelse(is.na(rank_minutes), mean(rank_minutes, na.rm = T), rank_minutes)) %>%
    ungroup()
  
  df2 <- val_data %>%
    filter(!(GW %in% temp$GW)) %>%
    select(name, web_name, team, position, season, GW, kickoff_time, h_a, opponent, strength, difficulty, xG_opponent, ict_index_opponent, rank_minutes) %>%
    rbind(temp2) %>%
    distinct(name, GW, .keep_all = T) %>%
    # mutate_if(is.numeric, function(x) ifelse(is.na(x), 0, x))
    group_by(team, h_a, strength, difficulty, position) %>%
    mutate(xG_opponent=ifelse(is.na(xG_opponent), mean(xG_opponent, na.rm = T), xG_opponent),
           ict_index_opponent=ifelse(is.na(ict_index_opponent), mean(ict_index_opponent, na.rm = T), ict_index_opponent),
           rank_minutes=ifelse(is.na(rank_minutes), mean(rank_minutes, na.rm = T), rank_minutes)) %>%
    ungroup()
  
} else {
  
  print('Error in assigning played vs not played games')
  
}

## Run the predictions
linear_save_predictions <- predict(linear_saves_model, df2) %>%
  data.frame() %>% 
  rename(Predicted_saves_linear=1) %>%
  cbind(df2) %>%
  left_join(val_data %>% select(name, position, team, GW, saves), by=c('name', 'position', 'team', 'GW')) %>%
  mutate(Predicted_saves_linear=ifelse(Predicted_saves_linear < 0, 0, Predicted_saves_linear),
         linear_saves_validation=saves-Predicted_saves_linear) %>%
  select(-saves)

linear_pen_predictions <- predict(linear_pen_model, df2) %>%
  data.frame() %>%
  rename(Predicted_pen_saves_linear=1) %>%
  cbind(df2) %>%
  left_join(val_data %>% select(name, position, team, GW, penalties_saved), by=c('name', 'position', 'team', 'GW')) %>%
  mutate(Predicted_pen_saves_linear=ifelse(Predicted_pen_saves_linear < 0, 0, Predicted_pen_saves_linear),
         linear_pen_saves_validation=penalties_saved-Predicted_pen_saves_linear) %>%
  select(-penalties_saved)

logit_save_predictions <- predict(logit_saves_model, df2) %>%
  data.frame() %>%
  rename(Predicted_saves_logit=1) %>%
  cbind(df2) %>% 
  left_join(val_data %>% select(name, position, team, GW, saves), by=c('name', 'position', 'team', 'GW')) %>%
  mutate(Predicted_saves_logit=ifelse(Predicted_saves_logit < 0, 0, Predicted_saves_logit),
         logit_saves_validation=saves-Predicted_saves_logit) %>%
  select(-saves)

logit_pen_predictions <- predict(logit_saves_model, df2) %>%
  data.frame() %>%
  rename(Predicted_pen_saves_logit=1) %>%
  cbind(df2) %>%
  left_join(val_data %>% select(name, position, team, GW, penalties_saved), by=c('name', 'position', 'team', 'GW')) %>%
  mutate(Predicted_pen_saves_logit=ifelse(Predicted_pen_saves_logit < 0, 0, Predicted_pen_saves_logit),
         logit_pen_saves_validation=penalties_saved-Predicted_pen_saves_logit) %>%
  select(-penalties_saved)

rf_save_predictions <- predict(rf_saves_model, df2) %>%
  data.frame() %>%
  rename(Predicted_saves_rf=1) %>%
  cbind(df2) %>% 
  left_join(val_data %>% select(name, position, team, GW, saves), by=c('name', 'position', 'team', 'GW')) %>%
  mutate(Predicted_saves_rf=ifelse(Predicted_saves_rf < 0, 0, Predicted_saves_rf),
         rf_saves_validation=saves-Predicted_saves_rf) %>%
  select(-saves)

rf_pen_predictions <- predict(rf_saves_model, df2) %>%
  data.frame() %>% 
  rename(Predicted_pen_saves_rf=1) %>%
  cbind(df2) %>%
  left_join(val_data %>% select(name, position, team, GW, penalties_saved), by=c('name', 'position', 'team', 'GW')) %>%
  mutate(Predicted_pen_saves_rf=ifelse(Predicted_pen_saves_rf < 0, 0, Predicted_pen_saves_rf),
         rf_pen_saves_validation=penalties_saved-Predicted_pen_saves_rf) %>%
  select(-penalties_saved)

linear_goals_conceded_prediction <- predict(linear_goals_conceded_model, df2) %>%
  data.frame() %>%
  rename(Predicted_goals_conceded_linear=1) %>%
  cbind(df2) %>% 
  left_join(val_data %>% select(name, position, team, GW, goals_conceded), by=c('name', 'position', 'team', 'GW')) %>%
  mutate(Predicted_goals_conceded_linear=ifelse(Predicted_goals_conceded_linear < 0, 0, Predicted_goals_conceded_linear),
         linear_goals_conceded_validation=goals_conceded-Predicted_goals_conceded_linear) %>%
  select(-goals_conceded)

logit_goals_conceded_prediction <- predict(logit_goals_conceded_model, df2) %>%
  data.frame() %>%
  rename(Predicted_goals_conceded_logit=1) %>%
  cbind(df2) %>% 
  left_join(val_data %>% select(name, position, team, GW, goals_conceded), by=c('name', 'position', 'team', 'GW')) %>%
  mutate(Predicted_goals_conceded_logit=ifelse(Predicted_goals_conceded_logit < 0, 0, Predicted_goals_conceded_logit),
         logit_goals_conceded_validation=goals_conceded-Predicted_goals_conceded_logit) %>%
  select(-goals_conceded)

rf_goals_conceded_prediction <- predict(rf_goals_conceded_model, df2) %>%
  data.frame() %>% 
  rename(Predicted_goals_conceded_rf=1) %>%
  cbind(df2) %>% 
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
