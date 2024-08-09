#---------------------------------------#
# Yellow and Red Cards model for the FPL Lineup Optimizer
# Written by: ncfisher
# Last updated: May 25 2024
#---------------------------------------#

## Need to predict number of red cards, yellow cards, based on controls, ICT Index, goals conceded

## Set up the models
linear_yc_model <- lm(yellow_cards ~ ict_index_opponent + goal_difference + position + h_a + strength + difficulty, data = est_data)
linear_rc_model <- lm(red_cards ~ ict_index_opponent + goal_difference + position + h_a + strength + difficulty, data = est_data)
logit_yc_model <- glm(yellow_cards ~ ict_index_opponent + goal_difference + position + h_a + strength + difficulty, data = est_data)
logit_rc_model <- lm(red_cards ~ ict_index_opponent + goal_difference + position + h_a + strength + difficulty, data = est_data)
summary(linear_yc_model)
summary(logit_yc_model)
summary(linear_rc_model)
summary(logit_rc_model)
rf_yc_model <- randomForest(yellow_cards ~ ict_index_opponent + goal_difference + position + h_a + strength + difficulty, data = est_data)
rf_rc_model <- randomForest(red_cards ~ ict_index_opponent + goal_difference + position + h_a + strength + difficulty, data = est_data)
gc()

## Get averages
avg_team <- est_data %>%
  group_by(name, position, team, difficulty, season) %>%
  summarize(goal_difference_opp=mean(goal_difference, na.rm = T),
            ict_index_opponent_opp=mean(ict_index_opponent, na.rm = T)) %>%
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
    summarize(goal_difference_opp=mean(goal_difference_opp, na.rm =T ),
              ict_index_opponent_opp=mean(ict_index_opponent_opp, na.rm = T)) %>%
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
  summarize(goal_difference=mean(goal_difference, na.rm = T),
            ict_index_opponent=mean(ict_index_opponent, na.rm = T)) %>%
  ungroup() %>%
  pivot_wider(names_from = h_a, values_from = c(goal_difference, ict_index_opponent))

if(max(avg_h_a$season)==max(val_data$season)) {
  avg_h_a <- avg_h_a %>% filter(season==max(season))
} else if(max(avg_h_a$season)!=max(val_data$season)) {
  avg_h_a <- avg_h_a %>%
    filter(season==max(season)) %>%
    mutate(season=max(val_data$season))
  
  temp <- avg_h_a %>%
    group_by(position) %>%
    summarize(goal_difference_a=mean(goal_difference_a, na.rm =T ),
              goal_difference_h=mean(goal_difference_h, na.rm =T ),
              ict_index_opponent_a=mean(ict_index_opponent_a, na.rm = T),
              ict_index_opponent_h=mean(ict_index_opponent_h, na.rm = T)) %>%
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
    summarize(goal_difference_a=mean(goal_difference_a, na.rm =T ),
              goal_difference_h=mean(goal_difference_h, na.rm =T ),
              ict_index_opponent_a=mean(ict_index_opponent_a, na.rm = T),
              ict_index_opponent_h=mean(ict_index_opponent_h, na.rm = T)) %>%
    ungroup()
}

avg_season <- est_data %>%
  group_by(name, position, team, season) %>%
  summarize(goal_difference_season=mean(goal_difference, na.rm = T),
            ict_index_opponent_season=mean(ict_index_opponent, na.rm = T)) %>%
  ungroup()

if(max(avg_season$season)==max(val_data$season)) {
  avg_season <- avg_season %>% filter(season==max(season))
} else if (max(avg_season$season)!=max(val_data$season)) {
  avg_season <- avg_season %>%
    filter(season==max(season)) %>%
    mutate(season=max(val_data$season))
  
  temp <- avg_season %>%
    group_by(position) %>%
    summarize(goal_difference_season=mean(goal_difference_season, na.rm = T),
              ict_index_opponent_season=mean(ict_index_opponent_season, na.rm = T)) %>%
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
    summarize(goal_difference_season=mean(goal_difference_season, na.rm = T),
              ict_index_opponent_season=mean(ict_index_opponent_season, na.rm = T)) %>%
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
      goal_difference=ifelse(h_a=='h', (goal_difference_season*0.5) + (goal_difference_h*0.3) + (goal_difference_opp*0.2),
                (goal_difference_season*0.5) + (goal_difference_a*0.3) + (goal_difference_opp*0.2)),
      ict_index_opponent=ifelse(h_a=='h', (ict_index_opponent_season*0.5) + (ict_index_opponent_h)*0.3 + (ict_index_opponent_opp*0.2),
                       (ict_index_opponent_season*0.5) + (ict_index_opponent_a*0.3) + (ict_index_opponent_opp*0.2))
    ) %>% select(name, web_name, team, position, season, GW, kickoff_time, h_a, opponent, strength, difficulty, goal_difference, ict_index_opponent) %>%
    distinct(name, GW, .keep_all = T) %>%
    # mutate_if(is.numeric, function(x) ifelse(is.na(x), 0, x)) %>%
    group_by(team, h_a, strength, difficulty, position) %>%
    mutate(goal_difference=ifelse(is.na(goal_difference), mean(goal_difference, na.rm = T), goal_difference),
           ict_index_opponent=ifelse(is.na(ict_index_opponent), mean(ict_index_opponent, na.rm = T), ict_index_opponent)) %>%
    ungroup()
  
} else if (nrow(temp) > 0) {
  
  temp2 <- temp %>%
    select(name, web_name, team, position, season, GW, kickoff_time, h_a, opponent, strength, difficulty) %>%
    left_join(avg_season) %>%
    left_join(avg_h_a) %>%
    left_join(avg_team) %>%
    mutate(
      goal_difference=ifelse(h_a=='h', (goal_difference_season*0.5) + (goal_difference_h*0.3) + (goal_difference_opp*0.2),
                (goal_difference_season*0.5) + (goal_difference_a*0.3) + (goal_difference_opp*0.2)),
      ict_index_opponent=ifelse(h_a=='h', (ict_index_opponent_season*0.5) + (ict_index_opponent_h)*0.3 + (ict_index_opponent_opp*0.2),
                       (ict_index_opponent_season*0.5) + (ict_index_opponent_a*0.3) + (ict_index_opponent_opp*0.2))
    ) %>% select(name, web_name, team, position, season, GW, kickoff_time, h_a, opponent, strength, difficulty, goal_difference, ict_index_opponent) %>%
    distinct(name, GW, .keep_all = T) %>%
    # mutate_if(is.numeric, function(x) ifelse(is.na(x), 0, x)) %>%
    group_by(team, h_a, strength, difficulty, position) %>%
    mutate(goal_difference=ifelse(is.na(goal_difference), mean(goal_difference, na.rm = T), goal_difference),
           ict_index_opponent=ifelse(is.na(ict_index_opponent), mean(ict_index_opponent, na.rm = T), ict_index_opponent)) %>%
    ungroup()
  
  df2 <- val_data %>%
    filter(!(GW %in% temp$GW)) %>%
    select(name, web_name, team, position, season, GW, kickoff_time, h_a, opponent, strength, difficulty, goal_difference, ict_index_opponent) %>%
    rbind(temp2) %>%
    distinct(name, GW, .keep_all = T) %>%
    # mutate_if(is.numeric, function(x) ifelse(is.na(x), 0, x))
    group_by(team, h_a, strength, difficulty, position) %>%
    mutate(goal_difference=ifelse(is.na(goal_difference), mean(goal_difference, na.rm = T), goal_difference),
           ict_index_opponent=ifelse(is.na(ict_index_opponent), mean(ict_index_opponent, na.rm = T), ict_index_opponent)) %>%
    ungroup()
  
} else {
  
  print('Error in assigning played vs not played games')
  
}

## Run the predictions for yellow cards
linear_yc_predictions <- predict(linear_yc_model, df2) %>%
  data.frame() %>%
  rename(Predicted_yellow_cards_linear=1) %>%
  cbind(df2) %>%
  left_join(val_data %>% select(name, position, team, GW, yellow_cards), by=c('name', 'position', 'team', 'GW')) %>%
  mutate(Predicted_yellow_cards_linear=ifelse(Predicted_yellow_cards_linear < 0, 0, Predicted_yellow_cards_linear),
         linear_yellow_cards_validation=yellow_cards-Predicted_yellow_cards_linear) %>%
  select(-yellow_cards)

logit_yc_predictions <- predict(logit_yc_model, df2) %>%
  data.frame() %>%
  rename(Predicted_yellow_cards_logit=1) %>%
  cbind(df2) %>%
  left_join(val_data %>% select(name, position, team, GW, yellow_cards), by=c('name', 'position', 'team', 'GW')) %>%
  mutate(Predicted_yellow_cards_logit=ifelse(Predicted_yellow_cards_logit < 0, 0, Predicted_yellow_cards_logit),
         logit_yellow_cards_validation=yellow_cards-Predicted_yellow_cards_logit) %>%
  select(-yellow_cards)

rf_yc_predictions <- predict(rf_yc_model, df2) %>%
  data.frame() %>%
  rename(Predicted_yellow_cards_rf=1) %>%
  cbind(df2) %>%
  left_join(val_data %>% select(name, position, team, GW, yellow_cards), by=c('name', 'position', 'team', 'GW')) %>%
  mutate(Predicted_yellow_cards_rf=ifelse(Predicted_yellow_cards_rf < 0, 0, Predicted_yellow_cards_rf),
         rf_yellow_cards_validation=yellow_cards-Predicted_yellow_cards_rf) %>%
  select(-yellow_cards)

## Run the predictions for red cards
linear_rc_predictions <- predict(linear_rc_model, df2) %>%
  data.frame() %>%
  rename(Predicted_red_cards_linear=1) %>%
  cbind(df2) %>%
  left_join(val_data %>% select(name, position, team, GW, red_cards), by=c('name', 'position', 'team', 'GW')) %>%
  mutate(Predicted_red_cards_linear=ifelse(Predicted_red_cards_linear < 0, 0, Predicted_red_cards_linear),
         linear_red_cards_validation=red_cards-Predicted_red_cards_linear) %>%
  select(-red_cards)

logit_rc_predictions <- predict(logit_rc_model, df2) %>%
  data.frame() %>%
  rename(Predicted_red_cards_logit=1) %>%
  cbind(df2) %>%
  left_join(val_data %>% select(name, position, team, GW, red_cards), by=c('name', 'position', 'team', 'GW')) %>%
  mutate(Predicted_red_cards_logit=ifelse(Predicted_red_cards_logit < 0, 0, Predicted_red_cards_logit),
         logit_red_cards_validation=red_cards-Predicted_red_cards_logit) %>%
  select(-red_cards)

rf_rc_predictions <- predict(rf_rc_model, df2) %>%
  data.frame() %>% 
  rename(Predicted_red_cards_rf=1) %>%
  cbind(df2) %>%
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
