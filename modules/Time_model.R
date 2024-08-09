#---------------------------------------#
# Time played model for the FPL Lineup Optimizer
# Written by: ncfisher
# Last updated: May 26 2024
#---------------------------------------#

# Model set up
linear_played_model <- lm(played ~ rank_minutes + ict_index + position + strength + difficulty + h_a, data = est_data)
summary(linear_played_model)
logit_played_model <- glm(played ~ rank_minutes + ict_index + position + strength + difficulty + h_a, data = est_data)
summary(logit_played_model)
rf_played_model <- randomForest(played ~ rank_minutes + ict_index + position + strength + difficulty + h_a, data = est_data %>% filter(!is.na(rank_minutes)))
gc()
linear_played60_model <- lm(played60 ~ rank_minutes + ict_index + position + strength + difficulty + h_a, data = est_data)
summary(linear_played60_model)
logit_played60_model <- glm(played60 ~ rank_minutes + ict_index + position + strength + difficulty + h_a, data = est_data)
summary(logit_played60_model)
rf_played60_model <- randomForest(played60 ~ rank_minutes + ict_index + position + strength + difficulty + h_a, data = est_data %>% filter(!is.na(rank_minutes)))
gc()
linear_cs_model <- lm(clean_sheet ~ rank_minutes + played60 + ict_index + position + strength + difficulty + h_a, data = est_data)
summary(linear_cs_model)
logit_cs_model <- glm(clean_sheet ~ rank_minutes + played60 + ict_index + position + strength + difficulty + h_a, data = est_data)
summary(logit_cs_model)
rf_cs_model <- randomForest(clean_sheet ~ rank_minutes + played60 + ict_index + position + strength + difficulty + h_a, data = est_data %>% filter(!is.na(rank_minutes)))
gc()

## Get averages
avg_team <- est_data %>%
 group_by(name, position, team, difficulty, season) %>%
 summarize(ict_index_opp=mean(ict_index, na.rm = T),
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
     summarize(ict_index_opp=mean(ict_index_opp, na.rm =T ),
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
   group_by(name, position, team, h_a, season) %>%
   summarize(ict_index=mean(ict_index, na.rm = T),
             rank_minutes=mean(rank_minutes, na.rm = T)) %>%
   ungroup() %>%
   pivot_wider(names_from = h_a, values_from = c(ict_index, rank_minutes))

 if(max(avg_h_a$season)==max(val_data$season)) {
   avg_h_a <- avg_h_a %>% filter(season==max(season))
 } else if(max(avg_h_a$season)!=max(val_data$season)) {
   avg_h_a <- avg_h_a %>%
     filter(season==max(season)) %>%
     mutate(season=max(val_data$season))

   temp <- avg_h_a %>%
     group_by(position) %>%
     summarize(rank_minutes_a=mean(rank_minutes_a, na.rm =T ),
               rank_minutes_h=mean(rank_minutes_h, na.rm =T ),
               ict_index_a=mean(ict_index_a, na.rm = T),
               ict_index_h=mean(ict_index_h, na.rm = T)) %>%
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
     summarize(rank_minutes_a=mean(rank_minutes_a, na.rm =T ),
               rank_minutes_h=mean(rank_minutes_h, na.rm =T ),
               ict_index_a=mean(ict_index_a, na.rm = T),
               ict_index_h=mean(ict_index_h, na.rm = T)) %>%
     ungroup()
 }

 avg_season <- est_data %>%
   group_by(name, position, team, season) %>%
   summarize(ict_index_season=mean(ict_index, na.rm = T),
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
     summarize(rank_minutes_season=mean(rank_minutes_season, na.rm = T),
               ict_index_season=mean(ict_index_season, na.rm = T)) %>%
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
     summarize(rank_minutes_season=mean(rank_minutes_season, na.rm = T),
               ict_index_season=mean(ict_index_season, na.rm = T))
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
       ict_index=ifelse(h_a=='h', (ict_index_season*0.5) + (ict_index_h)*0.3 + (ict_index_opp*0.2),
                                 (ict_index_season*0.5) + (ict_index_a*0.3) + (ict_index_opp*0.2)),
       rank_minutes=ifelse(h_a=='h', (rank_minutes_season*0.5) + (rank_minutes_h)*0.3 + (rank_minutes_opp*0.2),
                           (rank_minutes_season*0.5) + (rank_minutes_a*0.3) + (rank_minutes_opp*0.2))
     ) %>% select(name, web_name, team, position, season, GW, kickoff_time, h_a, opponent, strength, difficulty, ict_index, rank_minutes) %>%
     distinct(name, GW, .keep_all = T) %>%
     # mutate_if(is.numeric, function(x) ifelse(is.na(x), 0, x)) %>%
     group_by(team, h_a, strength, difficulty, position) %>%
     mutate(rank_minutes=ifelse(is.na(rank_minutes), mean(rank_minutes, na.rm = T), rank_minutes),
            ict_index=ifelse(is.na(ict_index), mean(ict_index, na.rm = T), ict_index)) %>%
     ungroup()

 } else if (nrow(temp) > 0) {

   temp2 <- temp %>%
     select(name, web_name, team, position, season, GW, kickoff_time, h_a, opponent, strength, difficulty) %>%
     left_join(avg_season) %>%
     left_join(avg_h_a) %>%
     left_join(avg_team) %>%
     mutate(
       ict_index=ifelse(h_a=='h', (ict_index_season*0.5) + (ict_index_h)*0.3 + (ict_index_opp*0.2),
                                 (ict_index_season*0.5) + (ict_index_a*0.3) + (ict_index_opp*0.2)),
       rank_minutes=ifelse(h_a=='h', (rank_minutes_season*0.5) + (rank_minutes_h)*0.3 + (rank_minutes_opp*0.2),
                           (rank_minutes_season*0.5) + (rank_minutes_a*0.3) + (rank_minutes_opp*0.2))
     ) %>% select(name, web_name, team, position, season, GW, kickoff_time, h_a, opponent, strength, difficulty, ict_index, rank_minutes) %>%
     distinct(name, GW, .keep_all = T) %>%
     # mutate_if(is.numeric, function(x) ifelse(is.na(x), 0, x)) %>%
     group_by(team, h_a, strength, difficulty, position) %>%
     mutate(rank_minutes=ifelse(is.na(rank_minutes), mean(rank_minutes, na.rm = T), rank_minutes),
            ict_index=ifelse(is.na(ict_index), mean(ict_index, na.rm = T), ict_index)) %>%
     ungroup()

   df2 <- val_data %>%
     filter(!(GW %in% temp$GW)) %>%
     select(name, web_name, team, position, season, GW, kickoff_time, h_a, opponent, strength, difficulty, ict_index, rank_minutes) %>%
     rbind(temp2) %>%
     distinct(name, GW, .keep_all = T) %>%
     # mutate_if(is.numeric, function(x) ifelse(is.na(x), 0, x)) %>%
     group_by(team, h_a, strength, difficulty, position) %>%
     mutate(rank_minutes=ifelse(is.na(rank_minutes), mean(rank_minutes, na.rm = T), rank_minutes),
            ict_index=ifelse(is.na(ict_index), mean(ict_index, na.rm = T), ict_index)) %>%
     ungroup()

 } else {

   print('Error in assigning played vs not played games')

 }

 #### Predictions
 linear_played_predictions <- predict(linear_played_model, df2) %>%
   data.frame() %>%
   rename(Probability_played_linear=1) %>%
   cbind(df2) %>%
   left_join(val_data %>% select(name, position, team, GW, minutes), by=c('name', 'position', 'team', 'GW')) %>%
   distinct(name, GW, .keep_all = T) %>%
   mutate(Probability_played_linear=ifelse(Probability_played_linear < 0, 0, Probability_played_linear),
          Probability_played_linear=ifelse(Probability_played_linear > 1, 1, Probability_played_linear),
          played=ifelse(minutes > 0, 1, 0),
          linear_played_validation=played-Probability_played_linear) %>%
   select(-played, -minutes)

 logit_played_predictions <- predict(logit_played_model, df2) %>%
   data.frame() %>%
   cbind(df2) %>%
   left_join(val_data %>% select(name, position, team, GW, minutes), by=c('name', 'position', 'team', 'GW')) %>%
   distinct(name, GW, .keep_all = T) %>%
   rename(Probability_played_logit=1) %>%
   mutate(Probability_played_logit=ifelse(Probability_played_logit < 0, 0, Probability_played_logit),
          Probability_played_logit=ifelse(Probability_played_logit > 1, 1, Probability_played_logit),
          played=ifelse(minutes > 0, 1, 0),
          logit_played_validation=played-Probability_played_logit) %>%
   select(-played, -minutes)

 rf_played_predictions <- predict(rf_played_model, df2) %>%
   data.frame() %>%
   cbind(df2) %>%
   left_join(val_data %>% select(name, position, team, GW, minutes), by=c('name', 'position', 'team', 'GW')) %>%
   distinct(name, GW, .keep_all = T) %>%
   rename(Probability_played_rf=1) %>%
   mutate(Probability_played_rf=ifelse(Probability_played_rf < 0, 0, Probability_played_rf),
          Probability_played_rf=ifelse(Probability_played_rf > 1, 1, Probability_played_rf),
          played=ifelse(minutes > 0, 1, 0),
          rf_played_validation=played-Probability_played_rf) %>%
   select(-minutes, -played)

 linear_played60_predictions <- predict(linear_played60_model, df2) %>%
   data.frame() %>%
   cbind(df2) %>%
   left_join(val_data %>% select(name, position, team, GW, minutes), by=c('name', 'position', 'team', 'GW')) %>%
   distinct(name, GW, .keep_all = T) %>%
   rename(Probability_played60_linear=1) %>%
   mutate(Probability_played60_linear=ifelse(Probability_played60_linear > 1, 1, Probability_played60_linear),
          Probability_played60_linear=ifelse(Probability_played60_linear < 0, 0, Probability_played60_linear),
          played60=ifelse(minutes > 59, 1, 0),
          linear_played60_validation=played60-Probability_played60_linear) %>%
   select(-minutes, -played60)

 logit_played60_predictions <- predict(logit_played60_model, df2) %>%
   data.frame() %>%
   rename(Probability_played60_logit=1) %>%
   cbind(df2) %>%
   left_join(val_data %>% select(name, position, team, GW, minutes), by=c('name', 'position', 'team', 'GW')) %>%
   distinct(name, GW, .keep_all = T) %>%
   mutate(Probability_played60_logit=ifelse(Probability_played60_logit > 1, 1, Probability_played60_logit),
          Probability_played60_logit=ifelse(Probability_played60_logit < 0, 0, Probability_played60_logit),
          played60=ifelse(minutes > 59, 1, 0),
          logit_played60_validation=played60-Probability_played60_logit) %>%
   select(-played60, -minutes)

 rf_played60_predictions <- predict(rf_played60_model, df2) %>%
   data.frame() %>%
   cbind(df2) %>%
   left_join(val_data %>% select(name, position, team, GW, minutes), by=c('name', 'position', 'team', 'GW')) %>%
   distinct(name, GW, .keep_all = T) %>%
   rename(Probability_played60_rf=1) %>%
   mutate(Probability_played60_rf=ifelse(Probability_played60_rf > 1, 1, Probability_played60_rf),
          Probability_played60_rf=ifelse(Probability_played60_rf < 0, 0, Probability_played60_rf),
          played60=ifelse(minutes > 59, 1, 0),
          rf_played60_validation=played60-Probability_played60_rf) %>%
   select(-played60, -minutes)

 linear_cs_prediction <- predict(linear_cs_model, df2 %>%
                                   left_join(linear_played60_predictions %>%
                                               select(name, team, position, GW, Probability_played60_linear) %>%
                                               rename(played60=Probability_played60_linear))) %>%
   data.frame() %>%
   cbind(df2) %>%
   left_join(val_data %>% select(name, position, team, GW, minutes, goals_conceded), by=c('name', 'position', 'team', 'GW')) %>%
   distinct(name, GW, .keep_all = T) %>%
   rename(Probability_cs_linear=1) %>%
   mutate(Probability_cs_linear=ifelse(Probability_cs_linear < 0, 0, Probability_cs_linear),
          Probability_cs_linear=ifelse(Probability_cs_linear > 1, 1, Probability_cs_linear),
          cs=ifelse(minutes > 59 & goals_conceded==0, 1, 0),
          linear_cs_validation=cs-Probability_cs_linear) %>%
   select(-cs, -goals_conceded, -minutes)

 logit_cs_prediction <- predict(logit_cs_model, df2 %>%
                                   left_join(logit_played60_predictions %>%
                                               select(name, team, position, GW, Probability_played60_logit) %>%
                                               rename(played60=Probability_played60_logit))) %>%
   data.frame() %>%
   cbind(df2) %>%
   left_join(val_data %>% select(name, position, team, GW, minutes, goals_conceded), by=c('name', 'position', 'team', 'GW')) %>%
   distinct(name, GW, .keep_all = T) %>%
   rename(Probability_cs_logit=1) %>%
   mutate(Probability_cs_logit=ifelse(Probability_cs_logit < 0, 0, Probability_cs_logit),
          Probability_cs_logit=ifelse(Probability_cs_logit > 1, 1, Probability_cs_logit),
          cs=ifelse(minutes > 59 & goals_conceded==0, 1, 0),
          logit_cs_validation=cs-Probability_cs_logit) %>%
   select(-cs, -goals_conceded, -minutes)

 rf_cs_prediction <- predict(rf_cs_model, df2 %>%
                                  left_join(rf_played60_predictions %>%
                                              select(name, team, position, GW, Probability_played60_rf) %>%
                                              rename(played60=Probability_played60_rf))) %>%
   data.frame() %>%
   cbind(df2) %>%
   left_join(val_data %>% select(name, position, team, GW, minutes, goals_conceded), by=c('name', 'position', 'team', 'GW')) %>%
   distinct(name, GW, .keep_all = T) %>%
   rename(Probability_cs_rf=1) %>%
   mutate(Probability_cs_rf=ifelse(Probability_cs_rf < 0, 0, Probability_cs_rf),
          Probability_cs_rf=ifelse(Probability_cs_rf > 1, 1, Probability_cs_rf),
          cs=ifelse(minutes > 59 & goals_conceded==0, 1, 0),
          rf_cs_validation=cs-Probability_cs_rf) %>%
   select(-cs, -minutes, -goals_conceded)

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
