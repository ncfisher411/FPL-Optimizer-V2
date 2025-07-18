#---------------------------------------#
# Validation script for FPL Probability Model
# Written by: ncfisher
# Last updated: July 17 2025
#---------------------------------------#

# ncfisher put through iterative testing on 2024 to see which parameters/weights
# best match the actual 2024 values. Look at this just at the overall scale

# 1) Load the 2024 results for validation
results_modeled <- read.xlsx('data/probability model results_2024.xlsx', sheet = 'overall')

# 2) Load the 2024 actual FPL results
results_actual <- read.csv('data/Combined_data.csv') %>%
  filter(season==2024) %>%
  mutate(position = ifelse(position=='GK', 'GKP', position),
         goal_points_actual = ifelse(position=='GKP', 10 * goals_scored, 0),
         goal_points_actual = ifelse(position=='DEF', 6 * goals_scored, goal_points_actual),
         goal_points_actual = ifelse(position=='MID', 5 * goals_scored, goal_points_actual),
         goal_points_actual = ifelse(position=='FWD', 4 * goals_scored, goal_points_actual),
         assist_points_actual = 3 * assists,
         time_points = ifelse(minutes > 0, 1, 0),
         time_points = ifelse(minutes >= 60, 2, time_points),
         cs_points_actual = ifelse(minutes >= 60 & goals_conceded==0 & position=='GKP', 4, 0),
         cs_points_actual = ifelse(minutes >= 60 & goals_conceded==0 & position=='DEF', 4, cs_points_actual),
         cs_points_actual = ifelse(minutes >= 60 & goals_conceded==0 & position=='MID', 1, cs_points_actual),
         goals_conceded_points_actual = ifelse(goals_conceded == 2, -1*(goals_conceded/2), 0),
         goals_conceded_points_actual = ifelse(goals_conceded == 4, -1*(goals_conceded/2), goals_conceded_points_actual),
         goals_conceded_points_actual = ifelse(goals_conceded == 6, -1*(goals_conceded/2), goals_conceded_points_actual),
         goals_conceded_points_actual = ifelse(goals_conceded == 8, -1*(goals_conceded/2), goals_conceded_points_actual),
         goals_conceded_points_actual = ifelse(goals_conceded == 10, -1*(goals_conceded/2), goals_conceded_points_actual),
         cards_deductions_actual = (-1 * yellow_cards) + (-3 * red_cards),
         save_points_actual = floor(saves/3) * 1,
         pen_saves_points_actual = penalties_saved * 5,
         neg_points_actual = (penalties_missed * -2) + (own_goals * -2)
         ) %>%
  group_by(name, season) %>%
  summarize(total_points_actual = sum(total_points, na.rm = T),
            across(ends_with('actual'), ~sum(., na.rm = T)),
            minutes = sum(minutes, na.rm = T)) %>%
  ungroup()

results_join <- results_modeled %>%
  left_join(results_actual %>% select(-minutes), by=c('full_name'='name', 'season')) %>%
  pivot_longer(cols = -c('name', 'full_name', 'position', 'season'), names_to = 'stat', values_to = 'values') %>%
  arrange(name, stat) %>%
  mutate(expected_value = ifelse(!grepl('actual', stat), values, 0),
         actual_value = ifelse(grepl('actual', stat), values, 0),
         stat = gsub('_actual', '', stat)) %>%
  group_by(name, full_name, position, season, stat) %>%
  summarize(expected_value = sum(expected_value, na.rm = T),
            actual_value = sum(actual_value, na.rm = T)) %>%
  ungroup() 

# 3) Validate the following:
list <- list()
performance <- data.frame()

for(i in unique(results_join$stat)){
  ## Overall rankings by stat for players that play at least 1000 minutes
  temp <- results_join %>%
    filter(stat==i) %>%
    mutate(expected_rank = rank(-expected_value, ties.method = 'first'),
           actual_rank = rank(-actual_value, ties.method = 'first')) %>%
    left_join(results_actual %>% select(name, minutes), by=c('full_name'='name')) %>%
    filter(minutes >= 1000)
  
  temp2 <- temp %>%
    mutate(`Actual_value - expected_value` = actual_value - expected_value,
           `Actual_rank - expected_Rank` = actual_rank - expected_rank) %>%
    group_by(stat) %>%
    summarize(`Actual_value - expected_value` = mean(`Actual_value - expected_value`, na.rm = T),
              `Actual_rank - expected_rank` = mean(`Actual_rank - expected_Rank`, na.rm = T)) %>%
    ungroup()
  
  list[[i]] <- temp %>% data.frame()
  performance <- performance %>% rbind(temp2)
}

##### The average errors are relatively small across all the stats
performance_position <- data.frame()

## Positional rankings
for(i in unique(results_join$stat)){
  for(j in unique(results_join$position)){
    temp <- results_join %>%
      filter(stat==i & position==j) %>%
      mutate(expected_position_rank = rank(-expected_value, ties.method = 'first'),
             actual_position_rank = rank(-actual_value, ties.method = 'first')) %>%
      left_join(results_actual %>% select(name, minutes), by=c('full_name'='name')) %>%
      filter(minutes >= 1000)
    
    temp2 <- temp %>%
      mutate(`Actual_value - expected_value` = actual_value - expected_value,
             `Actual_position_rank - expected_position_Rank` = actual_position_rank - expected_position_rank) %>%
      group_by(position, stat) %>%
      summarize(`Actual_value - expected_value` = mean(`Actual_value - expected_value`, na.rm = T),
                `Actual_position_rank - expected_position_rank` = mean(`Actual_position_rank - expected_position_Rank`, na.rm = T)) %>%
      ungroup()
    
    list[[j]] <- temp %>% data.frame()
    performance_position <- performance_position %>% rbind(temp2)
  }
}

# 4) write the validation results
performance_list <- list('performance_overall'=performance, 'performance_position' = performance_position)
write.xlsx(list, 'data/Probability model validation.xlsx', overwrite = T)
write.xlsx(performance_list, 'data/Probability model peformance.xlsx', overwrite = T)
