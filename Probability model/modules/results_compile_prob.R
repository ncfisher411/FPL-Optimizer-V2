#---------------------------------------#
# Results compile script for FPL Probability Model
# Written by: ncfisher
# Last updated: June 23 2026
#---------------------------------------#
status <- grepl('TRUE', unique(fixtures$finished))

# 1) Get the points from the time module
time_results <- fixtures %>%
  left_join(current_players %>% mutate(season=max(fixtures$season)), 
            by=c('team' = 'team_name', 'season')) %>%
  left_join(probs_time %>% 
              filter(name %in% current_players$name) %>%
              select(name, contains('Prob'))) %>%
  group_by(team, position) %>%
  mutate(Prob_played = ifelse(is.na(Prob_played), mean(Prob_played, na.rm = T), Prob_played),
         Prob_played60 = ifelse(is.na(Prob_played60), mean(Prob_played60, na.rm = T), Prob_played60)) %>%
  ungroup() %>%
  mutate(time_points = (Prob_played * 1) + (Prob_played60 * 1)) %>%
  distinct(name, GW, .keep_all = T)


# 2) Get the points from the goals module
df <- goal_probs %>%
  filter(season==max(season)) %>%
  select(name, position, team, opponent_name, was_home, xG_calculated) %>%
  filter(name %in% current_players$name) %>%
  mutate(position=ifelse(position=='GK', 'GKP', position))

goal_results <- fixtures %>%
  mutate(was_home = ifelse(h_a=='h', 'TRUE', 'FALSE')) %>%
  left_join(current_players %>% mutate(season=max(fixtures$season)), 
            by=c('team' = 'team_name', 'season')) %>%
  left_join(df) %>%
  mutate(goal_points = ifelse(position=='GKP', 10 * xG_calculated, 0),
         goal_points = ifelse(position=='DEF', 6 * xG_calculated, goal_points),
         goal_points = ifelse(position=='MID', 5 * xG_calculated, goal_points),
         goal_points = ifelse(position=='FWD', 4 * xG_calculated, goal_points)) %>%
  distinct(name, GW, .keep_all = T)

# 3) Get the points from assists module
df <- assist_probs %>%
  filter(season==max(season)) %>%
  select(name, position, team, opponent_name, was_home, xA_calculated) %>%
  filter(name %in% current_players$name) %>%
  rename(opponent = opponent_name) %>%
  mutate(position=ifelse(position=='GK', 'GKP', position),
         was_home = ifelse(was_home=='TRUE', 'True', 'False'))

assist_results <- fixtures %>%
  mutate(was_home = ifelse(h_a=='h', 'True', 'False')) %>%
  left_join(current_players %>% mutate(season=max(fixtures$season)), 
            by=c('team' = 'team_name', 'season')) %>%
  left_join(df) %>%
  mutate(assist_points = 3 * xA_calculated) %>%
  distinct(name, GW, .keep_all = T)

# 4) Get the points from clean sheets
cs_results <- fixtures %>%
  mutate(was_home = ifelse(h_a=='h', 'TRUE', 'FALSE')) %>%
  left_join(current_players %>% mutate(season=max(fixtures$season)), 
            by=c('team' = 'team_name', 'season')) %>%
  left_join(cs_probs %>% rename(opponent = opponent_name)) %>%
  left_join(time_results %>% select(-time_points)) %>%
  mutate(cs_points =  ifelse(position=='GKP' | position=='DEF', 4 * ((cs_team_weight_2*goals_conceded_0_team) + (cs_ha_weight_2*goals_conceded_0)), 0),
         cs_points =  ifelse(position=='MID', 1 * ((cs_team_weight_2*goals_conceded_0_team) + (cs_ha_weight_2*goals_conceded_0)), cs_points),
         goals_conceded_points =  ifelse(position=='GKP' | position=='DEF',
           (-1 * ((cs_team_weight_2*goals_conceded_2_team) + cs_ha_weight_2*goals_conceded_2)) +
           (-1 * ((cs_team_weight_2*goals_conceded_4_team) + cs_ha_weight_2*goals_conceded_4)) +
           (-1 * ((cs_team_weight_2*goals_conceded_6_team) + cs_ha_weight_2*goals_conceded_6)) +
           (-1 * ((cs_team_weight_2*goals_conceded_8_team) + cs_ha_weight_2*goals_conceded_8)), 0),
         cs_points = cs_points * Prob_played60) %>%
  distinct(name, GW, .keep_all = T)

# 5) Get the points lost from cards
cards_results <- fixtures %>%
  mutate(was_home = ifelse(h_a=='h', 'True', 'False')) %>%
  left_join(current_players %>% mutate(season=max(fixtures$season)), 
            by=c('team' = 'team_name', 'season')) %>%
  left_join(yellow_cards_probs %>% 
              mutate(was_home = ifelse(was_home=='TRUE', 'True', 'False')) %>%
              rename(opponent = opponent_name) %>%
              select(name, season, team, opponent, was_home, yellow_cards_calculated)) %>%
  left_join(red_cards_probs %>% 
              mutate(was_home = ifelse(was_home=='TRUE', 'True', 'False')) %>%
              rename(opponent = opponent_name) %>%
              select(name, season, team, opponent, was_home, red_cards_calculated)) %>%
  left_join(probs_time) %>%
  mutate(cards_deductions = (yellow_cards_calculated * -1) + (red_cards_calculated * -3)) %>%
  distinct(name, GW, .keep_all = T)

# 6) Get the points attributed to goalkeepers
gk_results <- fixtures %>%
  mutate(was_home = ifelse(h_a=='h', 'True', 'False')) %>%
  left_join(saves_probs %>% 
              rename(opponent = opponent_name) %>%
              mutate(position = 'GKP')) %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.), 0, .)),
         saves_points = (saves/3 * 1),
         pen_saves_points = pen_save_prob * 5) %>%
  distinct(name, GW, .keep_all = T)

# 7) Get the negative scoring events
neg_results <- fixtures %>%
  mutate(was_home = ifelse(h_a=='h', 'True', 'False')) %>%
  left_join(current_players %>% mutate(season=max(fixtures$season)), 
            by=c('team' = 'team_name', 'season')) %>%
  left_join(probs_neg %>% mutate(position=ifelse(position=='GK', 'GKP', position))) %>%
  mutate(neg_points = (-2 * prob_pen_miss * Prob_played) + (-2 * prob_own_goal * Prob_played)) %>%
  distinct(name, GW, .keep_all = T)

# 8) Get the bonus points
bonus_results <- fixtures %>%
  mutate(was_home = ifelse(h_a=='h', 'True', 'False')) %>%
  left_join(current_players %>% mutate(season=max(fixtures$season)), 
            by=c('team' = 'team_name', 'season')) %>%
  left_join(probs_bonus) %>%
  mutate(bonus_points = (b1*1) + (b2*2) + (b3*3)) %>%
  distinct(name, GW, .keep_all = T)

# 9) Get the defensive contribution points
def_results <- fixtures %>%
  mutate(was_home = ifelse(h_a=='h', 'True', 'False')) %>%
  left_join(current_players %>% mutate(season=max(fixtures$season)), 
            by=c('team' = 'team_name', 'season')) %>%
  left_join(def_probs) %>%
  left_join(time_results %>% select(-time_points)) %>% 
  mutate(def_actions =  Prob_played60 * def_actions_per_90,
         def_actions = def_actions/def_actions_per_90,
         defense_points = def_actions * 2,
         defense_points = ifelse(is.nan(defense_points) | is.na(defense_points), 0, defense_points),
         def_actions = ifelse(is.nan(def_actions) | is.na(def_actions), 0, def_actions),
         defense_points = ifelse(def_actions_per_90 >= 10 & position=='DEF', def_weight_def_1 * defense_points, defense_points),
         defense_points = ifelse(def_actions_per_90 < quantile(def_actions_per_90)[4] & position=='DEF', def_weight_def_2 * defense_points, defense_points),
         defense_points = ifelse(def_actions_per_90 < quantile(def_actions_per_90)[3] & position=='DEF', def_weight_def_3 * defense_points, defense_points),
         defense_points = ifelse(def_actions_per_90 < quantile(def_actions_per_90)[2] & position=='DEF', def_weight_def_4 * defense_points, defense_points),
         defense_points = ifelse(def_actions_per_90 >= 10 & position=='MID', def_weight_mid_1 * defense_points, defense_points),
         defense_points = ifelse(def_actions_per_90 < quantile(def_actions_per_90)[4] & position=='MID', def_weight_mid_2 * defense_points, defense_points),
         defense_points = ifelse(def_actions_per_90 < quantile(def_actions_per_90)[3] & position=='MID', def_weight_mid_3 * defense_points, defense_points),
         defense_points = ifelse(def_actions_per_90 < quantile(def_actions_per_90)[2] & position=='MID', def_weight_mid_4 * defense_points, defense_points),
         defense_points = ifelse(def_actions_per_90 >= 10 & position=='FWD', def_weight_fwd_1 * defense_points, defense_points),
         defense_points = ifelse(def_actions_per_90 < quantile(def_actions_per_90)[4] & position=='FWD', def_weight_fwd_2 * defense_points, defense_points),
         defense_points = ifelse(def_actions_per_90 < quantile(def_actions_per_90)[3] & position=='FWD', def_weight_fwd_3 * defense_points, defense_points),
         defense_points = ifelse(def_actions_per_90 < quantile(def_actions_per_90)[2] & position=='FWD', def_weight_fwd_4 * defense_points, defense_points),
         defense_points = ifelse(def_actions_per_90 == quantile(def_actions_per_90)[1], 0, defense_points)
         ) %>%
  distinct(name, GW, .keep_all = T)

# Compile the final results
weekly_results <- time_results %>%
  select(name, position, team, season, GW, opponent, h_a, time_points) %>%
  left_join(goal_results %>%
              rename(xG = xG_calculated) %>%
              select(name, position, team, season, GW, opponent, h_a, goal_points, xG)) %>%
  left_join(assist_results %>%
              rename(assists = xA_calculated) %>%
              select(name, position, team, season, GW, opponent, h_a, assist_points, assists)) %>%
  left_join(cs_results %>%
              select(name, position, team, season, GW, opponent, h_a, cs_points, goals_conceded_points, contains('goals_conceded'))) %>%
  left_join(cards_results %>%
              rename(yellow_cards = yellow_cards_calculated,
                     red_cards = red_cards_calculated) %>%
              select(name, position, team, season, GW, opponent, h_a, cards_deductions, yellow_cards, red_cards)) %>%
  left_join(gk_results %>%
              select(name, position, team, season, GW, opponent, h_a, saves_points, pen_saves_points, saves, pen_save_prob)) %>%
  left_join(neg_results %>%
              select(name, position, team, season, GW, opponent, h_a, neg_points, prob_pen_miss, prob_own_goal)) %>%
  left_join(bonus_results %>%
              rename(bonus_points_0 = b0, bonus_points_1 = b1, bonus_points_2 = b2,
                     bonus_points_3 = b3) %>%
              select(name, position, team, season, GW, opponent, h_a, bonus_points, bonus_points_0, bonus_points_1, bonus_points_2, bonus_points_3)) %>%
  left_join(def_results %>%
              select(name, position, team, season, GW, opponent, h_a, defense_points)) %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.), 0, .)),
         GW_points = time_points + goal_points + assist_points + cs_points + goals_conceded_points +
           cards_deductions + saves_points + pen_saves_points + neg_points + bonus_points + defense_points) %>%
  # left_join(current_players %>% select(name, chance_of_playing_this_round)) %>%
  # mutate(GW_points = GW_points * (chance_of_playing_this_round/100)) %>%
  select(name, position, team, season, GW, opponent, h_a, GW_points,
         # chance_of_playing_this_round,
         everything()) %>%
  left_join(current_players %>%
              select(name, web_name, team_name)) %>%
  rename(full_name = name, 
         name = web_name) %>%
  select(name, full_name, team_name, everything())

overall_results <- weekly_results %>% 
  group_by(name, full_name, position, season) %>%
  summarize(total_points = sum(GW_points, na.rm = T),
            points_per_week = mean(GW_points, na.rm = T),
            time_points = sum(time_points, na.rm = T),
            goal_points = sum(goal_points, na.rm = T),
            assist_points = sum(assist_points, na.rm = T),
            cs_points = sum(cs_points, na.rm = T),
            goals_conceded_points = sum(goals_conceded_points, na.rm = T),
            cards_deductions = sum(cards_deductions, na.rm = T),
            saves_points = sum(saves_points, na.rm = T),
            pen_saves_points = sum(pen_saves_points, na.rm = T),
            neg_points = sum(neg_points, na.rm = T),
            bonus_points = sum(bonus_points, na.rm = T),
            defense_points = sum(defense_points, na.rm = T)) %>%
  ungroup() %>%
  mutate(across(where(is.numeric) & !starts_with('points_'), ~as.integer(.))) %>%
  arrange(-total_points)  %>%
  left_join(current_players %>%
              rename(team = team_name) %>%
              select(name, team), by=c('full_name'='name')) %>%
  select(name, full_name, team, everything())

gkp <- overall_results %>%
  filter(position=='GKP') %>%
  arrange(-total_points)

def <- overall_results %>%
  filter(position=='DEF') %>%
  arrange(-total_points)

mid <- overall_results %>%
  filter(position=='MID') %>%
  arrange(-total_points)

fwd <- overall_results %>%
  filter(position=='FWD') %>%
  arrange(-total_points)

list <- list('overall' = overall_results,
             'weekly'= weekly_results,
             'gkp' = gkp,
             'def' = def,
             'mid' = mid,
             'fwd' = fwd)

write.xlsx(list, 'probability model results.xlsx', overwrite = T)

objects <- ls()
keep <- objects[grep('combined_data|test|fixture|team|current_players|probs|understat|weight', objects)]
rm(list=setdiff(objects, keep))
gc()

