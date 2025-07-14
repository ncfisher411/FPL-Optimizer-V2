#---------------------------------------#
# Results compile script for FPL Probability Model
# Written by: ncfisher
# Last updated: July 10 2025
#---------------------------------------#

# 1) Get the points from the time module
time_results <- fixtures %>%
  left_join(current_players %>% mutate(season=max(fixtures$season)), 
            by=c('team' = 'team_name', 'season')) %>%
  left_join(probs_time %>% 
              filter(season==max(season)) %>%
              filter(name %in% current_players$name) %>%
              select(name, contains('Prob'))) %>%
  mutate(time_points = (Prob_played * 1) + (Prob_played60 * 1))

# 2) Get the points from the goals module
df <- goal_probs %>%
  filter(season==max(season)) %>%
  select(name, position, team, opponent_name, was_home, xG_calculated) %>%
  filter(name %in% current_players$name) %>%
  rename(opponent = opponent_name) %>%
  mutate(position=ifelse(position=='GK', 'GKP', position))

goal_results <- fixtures %>%
  mutate(was_home = ifelse(h_a=='h', 'True', 'False')) %>%
  left_join(current_players %>% mutate(season=max(fixtures$season)), 
            by=c('team' = 'team_name', 'season')) %>%
  left_join(df) %>%
  mutate(goal_points = ifelse(position=='GKP', 10 * xG_calculated, 0),
         goal_points = ifelse(position=='DEF', 6 * xG_calculated, goal_points),
         goal_points = ifelse(position=='MID', 5 * xG_calculated, goal_points),
         goal_points = ifelse(position=='FWD', 4 * xG_calculated, goal_points))

# 3) Get the points from assists module
df <- assist_probs %>%
  filter(season==max(season)) %>%
  select(name, position, team, opponent_name, was_home, xA_calculated) %>%
  filter(name %in% current_players$name) %>%
  rename(opponent = opponent_name) %>%
  mutate(position=ifelse(position=='GK', 'GKP', position))

assist_results <- fixtures %>%
  mutate(was_home = ifelse(h_a=='h', 'True', 'False')) %>%
  left_join(current_players %>% mutate(season=max(fixtures$season)), 
            by=c('team' = 'team_name', 'season')) %>%
  left_join(df) %>%
  mutate(assist_points = ifelse(position=='GKP', 10 * xA_calculated, 0))

# 4) Get the points from clean sheets
cs_results <- fixtures %>%
  mutate(was_home = ifelse(h_a=='h', 'True', 'False')) %>%
  left_join(current_players %>% mutate(season=max(fixtures$season)), 
            by=c('team' = 'team_name', 'season')) %>%
  left_join(cs_probs %>% rename(opponent = opponent_name)) %>%
  left_join(probs_time) %>%
  mutate(cs_points =  ifelse(position=='GKP' | position=='DEF', 4 * ((cs_team_weight_2*goals_conceded_0_team) + (cs_ha_weight_2*goals_conceded_0)), 0),
         cs_points =  ifelse(position=='MID', 1 * ((cs_team_weight_2*goals_conceded_0_team) + (cs_ha_weight_2*goals_conceded_0)), cs_points),
         goals_conceded_points =  ifelse(position=='GKP' | position=='DEF',
           (-1 * ((cs_team_weight_2*goals_conceded_2_team) + cs_ha_weight_2*goals_conceded_2)) +
           (-1 * ((cs_team_weight_2*goals_conceded_4_team) + cs_ha_weight_2*goals_conceded_4)) +
           (-1 * ((cs_team_weight_2*goals_conceded_6_team) + cs_ha_weight_2*goals_conceded_6)) +
           (-1 * ((cs_team_weight_2*goals_conceded_8_team) + cs_ha_weight_2*goals_conceded_8)), 0),
         cs_points = cs_points * Prob_played60)

# 5) Get the points lost from cards
cards_results <- fixtures %>%
  mutate(was_home = ifelse(h_a=='h', 'True', 'False')) %>%
  left_join(current_players %>% mutate(season=max(fixtures$season)), 
            by=c('team' = 'team_name', 'season')) %>%
  left_join(yellow_cards_probs %>% 
              rename(opponent = opponent_name) %>%
              select(name, season, team, opponent, was_home, contains('cards'))) %>%
  left_join(red_cards_probs %>% 
              rename(opponent = opponent_name) %>%
              select(name, season, team, opponent, was_home, contains('cards'))) %>%
  left_join(probs_time) %>%
  mutate(cards_deductions = (yellow_cards_calculated * -1) + (red_cards_calculated * -3))

# 6) Get the points attributed to goalkeepers
gk_results <- fixtures %>%
  mutate(was_home = ifelse(h_a=='h', 'True', 'False')) %>%
  left_join(current_players %>% mutate(season=max(fixtures$season)), 
            by=c('team' = 'team_name', 'season')) %>%
  left_join(saves_probs %>% 
              rename(opponent = opponent_name) %>%
              mutate(position = 'GKP')) %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.), 0, .)),
         saves_points = (saves/3 * 1),
         pen_saves_points = pen_save_prob * 5)

# 7) Get the negative scoring events
neg_results <- fixtures %>%
  mutate(was_home = ifelse(h_a=='h', 'True', 'False')) %>%
  left_join(current_players %>% mutate(season=max(fixtures$season)), 
            by=c('team' = 'team_name', 'season')) %>%
  left_join(probs_neg %>% mutate(position=ifelse(position=='GK', 'GKP', position))) %>%
  mutate(neg_points = (-2 * prob_pen_miss * Prob_played) + (-2 * prob_own_goal * Prob_played))

# 8) Get the bonus points
bonus_results <- fixtures %>%
  mutate(was_home = ifelse(h_a=='h', 'True', 'False')) %>%
  left_join(current_players %>% mutate(season=max(fixtures$season)), 
            by=c('team' = 'team_name', 'season')) %>%
  left_join(probs_bonus) %>%
  mutate(bonus_points = (b1*1) + (b2*2) + (b3*3))

# Compile the final results
weekly_results <- time_results %>%
  select(name, position, team, season, GW, opponent, h_a, time_points) %>%
  left_join(goal_results %>%
              select(name, position, team, season, GW, opponent, h_a, goal_points)) %>%
  left_join(assist_results %>%
              select(name, position, team, season, GW, opponent, h_a, assist_points)) %>%
  left_join(cs_results %>%
              select(name, position, team, season, GW, opponent, h_a, cs_points, goals_conceded_points)) %>%
  left_join(cards_results %>%
              select(name, position, team, season, GW, opponent, h_a, cards_deductions)) %>%
  left_join(gk_results %>%
              select(name, position, team, season, GW, opponent, h_a, saves_points, pen_saves_points)) %>%
  left_join(neg_results %>%
              select(name, position, team, season, GW, opponent, h_a, neg_points)) %>%
  left_join(bonus_results %>%
              select(name, position, team, season, GW, opponent, h_a, bonus_points)) %>%
  mutate(GW_points = time_points + goal_points + assist_points + cs_points + goals_conceded_points +
           cards_deductions + saves_points + pen_saves_points + neg_points + bonus_points) %>%
  # left_join(current_players %>% select(name, chance_of_playing_this_round)) %>%
  # mutate(GW_points = GW_points * (chance_of_playing_this_round/100)) %>%
  select(name, position, team, season, GW, opponent, h_a, GW_points,
         # chance_of_playing_this_round,
         everything())

overall_results <- weekly_results %>%
  group_by(name, position, season) %>%
  summarize(total_points = sum(GW_points, na.rm = T),
            time_points = sum(time_points, na.rm = T),
            goal_points = sum(goal_points, na.rm = T),
            assist_points = sum(assist_points, na.rm = T),
            cs_points = sum(cs_points, na.rm = T),
            goals_conceded_points = sum(goals_conceded_points, na.rm = T),
            cards_deductions = sum(cards_deductions, na.rm = T),
            saves_points = sum(saves_points, na.rm = T),
            pen_saves_points = sum(pen_saves_points, na.rm = T),
            neg_points = sum(neg_points, na.rm = T),
            bonus_points = sum(bonus_points, na.rm = T)) %>%
  ungroup() %>%
  mutate(across(where(is.numeric), ~as.integer(.))) %>%
  arrange(position, -total_points)

gkp <- overall_results %>%
  filter(position=='GKP')

def <- overall_results %>%
  filter(position=='DEF')

mid <- overall_results %>%
  filter(position=='MID')

fwd <- overall_results %>%
  filter(position=='FWD')

list <- list(overall_results, weekly_results, gkp, def, mid, fwd)

write.xlsx(list, 'Probability model/probability model results.xlsx', overwrite = T)
