#---------------------------------------#
# Parameters for FPL Probability Model
# Written by: ncfisher
# Last updated: July 11 2025
#---------------------------------------#

# Use this script for adjusting weights used in the probability calculations

## Goal probability weights
player_goal_weight = 0.35
opponent_goal_weight = 0.35
team_goal_weight = 0.1
ha_goal_weight = 0.15
position_goal_weight = 0.05

## Assist probability weights
player_assist_weight = 0.35
opponent_assist_weight = 0.35
team_assist_weight = 0.1
ha_assist_weight = 0.15
position_assist_weight = 0.05

## Goalkeeper probability weights
player_xGA_gk_weight = 0.8
opponent_xGA_gk_weight = 0.1
team_xGA_gk_weight = 0.1
player_ga_gk_weight = 0.6
opponent_ga_gk_weight = 0.2
team_ga_gk_weight = 0.2

## Clean sheets weights
cs_team_weight = 0.7
cs_team_weight_2 = 0.5
cs_ha_weight = 0.7
cs_ha_weight_2 = 0.5

## Card probability weights
player_card_weight = 0.275
opponent_card_weight = 0.1
team_card_weight = 0.275
ha_card_weight = 0.1
position_card_weight = 0.25

## Time weights
time_weight_1 = 0.1
time_weight_2 = 0.05
time_weight_3 = 0.05