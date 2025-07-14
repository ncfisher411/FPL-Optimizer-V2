#---------------------------------------#
# Bonus estimation for FPL Probability Model
# Written by: ncfisher
# Last updated: July 11 2025
#---------------------------------------#

# Estimate the probability of getting bonus points by player

## 1) estimate of scoring bonus points by team
probs_bonus <- combined_data %>%
  mutate(b0 = ifelse(bonus==0, 1, 0),
         b1 = ifelse(bonus==1, 1, 0),
         b2 = ifelse(bonus==2, 1, 0),
         b3 = ifelse(bonus==3, 1, 0)) %>%
  group_by(name) %>%
  summarize(b0 = sum(b0, na.rm = T),
            b1 = sum(b1, na.rm = T),
            b2 = sum(b2, na.rm = T),
            b3 = sum(b3, na.rm = T)) %>%
  ungroup() %>%
  mutate(total = b0 + b1 + b2 + b3,
         across(contains('b'), ~./total))

objects <- ls()
keep <- objects[grep('combined_data|test|fixture|team|current_players|probs|understat|weight', objects)]
rm(list=setdiff(objects, keep))
gc()