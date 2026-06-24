#---------------------------------------#
# Bonus estimation for FPL Probability Model
# Written by: ncfisher
# Last updated: June 23 2026
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
         across(contains('b'), ~./total),
         across(contains('b'), ~ifelse(is.nan(.), 0, .)))

### Adding a step that will work for promoted teams without previous stats - really just Sunderland - if needed
status <- grepl('TRUE', unique(fixtures$finished))

if(status=='FALSE'){
  
  teams <- c('Burnley', 'Leeds', 'Ipswich', 'Leicester', 'Luton', 'Sheffield Utd', 'Southampton',
             'Hull City', 'Coventry City')
  
  ### Get the team data
  temp <- combined_data %>% filter(team %in% teams) %>%
    mutate(b0 = ifelse(bonus==0, 1, 0),
           b1 = ifelse(bonus==1, 1, 0),
           b2 = ifelse(bonus==2, 1, 0),
           b3 = ifelse(bonus==3, 1, 0),
           position = ifelse(position=='GK', 'GKP', position)) %>%
    group_by(position) %>%
    summarize(b0 = sum(b0, na.rm = T),
              b1 = sum(b1, na.rm = T),
              b2 = sum(b2, na.rm = T),
              b3 = sum(b3, na.rm = T)) %>%
    ungroup() %>%
    mutate(total = b0 + b1 + b2 + b3,
           across(contains('b'), ~./total)) %>%
    mutate(team = 'Sunderland',
           total = 0)
  
  temp <- combined_data %>% filter(team=='Sunderland') %>%
    distinct(name, position) %>%
    left_join(temp) %>%
    select(names(probs_bonus))
  
  probs_bonus <- probs_bonus %>% filter(!(name %in% temp$name)) %>%
    rbind(temp)

}

objects <- ls()
keep <- objects[grep('combined_data|test|fixture|team|current_players|probs|understat|weight', objects)]
rm(list=setdiff(objects, keep))
gc()