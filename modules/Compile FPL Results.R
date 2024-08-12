#---------------------------------------#
# Data compiler for the FPL Lineup Optimizer
# Written by: ncfisher
# Last updated: May 15 2024
#---------------------------------------#

## Paste timestamp for model beginning
print(paste0('Model run beginning at: ', Sys.time()))

## Source the other scripts
error_occured <- FALSE

print(paste0('Data compilation beginning at: ', Sys.time()))

tryCatch({
  suppressMessages(
    suppressWarnings(
      source('modules/Data compile.R', local = T) 
    )
  )
}, error = function(err){
  error_occured <<- TRUE
  cat('Error in data compilation: ', conditionMessage(err), '\n')
  cat('Traceback: \n')
  traceback()
}, finally = {
  print(paste0('Data compilation complete: ', Sys.time()))
})

print(paste0('Goals model beginning at: ', Sys.time()))

tryCatch({
suppressMessages(
  suppressWarnings(
    source('modules/G_model.R', local = T) 
  )
)
}, error = function(err){
  error_occured <<- TRUE
  cat('Error in goals model: ', conditionMessage(err), '\n')
  cat('Traceback: \n')
  traceback()
}, finally = {
  print(paste0('Goals model complete: ', Sys.time()))
})

print(paste0('Assists model beginning at: ', Sys.time()))

tryCatch({
  suppressMessages(
    suppressWarnings(
      source('modules/A_model.R', local = T) 
    )
  )
}, error = function(err){
  error_occured <<- TRUE
  cat('Error in assists model: ', conditionMessage(err), '\n')
  cat('Traceback: \n')
  traceback()
}, finally = {
  print(paste0('Assists model complete: ', Sys.time()))
})

print(paste0('Cards model beginning at: ', Sys.time()))

tryCatch({
  suppressMessages(
    suppressWarnings(
      source('modules/Cards_model.R', local = T) 
    )
  )
}, error = function(err){
  error_occured <<- TRUE
  cat('Error in cards model: ', conditionMessage(err), '\n')
  cat('Traceback: \n')
  traceback()
}, finally = {
  print(paste0('Cards model complete: ', Sys.time()))
})

print(paste0('Saves model beginning at: ', Sys.time()))

tryCatch({
  suppressMessages(
    suppressWarnings(
      source('modules/Saves_model.R', local = T) 
    )
  )
}, error = function(err){
  error_occured <<- TRUE
  cat('Error in saves model: ', conditionMessage(err), '\n')
  cat('Traceback: \n')
  traceback()
}, finally = {
  print(paste0('Saves model complete: ', Sys.time()))
})

print(paste0('Time module beginning at: ', Sys.time()))

tryCatch({
  suppressMessages(
    suppressWarnings(
      source('modules/Time_model.R', local = T) 
    )
  )
}, error = function(err){
  error_occured <<- TRUE
  cat('Error in time model: ', conditionMessage(err), '\n')
  cat('Traceback: \n')
  traceback()
}, finally = {
  print(paste0('Time module complete: ', Sys.time()))
})

print(paste0('Bonus model beginning at: ', Sys.time()))

tryCatch({
  suppressMessages(
    suppressWarnings(
      source('modules/Bonus_model.R', local = T) 
    )
  )
}, error = function(err){
  error_occured <<- TRUE
  cat('Error in bonus model: ', conditionMessage(err), '\n')
  cat('Traceback: \n')
  traceback()
}, finally = {
  print(paste0('Bonus model complete: ', Sys.time()))
})

print(paste0('Modules complete: ', Sys.time()))

## Compile the metrics to make model choices
objects <- ls()
temp <- mget(objects[grep('comp', objects)])
metrics <- do.call(rbind, temp) %>%
  mutate(`User score`=RMSE/3+R2/3+(1-abs(Validation))/3) %>%
  data.frame()
rownames(metrics) <- NULL
metrics <- metrics %>% 
  arrange(Stat, -User.score) %>%
    rbind(data.frame(
    Stat='User Score = average value of RMSE, R2, and 1-Validation. Highest user score should represent the preferred model',
    Model=NA,
    RMSE=NA,
    R2=NA,
    Validation=NA,
    User.score=NA
  )) %>% rename(`User score`=User.score)

## Models to use:
### Assists = logit
### Bonus points = random forest
### Goals = random forest
### Goals conceded = random forest
### Own goals = logit
### Penalties missed = random forest
### Penalty saves = linear
### Red cards = logit
### Saves = random forest
### Yellow cards = logit

## Compile the results; need to use the lowest denominator for number of observations and calculate expected points

print(paste0('Compiling final results: ', Sys.time()))

tryCatch({
  suppressMessages(
   suppressWarnings(
     results <- goals_results %>%
       left_join(assists_results) %>%
       left_join(cards_results) %>%
       left_join(time_results) %>%
       left_join(save_results) %>%
       left_join(bonus_results) %>%
       mutate_all(funs(ifelse(is.na(.), 0, .))) %>%
       rename(Goals=Predicted_goals_rf,
              Assists=Predicted_assists_logit,
              Played=Probability_played_linear, 
              `Played 60`=Probability_played60_rf,
              `Clean Sheet`=Probability_cs_linear,
              `Goals conceded`=Predicted_goals_conceded_rf,
              `Own goals`=Predicted_og_logit, 
              `Penalties missed`=Predicted_pen_rf,
              `Penalty saves`=Predicted_pen_saves_linear,
              Saves=Predicted_saves_rf,
              `Yellow cards`=Predicted_yellow_cards_logit,
              `Red cards`=Predicted_red_cards_logit,
              Bonus=Predicted_bonus_rf,
              Strength=strength,
              Difficulty=difficulty,
              Value=value) %>%
       mutate(
         `Penalty saves`=ifelse(Position!='GKP', 0, `Penalty saves`),
         Saves=ifelse(Position!='GKP', 0, Saves),
         `Penalties missed`=ifelse(Position=='GKP', 0, `Penalties missed`),
         played_points=(1*Played)+(1*`Played 60`),
         goal_points=ifelse(Position=='GKP' | Position=='DEF', 6*Goals, 0),
         goal_points=ifelse(Position=='MID', 5*Goals, goal_points),
         goal_points=ifelse(Position=='FWD', 4*Goals, goal_points),
         assist_points=3*Assists,
         cs_points=ifelse(Position=='GKP' | Position=='DEF', 4*`Clean Sheet`, 0),
         cs_points=ifelse(Position=='MID', 1*`Clean Sheet`, cs_points),
         gc_points=ifelse(Position=='GKP' | Position=='DEF', (`Goals conceded`/2)*-1, 0),
         og_points=-2*`Own goals`,
         pen_miss_points=-2*`Penalties missed`,
         pen_save_points=ifelse(Position=='GKP', 5*`Penalty saves`, 0),
         save_points=ifelse(Position=='GKP' & Saves >= 3, (Saves/3)*1, 0),
         yc_points=-1*`Yellow cards`,
         rc_points=-3*`Red cards`,
         `Expected points`=played_points+goal_points+assist_points+cs_points+gc_points+og_points+pen_miss_points+pen_save_points+save_points+yc_points+rc_points+Bonus
       ) %>%
       select(Player, Position, Value, Team, Gameweek, Opponent, `Home/Away`, Strength, Difficulty,
              `Expected points`, Goals, Assists, Played, `Played 60`,`Clean Sheet`,
              `Goals conceded`, `Own goals`, `Penalty saves`, `Penalties missed`, `Saves`,
              `Yellow cards`, `Red cards`, Bonus)
   ) 
  )
}, error = function(err){
  error_occured <<- TRUE
  cat('Error in results compile: ', conditionMessage(err), '\n')
  cat('Traceback: \n')
  traceback()
}, finally = {
  print(paste0('Results compile complete: ', Sys.time()))
})

## Use the fplr package/using API to filter if player is injured, suspended, etc
## Suspension/international duty = 0 pts
## 25% chance = 25% expected points
## 50% chance = 50% expected points
## 75% chance = 90% expected points
## Filter out transfers

print(paste0('Processing final results: ', Sys.time()))

tryCatch({

  url <- 'https://fantasy.premierleague.com/api/bootstrap-static/'
  json <- GET(url)
  
  suppressMessages(
    suppressWarnings(
      json <- content(json, 'text')
    )
  )
  
  temp <- fixtures %>%
    # mutate(finished=ifelse(GW==38, 'FALSE', finished)) %>%
    filter(finished=='FALSE')
  
  ls <- fromJSON(json)
  status <- ls$elements %>%
    mutate(position=ifelse(element_type==1, 'GKP', NA),
           position=ifelse(element_type==2, 'DEF', position),
           position=ifelse(element_type==3, 'MID', position),
           position=ifelse(element_type==4, 'FWD', position),
           name=paste0(first_name,' ', second_name)) %>%
    filter(status!='u') %>%
    select(name, status)
  
if(nrow(temp) > 0) {
  
  temp2 <- c(max(temp$GW):38)
  df <- data.frame()
  
  for (i in temp2) {
    
    temp <- status %>% 
      mutate(GW=i)
    df <- rbind(df, temp)
    
  }
  
} else {
  
  df <- status %>%
    mutate(GW=max(fixtures$GW))
  
}

  suppressMessages(
    suppressWarnings(
      results2 <- results %>%
        left_join(val_data,
                  by=c('Player'='name', 'Position'='position', 'Team'='team', 'Gameweek'='GW',
                       'Opponent'='opponent')) %>%
        mutate(
          `Points validation`=`Expected points`-total_points,
          `Goals validation`=Goals-goals,
          `Assists validation`=Assists-assists,
          `Played validation`=ifelse(minutes > 0, 1, 0),
          `Played 60 validation`=ifelse(minutes > 59, 1, 0),
          `Clean sheet validation`=ifelse(minutes > 59 & goals_conceded==0, 1, 0),
          `Goals conceded validation`=`Goals conceded`-goals_conceded,
          `Own goals validation`=`Own goals`-own_goals,
          `Penalty saves validation`=`Penalty saves`-penalties_saved,
          `Penalty miss validation`=`Penalties missed`-penalties_missed,
          `Saves validation`=Saves-saves,
          `Yellow cards validation`=`Yellow cards`-yellow_cards,
          `Red cards validation`=`Red cards`-red_cards,
          `Bonus validation`=Bonus-bonus
        ) %>% select(
          Player, Position, Value, Team, Gameweek, Opponent, `Home/Away`, Strength, Difficulty,
          `Expected points`, Goals, Assists, Played, `Played 60`, `Clean Sheet`,
          `Goals conceded`, `Own goals`, `Penalty saves`, `Penalties missed`, Saves, `Yellow cards`,
          `Red cards`, Bonus, xG, xA, contains('validation') 
        )
    )
  )

  suppressMessages(
    suppressWarnings(
      temp <- ls$elements %>%
        mutate(position=ifelse(element_type==1, 'GKP', NA),
               position=ifelse(element_type==2, 'DEF', position),
               position=ifelse(element_type==3, 'MID', position),
               position=ifelse(element_type==4, 'FWD', position),
               name=paste0(first_name,' ', second_name)) %>%
        filter(status!='u') %>%
        select(name, web_name)
    )
  )
  
suppressMessages(
  suppressWarnings(
      
      results3 <- results2 %>%
        left_join(df, by=c('Player'='name', 'Gameweek'='GW')) %>%
        mutate(status=ifelse(Gameweek < min(df$GW), 'a', status),
               status=ifelse(Gameweek > max(df$GW), 'a', status),
               `Expected points`=ifelse(status=='s' | status=='i', 0, `Expected points`),
               `Expected points`=ifelse(status=='d', `Expected points`*0.65, `Expected points`)) %>%
        left_join(temp, by=c('Player'='name')) %>%
        rename(`Full name`=Player, Player=web_name) %>%
        select(Player, `Full name`, everything())
        
      
    )
  )

suppressMessages(
  suppressWarnings(
    results4 <- results3 %>%
      group_by(Player, `Full name`, Position, Value, Team) %>%
      summarize(
        `Expected points`=sum(`Expected points`, na.rm = T),
        Goals=sum(Goals, na.rm = T),
        Assists=sum(Assists, na.rm = T),
        Played=sum(Played, na.rm = T),
        `Played 60`=sum(`Played 60`, na.rm = T),
        `Clean Sheet`=sum(`Clean Sheet`, na.rm = T),
        `Goals conceded`=sum(`Goals conceded`, na.rm = T),
        `Own goals`=sum(`Own goals`, na.rm = T),
        `Penalties missed`=sum(`Penalties missed`, na.rm = T),
        `Penalty saves`=sum(`Penalty saves`, na.rm = T),
        Saves=sum(Saves, na.rm = T),
        `Yellow cards`=sum(`Yellow cards`, na.rm = T),
        `Red cards`=sum(`Red cards`, na.rm = T),
        Bonus=sum(Bonus, na.rm = T)
      ) %>%
      ungroup() %>%
      mutate(`Points/value`=ifelse(Value==0, 0, `Expected points`/Value)) %>%
      group_by(Position) %>%
      mutate(`Points/value rank`= rank(-`Points/value`)) %>%
      select(Player, `Full name`, Position, Value, Team, `Points/value`, `Points/value rank`, everything()) %>%
      ungroup()
  )
)

suppressMessages(
  suppressWarnings(
    results5 <- results3 %>%
      left_join(
        val_data %>% select(
          name, position, GW, total_points
        ),
          by=c('Full name'='name', 'Position'='position', 'Gameweek'='GW')
      ) %>% select(
        Player, `Full name`, Gameweek, `Expected points`, total_points
      ) %>% rename(
        `Actual points`=total_points
      ) %>% mutate(
        Difference=`Actual points`-`Expected points`
      )
  )
)
  
}, error = function(err){
  error_occured <<- TRUE
  cat('Error in final results: ', conditionMessage(err), '\n')
  cat('Traceback: \n')
  traceback()
}, finally = {
  print(paste0('Final results complete: ', Sys.time()))
})

## Save model results

print(paste0('Writing final results: ', Sys.time()))

tryCatch({
  
  suppressMessages(
    suppressWarnings(
      wb <-createWorkbook()
    )
  )
  
  suppressMessages(
    suppressWarnings(
      addWorksheet(wb, 'Model Results')
    )
  )
  
  suppressMessages(
    suppressWarnings(
      writeData(wb, sheet = 1, x = results3)
    )
  )
  
  suppressMessages(
    suppressWarnings(
      addWorksheet(wb, 'Season Results')
    )
  )
  
  suppressMessages(
    suppressWarnings(
      writeData(wb, sheet = 2, x = results4)
    )
  )
  
  suppressMessages(
    suppressWarnings(
      addWorksheet(wb, 'Model Metrics')
    )
  )
  
  suppressMessages(
    suppressWarnings(
      writeData(wb, sheet = 3, x = metrics)
    )
  )
  
  suppressMessages(
    suppressWarnings(
      addWorksheet(wb, 'Validation data')
    )
  )
  
  suppressMessages(
    suppressWarnings(
      writeData(wb, sheet = 4, x = results5)
    )
  )
  
  
  suppressMessages(
    suppressWarnings(
      saveWorkbook(wb, 'FPL Model Results.xlsx', overwrite = T)
    )
  )
  
}, error = function(err){
  error_occured <<- TRUE
  cat('Error in writing final results: ', conditionMessage(err), '\n')
  cat('Traceback: \n')
  traceback()
}, finally = {
  print(paste0('Final results written: ', Sys.time()))
})

## Print model timestamp
print(paste0('Model run completed at: ', Sys.time()))
rm(list=ls())
gc()
