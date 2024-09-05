#---------------------------------------#
# FPL Lineup Optimizer and Shiny dashboard
# Written by: ncfisher
# Last updated: May 31 2024
#---------------------------------------#

packages <- c("shiny", "readr", "dplyr", "ggsoccer", "tidyverse", "rstudioapi", 'openxlsx', 'shinythemes', 'plotly', 'DT', 'bslib')

for (package in packages) {
  if (!requireNamespace(package, quietly = T)) {
    install.packages(package, dependencies = T)
  }
  library(package, character.only = T)
  cat(paste(package, "package loaded.\n"))
}

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

ui <- navbarPage(title='FPL Model and Lineup Optimizer',
                 theme = shinytheme('sandstone'),
  tabPanel(
           title = 'Menu',
           h1("FPL Lineup Viewer and Optimizer V2"),
           uiOutput("picture"),
           br(),
           
           h4("This tool uses output from the FPL Prediction Model, which models predicted FPL points
           by gameweek, to organize, filter, and view data. Model predictions
           are based on past and current season data, and updates with the FPL API.
           Estimation datasets include gameweek data from the 2021/22 through 2023/24
           seasons, and current season data. The model must be run before being able
           to use this tool as the tool will search for the Excel file output. Do not
              change the name of the Excel file output."),
           
           h4("Panel 1 is the lineup optimizer. This allows you to choose your gameweek,
           enter the players on your roster, and the tool will present you with
              your highest scoring lineup based on predictions from the model."),
           
           h4("Panel 2 is the player comparison tool. This tool allows FPL managers
           to select and compare predicted player performance by gameweek. Managers 
              can select a max of 5 gameweeks and 5 players for comparison."),
           
           h4("Panel 3 is the performance time series. This tool allows FPL managers
           to select up to 3 players and compare a single predicted statistic over the course of the season
           among these players in a time series format."),
           
           h4("Panel 4 is the performance validation tool. This tool shows a scatter
           plot of predicted versus actual values for the selected player or average
              by gameweek."),
           
           h4("Read the user guide for further instruction on installing the tool, using
           the tool, the logic behind the modeiling, and how to monitor the tool
              for updates.")
  ),
  
  tabPanel(
    'Lineup Card',
    h1('FPL Lineup Card'),
    h3('Input your players and desired gameweek to view your optimal lineup card using model results'),
    
    sidebarLayout(
      sidebarPanel(
        uiOutput("gw"),
        uiOutput("gk"),
        uiOutput("def"),
        uiOutput("mid"),
        uiOutput("fwd"),
        actionButton('GenerateLineup', 'Generate Lineup Card')
      ),
      
      mainPanel(fluidRow(
        column(
          width = 12,
          h3("Optimal Lineup Card"),
          uiOutput("Total_score"),
          uiOutput("subs")
        )
      ),
      
      fluidRow(
        column(
          width = 12,
          plotOutput("plots", width = "100%", height = "500px")
        )
      )
      
      )
  )
  
    ),
  
  tabPanel(
    'Comparison Tool',
    h1('Player Comparison Tool'),
    h3('Select up to 5 players and 5 statistics to compare'),
    
    sidebarLayout(
      sidebarPanel(
        uiOutput('gw2'),
        uiOutput('PlayerComp'),
        uiOutput('stats'),
        actionButton('GenerateComparison', 'Generate Player Comparison')
      ),
      
      mainPanel(
        fluidRow(
          column(width=12,
          plotlyOutput('plotComparisonTool', height = '500px'))
        )
      )
      
      
    )
  ),
  
  tabPanel(
    'Time Series Outlook',
    h1('Time Series Tool'),
    h4('Select up to three players and one statistic to compare their predicted trends for the season'),
    
    sidebarLayout(
      sidebarPanel(
        uiOutput('PlayersTimeSeries'),
        uiOutput('statTimeSeries'),
        actionButton('GenerateTimeSeries', 'Generate Time Series')
      ),
      
      mainPanel(
        fluidRow(
          column(
            width = 12,
            plotlyOutput('plotTimeSeries', height='300px'),
            dataTableOutput('tableTimeSeries')
          )
          )
          )
        )
      ),
  
  tabPanel(
    'Performance Validation Tool',
    h1('Player performance validation'),
    h4('Select a player to assess their actual versus predicted performance through out the season'),
    sidebarLayout(
      sidebarPanel(
        uiOutput('PlayerValidation'),
        actionButton('ValidationButton', 'Generate Player Validation')
      ),
      mainPanel(plotlyOutput('ValidationPlot', height = '500px'))
    )
  )
      )
  


server <- function(input, output, session) {
  
  upload <- reactive({
    
    output$picture <- renderUI({
      img_tag <- tags$img(src = "https://images.unsplash.com/photo-1555862124-94036092ab14?ixlib=rb-4.0.3&ixid=M3wxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8fA%3D%3D&auto=format&fit=crop&w=2071&q=80",
                          width = "750px", height = "375px")
      div(img_tag, style = "text_align: left;")
    })
    
    data <- read.xlsx('FPL Model Results.xlsx', sheet = 'Model Results') %>%
      select(-contains('validation'))
    colnames(data) <- gsub('\\.', ' ', colnames(data))
    return(data)
  })
  
  upload2 <- reactive({
    data <- read.xlsx('FPL Model Results.xlsx', sheet = 'Validation data')
    colnames(data) <- gsub('\\.', ' ', colnames(data))
    return(data)
  })
  
  
  output$gw <- renderUI({
    selectizeInput("gameweek",
                label = "Select Gameweek",
                choices=unique(upload()$Gameweek),
                multiple = F,
                options = list(
                  maxItems = 1
                ))
  })
  
  gk <- reactive({
    data <- subset(upload(), Gameweek %in% input$gameweek) %>%
      filter(Position=="GKP")
  })
  
  def <- reactive({
    data <- subset(upload(), Gameweek %in% input$gameweek) %>%
      filter(Position=="DEF")
  })
  
  mid <- reactive({
    data <- subset(upload(), Gameweek %in% input$gameweek) %>%
      filter(Position=="MID")
  })
  
  fwd <- reactive({
    data <- subset(upload(), Gameweek %in% input$gameweek) %>%
      filter(Position=="FWD")
  })
  
  output$gk <- renderUI({
    selectizeInput("keeper", "Select Goalkeepers",
                choices = unique(gk()$`Full name`),
                multiple = T,
                options = list(
                  maxItems = 2
                ))
  })
  
  output$def <- renderUI({
    selectizeInput("defender", "Select Defenders",
                choices = unique(def()$`Full name`),
                multiple = T,
                options = list(
                  maxItems = 5
                ))
  })
  
  output$mid <- renderUI({
    selectizeInput("midfield", "Select Midfielders",
                choices = unique(mid()$`Full name`),
                multiple = T,
                options = list(
                  maxItems = 5
                ))
  })
  
  output$fwd <- renderUI({
    selectizeInput("forward", "Select Forwards",
                choices = unique(fwd()$`Full name`),
                multiple = T,
                options = list(
                  maxItems = 3
                ))
  })
  
  selected <- reactive({
    req(gk(), input$keeper)
    gk_sub <- subset(gk(), `Full name` %in% input$keeper)
    
    req(def(), input$defender)
    def_sub <- subset(def(), `Full name` %in% input$defender)
    
    req(mid(), input$midfield)
    mid_sub <- subset(mid(), `Full name` %in% input$midfield)
    
    req(fwd(), input$forward)
    fwd_sub <- subset(fwd(), `Full name` %in% input$forward)
    
    data <- rbind(gk_sub, def_sub, mid_sub, fwd_sub)
    return(data)
  })
  
  observeEvent(input$GenerateLineup, {
    req(selected())
    
    data <- selected()
    
    data_split <- data %>% split(.$Position)
    one <- data_split$GKP$`Full name`
    two <- data_split$DEF$`Full name`
    three <- data_split$MID$`Full name`
    four <- data_split$FWD$`Full name`
    
    gkp_comb <- combn(one, 1) %>% t %>%
      data.frame() %>%
      as.tibble()
    
    def_comb <- data.frame()
    for(i in 3:min(5, length(two))){
      i=as.numeric(i)
      temp <- combn(two, i) %>% t %>% data.frame()
      colnames(temp) <- rep("n", ncol(temp))
      def_comb <- bind_rows(def_comb, temp)
    }
    def_comb <- def_comb %>%
      as.tibble()
    
    mid_comb <- data.frame()
    for(i in 2:min(5, length(three))){
      temp <- combn(three, i) %>% t %>% data.frame()
      colnames(temp) <- rep("n", ncol(temp))
      mid_comb <- bind_rows(mid_comb, temp)
    }
    mid_comb <- mid_comb %>%
      as.tibble()
    
    fwd_comb <- data.frame()
    for(i in 1:min(3, length(four))){
      temp <- combn(four, i) %>% t %>% data.frame()
      colnames(temp) <- rep("n", ncol(temp))
      fwd_comb <- bind_rows(fwd_comb, temp)
    }
    fwd_comb <- fwd_comb %>%
      as.tibble()
    
    combo <- crossing(gkp_comb, def_comb, mid_comb, fwd_comb, .name_repair = "unique") %>%
      mutate(sim_num=row_number()) %>%
      pivot_longer(-sim_num) %>%
      left_join(data, by=c("value"="Full name"), copy = TRUE) %>%
      filter(!is.na(value)) %>%
      ungroup() %>%
      group_by(sim_num) %>%
      mutate(total_points=sum(`Expected points`)) %>%
      mutate(n=n_distinct(value)) %>%
      ungroup() %>%
      filter(n == 11) %>%
      filter(total_points==max(total_points)) %>%
      select(value, Position, Team, Opponent, `Expected points`, total_points, Player) %>%
      rename(`Full name`=1) %>%
      data.frame()
    
    split <- split(combo, rep(1:nrow(combo), each=11)) %>%
      keep(~ nrow(.) > 0)
    
    optimal_team <- split[[1]]
    colnames(optimal_team) <- gsub('\\.', ' ', colnames(optimal_team))
    
    optimal_team <- optimal_team %>%
      mutate(`Expected points`=round(`Expected points`),
             total_points=round(total_points)) %>%
      group_by(Position) %>%
      mutate(n=n_distinct(`Full name`)) %>%
      mutate(n2=as.numeric(factor(`Full name`))) %>%
      ungroup() %>%
      mutate(x=ifelse(Position=="GKP", 97, 0)) %>% ### Goalkeepers: 1
      mutate(y=ifelse(Position=="GKP", 50, 0)) %>%
      mutate(x=ifelse(Position=="DEF", 87, x)) %>% ### Defenders: 3, 4, 5
      mutate(y=ifelse(Position=="DEF" & n==4 & n2==1, 10, y)) %>% ##### 4 defenders
      mutate(y=ifelse(Position=="DEF" & n==4 & n2==2, 35, y)) %>%
      mutate(y=ifelse(Position=="DEF" & n==4 & n2==3, 65, y)) %>%
      mutate(y=ifelse(Position=="DEF" & n==4 & n2==4, 90, y)) %>%
      mutate(y=ifelse(Position=="DEF" & n==5 & n2==1, 5, y)) %>% ##### 5 defenders
      mutate(y=ifelse(Position=="DEF" & n==5 & n2==2, 25, y)) %>%
      mutate(y=ifelse(Position=="DEF" & n==5 & n2==3, 50, y)) %>%
      mutate(y=ifelse(Position=="DEF" & n==5 & n2==4, 75, y)) %>%
      mutate(y=ifelse(Position=="DEF" & n==5 & n2==5, 95, y)) %>%
      mutate(y=ifelse(Position=="DEF" & n==3 & n2==1, 30, y)) %>% #### 3 defenders
      mutate(y=ifelse(Position=="DEF" & n==3 & n2==2, 50, y)) %>%
      mutate(y=ifelse(Position=="DEF" & n==3 & n2==3, 70, y)) %>%
      mutate(x=ifelse(Position=="MID", 70, x)) %>% ### Midfielders: 2, 3, 4, 5
      mutate(y=ifelse(Position=="MID" & n==4 & n2==1, 10, y)) %>% #### 4 mids
      mutate(y=ifelse(Position=="MID" & n==4 & n2==2, 35, y)) %>%
      mutate(y=ifelse(Position=="MID" & n==4 & n2==3, 65, y)) %>%
      mutate(y=ifelse(Position=="MID" & n==4 & n2==4, 90, y)) %>%
      mutate(y=ifelse(Position=="MID" & n==5 & n2==1, 5, y)) %>% #### 5 mids
      mutate(y=ifelse(Position=="MID" & n==5 & n2==2, 30, y)) %>%
      mutate(y=ifelse(Position=="MID" & n==5 & n2==3, 50, y)) %>%
      mutate(y=ifelse(Position=="MID" & n==5 & n2==4, 70, y)) %>%
      mutate(y=ifelse(Position=="MID" & n==5 & n2==5, 95, y)) %>%
      mutate(y=ifelse(Position=="MID" & n==3 & n2==1, 25, y)) %>% #### 3 mids
      mutate(y=ifelse(Position=="MID" & n==3 & n2==2, 50, y)) %>%
      mutate(y=ifelse(Position=="MID" & n==3 & n2==3, 75, y)) %>%
      mutate(y=ifelse(Position=="MID" & n==2 & n2==1, 35, y)) %>% #### 2 mids
      mutate(y=ifelse(Position=="MID" & n==2 & n2==2, 70, y)) %>%
      mutate(x=ifelse(Position=="FWD", 55, x)) %>% ### Forwards: 1, 2, 3
      mutate(y=ifelse(Position=="FWD" & n==2 & n2==1, 35, y)) %>% #### 2 fwds
      mutate(y=ifelse(Position=="FWD" & n==2 & n2==2, 65, y)) %>%
      mutate(y=ifelse(Position=="FWD" & n==3 & n2==1, 25, y)) %>% #### 3 fwds
      mutate(y=ifelse(Position=="FWD" & n==3 & n2==2, 50, y)) %>%
      mutate(y=ifelse(Position=="FWD" & n==3 & n2==3, 75, y)) %>%
      mutate(y=ifelse(Position=="FWD" & n==1 & n2==1, 50, y)) %>% #### 1 fwd
      mutate(x2=x-3) %>%
      mutate(x3=x-6)
    
    ##### Autosubs order
    subs <- data %>% filter(!`Full name` %in% optimal_team$`Full name`) %>%
      arrange(-`Expected points`) %>%
      mutate(`Expected points`=round(`Expected points`))
    
    output$plots <- renderPlot({

      ggplot(optimal_team, aes(y=y)) +
        annotate_pitch(colour = "white",
                       fill = 'darkgreen',
                       limits = F) +
        geom_point(aes(x=x)) +
        geom_point(aes(x=x2)) +
        geom_point(aes(x=x3)) +
        geom_label(aes(x=x, label=Player), color="white", fill="black", size=7) +
        geom_label(aes(x=x2, label=`Expected points`), color="white", fill="black", size=7) +
        geom_label(aes(x=x3, label=Opponent), color="white", fill="black", size=7) +
        theme_pitch() +
        theme(panel.background = element_rect(fill = "darkgreen")) +
        coord_flip(xlim = c(49, 101)) +
        scale_y_reverse() 
      
    })
    
    output$Total_score <- renderUI({
      paste0("Team Score: ", max(optimal_team$total_points))
    })
    
    output$subs <- renderUI({
      paste0("Subs: ", subs$Player[[1]], " (", subs$`Expected points`[[1]], "), ",
             subs$Player[[2]], " (", subs$`Expected points`[[2]], "), ",
             subs$Player[[3]], " (", subs$`Expected points`[[3]], "), ",
             subs$Player[[4]], " (", subs$Predicted_points[[4]], "), ")
    })
  })
    

  # Panel 3: gameweek player comparison tool
  
  names <- c('Expected points', 'Goals', 'Assists', 'Probability Played', 'Probability Played 60', 'Probability Clean Sheet',
      'Goals conceded', 'Own goals', 'Penalty saves', 'Penalties missed', 'Saves', 'Yellow cards', 'Red cards',
      'Bonus')
  
  output$gw2 <- renderUI({
    selectizeInput("gameweek2",
                   label = "Select Gameweek",
                   choices=unique(upload()$Gameweek),
                   multiple = F,
                   options = list(
                     maxItems = 1
                   ))
  })
  
  output$PlayerComp <- renderUI({
    selectizeInput(
      'playercomp',
      label = 'Select players',
      choices=unique(upload()$`Full name`),
      multiple=T,
      options = list(maxItems = 5)
    )
  })
  
  output$stats <- renderUI({
    selectizeInput(
      'statistic',
      label = 'Select statistics',
      choices=names,
      multiple=T,
      options = list(maxItems = 5)
    )
  })
  
  selected2 <- reactive({
    req(input$playercomp)
    req(input$gameweek2)
    req(input$statistic)
    
    data <- upload() %>%
      filter(`Full name` %in% input$playercomp) %>%
      filter(Gameweek %in% input$gameweek2) %>%
      mutate(Value=as.numeric(Value)) %>%
      rename(`FPL Value`=Value) %>%
      pivot_longer(cols = c(-Player, -Gameweek, -`Full name`, -Opponent, -`Home/Away`,
                            -Strength, -Difficulty, -Position, -Team, -status),
                   names_to = 'stat', values_to = 'value') %>%
      mutate(value=ifelse(
        stat=='Expected points' | stat=='Goals' | stat=='Assists' | stat=='Goals conceded' | stat=='Own goals' | stat=='Penalty saves' | stat=='Penalties missed' | stat=='Saves' | stat=='Bonus' | stat=='Yellow cards' | stat=='Red cards',
        round(value, 2), value
      ),
      value=ifelse(
        stat=='Played' | stat=='Played 60' | stat=='Clean Sheet',
        round(value, 2), value
      ),
      stat=ifelse(
        stat=='Played' | stat=='Played 60' | stat=='Clean Sheet',
        paste0('Probability ', stat), stat
      )) %>% filter(stat %in% input$statistic)
    
    return(data)
  })
  

  
  observeEvent(input$GenerateComparison, {
    req(selected2())

      data <- selected2()
      
      output$plotComparisonTool <- renderPlotly({
        plot_ly(data, x= ~stat, y= ~value, color = ~Player, type='bar') %>%
          layout(xaxis = list(title = 'Selected Statistic(s)'),
                 yaxis = list(title = ''),
                 margin = list(b=100))
      })
  })
  
  # Panel 4: Time series
  
  output$PlayersTimeSeries <- renderUI({
    selectizeInput(
      'playertime',
      label = 'Select players',
      choices=unique(upload()$`Full name`),
      multiple=T,
      options = list(maxItems = 3)
    )
  })
  
  output$statTimeSeries <- renderUI({
    selectizeInput(
      'statisticTimeSeries',
      label = 'Select statistic',
      choices=names,
      multiple=F,
      options = list(maxItems = 1)
    )
  })
  
  selected3 <- reactive({
    req(input$playertime)
    
    data <- upload() %>%
      filter(`Full name` %in% input$playertime) %>%
      mutate(`Opponent (Difficulty)`= paste0(Opponent, ' (', as.character(Difficulty),')')) %>%
      rename(`Probability Played`=Played, `Probability Played 60`=`Played 60`, `Probability Clean Sheet` = `Clean Sheet`) %>%
      mutate_if(is.numeric, funs(round(., 2)))
    
  })
  
  observeEvent(input$GenerateTimeSeries, {
    
    data <- selected3()
    
    stat <- input$statisticTimeSeries
    
    output$plotTimeSeries <- renderPlotly({
      plot_ly(data, x= ~Gameweek, y= data[, input$statisticTimeSeries], color = ~Player,
              type = 'scatter', mode = 'lines') %>%
        layout(
          xaxis = list(title='Gameweek', dtick = 1),
          yaxis = list(title=stat)
        )
    })
    
    output$tableTimeSeries <- renderDataTable({
      
      data %>% select(Player, Gameweek, `Opponent (Difficulty)`) %>%
        datatable(
          options = list(
            scrollX = T,
            paginate = F
          )
        ) %>% DT::formatStyle(columns = names(data %>% select(Player, Gameweek, `Opponent (Difficulty)`)),
                              color = 'black',
                              backgroundColor = 'white')
      
    })
    
  })
  
  # Panel 4: validation scatter plot
  
  output$PlayerValidation <- renderUI({
    selectizeInput(
      'playervalidation',
      label = 'Select player',
      choices=unique(upload2()$`Full name`),
      multiple=F
    )
  })
  
  selected5 <- reactive({
    data <- upload2() %>% 
      filter(`Full name`==input$playervalidation)
    return(data)
  })
  
  observeEvent(input$ValidationButton, {
    
    data <- selected5() %>%
      mutate(`Expected points`=round(`Expected points`, 2))
    
    output$ValidationPlot <- renderPlotly({
      fig <- plot_ly(data, x= ~Gameweek, y= ~`Expected points`, name = 'Expected points', type = 'scatter', mode = 'lines', color = 'black', text = ~paste('Gameweek:', Gameweek, '<br>Exp. Points:', `Expected points`)) %>%
        layout(yaxis = list(title='Points'),
               xaxis = list(title='Gameweek', dtick = 1))
      fig <- fig %>% add_trace(y = ~`Actual points`, name = 'Actual points', type = 'scatter', mode = 'lines', color = 'orange', text= ~paste('Gameweek:', Gameweek, '<br>Actual Points:', `Actual points`))
      return(fig)
    })
    
  })
  
}

shinyApp(ui, server)
