library(shiny)
library(shinydashboard)
library(dplyr)
library(DT)
library(plotly)

options(stringsAsFactors = FALSE)
## Clean names function #######################################################
clean_names <- function(dat){
  # Takes a data.frame, returns the same data frame with cleaned names
  old_names <- names(dat)
  new_names <- old_names %>%
    gsub("%", "percent", .) %>%
    make.names(.) %>%
    gsub("[.]+", "_", .) %>%
    tolower(.) %>%
    gsub("_$", "", .)
  setNames(dat, tolower(new_names))
}


## NA Is Zero #################################################################
na.is.zero <-
  function(X, value = 0) {
    
    X1 <-
      X
    
    X1[is.na(X1)] <-
      value
    
    return(X1)
    
  }


## PBP nice name function #####################################################


pbp_player_name_merge <- function(pbp_dat, pbp_names, year, cleaned_names = FALSE) {
  
  
  if(cleaned_names){
    pbp_names <-
      clean_names(pbp_names) %>%
      filter(Season == year)
    
    pos_type <-
      tolower(c('Passer', "Rusher", "Receiver"))
    
    name_type <-
      tolower(c("name", "name_pos", "name_pos_team", "Pos"))
    ## loop thru position types and name types 
    ## match gsis_id and pull in name; na to "None"
    ## grrr.. double loop... probs a better way but 
    ## whatevers.. fuck the haters.
    for(i in pos_type){
      for(j in name_type){
        match_id <- 
          tolower(paste0(i, '_ID'))
        
        pbp_dat[, paste(i, j, sep = "_")] <- 
          na.is.zero(
            pbp_names[match(pbp_dat[, match_id],
                            pbp_names$GSIS_ID), 
                      j], 
            "None")
      }
    }
    pbp_dat
    
  } else {
    ## filter name data 
    pbp_names <-
      pbp_names %>%
      filter(Season == year)
    
    pos_type <-
      c('Passer', "Rusher", "Receiver")
    
    name_type <-
      c("name", "name_pos", "name_pos_team", "Pos")
    ## loop thru position types and name types 
    ## match gsis_id and pull in name; na to "None"
    ## grrr.. double loop... probs a better way but 
    ## whatevers.. fuck the haters.
    for(i in pos_type){
      for(j in name_type){
        match_id <- 
          paste0(i, '_ID')
        
        pbp_dat[, paste(i, j, sep = "_")] <- 
          na.is.zero(
            pbp_names[match(pbp_dat[, match_id],
                            pbp_names$GSIS_ID), 
                      j], 
            "None")
      }
    }
    pbp_dat
  }
}


pbp_names <- read.csv('gsis_ids.csv')
pbp_2018 <- pbp_player_name_merge(read.csv("2018pbp.csv"), pbp_names, 2018)
pbp_2017 <- pbp_player_name_merge(read.csv("2017pbp.csv"), pbp_names, 2017)
pbp_2016 <- pbp_player_name_merge(read.csv("2016pbp.csv"), pbp_names, 2016)
pbp_2015 <- pbp_player_name_merge(read.csv("2015pbp.csv"), pbp_names, 2015)
pbp_2014 <- pbp_player_name_merge(read.csv("2014pbp.csv"), pbp_names, 2014)
pbp_2013 <- pbp_player_name_merge(read.csv("2013pbp.csv"), pbp_names, 2013)
pbp_2012 <- pbp_player_name_merge(read.csv("2012pbp.csv"), pbp_names, 2012)
pbp_2011 <- pbp_player_name_merge(read.csv("2011pbp.csv"), pbp_names, 2011)
pbp_2010 <- pbp_player_name_merge(read.csv("2010pbp.csv"), pbp_names, 2010)
pbp_2009 <- pbp_player_name_merge(read.csv("2009pbp.csv"), pbp_names, 2009)

finalWeeklyData <- read.csv("finalWeeklyData.csv")
finalWeeklyData2 <- finalWeeklyData %>% filter(Year %in% c(2004:2018))



ui <- dashboardPage(
  
  dashboardHeader(title = "Player Data"),
  
  dashboardSidebar(
    
    selectInput("player","Select Player:",
                choices = sort(unique(as.character(finalWeeklyData$Player2))),
                selected = "Aaron Rodgers, QB"),
    sliderInput("year","Select Year:",2000,2018,2018,1),
    radioButtons("scoring","Select Scoring:",
                 choices = c("PPR","HalfPPR","Standard","FourPtTD","SixPtTD"),
                 selected = "PPR", inline = TRUE)
    
  ),
  
  dashboardBody(tags$head(tags$style(HTML('
                              .sidebar {
                                          color: #FFF;
                                          position: fixed;
                                          width: 230px;
                                          white-space: nowrap;
                                          overflow: visible;
                                          }
                                          .main-header {
                                          position: fixed;
                                          width:100%;
                                          }
                                          .content-wrapper, .right-side {
                                          min-height: 100%;
                                          background-color: #efefef;
                                          z-index: 800;
                                          }
                                          table.dataTable thead th, table.dataTable thead td {
                                          padding: 10px 18px;
                                          border-bottom: 1px solid #111;
                                          background: #ffffff;
                                          }
                                          .content {
                                          padding-top: 120px!important;
                                          }
                                          
                                          @media (min-width:768px) {
                                          .content {
                                          padding-top: 80px!important;
                                          }
                                          }
                                          
                                          @media (max-width:767px) {
                                          .skin-black .main-header>.logo {
                                          text-align: left;
                                          }
                                          }'))),
    fluidRow(
      tabBox(title = "Volume by Quarters",
             tabPanel("Pass Attempts",plotlyOutput("passquarter")),
             tabPanel("Targets",plotlyOutput("targetquarter")),
             tabPanel("Rush Attempts",plotlyOutput("rushquarter"))),
      tabBox(title = "Volume% by Quarters",
             tabPanel("Targets%",plotlyOutput("targetquarter2")),
             tabPanel("Rush Attempts%",plotlyOutput("rushquarter2")))
    ),
    
    fluidRow(
    box(title = "Yearly Position Rank", plotOutput("yearchart"), collapsible = TRUE, solidHeader = TRUE, status = "primary"),
    box(title = "Weekly Points Scored", plotOutput("weeklypoints"), collapsible = TRUE, solidHeader = TRUE, status = "primary")
    ),
    
    br(),
    
    fluidRow(
      box(title = "Game Log", dataTableOutput("gamelog"), collapsible = TRUE, solidHeader = TRUE, status = "primary", width = 12)
    ),
    
    br(),

    fluidRow(
      box(title = "Career Stats", dataTableOutput("careerstats"), collapsible = TRUE, solidHeader = TRUE, status = "primary", width = 12)
    )
    
    
  )
  
  
  
  
  
)

server <- function(input, output, session) {
  
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query[['player']])) {
      updateSelectInput(session, "player", selected = query[['player']])
    }
    if (!is.null(query[['year']])) {
      updateSliderInput(session, "year", value = query[['year']])
    }
    if (!is.null(query[['scoring']])) {
      updateRadioButtons(session, "scoring", selected = query[['scoring']])
    }
  })

  output$targetquarter <- renderPlotly({
   
    y <- paste0("pbp_",input$year)
    
    player <- input$player
    p1 <- get(y) %>% filter(Receiver_name_pos == player & PlayType != "No Play" & Season == input$year) %>%
      select(Receiver_name_pos, Season, PassAttempt, Reception, qtr, posteam)
    
    if(nrow(targetdf) == 0){
      donut <- plot_ly(labels = NULL, values = 0) %>%
        add_pie(hole = 0.6) %>%
        layout(title = "Targets by Quarter",  showlegend = TRUE,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    }
    
    if(nrow(p1) > 0){
    
    targetdf <- matrix(ncol = 2,nrow = 4)
    colnames(targetdf) <- c("Quarter","Targets")
    for(i in 1:4){
        
      dfloop <- p1 %>% filter(PassAttempt == 1, qtr == i)
      
      targetdf[i,] <- c(i,sum(dfloop$PassAttempt))
        
    }
    targetdf <- as.data.frame(targetdf)
    targetdf$Targets <- as.numeric(targetdf$Targets)
    
      donut <- plot_ly(labels = ~Quarter, values = ~Targets, data = targetdf, text = ~Targets, textinfo = "text") %>%
        add_pie(hole = 0.6) %>%
        layout(title = "Targets by Quarter",  showlegend = TRUE,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    }
    
    donut
     
  })
  
  output$passquarter <- renderPlotly({
    
    y <- paste0("pbp_",input$year)
    
    player <- input$player
    p1 <- get(y) %>% filter(Passer_name_pos == player & PlayType != "No Play" & Season == input$year) %>%
      select(Passer_name_pos, Season, PassAttempt, Reception, qtr, posteam)
    
    if(nrow(p1) == 0){
      donut <- plot_ly(labels = NULL, values = 0) %>%
        add_pie(hole = 0.6) %>%
        layout(title = "Pass Attempts by Quarter",  showlegend = TRUE,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    }
    
    if(nrow(p1) > 0){
    
    passdf <- matrix(ncol = 2,nrow = 4)
    colnames(passdf) <- c("Quarter","Attempts")
    for(i in 1:4){
      
      dfloop <- p1 %>% filter(PassAttempt == 1, qtr == i)
      
      passdf[i,] <- c(i,sum(dfloop$PassAttempt))
      
    }
    passdf <- as.data.frame(passdf)
    passdf$Attempts <- as.numeric(passdf$Attempts)
    
      donut <- plot_ly(labels = ~Quarter, values = ~Attempts, data = passdf, text = ~Attempts, textinfo = "text") %>%
        add_pie(hole = 0.6) %>%
        layout(title = "Pass Attempts by Quarter",  showlegend = TRUE,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    }
    
    donut
    
  })
  
  output$rushquarter <- renderPlotly({
    
    y <- paste0("pbp_",input$year)
    
    player <- input$player
    p1 <- get(y) %>% filter(Rusher_name_pos == player & PlayType != "No Play" & Season == input$year) %>%
      select(Rusher_name_pos, Season, RushAttempt, qtr, posteam)
    
    if(nrow(p1) == 0){
      donut <- plot_ly(labels = NULL, values = 0) %>%
        add_pie(hole = 0.6) %>%
        layout(title = "Rush Attempts by Quarter",  showlegend = TRUE,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    }
    
    if(nrow(p1) > 0){
    
    rushdf <- matrix(ncol = 2,nrow = 4)
    colnames(rushdf) <- c("Quarter","Attempts")
    for(i in 1:4){
      
      dfloop <- p1 %>% filter(RushAttempt == 1, qtr == i)
      
      rushdf[i,] <- c(i,sum(dfloop$RushAttempt))
      
    }
    rushdf <- as.data.frame(rushdf)
    rushdf$Attempts <- as.numeric(rushdf$Attempts)
    
      donut <- plot_ly(labels = ~Quarter, values = ~Attempts, data = rushdf, text = ~Attempts, textinfo = "text") %>%
        add_pie(hole = 0.6) %>%
        layout(title = "Rush Attempts by Quarter",  showlegend = TRUE,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    }
    
    donut
    
  })
  
  output$targetquarter2 <- renderPlotly({
    
    y <- paste0("pbp_",input$year)
    
    player <- input$player
    p1 <- get(y) %>% filter(Receiver_name_pos == player & PlayType != "No Play" & Season == input$year) %>%
      select(Receiver_name_pos, Season, PassAttempt, Reception, qtr, posteam, Date)
    team <- unique(as.character(p1$posteam))
    gamedays <- p1$Date
    
    if(nrow(p1) == 0){
      donut <- plot_ly(labels = NULL, values = 0) %>%
        add_pie(hole = 0.6) %>%
        layout(title = "%Team Targets by Quarter",  showlegend = TRUE,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    }
    
    if(nrow(p1) > 0){
    
    p1teamdf <- get(y) %>% filter(posteam == team, PlayType != "No Play", Season == input$year, Date %in% gamedays) %>%
      select(Receiver_name_pos, Season, PassAttempt, Reception, qtr, posteam, Date)
    
    targetdf <- matrix(ncol = 2,nrow = 4)
    colnames(targetdf) <- c("Quarter","Targets")
    for(i in 1:4){
      
      dfloop <- p1 %>% filter(PassAttempt == 1, qtr == i)
      dfloop2 <- p1teamdf %>% filter(PassAttempt == 1, qtr == i)
      
      targetdf[i,] <- c(i,round((sum(dfloop$PassAttempt)/sum(dfloop2$PassAttempt))*100,2))
      
    }
    targetdf <- as.data.frame(targetdf)
    targetdf$Targets <- as.numeric(targetdf$Targets)
    
      donut <- plot_ly(labels = ~Quarter, values = ~Targets, data = targetdf, text = ~Targets, textinfo = "text") %>%
        add_pie(hole = 0.6) %>%
        layout(title = "%Team Targets by Quarter",  showlegend = TRUE,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    }

    donut
    
  })
  
  output$rushquarter2 <- renderPlotly({
    
    y <- paste0("pbp_",input$year)
    
    player <- input$player
    p1 <- get(y) %>% filter(Rusher_name_pos == player & PlayType != "No Play" & Season == input$year) %>%
      select(Rusher_name_pos, Season, RushAttempt, qtr, posteam, Date)
    team <- unique(as.character(p1$posteam))
    gamedays <- p1$Date
    
    if(nrow(p1) == 0){
      donut <- plot_ly(labels = NULL, values = 0) %>%
        add_pie(hole = 0.6) %>%
        layout(title = "%Team Rush Attempts by Quarter",  showlegend = TRUE,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    }
    
    if(nrow(p1) > 0){
    
    p1teamdf <- get(y) %>% filter(posteam == team, PlayType != "No Play", Season == input$year, Date %in% gamedays) %>%
      select(Rusher_name_pos, Season, RushAttempt, qtr, posteam, Date)
    
    targetdf <- matrix(ncol = 2,nrow = 4)
    colnames(targetdf) <- c("Quarter","Attempts")
    for(i in 1:4){
      
      dfloop <- p1 %>% filter(RushAttempt == 1, qtr == i)
      dfloop2 <- p1teamdf %>% filter(RushAttempt == 1, qtr == i)
      
      targetdf[i,] <- c(i,round((sum(dfloop$RushAttempt)/sum(dfloop2$RushAttempt))*100,2))
      
    }
    targetdf <- as.data.frame(targetdf)
    targetdf$Attempts <- as.numeric(targetdf$Attempts)
    
      donut <- plot_ly(labels = ~Quarter, values = ~Attempts, data = targetdf, text = ~Attempts, textinfo = "text") %>%
        add_pie(hole = 0.6) %>%
        layout(title = "%Team Rush Attempts by Quarter",  showlegend = TRUE,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    }
    
    donut
    
  })
  
  
  
  
  
  
  
  output$gamelog <- renderDataTable({
    
    player <- input$player
    position <- unique(as.character(finalWeeklyData[finalWeeklyData$Player2 == player,2]))
    
    if(input$scoring == "PPR"){
      rank <- c("PPR","PPR_PR")
    }
    if(input$scoring == "FourPtTD"){
      rank <- c("FourPtTD","FourPtTD_PR")
    }
    if(input$scoring == "HalfPPR"){
      rank <- c("HalfPPR","HalfPPR_PR")
    }
    if(input$scoring == "Standard"){
      rank <- c("Standard","Standard_PR")
    }
    if(input$scoring == "SixPtTD"){
      rank <- c("SixPtTD","SixPtTD_PR")
    }
    
    if(position == "QB"){
      df1 <- finalWeeklyData[finalWeeklyData$Player2 == player,c(3,5,9:16)]
    }
    if(position == "RB"){
      df1 <- finalWeeklyData[finalWeeklyData$Player2 == player,c(3,5,14:20)]
    }
    if(position == "WR"){
      df1 <- finalWeeklyData[finalWeeklyData$Player2 == player,c(3,5,17:20,14:16)]
    }
    if(position == "TE"){
      df1 <- finalWeeklyData[finalWeeklyData$Player2 == player,c(3,5,17:20)]
    }
    
    glrank <- finalWeeklyData[finalWeeklyData$Year == input$year & finalWeeklyData$Player2 == player, rank]
    
    df2 <- df1[df1$Year == input$year,1:length(colnames(df1))]
    df2 <- cbind(df2,glrank)
    
    df3 <- df2[,1:length(colnames(df2))]
    
    df3
    
    
  }, rownames = FALSE, options = list(paging = FALSE, dom = "t")) 
  
  
  output$careerstats <- renderDataTable({
    
    player <- input$player
    position <- unique(as.character(finalWeeklyData[finalWeeklyData$Player2 == player,2]))
    
    if(position == "QB"){
      fwd_filter <- finalWeeklyData %>% select(Year,Player2,Position,Team,Comp,PassAtt,PassYards,PassTDs,INT,RushAtt,
                                               RushYards,RushTDs,PPR,HalfPPR,Standard,FourPtTD,SixPtTD) %>% filter(Position == "QB")
      team <- unique(fwd_filter %>% select(Year,Player2,Team))
      
      fwd_filter2 <- fwd_filter %>% group_by(Year,Player2) %>%
        summarise(Comp = sum(Comp),
                  PassAtt = sum(PassAtt),
                  PassYards = sum(PassYards),
                  PassTDs = sum(PassTDs),
                  INT = sum(INT),
                  RushAtt = sum(RushAtt),
                  RushYards = sum(RushYards),
                  RushTDs = sum(RushTDs),
                  PPR = sum(PPR),
                  HalfPPR = sum(HalfPPR),
                  Standard = sum(Standard),
                  FourPtTD = sum(FourPtTD),
                  SixPtTD = sum(SixPtTD)) %>% 
        mutate(PPRrank = rank(-PPR, ties.method = "first"),
               HalfPPRrank = rank(-HalfPPR, ties.method = "first"),
               Standardrank = rank(-Standard, ties.method = "first"),
               FourTDrank = rank(-FourPtTD, ties.method = "first"),
               SixTDrank = rank(-SixPtTD, ties.method = "first"))
      
      fwd_filter2 <- merge(fwd_filter2,team, by = c("Year","Player2"))
      
      if(input$scoring == "PPR"){
        
        df <- fwd_filter2[fwd_filter2$Player2 == player,c(1,21,3:11,16)]
        
      }
      
      if(input$scoring == "HalfPPR"){
        
        df <- fwd_filter2[fwd_filter2$Player2 == player,c(1,21,3:10,12,17)]
        
      }
      
      if(input$scoring == "Standard"){
        
        df <- fwd_filter2[fwd_filter2$Player2 == player,c(1,21,3:10,13,18)]
        
      }
      
      if(input$scoring == "FourPtTD"){
        
        df <- fwd_filter2[fwd_filter2$Player2 == player,c(1,21,3:10,14,19)]
        
      }
      
      if(input$scoring == "SixPtTD"){
        
        df <- fwd_filter2[fwd_filter2$Player2 == player,c(1,21,3:10,15,20)]
        
      }
      
    }
    if(position == "RB"){
      fwd_filter <- finalWeeklyData %>% select(Year,Player2,Position,Team,RushAtt,RushYards,RushTDs,
                                               PassAtt,Receptions,RecYards,RecTDs,
                                               PPR,HalfPPR,Standard,FourPtTD,SixPtTD) %>% filter(Position == "RB")
      team <- unique(fwd_filter %>% select(Year,Player2,Team))
      
      fwd_filter2 <- fwd_filter %>% group_by(Year,Player2) %>%
        summarise(RushAtt = sum(RushAtt),
                  RushYards = sum(RushYards),
                  RushTDs = sum(RushTDs),
                  PassAtt = sum(PassAtt),
                  Receptions = sum(Receptions),
                  RecYards = sum(RecYards),
                  RecTDs = sum(RecTDs),
                  PPR = sum(PPR),
                  HalfPPR = sum(HalfPPR),
                  Standard = sum(Standard),
                  FourPtTD = sum(FourPtTD),
                  SixPtTD = sum(SixPtTD)) %>% 
        mutate(PPRrank = rank(-PPR, ties.method = "first"),
               HalfPPRrank = rank(-HalfPPR, ties.method = "first"),
               Standardrank = rank(-Standard, ties.method = "first"),
               FourTDrank = rank(-FourPtTD, ties.method = "first"),
               SixTDrank = rank(-SixPtTD, ties.method = "first"))
      
      fwd_filter2 <- merge(fwd_filter2,team, by = c("Year","Player2"))
      
      if(input$scoring == "PPR"){
        
        df <- fwd_filter2[fwd_filter2$Player2 == player,c(1,20,3:10,15)]
        
      }
      
      if(input$scoring == "HalfPPR"){
        
        df <- fwd_filter2[fwd_filter2$Player2 == player,c(1,20,3:9,11,16)]
        
      }
      
      if(input$scoring == "Standard"){
        
        df <- fwd_filter2[fwd_filter2$Player2 == player,c(1,20,3:9,12,17)]
        
      }
      
      if(input$scoring == "FourPtTD"){
        
        df <- fwd_filter2[fwd_filter2$Player2 == player,c(1,20,3:9,13,18)]
        
      }
      
      if(input$scoring == "SixPtTD"){
        
        df <- fwd_filter2[fwd_filter2$Player2 == player,c(1,20,3:9,14,19)]
        
      }
    }
    if(position == "WR"){
      fwd_filter <- finalWeeklyData %>% select(Year,Player2,Position,Team,RushAtt,RushYards,RushTDs,
                                               PassAtt,Receptions,RecYards,RecTDs,
                                               PPR,HalfPPR,Standard,FourPtTD,SixPtTD) %>% filter(Position == "WR")
      team <- unique(fwd_filter %>% select(Year,Player2,Team))
      
      fwd_filter2 <- fwd_filter %>% group_by(Year,Player2) %>%
        summarise(PassAtt = sum(PassAtt),
                  Receptions = sum(Receptions),
                  RecYards = sum(RecYards),
                  RecTDs = sum(RecTDs),
                  RushAtt = sum(RushAtt),
                  RushYards = sum(RushYards),
                  RushTDs = sum(RushTDs),
                  PPR = sum(PPR),
                  HalfPPR = sum(HalfPPR),
                  Standard = sum(Standard),
                  FourPtTD = sum(FourPtTD),
                  SixPtTD = sum(SixPtTD)) %>% 
        mutate(PPRrank = rank(-PPR, ties.method = "first"),
               HalfPPRrank = rank(-HalfPPR, ties.method = "first"),
               Standardrank = rank(-Standard, ties.method = "first"),
               FourTDrank = rank(-FourPtTD, ties.method = "first"),
               SixTDrank = rank(-SixPtTD, ties.method = "first"))
      
      fwd_filter2 <- merge(fwd_filter2,team, by = c("Year","Player2"))
      
      if(input$scoring == "PPR"){
        
        df <- fwd_filter2[fwd_filter2$Player2 == player,c(1,20,3:10,15)]
        
      }
      
      if(input$scoring == "HalfPPR"){
        
        df <- fwd_filter2[fwd_filter2$Player2 == player,c(1,20,3:9,11,16)]
        
      }
      
      if(input$scoring == "Standard"){
        
        df <- fwd_filter2[fwd_filter2$Player2 == player,c(1,20,3:9,12,17)]
        
      }
      
      if(input$scoring == "FourPtTD"){
        
        df <- fwd_filter2[fwd_filter2$Player2 == player,c(1,20,3:9,13,18)]
        
      }
      
      if(input$scoring == "SixPtTD"){
        
        df <- fwd_filter2[fwd_filter2$Player2 == player,c(1,20,3:9,14,19)]
        
      }
    }
    if(position == "TE"){
      fwd_filter <- finalWeeklyData %>% select(Year,Player2,Position,Team,
                                               PassAtt,Receptions,RecYards,RecTDs,
                                               PPR,HalfPPR,Standard,FourPtTD,SixPtTD) %>% filter(Position == "TE")
      team <- unique(fwd_filter %>% select(Year,Player2,Team))
      
      fwd_filter2 <- fwd_filter %>% group_by(Year,Player2) %>%
        summarise(PassAtt = sum(PassAtt),
                  Receptions = sum(Receptions),
                  RecYards = sum(RecYards),
                  RecTDs = sum(RecTDs),
                  PPR = sum(PPR),
                  HalfPPR = sum(HalfPPR),
                  Standard = sum(Standard),
                  FourPtTD = sum(FourPtTD),
                  SixPtTD = sum(SixPtTD)) %>% 
        mutate(PPRrank = rank(-PPR, ties.method = "first"),
               HalfPPRrank = rank(-HalfPPR, ties.method = "first"),
               Standardrank = rank(-Standard, ties.method = "first"),
               FourTDrank = rank(-FourPtTD, ties.method = "first"),
               SixTDrank = rank(-SixPtTD, ties.method = "first"))
      
      fwd_filter2 <- merge(fwd_filter2,team, by = c("Year","Player2"))
      
      if(input$scoring == "PPR"){
        
        df <- fwd_filter2[fwd_filter2$Player2 == player,c(1,17,3:7,12)]
        
      }
      
      if(input$scoring == "HalfPPR"){
        
        df <- fwd_filter2[fwd_filter2$Player2 == player,c(1,17,3:6,8,13)]
        
      }
      
      if(input$scoring == "Standard"){
        
        df <- fwd_filter2[fwd_filter2$Player2 == player,c(1,17,3:6,9,14)]
        
      }
      
      if(input$scoring == "FourPtTD"){
        
        df <- fwd_filter2[fwd_filter2$Player2 == player,c(1,17,3:6,10,15)]
        
      }
      
      if(input$scoring == "SixPtTD"){
        
        df <- fwd_filter2[fwd_filter2$Player2 == player,c(1,17,3:6,11,16)]
        
      }
    }
    
    df <- df[order(-df$Year),]
    df
    
  }, rownames = FALSE, options = list(paging = FALSE, dom = "t"))
  
  output$yearchart <- renderPlot({
    
    player <- input$player
    position <- unique(as.character(finalWeeklyData[finalWeeklyData$Player2 == player,2]))
    
    fwd_filter <- finalWeeklyData %>% select(Year,Player2,Position,PPR,HalfPPR,Standard,FourPtTD,SixPtTD)
    
    fwd_filter2 <- fwd_filter %>% group_by(Year,Player2,Position) %>%
      summarise(PPR = sum(PPR),
                HalfPPR = sum(HalfPPR),
                Standard = sum(Standard),
                FourPtTD = sum(FourPtTD),
                SixPtTD = sum(SixPtTD)) 
    fwd_filter2 <- fwd_filter2 %>% group_by(Year,Position) %>%
      mutate(PPRrank = rank(-PPR, ties.method = "first"),
             HalfPPRrank = rank(-HalfPPR, ties.method = "first"),
             Standardrank = rank(-Standard, ties.method = "first"),
             FourTDrank = rank(-FourPtTD, ties.method = "first"),
             SixTDrank = rank(-SixPtTD, ties.method = "first"))
    
    p1 <- fwd_filter2[fwd_filter2$Player2 == player,]
    
    if(input$scoring == "PPR"){
      
      plot(p1$Year,p1$PPRrank, main = paste(player, "PPR Rank"), axes = FALSE,
           ylab = "Rank", xlab = "Year", ylim = rev(c(1,max(p1$PPRrank))), type = "l")
      axis(1, at = p1$Year, lab = p1$Year)
      axis(2, at = c(1,1:max(p1$PPRrank)*5))
      
    }
    
    if(input$scoring == "HalfPPR"){
      
      plot(p1$Year,p1$HalfPPRrank, main = paste(player, "HalfPPR Rank"), axes = FALSE,
           ylab = "Rank", xlab = "Year", ylim = rev(c(1,max(p1$HalfPPRrank))), type = "l")
      axis(1, at = p1$Year, lab = p1$Year)
      axis(2, at = c(1,1:max(p1$HalfPPRrank)*5))
      
    }
    
    if(input$scoring == "Standard"){
      
      plot(p1$Year,p1$Standardrank, main = paste(player, "Standard Rank"), axes = FALSE,
           ylab = "Rank", xlab = "Year", ylim = rev(c(1,max(p1$Standardrank))), type = "l")
      axis(1, at = p1$Year, lab = p1$Year)
      axis(2, at = c(1,1:max(p1$Standardrank)*5))
      
    }
    
    if(input$scoring == "FourPtTD"){
      
      plot(p1$Year,p1$FourTDrank, main = paste(player, "FourPtTD Rank"), axes = FALSE,
           ylab = "Rank", xlab = "Year", ylim = rev(c(1,max(p1$FourTDrank))), type = "l")
      axis(1, at = p1$Year, lab = p1$Year)
      axis(2, at = c(1,1:max(p1$FourTDrank)*5))
      
    }
    
    if(input$scoring == "SixPtTD"){
      
      plot(p1$Year,p1$SixTDrank, main = paste(player, "SixPtTD Rank"), axes = FALSE,
           ylab = "Rank", xlab = "Year", ylim = rev(c(1,max(p1$SixTDrank))), type = "l")
      axis(1, at = p1$Year, lab = p1$Year)
      axis(2, at = c(1,1:max(p1$SixTDrank)*5))
      
    }
    
    
    
  })
  
  output$weeklypoints <- renderPlot({
    
    if(input$scoring == "PPR"){
      
      pointsv2 <- NULL
      for(i in 1:17){
        playerdf <- finalWeeklyData %>% filter(Player2 == input$player, Year == input$year, Week == i)
        
        if(nrow(playerdf) == 0){
          p <- NA
        }
        if(nrow(playerdf) > 0){
          p <- playerdf$PPR
        }
        
        pointsv2 <- c(pointsv2,p)
        
      }
      
      
      plot(pointsv2,main = paste(input$player,"PPR Weekly Points",input$year), axes = FALSE,
           ylab = "Points", xlab = "Week", ylim = c(0,max(pointsv2, na.rm = TRUE)), xlim = c(1,17), type = "o")
      par(new=TRUE)
      abline(h = mean(pointsv2, na.rm = TRUE),col = "red")
      axis(1, at = 1:17, lab = 1:17)
      axis(2, at = c(0,0:max(pointsv2, na.rm = TRUE)*4))
      
    }
    
    if(input$scoring == "HalfPPR"){
      
      pointsv2 <- NULL
      for(i in 1:17){
        playerdf <- finalWeeklyData %>% filter(Player2 == input$player, Year == input$year, Week == i)
        
        if(nrow(playerdf) == 0){
          p <- NA
        }
        if(nrow(playerdf) > 0){
          p <- playerdf$HalfPPR
        }
        
        pointsv2 <- c(pointsv2,p)
        
      }
      
      plot(pointsv2,main = paste(input$player,"HalfPPR Weekly Points",input$year), axes = FALSE,
           ylab = "Points", xlab = "Week", ylim = c(0,max(pointsv2, na.rm = TRUE)), xlim = c(1,17), type = "o")
      par(new=TRUE)
      abline(h = mean(pointsv2, na.rm = TRUE),col = "red")
      axis(1, at = 1:17, lab = 1:17)
      axis(2, at = c(0,0:max(pointsv2, na.rm = TRUE)*4))
      
    }
    
    if(input$scoring == "Standard"){
      
      pointsv2 <- NULL
      for(i in 1:17){
        playerdf <- finalWeeklyData %>% filter(Player2 == input$player, Year == input$year, Week == i)
        
        if(nrow(playerdf) == 0){
          p <- NA
        }
        if(nrow(playerdf) > 0){
          p <- playerdf$Standard
        }
        
        pointsv2 <- c(pointsv2,p)
        
      }
      
      plot(pointsv2,main = paste(input$player,"Standard Weekly Points",input$year), axes = FALSE,
           ylab = "Points", xlab = "Week", ylim = c(0,max(pointsv2, na.rm = TRUE)), xlim = c(1,17), type = "o")
      par(new=TRUE)
      abline(h = mean(pointsv2, na.rm = TRUE),col = "red")
      axis(1, at = 1:17, lab = 1:17)
      axis(2, at = c(0,0:max(pointsv2, na.rm = TRUE)*4))
      
    }
    
    if(input$scoring == "FourPtTD"){
      
      pointsv2 <- NULL
      for(i in 1:17){
        playerdf <- finalWeeklyData %>% filter(Player2 == input$player, Year == input$year, Week == i)
        
        if(nrow(playerdf) == 0){
          p <- NA
        }
        if(nrow(playerdf) > 0){
          p <- playerdf$FourPtTD
        }
        
        pointsv2 <- c(pointsv2,p)
        
      }
      
      plot(pointsv2,main = paste(input$player,"FourPtTD Weekly Points",input$year), axes = FALSE,
           ylab = "Points", xlab = "Week", ylim = c(0,max(pointsv2, na.rm = TRUE)), xlim = c(1,17), type = "o")
      par(new=TRUE)
      abline(h = mean(pointsv2, na.rm = TRUE),col = "red")
      axis(1, at = 1:17, lab = 1:17)
      axis(2, at = c(0,0:max(pointsv2, na.rm = TRUE)*4))
      
    }
    
    if(input$scoring == "SixPtTD"){
      
      pointsv2 <- NULL
      for(i in 1:17){
        playerdf <- finalWeeklyData %>% filter(Player2 == input$player, Year == input$year, Week == i)
        
        if(nrow(playerdf) == 0){
          p <- NA
        }
        if(nrow(playerdf) > 0){
          p <- playerdf$SixPtTD
        }
        
        pointsv2 <- c(pointsv2,p)
        
      }
      
      plot(pointsv2,main = paste(input$player,"SixPtTD Weekly Points",input$year), axes = FALSE,
           ylab = "Points", xlab = "Week", ylim = c(0,max(pointsv2, na.rm = TRUE)), xlim = c(1,17), type = "o")
      par(new=TRUE)
      abline(h = mean(pointsv2, na.rm = TRUE),col = "red")
      axis(1, at = 1:17, lab = 1:17)
      axis(2, at = c(0,0:max(pointsv2, na.rm = TRUE)*4))
      
    }
    
  })
  
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

