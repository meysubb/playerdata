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
      
      targetdf = p1 %>% filter(PassAttempt == 1) %>% group_by(qtr) %>% 
        summarize(Targets = sum(PassAttempt))
      
      donut <-
        plot_ly(
          labels = ~ qtr,
          values = ~ Targets,
          data = targetdf,
          text = ~ Targets,
          textinfo = "text"
        ) %>%
        add_pie(hole = 0.6) %>%
        layout(
          title = "Targets by Quarter",
          showlegend = TRUE,
          xaxis = list(
            showgrid = FALSE,
            zeroline = FALSE,
            showticklabels = FALSE
          ),
          yaxis = list(
            showgrid = FALSE,
            zeroline = FALSE,
            showticklabels = FALSE
          )
        )
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
      
      passdf = p1 %>% filter(PassAttempt == 1) %>% group_by(qtr) %>% 
        summarize(Attempts = sum(PassAttempt))
      
      
      donut <-
        plot_ly(
          labels = ~ qtr,
          values = ~ Attempts,
          data = passdf,
          text = ~ Attempts,
          textinfo = "text"
        ) %>%
        add_pie(hole = 0.6) %>%
        layout(
          title = "Pass Attempts by Quarter",
          showlegend = TRUE,
          xaxis = list(
            showgrid = FALSE,
            zeroline = FALSE,
            showticklabels = FALSE
          ),
          yaxis = list(
            showgrid = FALSE,
            zeroline = FALSE,
            showticklabels = FALSE
          )
        )
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
      
      rushdf = p1 %>% filter(RushAttempt == 1) %>% group_by(qtr) %>% 
        summarize(Attempts = sum(RushAttempt))
      
      donut <-
        plot_ly(
          labels = ~ qtr,
          values = ~ Attempts,
          data = rushdf,
          text = ~ Attempts,
          textinfo = "text"
        ) %>%
        add_pie(hole = 0.6) %>%
        layout(
          title = "Rush Attempts by Quarter",
          showlegend = TRUE,
          xaxis = list(
            showgrid = FALSE,
            zeroline = FALSE,
            showticklabels = FALSE
          ),
          yaxis = list(
            showgrid = FALSE,
            zeroline = FALSE,
            showticklabels = FALSE
          )
        )
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
      
      donut <-
        plot_ly(
          labels = ~ Quarter,
          values = ~ Targets,
          data = targetdf,
          text = ~ Targets,
          textinfo = "text"
        ) %>%
        add_pie(hole = 0.6) %>%
        layout(
          title = "%Team Targets by Quarter",
          showlegend = TRUE,
          xaxis = list(
            showgrid = FALSE,
            zeroline = FALSE,
            showticklabels = FALSE
          ),
          yaxis = list(
            showgrid = FALSE,
            zeroline = FALSE,
            showticklabels = FALSE
          )
        )
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
      
      donut <-
        plot_ly(
          labels = ~ Quarter,
          values = ~ Attempts,
          data = targetdf,
          text = ~ Attempts,
          textinfo = "text"
        ) %>%
        add_pie(hole = 0.6) %>%
        layout(
          title = "%Team Rush Attempts by Quarter",
          showlegend = TRUE,
          xaxis = list(
            showgrid = FALSE,
            zeroline = FALSE,
            showticklabels = FALSE
          ),
          yaxis = list(
            showgrid = FALSE,
            zeroline = FALSE,
            showticklabels = FALSE
          )
        )
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