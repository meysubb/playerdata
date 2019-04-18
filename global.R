library(dplyr)
library(DT)
library(plotly)
library(janitor)

options(stringsAsFactors = FALSE)

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