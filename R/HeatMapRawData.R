
# StatsBomb Data Retrieval -- saves data loaded using functions provided by
# StatsBomb in their package StatsBombR

get_statsbomb_data <- function(){
  # library(tidyverse)
  # library(StatsBombR)
  comp <- FreeCompetitions()
  WorldCup <- comp %>% filter(competition_name == "FIFA World Cup")
  matches_WC <- FreeMatches(WorldCup)
  events_WC <- StatsBombFreeEvents(MatchesDF = matches_WC)
  events_WC <- allclean(events_WC)
  lineups_WC <- StatsBombFreeLineups(MatchesDF = matches_WC)
  Euros <- comp %>% filter(competition_name == "UEFA Euro")
  matches_Euros <- FreeMatches(Euros)
  events_Euros <- StatsBombFreeEvents(MatchesDF = matches_Euros)
  events_Euros <- allclean(events_Euros)
  lineups_Euros <- StatsBombFreeLineups(MatchesDF = matches_Euros)
  save(comp, WorldCup, matches_WC, events_WC, lineups_WC, 
       Euros, matches_Euros, events_Euros, lineups_Euros, file = "data/StatsBombData.Rdata")
  if (file.exists("data/StatsBombData.Rdata")){
    cat('\n\n\n\n\nNow run the following: load("data/StatsBombData.Rdata")')
  }
}

# Data Retrieval Confirmation function:
check_statsbomb_data <- function(){
  if (file.exists("data/StatsBombData.Rdata")){
    cat('Now run the following if you have not already: load("data/StatsBombData.Rdata")')
  }
  else{
    cat("Oops! Make sure you run `get_statsbomb_data()` to save the match, lineup, and event data provided by StatsBomb. \n \tNote: you need to have a folder named 'data' in your current working directory!")
  }
}
