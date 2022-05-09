
# Make sure you have run the following if you haven't already:
# load("data/StatsBombData.Rdata")

# Heat Map Data Retrieval -- loading the data this saves yields the pre-processed
# player data that will serve as the focus of our project.
get_involved_players <- function(){
  # library(tidyverse)
  # library(dplyr)
  # setwd(rprojroot::find_rstudio_root_file())
  
  # World Cup
  WC_matchIDs <- matches_WC$match_id
  WC_n <- length(WC_matchIDs)
  WC_hometeams <- rep(NA, WC_n)
  WC_awayteams <- rep(NA, WC_n)
  
  for (i in 1:WC_n){
    WC_hometeams[i] <- matches_WC$home_team.home_team_name[i]
    WC_awayteams[i] <- matches_WC$away_team.away_team_name[i]
  }
  WC_homelineups <- data.frame(playerID = integer(),
                               playerName = character(),
                               matchID = integer(),
                               team = character(),
                               involvement = character(),
                               position = character(),
                               start = character(),
                               start_half = character(),
                               end = character(),
                               end_half = character(),
                               n_events = integer(),
                               n_pass = integer(),
                               pass_dist_mean = integer(),
                               pass_dist_sd = integer(),
                               n_carry = integer(),
                               n_pressure = integer(),
                               n_shot = integer(),
                               n_clearance = integer(),
                               n_duel = integer())
  WC_awaylineups <- data.frame(playerID = integer(),
                               playerName = character(),
                               matchID = integer(),
                               team = character(),
                               involvement = character(),
                               position = character(),
                               start = character(),
                               start_half = character(),
                               end = character(),
                               end_half = character(),
                               n_events = integer(),
                               n_pass = integer(),
                               pass_dist_mean = integer(),
                               pass_dist_sd = integer(),
                               n_carry = integer(),
                               n_pressure = integer(),
                               n_shot = integer(),
                               n_clearance = integer(),
                               n_duel = integer())
  
  for (i in 1:WC_n){
    temp_match <- WC_matchIDs[i]
    temp_lineups <- lineups_WC[lineups_WC$match_id == temp_match, ]
    # HOME
    temp_hometeam <- WC_hometeams[i]
    temp_home <- temp_lineups[temp_lineups$team_name == temp_hometeam, ]
    temp_players_home <- temp_home[[3]][[1]][[2]]
    temp_players_home_ID <- temp_home[[3]][[1]][[1]]
    temp_positions_home <- temp_home[[3]][[1]][[6]]
    involve_home <- rep(NA, length(temp_positions_home))
    for (j in 1:length(involve_home)){
      if (length(temp_positions_home[[j]]) == 0){
        temp_positions <- NA
        involve_home[j] <- NA
        from <- NA
        from_half <- NA
        to <- NA
        to_half <- NA
        n_events <- NA
        n_pass <- NA
        mean_pass <- NA
        sd_pass <- NA
        n_carry <- NA
        n_pressure <- NA
        n_shot <- NA
        n_clearance <- NA
        n_duel <- NA
      }
      else {
        temp_positions <- temp_positions_home[[j]][[2]]
        from <- temp_positions_home[[j]]$from
        from_half <- temp_positions_home[[j]]$from_period
        to <- temp_positions_home[[j]]$to
        to_half <- temp_positions_home[[j]]$to_period
        start_reason <- temp_positions_home[[j]]$start_reason[1]
        if (start_reason == "Starting XI"){
          involve_home[j] <- start_reason
        }
        else {
          involve_home[j] <- "Substitute"
        }
      }
      for (k in 1:length(temp_positions)){
        temp_events <- events_WC %>%
          filter(player.name == temp_players_home[j]) %>%
          filter(match_id == temp_match) %>%
          filter(position.name == temp_positions[k]) %>%
          dplyr::select(player.name, type.name, pass.length)
        n_events <- nrow(temp_events)
        n_carry <- sum(temp_events$type.name == "Carry")
        n_pressure <- sum(temp_events$type.name == "Pressure")
        n_shot <- sum(temp_events$type.name == "Shot")
        n_clearance <- sum(temp_events$type.name == "Clearance")
        n_duel <- sum(temp_events$type.name == "Duel")
        temp_pass <- temp_events %>%
          filter(type.name == "Pass")
        n_pass <- nrow(temp_pass)
        if (n_pass == 0){
          mean_pass <- 0
          sd_pass <- 0
        }
        else{
          mean_pass <- mean(temp_pass$pass.length)
          sd_pass <- sd(temp_pass$pass.length)
        }
        
        WC_homelineups[nrow(WC_homelineups) + 1, ] <- list(temp_players_home_ID[j], 
                                                           temp_players_home[j],
                                                           temp_match, 
                                                           temp_hometeam, 
                                                           involve_home[j],
                                                           temp_positions[k],
                                                           from[k],
                                                           from_half[k],
                                                           to[k],
                                                           to_half[k],
                                                           n_events,
                                                           n_pass,
                                                           mean_pass,
                                                           sd_pass,
                                                           n_carry,
                                                           n_pressure,
                                                           n_shot,
                                                           n_clearance,
                                                           n_duel)
      }
    }
    # AWAY
    temp_awayteam <- WC_awayteams[i]
    temp_away <- temp_lineups[temp_lineups$team_name == temp_awayteam, ]
    temp_players_away <- temp_away[[3]][[1]][[2]]
    temp_players_away_ID <- temp_away[[3]][[1]][[1]]
    temp_positions_away <- temp_away[[3]][[1]][[6]]
    involve_away <- rep(NA, length(temp_positions_away))
    for (j in 1:length(involve_away)){
      if (length(temp_positions_away[[j]]) == 0){
        temp_positions <- NA
        involve_away[j] <- NA
        from <- NA
        from_half <- NA
        to <- NA
        to_half <- NA
        n_events <- NA
        n_pass <- NA
        mean_pass <- NA
        sd_pass <- NA
        n_carry <- NA
        n_pressure <- NA
        n_shot <- NA
        n_clearance <- NA
        n_duel <- NA
      }
      else {
        temp_positions <- temp_positions_away[[j]][[2]]
        from <- temp_positions_away[[j]]$from
        from_half <- temp_positions_away[[j]]$from_period
        to <- temp_positions_away[[j]]$to
        to_half <- temp_positions_away[[j]]$to_period
        start_reason <- temp_positions_away[[j]]$start_reason[1]
        if (start_reason == "Starting XI"){
          involve_away[j] <- start_reason
        }
        else {
          involve_away[j] <- "Substitute"
        }
      }
      for (k in 1:length(temp_positions)){
        temp_events <- events_WC %>%
          filter(player.name == temp_players_away[j]) %>%
          filter(match_id == temp_match) %>%
          filter(position.name == temp_positions[k]) %>%
          dplyr::select(player.name, type.name, pass.length)
        n_events <- nrow(temp_events)
        n_carry <- sum(temp_events$type.name == "Carry")
        n_pressure <- sum(temp_events$type.name == "Pressure")
        n_shot <- sum(temp_events$type.name == "Shot")
        n_clearance <- sum(temp_events$type.name == "Clearance")
        n_duel <- sum(temp_events$type.name == "Duel")
        temp_pass <- temp_events %>%
          filter(type.name == "Pass")
        n_pass <- nrow(temp_pass)
        if (n_pass == 0){
          mean_pass <- 0
          sd_pass <- 0
        }
        else{
          mean_pass <- mean(temp_pass$pass.length)
          sd_pass <- sd(temp_pass$pass.length)
        }
        
        WC_awaylineups[nrow(WC_awaylineups) + 1, ] <- list(temp_players_away_ID[j], 
                                                           temp_players_away[j], 
                                                           temp_match, 
                                                           temp_awayteam, 
                                                           involve_away[j],
                                                           temp_positions[k],
                                                           from[k],
                                                           from_half[k],
                                                           to[k],
                                                           to_half[k],
                                                           n_events,
                                                           n_pass,
                                                           mean_pass,
                                                           sd_pass,
                                                           n_carry,
                                                           n_pressure,
                                                           n_shot,
                                                           n_clearance,
                                                           n_duel)
      }
    }
  }
  
  # Euros
  EU_matchIDs <- matches_Euros$match_id
  EU_n <- length(EU_matchIDs)
  EU_hometeams <- rep(NA, EU_n)
  EU_awayteams <- rep(NA, EU_n)
  for (i in 1:EU_n){
    EU_hometeams[i] <- matches_Euros$home_team.home_team_name[i]
    EU_awayteams[i] <- matches_Euros$away_team.away_team_name[i]
  }
  EU_homelineups <- data.frame(playerID = integer(),
                               playerName = character(),
                               matchID = integer(),
                               team = character(),
                               involvement = character(),
                               position = character(),
                               start = character(),
                               start_half = character(),
                               end = character(),
                               end_half = character(),
                               n_events = integer(),
                               n_pass = integer(),
                               pass_dist_mean = integer(),
                               pass_dist_sd = integer(),
                               n_carry = integer(),
                               n_pressure = integer(),
                               n_shot = integer(),
                               n_clearance = integer(),
                               n_duel = integer())
  EU_awaylineups <- data.frame(playerID = integer(),
                               playerName = character(),
                               matchID = integer(),
                               team = character(),
                               involvement = character(),
                               position = character(),
                               start = character(),
                               start_half = character(),
                               end = character(),
                               end_half = character(),
                               n_events = integer(),
                               n_pass = integer(),
                               pass_dist_mean = integer(),
                               pass_dist_sd = integer(),
                               n_carry = integer(),
                               n_pressure = integer(),
                               n_shot = integer(),
                               n_clearance = integer(),
                               n_duel = integer())
  for (i in 1:EU_n){
    temp_match <- EU_matchIDs[i]
    temp_lineups <- lineups_Euros[lineups_Euros$match_id == temp_match, ]
    # HOME
    temp_hometeam <- EU_hometeams[i]
    temp_home <- temp_lineups[temp_lineups$team_name == temp_hometeam, ]
    temp_players_home <- temp_home[[3]][[1]][[2]]
    temp_players_home_ID <- temp_home[[3]][[1]][[1]]
    temp_positions_home <- temp_home[[3]][[1]][[6]]
    involve_home <- rep(NA, length(temp_positions_home))
    for (j in 1:length(involve_home)){
      if (length(temp_positions_home[[j]]) == 0){
        temp_positions <- NA
        involve_home[j] <- NA
        from <- NA
        from_half <- NA
        to <- NA
        to_half <- NA
        n_events <- NA
        n_pass <- NA
        mean_pass <- NA
        sd_pass <- NA
        n_carry <- NA
        n_pressure <- NA
        n_shot <- NA
        n_clearance <- NA
        n_duel <- NA
      }
      else {
        temp_positions <- temp_positions_home[[j]][[2]]
        from <- temp_positions_home[[j]]$from
        from_half <- temp_positions_home[[j]]$from_period
        to <- temp_positions_home[[j]]$to
        to_half <- temp_positions_home[[j]]$to_period
        start_reason <- temp_positions_home[[j]]$start_reason[1]
        if (start_reason == "Starting XI"){
          involve_home[j] <- start_reason
        }
        else {
          involve_home[j] <- "Substitute"
        }
      }
      for (k in 1:length(temp_positions)){
        temp_events <- events_Euros %>%
          filter(player.name == temp_players_home[j]) %>%
          filter(match_id == temp_match) %>%
          filter(position.name == temp_positions[k]) %>%
          dplyr::select(player.name, type.name, pass.length)
        n_events <- nrow(temp_events)
        n_carry <- sum(temp_events$type.name == "Carry")
        n_pressure <- sum(temp_events$type.name == "Pressure")
        n_shot <- sum(temp_events$type.name == "Shot")
        n_clearance <- sum(temp_events$type.name == "Clearance")
        n_duel <- sum(temp_events$type.name == "Duel")
        temp_pass <- temp_events %>%
          filter(type.name == "Pass")
        n_pass <- nrow(temp_pass)
        if (n_pass == 0){
          mean_pass <- 0
          sd_pass <- 0
        }
        else{
          mean_pass <- mean(temp_pass$pass.length)
          sd_pass <- sd(temp_pass$pass.length)
        }
        
        EU_homelineups[nrow(EU_homelineups) + 1, ] <- list(temp_players_home_ID[j], 
                                                           temp_players_home[j],
                                                           temp_match, 
                                                           temp_hometeam, 
                                                           involve_home[j],
                                                           temp_positions[k],
                                                           from[k],
                                                           from_half[k],
                                                           to[k],
                                                           to_half[k],
                                                           n_events,
                                                           n_pass,
                                                           mean_pass,
                                                           sd_pass,
                                                           n_carry,
                                                           n_pressure,
                                                           n_shot,
                                                           n_clearance,
                                                           n_duel)
      }
    }
    # AWAY
    temp_awayteam <- EU_awayteams[i]
    temp_away <- temp_lineups[temp_lineups$team_name == temp_awayteam, ]
    temp_players_away <- temp_away[[3]][[1]][[2]]
    temp_players_away_ID <- temp_away[[3]][[1]][[1]]
    temp_positions_away <- temp_away[[3]][[1]][[6]]
    involve_away <- rep(NA, length(temp_positions_away))
    for (j in 1:length(involve_away)){
      if (length(temp_positions_away[[j]]) == 0){
        temp_positions <- NA
        involve_away[j] <- NA
        from <- NA
        from_half <- NA
        to <- NA
        to_half <- NA
        n_events <- NA
        n_pass <- NA
        mean_pass <- NA
        sd_pass <- NA
        n_carry <- NA
        n_pressure <- NA
        n_shot <- NA
        n_clearance <- NA
        n_duel <- NA
      }
      else {
        temp_positions <- temp_positions_away[[j]][[2]]
        from <- temp_positions_away[[j]]$from
        from_half <- temp_positions_away[[j]]$from_period
        to <- temp_positions_away[[j]]$to
        to_half <- temp_positions_away[[j]]$to_period
        start_reason <- temp_positions_away[[j]]$start_reason[1]
        if (start_reason == "Starting XI"){
          involve_away[j] <- start_reason
        }
        else {
          involve_away[j] <- "Substitute"
        }
      }
      for (k in 1:length(temp_positions)){
        temp_events <- events_Euros %>%
          filter(player.name == temp_players_away[j]) %>%
          filter(match_id == temp_match) %>%
          filter(position.name == temp_positions[k]) %>%
          dplyr::select(player.name, type.name, pass.length)
        n_events <- nrow(temp_events)
        n_carry <- sum(temp_events$type.name == "Carry")
        n_pressure <- sum(temp_events$type.name == "Pressure")
        n_shot <- sum(temp_events$type.name == "Shot")
        n_clearance <- sum(temp_events$type.name == "Clearance")
        n_duel <- sum(temp_events$type.name == "Duel")
        temp_pass <- temp_events %>%
          filter(type.name == "Pass")
        n_pass <- nrow(temp_pass)
        if (n_pass == 0){
          mean_pass <- 0
          sd_pass <- 0
        }
        else{
          mean_pass <- mean(temp_pass$pass.length)
          sd_pass <- sd(temp_pass$pass.length)
        }
        
        EU_awaylineups[nrow(EU_awaylineups) + 1, ] <- list(temp_players_away_ID[j], 
                                                           temp_players_away[j], 
                                                           temp_match, 
                                                           temp_awayteam, 
                                                           involve_away[j],
                                                           temp_positions[k],
                                                           from[k],
                                                           from_half[k],
                                                           to[k],
                                                           to_half[k],
                                                           n_events,
                                                           n_pass,
                                                           mean_pass,
                                                           sd_pass,
                                                           n_carry,
                                                           n_pressure,
                                                           n_shot,
                                                           n_clearance,
                                                           n_duel)
      }
    }
  }

  allplayers_WC <- rbind(WC_homelineups, WC_awaylineups)
  allplayers_WC$comp <- "WC"
  
  # It turns out there are 2 instances in the World Cup data where a player's position
  # is missing in the lineup data, so we manually set it after checking their event data.
  # We encounter a similar issue in the World Cup data, except this time this issue occurred
  # for 5 players. We account for this in the code below, along with other bits of
  # data cleaning / re-formatting...
  
  allplayers_WC <- allplayers_WC %>%
    drop_na(involvement) %>%
    mutate(position = ifelse(playerName == "Yo-Han Go" & matchID == 7567, 
                             "Left Midfield", position)) %>%
    mutate(position = ifelse(playerName == "Ahmed Musa" & matchID == 7529, 
                        "Left Defensive Midfield", position)) %>%
    group_by(playerID, playerName, matchID, position) %>%
    mutate(start = list(start)) %>%
    mutate(start_half = list(start_half)) %>%
    mutate(end = list(end)) %>%
    mutate(end_half = list(end_half)) %>%
    filter(!(length(unlist(start)) > 1 & n_events == 0)) %>%
    arrange(matchID) %>%
    distinct() %>% as.data.frame()
  
  allplayers_Euros <- rbind(EU_homelineups, EU_awaylineups)
  allplayers_Euros$comp <- "EU"
  
  allplayers_Euros <- allplayers_Euros %>%
    drop_na(involvement) %>%
    mutate(position = ifelse(playerName == "Mykola Shaparenko" & matchID == 3788746, 
                             "Left Wing", position)) %>%
    mutate(position = ifelse(playerName == "Adrien Rabiot" & matchID == 3788773, 
                             "Left Back", position)) %>%
    mutate(position = ifelse(playerName == "Marcus Thuram" & matchID == 3794691, 
                             "Left Midfield", position)) %>%
    mutate(position = ifelse(playerName == "Viktor Tsygankov" & matchID == 3794692, 
                             "Left Center Forward", position)) %>%
    mutate(position = ifelse(playerName == "Kieran Trippier" & matchID == 3795221, 
                             "Right Wing Back", position)) %>%
    group_by(playerID, playerName, matchID, position) %>%
    mutate(start = list(start)) %>%
    mutate(start_half = list(start_half)) %>%
    mutate(end = list(end)) %>%
    mutate(end_half = list(end_half)) %>%
    filter(!(length(unlist(start)) > 1 & n_events == 0)) %>%
    arrange(matchID) %>%
    distinct() %>% as.data.frame()
  
  allplayers <- rbind(allplayers_WC, allplayers_Euros)

  save(allplayers, allplayers_Euros, allplayers_WC, file = "data/Players.Rdata")
  if (file.exists("data/Players.Rdata")){
    cat('Now run the following: load("data/Players.Rdata")')
  }
}

# Data Retrieval Confirmation function:
check_involved_players <- function(){
  if (file.exists("data/Players.Rdata")){
    cat('Now run the following if you have not already: load("data/Players.Rdata")')
  }
  else{
    cat("Oops! Make sure you run `get_involved_players()` to get the information corresponding to each player who appeared in the 2018 World Cup and 2020 Euros. \n \tNote: you need to have a folder named 'data' in your current working directory!")
  }
}

