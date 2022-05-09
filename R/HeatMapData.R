
# Heat Map Data Generator -- returns list of arrays, where each array is the heat map 
# for the corresponding player in the player data `allplayers`.
# We normalize the values to be between 0 and 1 for modelling purposes (obviously removes
# interpretations of heat maps as densities).

### USAGE: dirty_heatmaps <- get_heatmaps()
### Estimated run time: 10 minutes
get_heatmaps <- function(h = 5.2, x_res = 1, y_res = 1){
  # library(reticulate)
  # library(tidyverse)
  # library(dplyr)
  # source("R/HeatMapsCreate.R")
  num_players <- nrow(allplayers)
  heatmap_list <- as.list(rep(NA, num_players))
  for (i in 1:num_players) {
    player_name <- allplayers$playerName[i]
    game_id <- allplayers$matchID[i]
    position <- allplayers$position[i]
    comp <- allplayers$comp[i]
    if (comp == "WC"){
      events_DB <<- events_WC
    }
    else if (comp == "EU"){
      events_DB <<- events_Euros
    }
    temp_touches <- events_DB %>% 
      filter(player.name == player_name) %>%
      filter(match_id == game_id) %>%
      filter(position.name == position) %>%
      dplyr::select(id, location.x, location.y)
    X_obs <- temp_touches$location.x
    Y_obs <- temp_touches$location.y
    temp <- data.frame(X_obs, Y_obs)
    temp_touches <- temp_touches[complete.cases(temp),]
    X_obs <- temp_touches$location.x
    Y_obs <- temp_touches$location.y
    temp_heatmap <- heatmap_KDE(X = X_obs, Y = Y_obs, h = h, res_x = x_res, res_y = y_res)
    if (i %% 200 == 0){
      print(paste("Progress:", i, "/", num_players))
    }
    temp_heatmap[temp_heatmap < 0.00001] <- 0
    # NORMALIZE (values between 0 and 1)
    temp_heatmap <- (temp_heatmap - min(temp_heatmap)) / (max(temp_heatmap) - min(temp_heatmap))
    heatmap_list[[i]] <- temp_heatmap
  }
  # return list, where second object is vector of number of involvements?
  # this will allow us to only consider heat maps involving a certain number of touches...
  return(heatmap_list)
}

# Heat Map Data Cleaner -- given the output of `get_heatmaps()`, this function yields a
# named list: 
# * `allplayers_indices` is a vector of indices for each player that has a valid
# heat map (not an array of NAs, which happens if the player was in a position but never 
# had an involvement while in that position)
# * the corresponding `heatmaps`, this time as a 3-dimensional array 
# dim = c(# of maps, # of xpixels, # of ypixels), and the vector of
# * `positions` corresponding to each heat map.

### USAGE: clean_heatmaps <- clean_heatmaps_and_labels(dirty_heatmaps)
clean_heatmaps_and_labels <- function(heatmaps){
  indices <- c()
  for (i in 1:length(heatmaps)){
    temp <- heatmaps[[i]]
    if (sum(is.na(temp)) == 0){
      indices <- append(indices, i)
    }
  }
  cleaned_heatmaps <- array(NA, dim = c(length(indices), dim(heatmaps[[2]])))
  for (i in 1:length(indices)){
    cleaned_heatmaps[i, , ] <- heatmaps[[indices[i]]]
  }
  temp_players <- allplayers[indices, ]
  positions <- temp_players$position
  output <- list(indices, cleaned_heatmaps, positions)
  names(output) <- c("allplayers_indices", "heatmaps", "positions")
  return(output)
}
