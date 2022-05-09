
# Make sure you have run the following if you haven't already:
# library(tidyverse)
# library(dplyr)
# library(ggplot2)
# library(MASS)
setwd(rprojroot::find_rstudio_root_file())
# load("data/StatsBombData.Rdata")

# Two-Dimensional Gaussian Kernel -- used for KDE in generation of player heat maps.
gaussian_kernel_2d <- function(centered_x, centered_y, h){
  x_component <- 1 / sqrt(2 * pi) * (1 / h) * exp(-(1/2)*(centered_x / h)^2)
  y_component <- 1 / sqrt(2 * pi) * (1 / h) * exp(-(1/2)*(centered_y / h)^2)
  return(x_component * y_component)
}

# Heat Map Creation -- function that yields a heatmap (in array format) given
# the X and Y coordinates of a player's involvements/events.
heatmap_KDE <- function(X = NULL, Y = NULL, res_x = 1, res_y = 1, h = 1){
  x <<- seq(0, 120, res_x)
  y <<- seq(0, 80, res_y)
  if (length(X) == 0){
    return(array(NA, dim = c(length(x), length(y))))
  }
  if (length(X) == length(Y)){
    n <- length(X)
  }
  else{
    stop("ERROR - X and Y need to be of same length")
  }
  heatmap <- array(rep(NA, length(x) * length(y) * n), 
                   dim = c(length(x), length(y), n))
  for (i in 1:n) {
    for (j in 1:length(y)) {
      heatmap[, j, i] <- gaussian_kernel_2d(centered_x = x - X[i], centered_y = y[j] - Y[i], h = h)
    }
  }
  return(apply(heatmap, c(1,2), mean))
}

# Heat Map Plotting Function -- takes output of `clean_heatmaps_and_labels()` as input, and
# given the matchid, player name, and position, outputs the corresponding heat map. Analogous to what we
# see in our Shiny application, except here we do not remove near-zero values, resulting in the entire plot
# having a "heat" reading.
heatmap_plotter <- function(heatmaps_data, matchid, playerid, pos, res_x, res_y){
  player_of_focus <- allplayers %>%
    rownames_to_column("index") %>% 
    filter(matchID == matchid) %>%
    filter(playerID == playerid) %>%
    filter(position == pos)
  playername <- player_of_focus$playerName
  competition <- player_of_focus$comp
  index_of_focus <- player_of_focus$index
  index_of_focus <- which(heatmaps_data$allplayers_indices == index_of_focus)
  heatmap_of_focus <- heatmaps_data$heatmaps[index_of_focus, ,]
  x <- seq(0, 120, res_x)
  y <- seq(0, 80, res_y)
  heatmap_df <- data.frame(expand.grid(x, y), as.vector(heatmap_of_focus))
  colnames(heatmap_df) <- c("x", "y", "z")
  p <- ggplot(data = heatmap_df) +
    geom_raster(aes(x = x, y = y, fill = z)) +
    scale_fill_gradientn(colours = hcl.colors(12, "Temps"), 
                         na.value = "transparent") +
    scale_y_continuous(trans = "reverse") +
    xlab("Attacking Left to Right") +
    ylab("") + 
    theme(panel.grid.major = element_blank()) +
    theme(panel.grid.minor = element_blank()) +
    ggtitle(paste0(playername, ": ", "Match ", matchid, " (", competition, ")", "\n", pos, " (Index: ", index_of_focus, ")")) +
    theme(axis.title.x = element_text(face = "italic", size = 10)) +
    theme(plot.title = element_text(size = 10)) +
    labs(fill = "Activity Level")
  p
}

