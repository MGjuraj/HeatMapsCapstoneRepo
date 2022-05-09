
# Shiny Application: Heat Map Visualization
# -----------------------
# Libraries / Source Code
# -----------------------

### UNCOMMENT for shinyapp.io
library(shiny)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(MASS)
library(DT)
library(shinyjs)
library(shinyWidgets)
library(shinydisconnect)

# print(getwd())

# If you want to run app locally:
setwd(rprojroot::find_rstudio_root_file())
source("R/HeatMapsCreate.R")
load("data/StatsBombData.Rdata")

# If you plan on updating the on shinyapp.io:
# source("HeatMapsCreate.R")
# load("StatsBombData.Rdata")

# -----------------------
# UI
# -----------------------

ui <- fluidPage(
  
  # SOURCE: https://stackoverflow.com/questions/47784427/shiny-how-to-center-and-fix-width-of-dashboard
  tags$head(tags$style("body{height: auto; max-width: 1600px; margin: auto;}")),
  
  ### UNCOMMENT for shinyapp.io
  title = "Heat Map Sandbox",
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css")),
  titlePanel(tags$div(class = "jumbotron jumbotron-fluid text-white",
                      style = "background-color:#fffff8",
                      tags$div(class = "container blockquote text-center",
                               tags$h1(class = "display-1", "Heat Map Sandbox"),
                               tags$i(class = "display-1", 
                               tags$h3("Positioning in International Football:
                                       An Exploratory Analysis Using Convolutional Neural Networks")),
                               tags$p(class = "display-1", "Mark Gjuraj '22")
                               )
                      )
             ),

  # yields inputs: competition, matchid
  # calls outputs: date, info, score
  fluidRow(
    disconnectMessage(),
    column(width = 3, selectInput(inputId = "competition", label = "Select Competition:",
                                  choices = c("2018 FIFA World Cup", "UEFA Euro 2020"), 
                                  selected = "2018 FIFA World Cup")),
    column(width = 2, offset = 2, textInput(inputId = "matchid", value = 8650, label = "Input Valid Match ID:")),
    column(width = 4,tags$div(class = "blockquote text-right",
                              tags$h2(tags$i(textOutput("score"))),
                              tags$h5(textOutput("info")),
                              tags$h5(textOutput("date"))))
  ),
  
  # yields inputs: playername, h, x_res, y_res
  # calls outputs: matches, team1, lineup1, team2, lineup2, positional
  fluidRow(
    column(width = 4, DTOutput("matches")),
    column(width = 4, 
           tabsetPanel(type = "tabs",
                       tabPanel(title = textOutput("team1"), DTOutput("lineup1")),
                       tabPanel(title = textOutput("team2"), DTOutput("lineup2")))),
    column(width = 4, textInput(inputId = "playername", 
                                value = "Eden Hazard", 
                                label = tags$h5("Input Player:")),
           sliderInput("h", "Bandwidth:", min = 0.001, max = 20, value = 5.2),
           sliderInput("x_res", "x Resolution:", min = 0.2, max = 10, step = 0.2, value = 1),
           sliderInput("y_res", "y Resolution:", min = 0.2, max = 10, step = 0.2, value = 1),
           tags$div(tags$h5("Positions Played:")),
           DTOutput("positional", width = "400px"))
  ),
  
  # yields inputs: field, grid, events, hcl, plotposition
  fluidRow(
    column(width = 1),
    column(width = 2,
           tags$u(tags$h3("Toggle Features:")),
           switchInput(inputId = "field", label = "Field Overlay"),
           switchInput(inputId = "grid", label = "StatsBomb Positional Grid"),
           switchInput(inputId = "events", label = "Events as Points"),
           selectInput(inputId = "hcl", label = "HCL Palette:", selected = "Temps",
                       choices = c("Pastel 1", "Dark 2", "Dark 3", "Set 2",
                                   "Set 3", "Warm", "Cold", "Harmonic", 
                                   "Dynamic", "Grays", "Light Grays", 
                                   "Blues 2", "Blues 3", "Purples 2", 
                                   "Purples 3", "Reds 2", "Reds 3", "Greens 2",
                                   "Greens 3", "Oslo", "Purple-Blue", "Red-Purple",
                                   "Red-Blue", "Purple-Orange", "Purple-Yellow",
                                   "Blue-Yellow", "Green-Yellow", "Red-Yellow",
                                   "Heat", "Heat 2", "Terrain", "Terrain 2",
                                   "Viridis", "Plasma", "Inferno", "Rocket",
                                   "Mako", "Dark Mint", "Mint", "BluGrn", "Teal",
                                   "TealGrn", "Emrld", "BluYl", "ag_GrnYl", "Peach",
                                   "PinkYl", "Burg", "BurgYl", "RedOr", "OrYel",
                                   "Purp", "PurpOr", "Sunset", "Magenta", "SunsetDark",
                                   "ag_Sunset", "BrwnYl", "YlOrRd", "YlOrBr",
                                   "OrRd", "Oranges", "YlGn", "YlGnBu", "Reds",
                                   "RdPu", "PuRd", "Purples", "PuBuGn", "PuBu",
                                   "Greens", "BuGn", "GnBu", "BuPu", "Blues",
                                   "Lajolla", "Turku", "Hawaii", "Batlow", 
                                   "Blue-Red", "Blue-Red 2", "Blue-Red 3",
                                   "Red-Green", "Purple-Green", "Purple-Brown",
                                   "Green-Brown", "Blue-Yellow 2", "Blue-Yellow 3",
                                   "Green-Orange", "Cyan-Magenta", "Tropic", "Broc",
                                   "Cork", "Vik", "Berlin", "Lisbon", "Tofino",
                                   "ArmyRose", "Earth", "Fall", "Geyser", "TealRose",
                                   "Temps", "PuOr", "RdBu", "RdGy", "PiYG", "PRGn", "BrBG", 
                                   "RdYlBu", "RdYlGn", "Spectral", "Zissou 1",
                                   "Cividis", "Roma")),
           selectInput(inputId = "plotposition", label = "Position", 
                       selected = "ALL",
                       choices = c("ALL", "Goalkeeper", "Right Back",
                                   "Right Center Back", "Center Back", 
                                   "Left Center Back", "Left Back",
                                   "Right Wing Back", "Right Defensive Midfield",
                                   "Center Defensive Midfield", "Left Defensive Midfield",
                                   "Left Wing Back", "Right Midfield", "Right Center Midfield",
                                   "Center Midfield", "Left Center Midfield", "Left Midfield",
                                   "Right Wing", "Right Attacking Midfield", "Center Attacking Midfield",
                                   "Left Attacking Midfield", "Left Wing", "Secondary Striker",
                                   "Right Center Forward", "Center Forward", "Left Center Forward"))),
    ### UNCOMMENT for shinyapp.io
    column(width = 6, plotOutput("heatmap1", width = "1000px", height = "667px"))
  ),
  fluidRow(headerPanel(""), headerPanel(""), headerPanel(""))
)

# -----------------------
# SERVER
# -----------------------

server <- function(input, output) {
  
  # reactive function
  # calls input: competition
  get_comp <- reactive({input$competition})
  
  # reactive function
  # updates data (globally) based on inputted competition
  set_DB <- reactive({
    if (get_comp() == "2018 FIFA World Cup"){
      matches_DB <- matches_WC
      lineups_DB <- lineups_WC
      events_DB <- events_WC
    }
    else if (get_comp() == "UEFA Euro 2020"){
      matches_DB <- matches_Euros
      lineups_DB <- lineups_Euros
      events_DB <- events_Euros
    }
    data <- list(matches_DB, lineups_DB, events_DB)
    names(data) <- c("matches", "lineups", "events")
    data
  })
  
  # reactive function
  # calls input: matchid
  getgameid <- reactive({input$matchid})
  
  # reactive function
  # uses matchid to obtain home team name
  team_1 <- reactive({
    data <- set_DB()
    matches_DB <- data$matches
    id <- getgameid()
    req(id)
    temp_team1 <- matches_DB[matches_DB$match_id == id, ]
    temp_team1$home_team.home_team_name
    })
  # yields output: team1
  output$team1 <- renderText({team_1()})
  
  # reactive function
  # uses matchid to obtain away team name
  team_2 <- reactive({
    data <- set_DB()
    matches_DB <- data$matches
    id <- getgameid()
    req(id)
    temp_team2 <- matches_DB[matches_DB$match_id == id, ]
    temp_team2$away_team.away_team_name
  })
  # yields output: team2
  output$team2 <- renderText({team_2()})
  
  # yields output: matches
  output$matches <- renderDT({
    data <- set_DB()
    matches_DB <- data$matches
    matches_display <- matches_DB %>%
      arrange(match_date) %>%
      dplyr::select(match_id, competition_stage.name, home_team.home_team_name, away_team.away_team_name)
    DT::datatable(data = matches_display, rownames = FALSE, 
                  options = list(pageLength = 10, lengthChange = FALSE, search = list(search = "Belgium")),
                  colnames = c("Match ID", "Stage", "Home", "Away"))
  })
  
  # yields output: lineup1
  output$lineup1 <- renderDT({
    data <- set_DB()
    lineups_DB <- data$lineups
    team1 <- team_1()
    game_id <- getgameid()
    req(game_id, team1)
    temp_lineups <- lineups_DB[lineups_DB$match_id == game_id, ]
    temp_team1 <- temp_lineups[temp_lineups$team_name == team1, ]
    temp_players <- temp_team1[[3]][[1]][[2]]
    temp_positions <- temp_team1[[3]][[1]][[6]]
    involve <- rep(NA, length(temp_positions))
    for (i in 1:length(temp_positions)){
      if (length(temp_positions[[i]]) == 0){
        involve[i] <- NA
      }
      else {
        start_reason <- temp_positions[[i]]$start_reason[1]
        if (start_reason == "Starting XI"){
          involve[i] <- start_reason
        }
        else {
          involve[i] <- "Substitute"
        }
      }
    }
    lineup1 <- data.frame(temp_players, involve)
    lineup1 <- lineup1 %>% arrange(involve)
    DT::datatable(data = lineup1, rownames = FALSE, 
                  options = list(pageLength = 11, lengthChange = FALSE, bFilter = 0), 
                  colnames = c(paste(team1, "Players"), "Involvement"))
    })
  
  # yields output: lineup2
  output$lineup2 <- renderDT({
    data <- set_DB()
    lineups_DB <- data$lineups
    team2 <- team_2()
    game_id <- getgameid()
    req(game_id, team2)
    temp_lineups <- lineups_DB[lineups_DB$match_id == game_id, ]
    temp_team2 <- temp_lineups[temp_lineups$team_name == team2, ]
    temp_players <- temp_team2[[3]][[1]][[2]]
    temp_positions <- temp_team2[[3]][[1]][[6]]
    involve <- rep(NA, length(temp_positions))
    for (i in 1:length(temp_positions)){
      if (length(temp_positions[[i]]) == 0){
        involve[i] <- NA
      }
      else {
        start_reason <- temp_positions[[i]]$start_reason[1]
        if (start_reason == "Starting XI"){
          involve[i] <- start_reason
        }
        else {
          involve[i] <- "Substitute"
        }
      }
    }
    lineup2 <- data.frame(temp_players, involve)
    lineup2 <- lineup2 %>% arrange(involve)
    DT::datatable(data = lineup2, rownames = FALSE, 
                  options = list(pageLength = 11, lengthChange = FALSE, bFilter = 0),
                  colnames = c(paste(team2, "Players"), "Involvement"))
    })
  
  # reactive function
  # uses matchid to get summary information about match
  overview <- reactive({
    data <- set_DB()
    matches_DB <- data$matches
    game_id <- getgameid()
    req(game_id)
    match_of_focus <- matches_DB %>%
      filter(match_id == game_id)
    home <- match_of_focus$home_team.home_team_name
    away <- match_of_focus$away_team.away_team_name
    date <- match_of_focus$match_date
    stage <- match_of_focus$competition_stage.name
    scorehome <- match_of_focus$home_score
    scoreaway <- match_of_focus$away_score
    ref <- match_of_focus$referee.name
    stadium <- gsub("Kazan..", "Kazan", match_of_focus$stadium.name)
    c(home, away, date, stage, scorehome, scoreaway, ref, stadium)
  })
  # yields output: date
  output$date <- renderText({paste0(overview()[4], ", ", overview()[8], " (", overview()[3], ")")})
  # yields output: info
  output$info <- renderText({paste0("Match Official: ", overview()[7])})
  # yields output: score
  output$score <- renderText({paste(overview()[1], overview()[5], "-",
                                    overview()[6], overview()[2])})
  
  # reactive function
  # calls input: playername
  player_name <- reactive({input$playername})
  
  # reactive function
  # gets positions player played in game of focus
  positiondb <- reactive({
    data <- set_DB()
    lineups_DB <- data$lineups
    team1 <- team_1()
    req(team1)
    team2 <- team_2()
    req(team2)
    game_id <- getgameid()
    req(game_id)
    temp_lineups <- lineups_DB[lineups_DB$match_id == game_id, ]
    temp_team1 <- temp_lineups[temp_lineups$team_name == team1, ]
    temp_players1 <- temp_team1[[3]][[1]][[2]]
    temp_team2 <- temp_lineups[temp_lineups$team_name == team2, ]
    temp_players2 <- temp_team2[[3]][[1]][[2]]
    playername <- player_name()
    req(playername)
    if (playername %in% temp_players1){
      temp_team <- temp_team1[[3]][[1]]
    }
    else if (playername %in% temp_players2){
      temp_team <- temp_team2[[3]][[1]]
    }
    else {
      return()
      # stop("Enter Valid Player Name & Match ID")
    }
    temp_player <- temp_team[temp_team$player_name == playername, ]
    positions <- as.data.frame(temp_player[[6]])[, c(2, 3, 4, 7)]
  })
  # yields output: positional
  output$positional <- renderDataTable({
    positions <- positiondb()
    req(positions)
    DT::datatable(data = positions, rownames = FALSE,
                  options = list(pageLength = 5, lengthChange = FALSE, bFilter = 0),
                  colnames = c("Position", "Start", "End", "Shift"))
    })
  
  # reactive function
  # gets player event data for match of focus
  # calls input: plotposition
  player_data <- reactive({
    data <- set_DB()
    events_DB <- data$events
    playername <- player_name()
    game_id <- getgameid()
    positions <- positiondb()
    req(positions)
    req(game_id)
    req(playername)
    req(input$plotposition)
    positions <- positions[, 1]
    # If "ALL", return all event data for player
    if (input$plotposition == "ALL") {
      player <- events_DB %>%
        filter(player.name == playername) %>%
        filter(match_id == game_id) %>%
        dplyr::select(player.name, minute, second, possession, duration,
                      location, location.x, location.y, type.name,
                      possession_team.name, play_pattern.name, position.name,
                      pass.end_location, pass.recipient.name, pass.height.name,
                      pass.body_part.name, pass.outcome.name, carry.end_location,
                      carry.end_location.x, carry.end_location.y, shot.end_location,
                      shot.freeze_frame, shot.body_part.name, shot.technique.name,
                      shot.type.name, dribble.outcome.name, id)
    }
    # If player did not play in specified position, return error
    else if (!(input$plotposition %in% positions)){
      return()
      # stop("Player did not play in this position")
    }
    # Else, return event data for when player played in this position
    else {
      player <- events_DB %>%
        filter(player.name == playername) %>%
        filter(match_id == game_id) %>%
        filter(position.name == input$plotposition) %>%
        dplyr::select(player.name, minute, second, possession, duration,
                      location, location.x, location.y, type.name,
                      possession_team.name, play_pattern.name, position.name,
                      pass.end_location, pass.recipient.name, pass.height.name,
                      pass.body_part.name, pass.outcome.name, carry.end_location,
                      carry.end_location.x, carry.end_location.y, shot.end_location,
                      shot.freeze_frame, shot.body_part.name, shot.technique.name,
                      shot.type.name, dribble.outcome.name, id)
    }
    # For interactivity -- TBD
    player <- player %>%
      mutate(plot_text = paste0("<b>", type.name, "</b>", "<br>",
                                "Time: Minute ", minute, ", Second ", second, "<br>",
                                "Playing: ", position.name, "<br>",
                                "Duration: ", duration))
    player
  })
  
  # -----------------------
  # HEATMAP CREATION
  # -----------------------
  
  # reactive function
  # calls input: hcl
  hcl_palette <- reactive({input$hcl})
  
  # reactive function
  # creates heatmap data using `heatmap_KDE()`
  # calls input: h, x_res, y_res
  # creates plot of heatmap
  # calls input: plotposition, field, grid, events
  heatmap <- reactive({
    set_DB()
    comp <- get_comp()
    req(comp)
    player <- player_data()
    req(player)
    playername <- player_name()
    req(playername)
    gameoverview <- overview()
    req(gameoverview)
    X_obs <- player$location.x
    Y_obs <- player$location.y
    temp <- data.frame(X_obs, Y_obs)
    player <- player[complete.cases(temp),]
    X_obs <- player$location.x
    Y_obs <- player$location.y
    heatmap_temp <- heatmap_KDE(X = X_obs, Y = Y_obs, h = input$h, 
                                res_x = input$x_res, res_y = input$y_res)
    heatmap_temp[heatmap_temp < 0.00001] <- NA
    heatmap_df <- data.frame(expand.grid(x, y), as.vector(heatmap_temp))
    colnames(heatmap_df) <- c("x", "y", "z")
    p <- ggplot(data = heatmap_df) +
      geom_raster(aes(x = x, y = y, fill = z)) +
      scale_fill_gradientn(colours = hcl.colors(12, hcl_palette()), 
                           na.value = "transparent") +
      scale_y_continuous(trans = "reverse") +
      xlab("Attacking Left to Right") +
      ylab("") + 
      theme(panel.grid.major = element_blank()) +
      theme(panel.grid.minor = element_blank()) +
      theme(axis.title.x = element_text(face = "italic", size = 20))
    req(input$plotposition)
    if (input$plotposition == "ALL"){
      p <- p + ggtitle(paste0(playername, ": ", gameoverview[1], " v. ", gameoverview[2], "\n", comp))
    }
    else {
      p <- p + ggtitle(paste0(playername, " (at ", input$plotposition, "): "
                              , gameoverview[1], " v. ", gameoverview[2],
                              "\n", comp))
    }
    p <- p + theme(plot.title = element_text(size = 30)) +
      labs(fill = "Activity Level")
    if (input$field == TRUE){
      p <- p + theme(panel.background = element_rect(fill = "#3ab54a")) + 
        annotate("rect", xmin = 0, xmax = 120, ymin = 0, ymax = 80,
                 fill = NA, colour = "white", size = 1) + 
        annotate("rect", xmin = 0, xmax = 60, ymin = 0, ymax = 80,
                 fill = NA, colour = "white", size = 0.3) + 
        annotate("rect", xmin = -2, xmax = 0, ymin = 36, ymax = 44, 
                 fill = NA, colour = "white", size = 0.3) +
        annotate("rect", xmin = 0, xmax = 18, ymin = 18, ymax = 62,
                 fill = NA, colour = "white", size = 0.3) + 
        annotate("rect", xmin = 0, xmax = 6, ymin = 30, ymax = 50,
                 fill = NA, colour = "white", size = 0.3) + 
        annotate("point", x = 12, y = 40, colour = "white", size = 2) +
        annotate("rect", xmin = 60, xmax = 120, ymin = 0, ymax = 80,
                 fill = NA, colour = "white", size = 0.3) + 
        annotate("rect", xmin = 102, xmax = 120, ymin = 18, ymax = 62,
                 fill = NA, colour = "white", size = 0.3) + 
        annotate("rect", xmin = 114, xmax = 120, ymin = 30, ymax = 50,
                 fill = NA, colour = "white", size = 0.3) +
        annotate("rect", xmin = 120, xmax = 122, ymin = 36, ymax = 44,
                 fill = NA, colour = "white", size = 0.3) + 
        annotate("point", x = 108, y = 40, colour = "white", size = 2) +
        annotate("point", x = 60, y = 40, colour = "white", size = 2) +
        annotate("path", colour = "white", size = 0.3,
                 x = 60 + 10*cos(seq(0, 2*pi, length.out = 2000)),
                 y = 40 + 10*sin(seq(0, 2*pi, length.out = 2000))) +
        annotate("path", colour = "white", size = 0.3,
                 x = 108 + 10*cos(seq(0.7*pi, 1.3*pi, length.out = 30)),
                 y = 40 + 10*sin(seq(0.7*pi, 1.3*pi, length.out = 30))) +
        annotate("path", colour = "white", size = 0.3,
                 x = 12 + 10*cos(seq(-0.3*pi, 0.3*pi, length.out = 30)),
                 y = 40 + 10*sin(seq(-0.3*pi, 0.3*pi, length.out = 30)))
    }
    if (input$grid == TRUE){
      p <- p + annotate("text", x = 8, y = 40, label = "GK", size = 10, alpha = 0.5, fontface = "bold") +
        annotate("text", x = 25, y = 10, label = "LB", size = 10, alpha = 0.5, fontface = "bold") + 
        annotate("text", x = 25, y = 25, label = "LCB", size = 10, alpha = 0.5, fontface = "bold") + 
        annotate("text", x = 25, y = 40, label = "CB", size = 10, alpha = 0.5, fontface = "bold") + 
        annotate("text", x = 25, y = 55, label = "RCB", size = 10, alpha = 0.5, fontface = "bold") + 
        annotate("text", x = 25, y = 70, label = "RB", size = 10, alpha = 0.5, fontface = "bold") + 
        annotate("text", x = 40, y = 10, label = "LWB", size = 10, alpha = 0.5, fontface = "bold") + 
        annotate("text", x = 40, y = 25, label = "LDM", size = 10, alpha = 0.5, fontface = "bold") + 
        annotate("text", x = 40, y = 40, label = "CDM", size = 10, alpha = 0.5, fontface = "bold") + 
        annotate("text", x = 40, y = 55, label = "RDM", size = 10, alpha = 0.5, fontface = "bold") + 
        annotate("text", x = 40, y = 70, label = "RWB", size = 10, alpha = 0.5, fontface = "bold") + 
        annotate("text", x = 62, y = 10, label = "LM", size = 10, alpha = 0.5, fontface = "bold") + 
        annotate("text", x = 62, y = 25, label = "LCM", size = 10, alpha = 0.5, fontface = "bold") + 
        annotate("text", x = 62, y = 40, label = "CM", size = 10, alpha = 0.5, fontface = "bold") + 
        annotate("text", x = 62, y = 55, label = "RCM", size = 10, alpha = 0.5, fontface = "bold") + 
        annotate("text", x = 62, y = 70, label = "RM", size = 10, alpha = 0.5, fontface = "bold") + 
        annotate("text", x = 80, y = 10, label = "LW", size = 10, alpha = 0.5, fontface = "bold") + 
        annotate("text", x = 80, y = 25, label = "LAM", size = 10, alpha = 0.5, fontface = "bold") + 
        annotate("text", x = 80, y = 40, label = "CAM", size = 10, alpha = 0.5, fontface = "bold") + 
        annotate("text", x = 80, y = 55, label = "RAM", size = 10, alpha = 0.5, fontface = "bold") + 
        annotate("text", x = 80, y = 70, label = "RW", size = 10, alpha = 0.5, fontface = "bold") + 
        annotate("text", x = 95, y = 40, label = "SS", size = 10, alpha = 0.5, fontface = "bold") + 
        annotate("text", x = 105, y = 25, label = "LCF", size = 10, alpha = 0.5, fontface = "bold") + 
        annotate("text", x = 105, y = 40, label = "CF", size = 10, alpha = 0.5, fontface = "bold") + 
        annotate("text", x = 105, y = 55, label = "RCF", size = 10, alpha = 0.5, fontface = "bold")
    }
    if (input$events == TRUE){
      p <- p + geom_point(data = player, aes(x = location.x, y = location.y),
                          size = 4, alpha = 0.4,  colour = "white") +
        geom_point(data = player, aes(x = location.x, y = location.y), size = 2)
    }
    p
  })
  # yields output: heatmap1
  output$heatmap1 <- renderPlot({heatmap()})
}

# Run the application 
shinyApp(ui = ui, server = server, options = list(height = 2000, width = 1200))
