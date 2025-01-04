setwd("C:/Users/jsull/Python Projects/big-data-bowl-2025/bigdatabowl")
library(sportyR)
library(ggplot2)
library(gganimate)
library(readr)
library(dplyr)

animate_play <- function(week, game, play, home_color, visitor_color){
  file <- paste("data/tracking_week_", week, ".csv", sep = "")
  week_data <- read_csv(file)
  games <- read_csv("data/games.csv")

  play <- week_data %>%
    filter(gameId == game & playId == play) %>%
    left_join(games, by = "gameId") %>%
    mutate(
      team = case_when(
        club == homeTeamAbbr ~ "home",
        club == visitorTeamAbbr ~ "away",
        club == "football" ~ "football"
      )
    ) %>%
    select(time, x, y, s, a, dis, o, dir, event, nflId, displayName, jerseyNumber,
           frameId, team, gameId, playId, playDirection)# %>%  # missing route and jersey num
    # filter(frameId%%2==0)


  # Prep data for plotting
  play[play["team"] == "home", "color"] <- home_color
  play[play["team"] == "away", "color"] <- visitor_color
  play[play["team"] == "football", "color"] <- "#624a2e"

  # Create the field
  nfl_field <- geom_football("nfl", x_trans = 60, y_trans = 26.6667)

  # Add the points on the field
  play_anim <- nfl_field +
    geom_point(
      data = play,
      aes(x, y),
      color = play$color
    ) +
    transition_time(play$frameId) +
    ease_aes('linear')

  return(play_anim)
}

animate_presnap <- function(week, game, play, home_color, visitor_color) {
  file <- paste("data/tracking_week_", week, ".csv", sep = "")
  week_data <- read_csv(file)
  games <- read_csv("data/games.csv")
  
  # Filter play data
  play_data <- week_data %>% 
    filter(gameId == game & playId == play) %>% 
    left_join(games, by = "gameId") %>% 
    mutate(
      team = case_when(
        club == homeTeamAbbr ~ "home",
        club == visitorTeamAbbr ~ "away",
        club == "football" ~ "football"
      )
    ) %>% 
    select(time, x, y, s, a, dis, o, dir, event, nflId, displayName, jerseyNumber,
           frameId, team, gameId, playId, playDirection, frameType)
  
  # Extract the line_set frames
  lineset_frames <- play_data %>%
    filter(event == "line_set") %>%
    group_by(gameId, playId) %>%
    summarize(line_set_frame = min(frameId), .groups = "drop")
  
  # Filter to presnap motion (line_set to snap)
  filtered_data <- play_data %>%
    left_join(lineset_frames, by = c("gameId", "playId")) %>%
    filter(frameId >= line_set_frame) %>%
    filter(frameType %in% c("BEFORE_SNAP", "SNAP"))
  
  # Assign colors for teams
  filtered_data[filtered_data["team"] == "home", "color"] <- home_color
  filtered_data[filtered_data["team"] == "away", "color"] <- visitor_color
  filtered_data[filtered_data["team"] == "football", "color"] <- "#624a2e"
  
  # Create the field
  nfl_field <- geom_football("nfl", x_trans = 60, y_trans = 26.6667)
  
  # Add the points on the field and animate
  play_anim <- nfl_field +
    geom_point(
      data = filtered_data,
      aes(x, y),
      color = filtered_data$color
    ) +
    transition_time(filtered_data$frameId) +
    ease_aes('linear')
  
  return(play_anim)
}

# animate_presnap(2, 2022091800, 1108, "purple", "aquamarine")
