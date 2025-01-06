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
  plays <- read_csv("data/plays.csv") %>% select(gameId, playId, defensiveTeam)
  players <- read_csv("data/players.csv")
  
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
           frameId, team, gameId, playId, playDirection, club) %>%  # missing route and jersey num
    left_join(plays, by = c("gameId", "playId"))

  
  # Prep data for plotting
  play[play["team"] == "home", "color"] <- home_color
  play[play["team"] == "away", "color"] <- visitor_color
  play[play["team"] == "football", "color"] <- "#624a2e"
  
  
  # Filter defensive players (not home, not football)
  defense <- play %>%
    left_join(players %>% select(nflId, position), by = "nflId") %>% 
    filter(club == defensiveTeam) %>%  # Adjust filter condition if necessary
    filter(!(position %in% c("DE", "DT", "NT"))) %>% 
    group_by(frameId) %>%
    summarize(
      avg_x = mean(x, na.rm = TRUE),
      avg_y = mean(y, na.rm = TRUE)
    )
  
  # Create the field
  nfl_field <- geom_football("nfl", x_trans = 60, y_trans = 26.6667)
  
  # Create the animation
  play_anim <- nfl_field +
    geom_point(
      data = play,
      aes(x = x, y = y),
      color = play$color
    ) +
    geom_point(
      data = defense,
      aes(x = avg_x, y = avg_y),
      color = "black",
      shape = "x",  # Red "x" for defense center of motion
      size = 4
    ) +
    transition_time(frameId) +
    ease_aes('linear')

  return(play_anim)
}

animate_presnap <- function(week, game, play, home_color, visitor_color) {
  file <- paste("data/tracking_week_", week, ".csv", sep = "")
  week_data <- read_csv(file)
  games <- read_csv("data/games.csv")
  plays <- read_csv("data/plays.csv") %>% select(gameId, playId, defensiveTeam)
  players <- read_csv("data/players.csv")
  
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
           frameId, team, gameId, playId, playDirection, club, frameType) %>%  # missing route and jersey num
    # filter(frameId%%2==0)
    left_join(plays, by = c("gameId", "playId"))

  
  # Prep data for plotting
  play[play["team"] == "home", "color"] <- home_color
  play[play["team"] == "away", "color"] <- visitor_color
  play[play["team"] == "football", "color"] <- "#624a2e"
  
  
  # Filter defensive players (not home, not football)
  defense <- play %>%
    left_join(players %>% select(nflId, position), by = "nflId") %>% 
    filter(club == defensiveTeam) %>%  # Adjust filter condition if necessary
    filter(!(position %in% c("DE", "DT", "NT"))) %>% 
    group_by(frameId) %>%
    summarize(
      gameId,
      playId,
      avg_x = mean(x, na.rm = TRUE),
      avg_y = mean(y, na.rm = TRUE)
    ) %>% 
    distinct()
  
  # Extract the line_set frames
  lineset_frames <- play %>%
    filter(event == "line_set"| frameType == "SNAP") %>%
    group_by(gameId, playId) %>%
    summarize(
      line_set_frame = min(frameId[event == "line_set"], na.rm = TRUE),
      snap_frame = min(frameId[frameType == "SNAP"], na.rm = TRUE),
      .groups = "drop"
    )
  
  play <- play %>% 
    left_join(lineset_frames, by = c("gameId", "playId")) %>%
    filter(
      frameId >= line_set_frame & 
      frameId <= snap_frame + 7
    )
  
  defense <- defense %>% 
    left_join(lineset_frames, by = c("gameId", "playId")) %>%
    filter(
      frameId >= line_set_frame & 
        frameId <= snap_frame + 7
    )
  # Create the field
  nfl_field <- geom_football("nfl", x_trans = 60, y_trans = 26.6667)
  
  # Create the animation
  play_anim <- nfl_field +
    geom_point(
      data = play,
      aes(x = x, y = y),
      color = play$color
    ) +
    geom_point(
      data = defense,
      aes(x = avg_x, y = avg_y),
      color = "black",
      shape = "x",  # Red "x" for defense center of motion
      size = 4
    ) +
    transition_time(frameId) +
    ease_aes('linear')

  return(play_anim)
}

animate_presnap <- function(week, game, play, home_color, visitor_color) {
  file <- paste("data/tracking_week_", week, ".csv", sep = "")
  week_data <- read_csv(file)
  games <- read_csv("data/games.csv")
  plays <- read_csv("data/plays.csv") %>% select(gameId, playId, defensiveTeam)
  players <- read_csv("data/players.csv")
  
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
           frameId, team, gameId, playId, playDirection, club, frameType) %>%
    left_join(plays, by = c("gameId", "playId"))
  
  # Assign colors
  play[play["team"] == "home", "color"] <- home_color
  play[play["team"] == "away", "color"] <- visitor_color
  play[play["team"] == "football", "color"] <- "#624a2e"
  
  # Filter defensive players
  defense <- play %>%
    left_join(players %>% select(nflId, position), by = "nflId") %>%
    filter(club == defensiveTeam) %>%
    filter(!(position %in% c("DE", "DT", "NT"))) %>%
    group_by(frameId) %>%
    summarize(
      gameId,
      playId,
      avg_x = mean(x, na.rm = TRUE),
      avg_y = mean(y, na.rm = TRUE)
    ) %>%
    distinct()
  
  # Line set and snap frames
  lineset_frames <- play %>%
    filter(event == "line_set" | frameType == "SNAP") %>%
    group_by(gameId, playId) %>%
    summarize(
      line_set_frame = min(frameId[event == "line_set"], na.rm = TRUE),
      snap_frame = min(frameId[frameType == "SNAP"], na.rm = TRUE),
      .groups = "drop"
    )
  
  play <- play %>%
    left_join(lineset_frames, by = c("gameId", "playId")) %>%
    filter(
      frameId >= line_set_frame &
        frameId <= snap_frame + 7
    )
  
  defense <- defense %>%
    left_join(lineset_frames, by = c("gameId", "playId")) %>%
    filter(
      frameId >= line_set_frame &
        frameId <= snap_frame + 7
    )
  
  # Create the field
  nfl_field <- geom_football("nfl", x_trans = 60, y_trans = 26.6667)
  
  # Create the animation
  play_anim <- nfl_field +
    geom_point(
      data = play,
      aes(
        x = x, y = y, color = team
      ),
      size = 2
    ) +
    geom_point(
      data = defense,
      aes(x = avg_x, y = avg_y, shape = "defense"),
      color = "black",
      size = 4
    ) +
    scale_color_manual(
      values = c(home = home_color, away = visitor_color, football = "#624a2e")
    ) +
    scale_shape_manual(
      values = c(defense = 4),
      labels = c(defense = "Defensive Centroid")
    ) +
    guides(
      shape = guide_legend(title = NULL)
    ) +
    transition_time(frameId) +
    ease_aes('linear')
  
  return(play_anim)
}


# animate_presnap(2, 2022091800, 1108, "purple", "aquamarine")
