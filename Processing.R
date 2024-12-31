setwd("C:/Users/jsull/Python Projects/big-data-bowl-2025/bigdatabowl")

# import libraries
library(dplyr)
library(tidyr)
library(readr)
library(rvest)
library(stringr)
library(ggplot2)
library(purrr)

# read the raw data
plays <-  read_csv("data/plays.csv")
player_play <- read_csv("data/player_play.csv")
games<- read_csv("data/games.csv")
players <-  read_csv("data/players.csv")
week1 <- read_csv("data/tracking_week_1.csv")

# grab all players who went into motion; includes gameId, playId, and nflId
motion_players <- player_play %>%
  select(gameId, playId, nflId, inMotionAtBallSnap, motionSinceLineset) %>% 
  filter((inMotionAtBallSnap == TRUE) | (motionSinceLineset == TRUE)) # only keep records of players in motion sometime after line set

# remove some unnecessary columns
game_data <- games %>% 
  select(gameId, week, homeTeamAbbr, visitorTeamAbbr, homeFinalScore, visitorFinalScore)

# offensive player detail
offensive_play_details <- player_play %>% 
  select(gameId, playId, nflId, teamAbbr, hadRushAttempt, hadPassReception, rushingYards, receivingYards, wasTargettedReceiver, yardageGainedAfterTheCatch)

# only get relevant player data
players_info <- players %>% 
  select(nflId, position, displayName)


# JOINS TO GET PLAYERS IN MOTION FOR WEEK 1 FROM LINESET TO SNAP
test_join <- motion_players %>% 
  left_join(week1, by=c("gameId", "playId", "nflId"))

lineset_frames <- test_join %>%
  filter(event == "line_set") %>%
  group_by(gameId, playId, nflId) %>%
  summarize(line_set_frame = min(frameId), .groups = "drop")

# Step 2: Join this result back to the original dataframe to filter records
filtered_data <- test_join %>%
  left_join(lineset_frames, by = c("gameId", "playId", "nflId")) %>%
  filter(frameId >= line_set_frame) %>% 
  filter(frameType == "BEFORE_SNAP" | frameType == "SNAP")

#===========================================================================================================================================
#===========================================================================================================================================

# GENERALIZE TO GET MOTION PLAYER TRACKING FOR EVERY WEEK

all_filtered_data <- vector("list", 9)  # Initialize an empty list


library(foreach)
library(doParallel)

# Set up parallel backend
num_cores <- parallel::detectCores() - 1  # Use available cores minus 1
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Parallelized loop with foreach
all_filtered_data <- foreach(i = 1:9, .packages = c("dplyr", "readr")) %dopar% {
  file_path <- paste0("data/tracking_week_", i, ".csv")  # Construct the file path
  tracking <- read_csv(file_path)                        # Read the CSV file
  
  first_join <- motion_players %>% 
    left_join(tracking, by = c("gameId", "playId", "nflId"))  # Join with motion_players
  
  lineset_frames <- first_join %>%
    filter(event == "line_set") %>%
    group_by(gameId, playId, nflId) %>%
    summarize(line_set_frame = min(frameId), .groups = "drop")
  
  # Filter to get records starting at "line_set"
  filtered_data <- first_join %>%
    left_join(lineset_frames, by = c("gameId", "playId", "nflId")) %>%
    filter(frameId >= line_set_frame) %>%
    filter(frameType %in% c("BEFORE_SNAP", "SNAP"))
  
  return(filtered_data)  # Return filtered data
}
  
# Combine all weeks into a single dataframe
motion_players_tracking <- bind_rows(all_filtered_data)


motion_plays <- motion_players %>% 
  select(gameId, playId) %>% 
  distinct()


# qb_function <- function(filename) {
#   week_data <- read_csv(filename)
# 
#   qb_ball_data <- motion_plays %>%
#     left_join(week_data, by = c("gameId", "playId")) %>%
#     left_join(players_info %>% select(nflId, position), by = "nflId") %>%
#     filter(frameType == "SNAP", position == "QB" | displayName == "football") %>%
#     mutate(position = replace_na(position, "ball")) %>%
#     select(gameId, playId, position, x, y) %>%
#     group_by(gameId, playId) %>%
#     pivot_wider(
#       names_from = position,
#       values_from = c(x, y),
#       names_glue = "{position}_{.value}"
#     ) %>%
#     filter(sapply(QB_x, length) == 1) %>%
#     ungroup() %>% 
#     select(gameId, playId, QB_x, QB_y, ball_x, ball_y)
# 
# 
#   return(qb_ball_data)
# }
# 
# 
# qb_ball_data_combined <- data.frame(
#   gameId = numeric(),
#   playId = numeric(),
#   QB_x = numeric(),
#   QB_y = numeric(),
#   ball_x = numeric(),
#   ball_y = numeric()
# )
# 
# 
# for (week in 1:9){
#   file = paste("data/tracking_week_", week, ".csv", sep = "")
#   qb_ball_data <- qb_function(file)
#   qb_ball_data_combined <- bind_rows(qb_ball_data_combined, data.frame(
#       gameId = qb_ball_data$gameId,
#       playId = qb_ball_data$playId,
#       QB_x = unlist(qb_ball_data$QB_x),
#       QB_y = unlist(qb_ball_data$QB_y),
#       ball_x = unlist(qb_ball_data$ball_x),
#       ball_y = unlist(qb_ball_data$ball_y)
#       ))
# }

write_csv(qb_ball_data_combined, "qb_ball_coords_motion_plays.csv")

qb_ball_data <- read_csv("qb_ball_coords_motion_plays.csv") %>% distinct()

all_tracking_motion <- motion_players_tracking  %>% 
  left_join(qb_ball_data, by=c("gameId", "playId"), relationship = "many-to-many") %>% 
  left_join(players_info %>% select("nflId", "position"), by="nflId") %>%
  distinct()

#=====================================================================================================
#=====================================================================================================
# Classifying motion

test <- all_tracking_motion %>% 
  filter(gameId == 2022090800 & playId == 56)

# Create the field
nfl_field <- geom_football("nfl", x_trans = 60, y_trans = 26.6667)


plot_motion <- function(df){
  nfl_field +
    geom_point(data = df, aes(x = x, y = y), color = "blue", size = 3) + # Motion man
    geom_point(data = df, aes(x = QB_x, y = QB_y), color = "red", size = 3) + # QB
    geom_point(data = df, aes(x = ball_x, y = ball_y), color = "#624a2e", size = 3) + # Ball
    labs(x = "X Coordinate", y = "Y Coordinate") +
    theme(legend.position = "bottom") +
    annotate("text", x = 10, y = 5, label = "Legend: Blue = Motion Man, Red = QB, Brown = Ball")
}

plot_motion(test)
