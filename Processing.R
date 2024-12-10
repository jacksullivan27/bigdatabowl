setwd("C:/Users/jsull/Python Projects/big-data-bowl-2025")

# import libraries
library(dplyr)
library(readr)
library(rvest)
library(stringr)
library(ggplot2)

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


# GENERALIZE TO GET MOTION PLAYER TRACKING FOR EVERY WEEK

all_filtered_data <- vector("list", 9)  # Initialize an empty list

# for (i in 1:9) {
#   file_path <- paste0("data/tracking_week_", i, ".csv")  # Construct the file path
#   tracking <- read_csv(file_path)                        # Read the CSV file
#   
#   first_join <- motion_players %>% 
#     left_join(tracking, by = c("gameId", "playId", "nflId"))  # Join with motion_players
#   
#   lineset_frames <- first_join %>%
#     filter(event == "line_set") %>%
#     group_by(gameId, playId, nflId) %>%
#     summarize(line_set_frame = min(frameId), .groups = "drop")
#   
#   # Filter to get records starting at "line_set"
#   filtered_data <- first_join %>%
#     left_join(lineset_frames, by = c("gameId", "playId", "nflId")) %>%
#     filter(frameId >= line_set_frame) %>%
#     filter(frameType %in% c("BEFORE_SNAP", "SNAP"))
#   
#   # Store filtered data for each week
#   all_filtered_data[[i]] <- filtered_data
# }

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

