library(dplyr)


qbballcoords <- read.csv("qb_ball_coords_motion_plays (1).csv")
motion_players <- read.csv("motion_players_tracking_data.csv")

all_tracking_motion <- qbballcoords %>% 
  left_join(motion_players, by=c("gameId", "playId"), relationship = "many-to-many")

tracking_motion_labeled <- all_tracking_motion %>%
  left_join(plays, by = c("gameId", "playId"))

# Filter dataset for frames between "man_in_motion" and "ball_snap"


tracking_motion_labeled <- tracking_motion_labeled %>%
  group_by(gameId, playId) %>%
  filter(any(event == "ball_snap") & (any(event == "man_in_motion") | any(event == "shift"))) %>%
  ungroup()

classify_motion <- function(motion_data) {
  # Extract play direction and motion characteristics
  play_direction <- first(motion_data$playDirection)  # "left" or "right"
  
  # Adjust x-axis based on play direction
  adjust_x <- if_else(play_direction == "left", -1, 1)  # Multiplier for x logic
  
  # Filter motion data between "man_in_motion" and "ball_snap"
  motion_segment <- motion_data %>%
    filter(event %in% c("man_in_motion", "ball_snap") | 
             between(frameId, 
                     min(frameId[event == "man_in_motion"]), 
                     max(frameId[event == "ball_snap"])
             ))
  
  # Calculate total distance traveled
  motion_segment <- motion_segment %>%
    arrange(frameId) %>%
    mutate(
      distance_step = sqrt((x - lag(x, default = x[1]))^2 + (y - lag(y, default = y[1]))^2),
      time_step = frameId - lag(frameId, default = frameId[1])
    )
  
  total_distance <- sum(motion_segment$distance_step, na.rm = TRUE)
  total_time <- max(motion_segment$frameId, na.rm = TRUE) - min(motion_segment$frameId, na.rm = TRUE)
  average_speed <- ifelse(total_time > 0, total_distance / total_time, NA)
  
  motion_summary <- motion_data %>%
    summarise(
      # Get start coordinates (at man_in_motion or shift event)
      start_x = motion_data %>%
        filter(event == "man_in_motion" | event == "shift") %>%
        pull(x) %>%
        first(),
      start_y = motion_data %>%
        filter(event == "man_in_motion" | event == "shift") %>%
        pull(y) %>%
        first(),
      
      # Get end coordinates (at ball_snap event)
      end_x = motion_data %>%
        filter(event == "ball_snap") %>%
        pull(x) %>%
        first(),
      end_y = motion_data %>%
        filter(event == "ball_snap") %>%
        pull(y) %>%
        first(),
      
      # Get QB and ball coordinates
      offenseFormation = first(offenseFormation),
      qb_snap_x = first(QB_x),
      qb_snap_y = first(QB_y),
      ball_snap_x = first(ball_x),
      ball_snap_y = first(ball_y),
      
      # Calculate deltas
      delta_x = (end_x - start_x) * adjust_x,  # Adjust for play direction
      delta_y = end_y - start_y,
      
      # Calculate half the distance to the center/ball
      distance_to_center = abs(start_y - ball_snap_y) - 3.5,
      
      # Maximum y-distance traveled
      max_y_distance = max(abs(y - start_y), na.rm = TRUE),
      
      # Check if player crossed the ball's position (crossed_center)
      crossed_center = if (!is.na(ball_snap_y)) {
        if (start_y < ball_snap_y) {
          any(y > ball_snap_y)
        } else {
          any(y < ball_snap_y)
        }
      } else {
        FALSE
      },
      
      # Check if player goes behind QB
      behind_qb = any((x - qb_snap_x) * adjust_x < 0),
      
      # Check for direction changes in y-coordinate (sideline to sideline movement)
      direction_change = if (nrow(motion_data) > 2) {
        any(diff(sign(diff(y))) != 0)
      } else {
        FALSE
      }
    )
  
  # Classification logic
  motion_type <- case_when(
    # Out and In motions (check max_y_distance and direction)
    !motion_summary$crossed_center & motion_summary$delta_y > 0 & 
      motion_summary$max_y_distance <= motion_summary$distance_to_center ~ "Out",
    !motion_summary$crossed_center & motion_summary$delta_y < 0 & 
      motion_summary$max_y_distance <= motion_summary$distance_to_center ~ "In",
    
    # Shotgun/Pistol/Empty formations
    motion_summary$offenseFormation %in% c("SHOTGUN", "EMPTY", "PISTOL") & 
      motion_summary$behind_qb ~ "Orbit",
    motion_summary$offenseFormation %in% c("SHOTGUN", "EMPTY", "PISTOL") & 
      !motion_summary$behind_qb & motion_summary$crossed_center ~ "Fly",
    motion_summary$offenseFormation %in% c("SHOTGUN", "EMPTY", "PISTOL") & 
      !motion_summary$behind_qb & !motion_summary$crossed_center ~ "Jet",
    
    # Under Center formations
    motion_summary$offenseFormation %in% c("I_FORM", "SINGLEBACK") & 
      motion_summary$behind_qb & motion_summary$crossed_center ~ "Fly",
    motion_summary$offenseFormation %in% c("I_FORM", "SINGLEBACK") & 
      motion_summary$behind_qb & !motion_summary$crossed_center ~ "Jet",
    
    TRUE ~ "Unknown"  # Default case
  )
  
  return(data.frame(motion_type, total_distance, average_speed))
}




classified_motions<- tracking_motion_labeled %>%
  group_by(gameId, playId, nflId) %>%
  summarise(
    motion_metrics = classify_motion(cur_data()), # Apply the function to each group
    .groups = "drop"
  ) %>%
  unnest(cols = motion_metrics) # Expand the dataframe from the returned data.frame


joinedmotions <- plays %>%
  left_join(classified_motions, by = c("gameId", "playId"))

joinedmotions <- joinedmotions %>%
  filter(motion_type != "Unknown")

epa_by_motion <- joinedmotions %>%
  group_by(motion_type) %>%
  summarise(
    mean_epa = mean(expectedPointsAdded, na.rm = TRUE),
    median_epa = median(expectedPointsAdded, na.rm = TRUE),
    sd_epa = sd(expectedPointsAdded, na.rm = TRUE),
    count = n(),  # Number of plays per motion type
    .groups = "drop"  # Ungroup the data for a flat dataframe
  )

print(epa_by_motion)

# View the classified motions
print(classified_motions)



motions_per_play <- player_play %>%
  group_by(gameId, playId) %>%
  summarise(
    total_motions = sum(motionSinceLineset == TRUE),  # Count "man_in_motion" events
    .groups = "drop"  # Ungroup the result for a flat dataframe
  )

print(motions_per_play)


