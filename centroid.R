
#create a processor that has the centroid by frame to create the graphic

process_week <- function(week_number, players_data) {
  week_file <- sprintf("nfl-big-data-bowl-2025/tracking_week_%d.csv", week_number)
  tracking_week <- read_csv(week_file, show_col_types = FALSE)
  
  # Filter for plays with line_set and ball_snap events
  tracking_week <- tracking_week %>%
    group_by(gameId, playId) %>%
    filter(any(event == "man_in_motion") & any(event == "ball_snap")) %>%
    ungroup()
  
  # Filter only defensive players
  defensive_players <- tracking_week %>%
    filter(event %in% c("man_in_motion", "ball_snap")) %>%
    left_join(players_data, by = "nflId") %>%
    filter(position %in% c("CB", "SS", "FS", "ILB", "MLB", "OLB", "DE"))
  
  # Calculate centroids at each event
  centroids <- defensive_players %>%
    group_by(gameId, playId, event) %>%
    summarise(
      centroid_x = mean(x, na.rm = TRUE),
      centroid_y = mean(y, na.rm = TRUE),
      .groups = "drop_last"
    ) %>%
    pivot_wider(
      names_from = event,
      values_from = c(centroid_x, centroid_y),
      names_glue = "{event}_{.value}"
    )
  
  # Calculate the centroid change (Euclidean distance)
  centroid_changes <- centroids %>%
    mutate(
      centroid_change = sqrt(
        (ball_snap_centroid_x - man_in_motion_centroid_x)^2 + 
          (ball_snap_centroid_y - man_in_motion_centroid_y)^2
      )
    )
  
  return(centroid_changes)
}

# Apply to all weeks using parallel processing
num_cores <- parallel::detectCores() - 1
cl <- makeCluster(num_cores)
registerDoParallel(cl)

all_centroid_changes <- foreach(
  week = 1:9,
  .packages = c("dplyr", "readr", "tidyr"),
  .combine = bind_rows
) %dopar% {
  process_week(week, players_data)
}

stopCluster(cl)