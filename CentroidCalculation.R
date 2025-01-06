library(dplyr)
library(readr)
library(tidyr)
library(foreach)
library(doParallel)


#PROCESSOR FOR MAN_IN_MOTION TO BALLSNAP

process_week_frame_by_frame <- function(week_number, players_data) {
  week_file <- sprintf("nfl-big-data-bowl-2025/tracking_week_%d.csv", week_number)
  tracking_week <- read_csv(week_file, show_col_types = FALSE)
  
  tracking_week <- tracking_week %>%
    group_by(gameId, playId) %>%
    filter(any(event == "man_in_motion") & any(event == "ball_snap")) %>%
    ungroup()
  
  defensive_players <- tracking_week %>%
    left_join(players_data, by = "nflId") %>%
    filter(position %in% c("CB", "SS", "FS", "ILB", "MLB", "OLB", "DE"))
  
  event_frames <- defensive_players %>%
    filter(event %in% c("man_in_motion", "ball_snap")) %>%
    group_by(gameId, playId) %>%
    summarise(
      motion_frame = min(frameId[event == "man_in_motion"]),
      snap_frame = max(frameId[event == "ball_snap"]),
      .groups = "drop"
    )
  

  event_centroids <- defensive_players %>%
    filter(event %in% c("man_in_motion", "ball_snap")) %>%
    group_by(gameId, playId, event) %>%
    summarise(
      centroid_x = mean(x, na.rm = TRUE),
      centroid_y = mean(y, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_wider(
      names_from = event,
      values_from = c(centroid_x, centroid_y),
      names_glue = "{event}_{.value}"
    ) %>%
    mutate(
      centroid_change = sqrt(
        (ball_snap_centroid_x - man_in_motion_centroid_x)^2 + 
          (ball_snap_centroid_y - man_in_motion_centroid_y)^2
      )
    )
  
  frame_centroids <- defensive_players %>%
    inner_join(event_frames, by = c("gameId", "playId")) %>%
    filter(frameId >= motion_frame & frameId <= snap_frame) %>%
    group_by(gameId, playId, frameId) %>%
    summarise(
      centroid_x = mean(x, na.rm = TRUE),
      centroid_y = mean(y, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Return both datasets in a list
  return(list(
    event_centroids = event_centroids,
    frame_centroids = frame_centroids
  ))
}

# Parallel processing
num_cores <- parallel::detectCores() - 1
cl <- makeCluster(num_cores)
registerDoParallel(cl)

results <- foreach(
  week = 1:9,
  .packages = c("dplyr", "readr", "tidyr"),
  .combine = function(x, y) {
    list(
      event_centroids = bind_rows(x$event_centroids, y$event_centroids),
      frame_centroids = bind_rows(x$frame_centroids, y$frame_centroids)
    )
  }
) %dopar% {
  process_week_frame_by_frame(week, players_data)
}

stopCluster(cl)


frame_centroids <- as.data.frame(results$frame_centroids)

centroid_changes_by_play <- frame_centroids %>%
  group_by(gameId, playId) %>%
  summarise(
    start_centroid_x = first(centroid_x, na.rm = TRUE),
    start_centroid_y = first(centroid_y, na.rm = TRUE),
    end_centroid_x = last(centroid_x, na.rm = TRUE),
    end_centroid_y = last(centroid_y, na.rm = TRUE),
    centroid_change = sqrt(
      (end_centroid_x - start_centroid_x)^2 + 
        (end_centroid_y - start_centroid_y)^2
    ),
    .groups = "drop"
  )

write.csv(results$event_centroids, "event_centroids.csv", row.names = FALSE)
write.csv(results$frame_centroids, "frame_centroids.csv", row.names = FALSE)





#PROCESSOR FOR LINE_SET TO BALLSNAP

process_week_frame_by_frame <- function(week_number, players_data) {
  week_file <- sprintf("nfl-big-data-bowl-2025/tracking_week_%d.csv", week_number)
  tracking_week <- read_csv(week_file, show_col_types = FALSE)
  
  tracking_week <- tracking_week %>%
    group_by(gameId, playId) %>%
    filter(any(event == "line_set") & any(event == "ball_snap")) %>%
    ungroup()
  
  defensive_players <- tracking_week %>%
    left_join(players_data, by = "nflId") %>%
    filter(position %in% c("CB", "SS", "FS", "ILB", "MLB", "OLB", "DE"))
  
  event_frames <- defensive_players %>%
    filter(event %in% c("line_set", "ball_snap")) %>%
    group_by(gameId, playId) %>%
    summarise(
      motion_frame = min(frameId[event == "line_set"]),
      snap_frame = max(frameId[event == "ball_snap"]),
      .groups = "drop"
    )
  

  event_centroidsLS <- defensive_players %>%
    filter(event %in% c("line_set", "ball_snap")) %>%
    group_by(gameId, playId, event) %>%
    summarise(
      centroid_x = mean(x, na.rm = TRUE),
      centroid_y = mean(y, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_wider(
      names_from = event,
      values_from = c(centroid_x, centroid_y),
      names_glue = "{event}_{.value}"
    ) %>%
    mutate(
      centroid_change = sqrt(
        (ball_snap_centroid_x - line_set_centroid_x)^2 + 
          (ball_snap_centroid_y - line_set_centroid_y)^2
      )
    )
  
  # 2. Frame-by-frame centroids (for visualization)
  frame_centroidsLS <- defensive_players %>%
    inner_join(event_frames, by = c("gameId", "playId")) %>%
    filter(frameId >= motion_frame & frameId <= snap_frame) %>%
    group_by(gameId, playId, frameId) %>%
    summarise(
      centroid_x = mean(x, na.rm = TRUE),
      centroid_y = mean(y, na.rm = TRUE),
      .groups = "drop"
    )
  
  return(list(
    event_centroidsLS = event_centroidsLS,
    frame_centroidsLS = frame_centroidsLS
  ))
}

# Parallel processing
num_cores <- parallel::detectCores() - 1
cl <- makeCluster(num_cores)
registerDoParallel(cl)

results <- foreach(
  week = 1:9,
  .packages = c("dplyr", "readr", "tidyr"),
  .combine = function(x, y) {
    list(
      event_centroidsLS = bind_rows(x$event_centroidsLS, y$event_centroidsLS),
      frame_centroidsLS = bind_rows(x$frame_centroidsLS, y$frame_centroidsLS)
    )
  }
) %dopar% {
  process_week_frame_by_frame(week, players_data)
}

stopCluster(cl)


frame_centroidsLS <- as.data.frame(results$frame_centroidsLS)

centroid_changes_by_playCALC <- frame_centroidsLS %>%
  group_by(gameId, playId) %>%
  summarise(
    start_centroid_x = first(centroid_x, na.rm = TRUE),
    start_centroid_y = first(centroid_y, na.rm = TRUE),
    end_centroid_x = last(centroid_x, na.rm = TRUE),
    end_centroid_y = last(centroid_y, na.rm = TRUE),
    centroid_change = sqrt(
      (end_centroid_x - start_centroid_x)^2 + 
        (end_centroid_y - start_centroid_y)^2
    ),
    .groups = "drop"
  )

write.csv(results$event_centroidsLS, "event_centroidsLS.csv", row.names = FALSE)
write.csv(results$frame_centroidsLS, "frame_centroidsLS.csv", row.names = FALSE)

