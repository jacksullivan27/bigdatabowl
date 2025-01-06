library(gt)
library(gtExtras)
library(ggthemes)
library(nflfastR)

results <- plays %>%
  left_join(results, by = c("gameId", "playId"))


results <- results %>%
  mutate(
    success = ifelse(expectedPointsAdded > 0, 1, 0)
  )

team_coe_summary <- results %>%
  group_by(motion_type) %>%
  summarise(
    CCOE = mean((coe), na.rm = TRUE),
    EPA = mean(expectedPointsAdded),
    `SUCCESS %` = 100 * (mean(success)),
    total_plays = n(),  
    .groups = "drop"
  ) %>%
  filter(!is.na(motion_type))

motion_results <- team_coe_summary %>%
  gt() %>%
  gt_theme_538() %>%
  tab_header(
    title = "Motion Analysis by Type - Weeks 7-9"
  ) %>%
  fmt_number(columns = c(CCOE, EPA, `SUCCESS %`), decimals = 2)

motion_results


motion_usage <- joinedmotions %>%
  group_by(motion_type) %>%
  summarise(
    Plays = n()
  ) %>%
  mutate(
    Usage_Percent = Plays / sum(Plays) * 100 
  ) %>%
  arrange(-Usage_Percent)

usage_chart <- motion_usage %>%
  select(motion_type, Usage_Percent) %>%
  gt() %>%
  gt_theme_538() %>%
  tab_header(
    title = "Motion Usage Rate Weeks 1-9"
  ) %>%
  fmt_number(
    columns = c(Usage_Percent), 
    decimals = 2
  ) %>%
  cols_label(
    motion_type = "Motion Type",
    Usage_Percent = "Usage Rate (%)"
  )

usage_chart

#Vikings Motion Report
vikings_data <- results %>%
  filter(possessionTeam.y == "MIN")  


vikings_by_down <- vikings_data %>%
  group_by(motion_type, down.y) %>%
  summarise(
    avg_coe = mean(coe, na.rm = TRUE), 
    avg_epa = mean(expectedPointsAdded, na.rm = TRUE),  
    total_plays = n(),  # Total plays
    .groups = "drop"
  ) %>%
  filter(!is.na(motion_type)) %>%
  filter(total_plays >= 2) %>%
  arrange(-avg_coe)

vikings_by_down_table <- vikings_by_down %>%
  gt() %>%
  gt_theme_538() %>%
  tab_header(
    title = "Vikings Passing Motion Tendency and CCOE by Down",
    subtitle = "Min. 2 Plays Running The Motion"
  ) %>%
  fmt_number(columns = c(avg_coe, avg_epa), decimals = 2) %>%
  fmt_number(columns = c(total_plays), decimals = 0) %>%
  cols_label(
    motion_type = "Motion Type",
    down.y = "Down",
    avg_coe = "CCOE/Play",
    avg_epa = "EPA/Play",
    total_plays = "Total Plays"
  ) %>%
  data_color(
    columns = c(avg_coe),
    colors = scales::col_numeric(
      palette = c("white", "purple"),
      domain = range(vikings_by_down$avg_coe, na.rm = TRUE)
    )
  ) %>%
  # Add bold to column labels
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  )
vikings_by_down_table



vikings_by_alignment <- vikings_data %>%
  group_by(motion_type, receiverAlignment.y) %>%
  summarise(
    avg_coe = mean(coe, na.rm = TRUE),
    avg_epa = mean(expectedPointsAdded, na.rm = TRUE),
    total_plays = n(),
    .groups = "drop"
  ) %>%
  arrange(-avg_coe) %>%
  filter(total_plays >= 2) %>%
  filter(!is.na(motion_type))

vikings_by_alignment_table <- vikings_by_alignment %>%
  gt() %>%
  gt_theme_538() %>%
  tab_header(
    title = "Vikings Passing Motion Tendency by Receiver Alignment",
    subtitle = "Min. 2 Plays of Running Motion in Situation"
  ) %>%
  fmt_number(columns = c(avg_coe, avg_epa), decimals = 2) %>%
  fmt_number(columns = c(total_plays), decimals = 0) %>%
  cols_label(
    motion_type = "Motion Type",
    receiverAlignment.y = "Receiver Alignment",
    avg_coe = "CCOE/Play",
    avg_epa = "EPA/Play",
    total_plays = "Total Plays"
  ) %>%
  data_color(
    columns = c(avg_coe),
    colors = scales::col_numeric(
      palette = c("white", "purple"),
      domain = range(vikings_by_alignment$avg_coe, na.rm = TRUE)
    )
  ) %>%
  # Add bold to column labels
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  )
vikings_by_alignment_table

vikings_by_coverage <- vikings_data %>%
  group_by(motion_type, pff_manZone.y) %>%
  summarise(
    avg_coe = mean(coe, na.rm = TRUE),
    avg_epa = mean(expectedPointsAdded, na.rm = TRUE),
    total_plays = n(),
    .groups = "drop"
  ) %>%
  arrange(-avg_coe) %>%
  filter(total_plays >= 2) %>%
  filter(!is.na(motion_type))

vikings_by_coverage_table <- vikings_by_coverage %>%
  gt() %>%
  gt_theme_538() %>%
  tab_header(
    title = "Vikings Passing Motion Tendency by Coverage Type",
    subtitle = "Min. 2 Plays of Running Motion in Situation"
  ) %>%
  fmt_number(columns = c(avg_coe, avg_epa), decimals = 2) %>%
  fmt_number(columns = c(total_plays), decimals = 0) %>%
  cols_label(
    motion_type = "Motion Type",
    pff_manZone.y = "Coverage Type",
    avg_coe = "CCOE/Play",
    avg_epa = "EPA/Play",
    total_plays = "Total Plays"
  ) %>%
  data_color(
    columns = c(avg_coe),
    colors = scales::col_numeric(
      palette = c("white", "purple"),
      domain = range(vikings_by_coverage$avg_coe, na.rm = TRUE)
    )
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  )
    
vikings_by_coverage_table
  
#Situational EPA Correlation
  OutLongBalance <- results %>%
    filter(!is.na(coe)) %>%
    filter(motion_type == "Out" & yardsToGo.y >= 8 & receiverAlignment.y == "2x2")
  
  
  
  ggplot(OutLongBalance, aes(x = coe, y = expectedPointsAdded)) +
    geom_point(alpha = 0.6, color = "black") +  
    geom_smooth(method = "lm", color = "blue", se = TRUE) +
    theme_fivethirtyeight() +
    labs(
      title = "Comparing Offensive EPA Against Situational CCOE",
      subtitle = "Balanced Out Motions in Longer Yards To Go",
      x = "Centroid Change Over Expected",
      y = "EPA"
    ) +
    theme(
      axis.title = element_text(size = 13)
    )
  
  
  JetMan <- results %>%
    filter(!is.na(coe)) %>%
    filter(motion_type == "Jet" & pff_manZone.y == "Man")
  
  
  
  ggplot(JetMan, aes(x = coe, y = expectedPointsAdded)) +
    geom_point(alpha = 0.6, color = "black") +  
    geom_smooth(method = "lm", color = "blue", se = TRUE) +  
    theme_fivethirtyeight() +
    labs(
      title = "Comparing Offensive EPA Against Situational CCOE",
      subtitle = "Jet Motions Against Man Coverage",
      x = "Centroid Change Over Expected",
      y = "EPA"
    ) +
    theme(
      axis.title = element_text(size = 13)
    )
  
  EarlyDownFlyMan <- results %>%
    filter(!is.na(coe)) %>%
    filter(motion_type == "Fly" & pff_manZone.y == "Man"  & (down.y == 1 | down.y == 2) & pff_runPassOption == 0 & playAction == FALSE) 

  
  ggplot(EarlyDownFlyMan, aes(x = coe, y = expectedPointsAdded)) +
    geom_point(alpha = 0.6, color = "black") + 
    geom_smooth(method = "lm", color = "blue", se = TRUE) +  
    theme_fivethirtyeight() +
    labs(
      title = "Comparing Offensive EPA Against Situational CCOE",
      subtitle = "Fly Motions Against Man Coverage in Early Down Standard Dropbacks",
      x = "Centroid Change Over Expected",
      y = "EPA"
    ) +
    theme(
      axis.title = element_text(size = 13)
    )

  

  team_logos <- teams_colors_logos %>%
    select(team_abbr, team_logo_espn) %>%
    rename(possessionTeam.y = team_abbr, team_logo = team_logo_espn)

team_summary <- results %>%
  filter(!is.na(coe)) %>%
  group_by(possessionTeam.y) %>%
  summarise(
    epa = mean(expectedPointsAdded),
    avgccoe = mean(coe)
  ) %>%
  arrange(-avgccoe) %>%
  head(10) %>%
  left_join(team_logos, by = "possessionTeam.y") %>%
  select(team_logo, epa, avgccoe) %>%
  rename(Team = team_logo)




team_summary_table <- team_summary %>%
  gt() %>%
  gt_theme_538() %>%
  tab_header(
    title = "Top 10 Offenses by Average CCOE Created On Motion Plays",
    subtitle = "2022 Season Week 7-9"
  ) %>%
  fmt_number(
    columns = c(epa, avgccoe),
    decimals = 2
  ) %>%
  cols_label(
    Team = "",
    epa = "Passing EPA/Play",
    avgccoe = "CCOE/Play Created"
) %>%
  gt_img_rows(
    columns = Team,  # Add logos in the "team_logo" column
    height = 30
  ) %>%
  data_color(
    columns = avgccoe,
    colors = scales::col_numeric(
      palette = c("white", "green"),
      domain = NULL
    )
  ) 

team_summary_table

motion_centroid_data <- epacorr %>%
  filter(!is.na(down) & !is.na(yardsToGo) & !is.na(centroid_change)) %>%
  filter(down != 4) %>%
  mutate(
    down = factor(down, levels = c(1, 2, 3), labels = c("1st Down", "2nd Down", "3rd Down")),
    yards_to_go_bin = cut(
      yardsToGo,
      breaks = c(-Inf, 5, 10, 15, Inf),
      labels = c("0-5 Yards", "6-10 Yards", "11-15 Yards", "16+ Yards")
    )
  )

ggplot(motion_centroid_data, aes(x = motion_type, y = centroid_change, fill = motion_type)) +
  geom_boxplot(outlier.alpha = 0.3, alpha = 0.7) +  
  facet_grid(down ~ yards_to_go_bin, scales = "free", space = "free_x") + 
  scale_fill_viridis_d(option = "plasma", name = "Motion Type") +  
  theme_fivethirtyeight() + 
  labs(
    title = "Centroid Change by Down, Distance, and Motion Type",
    x = "Motion Type",
    y = "Centroid Change (Yards)"
  ) 



  
