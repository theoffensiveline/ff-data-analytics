##### New Site Stuff #####
###### Efficiency Data ######
chart_data <- best_ball_matchups %>%
  left_join(all_matchups, by = c('week', 'manager_id')) %>%
  mutate(
    max_points = team_points.x,       # Rename team_points.y to max_points
    actual_points = team_points.y,    # Rename team_points.x to actual_points
    team_name = team_name.x,          # Rename team_name.x to team_name
    percentage = actual_points / max_points * 100
  ) %>%
  filter(week == current_week) %>%
  arrange(desc(actual_points), desc(max_points)) %>%       # Sort by percentage in descending order
  select(team_name, actual_points, max_points, percentage)

library(clipr)

# Assuming 'chart_data' is your dataframe
json_data <- jsonlite::toJSON(chart_data, pretty = TRUE)

# Copy the JSON data to the clipboard
write_clip(json_data)

###### Scoring Distribution ######
matchup_data <- all_matchups

matchup_data$Group <-
  ifelse(matchup_data$week < current_week, 'Historic', 'This Week')

matchup_data$Group <-
  factor(matchup_data$Group, levels = c('Historic', 'This Week'))

matchup_data$Score <- matchup_data$team_points

# Assuming 'chart_data' is your dataframe
json_data <- jsonlite::toJSON(matchup_data, pretty = TRUE)

# Copy the JSON data to the clipboard
write_clip(json_data)
