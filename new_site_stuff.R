source("main.R")

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



###### matchup plot ######
all_starters <- na.omit(all_players[all_players$starter_id == 1, ])
all_starters <- all_starters[all_starters$week == current_week,]

# Group by team_name and summarize data
grouped_data <- all_starters %>%
  group_by(team_name, matchup_id) %>%
  summarize(entries = list(data.frame(full_name = full_name, position = position, points = points)))

# Convert to JSON
json_data <- jsonlite::toJSON(grouped_data, pretty = TRUE)

# Copy the JSON data to the clipboard
write_clip(json_data)



###### motw history table ######
motw_table <- motw_data %>%
  filter(motw == 1) %>%
  group_by(week) %>%
  arrange(matchup_id) %>%
  reframe(
    Week = week,
    `Winner Team` = team_name[which.max(winner)],
    `Winner Score` = max(team_points),
    `Loser Score` = min(team_points),
    `Loser Team` = team_name[which.min(winner)],
    `# of Shots/Dogs` = `# of Shots`[which.min(winner)]
  ) %>%
  distinct() %>%
  select(-week)

# Function to generate color codes based on spec_color2
spec_color2_scale <- function(x, scale_from, direction = 1) {
  if (direction == -1) {
    scale_from <- rev(scale_from)
  }
  
  n <- length(custom_palette36)
  x <- round(scales::rescale(x, to = c(1, n), from = scale_from))
  color_code <- custom_palette36[x]
  color_code[is.na(color_code)] <- "#BBBBBB"  # Replace NA color as needed
  return(color_code)
}

# Add color scale columns for Winner Score, Loser Score, and # of Shots/Dogs
motw_table$WinnerScoreColor <- spec_color2_scale(
  motw_table$`Winner Score`,
  scale_from = c(min(all_matchups$team_points), max(all_matchups$team_points)),
  direction = 1
)

motw_table$LoserScoreColor <- spec_color2_scale(
  motw_table$`Loser Score`,
  scale_from = c(min(all_matchups$team_points), max(all_matchups$team_points)),
  direction = 1
)

motw_table$ShotsDogsColor <- spec_color2_scale(
  motw_table$`# of Shots/Dogs`,
  scale_from = c(min(motw_table$`# of Shots/Dogs`), max(motw_table$`# of Shots/Dogs`)),
  direction = -1
)

# Assuming 'motw_table' is your data frame
json_data <- jsonlite::toJSON(motw_table, pretty = TRUE)

# Copy the JSON data to the clipboard
write_clip(json_data)
