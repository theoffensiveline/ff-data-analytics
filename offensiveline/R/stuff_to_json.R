awards_to_json <- function(matchup_data,
                           player_data,
                           max_week,
                           team_photos) {
  best_ball_lineups <- calc_best_ball_lineups(player_data, max_week)

  best_ball_matchups <- create_best_ball_matchups(optimal_lineups = best_ball_lineups)

  awards_data <- create_awards_table(player_data, matchup_data, best_ball_matchups, team_photos)

  json_data <- jsonlite::toJSON(awards_data, pretty = TRUE)

  return(json_data)
}

efficiency_to_json <- function(matchup_data, player_data, max_week) {
  best_ball_lineups <- calc_best_ball_lineups(player_data, max_week)

  best_ball_matchups <- create_best_ball_matchups(optimal_lineups = best_ball_lineups)

  chart_data <- best_ball_matchups %>%
    left_join(matchup_data, by = c('week', 'manager_id')) %>%
    mutate(
      max_points = team_points.x,
      # Rename team_points.y to max_points
      actual_points = team_points.y,
      # Rename team_points.x to actual_points
      team_name = team_name.x,
      # Rename team_name.x to team_name
      percentage = actual_points / max_points * 100
    ) %>%
    filter(week == max_week) %>%
    arrange(desc(actual_points), desc(max_points)) %>%       # Sort by percentage in descending order
    select(team_name, actual_points, max_points, percentage)

  # Assuming 'chart_data' is your dataframe
  json_data <- jsonlite::toJSON(chart_data, pretty = TRUE)

  return(json_data)
}

matchup_info_to_json <- function(matchup_data) {
  matchup_data <- matchup_data %>%
    group_by(week) %>%
    mutate(
      Average = mean(team_points),
      Median = median(team_points),
      Maximum = max(team_points),
      Minimum = min(team_points)
    ) %>%
    ungroup() %>%
    left_join(team_photos) %>%
    select(
      week,
      team_name,
      image_or_text,
      matchup_id,
      team_points,
      Average,
      Median,
      Maximum,
      Minimum
    )

  # Assuming 'chart_data' is your dataframe
  json_data <- jsonlite::toJSON(matchup_data, pretty = TRUE)

  return(json_data)
}

matchup_plot_to_json <- function(player_data, max_week) {
  all_starters <- player_data[!is.na(player_data$starter_id), ]
  all_starters <- all_starters[all_starters$week == max_week, ]

  # Group by team_name and summarize data
  grouped_data <- all_starters %>%
    group_by(team_name, matchup_id) %>%
    summarize(entries = list(
      data.frame(
        full_name = full_name,
        nickname = nickname,
        position = position,
        points = points
      )
    ))

  # Convert to JSON
  json_data <- jsonlite::toJSON(grouped_data, pretty = TRUE)

  return(json_data)
}

# Function to generate color codes based on spec_color2
spec_color2_scale <- function(x, scale_from, direction = 1) {
  if (direction == -1) {
    scale_from <- rev(scale_from)
  }

  n <- length(custom_palette36)
  x <- round(scales::rescale(x, to = c(1, n), from = scale_from))
  color_code <- custom_palette36[x]
  color_code[is.na(color_code)] <-
    "#BBBBBB"  # Replace NA color as needed
  return(color_code)
}

motw_table_to_json <- function(motw_data) {
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

  # Add color scale columns for Winner Score, Loser Score, and # of Shots/Dogs
  motw_table$WinnerScoreColor <- spec_color2_scale(
    motw_table$`Winner Score`,
    scale_from = c(
      min(all_matchups$team_points),
      max(all_matchups$team_points)
    ),
    direction = 1
  )

  motw_table$LoserScoreColor <- spec_color2_scale(
    motw_table$`Loser Score`,
    scale_from = c(
      min(all_matchups$team_points),
      max(all_matchups$team_points)
    ),
    direction = 1
  )

  motw_table$ShotsDogsColor <- spec_color2_scale(
    motw_table$`# of Shots/Dogs`,
    scale_from = c(
      min(motw_table$`# of Shots/Dogs`),
      max(motw_table$`# of Shots/Dogs`)
    ),
    direction = -1
  )

  # Assuming 'motw_table' is your data frame
  json_data <- jsonlite::toJSON(motw_table, pretty = TRUE)

  return(json_data)
}

shots_dist_to_json <- function(motw_data) {
  # Filter data for losing teams
  losing_teams_data <- motw_data[motw_data$winner == 0, ]

  # Create a data frame for Victory chart
  victory_data <- data.frame(x = losing_teams_data$`# of Shots`,
                             group = ifelse(is.na(losing_teams_data$motw), 'Not MotW', 'MotW'))

  # Convert the data frame to a JSON string
  json_data <- jsonlite::toJSON(victory_data, pretty = TRUE)

  return(json_data)
}

leaderboard_to_json <- function(matchup_data, max_week, team_photos) {
  leaderboard_data <- create_leaderboard(matchup_data, max_week)

  leaderboard_data$PFColor <- spec_color2_scale(leaderboard_data$PF,
                                                scale_from = c(min(leaderboard_data$PF), max(leaderboard_data$PF)),
                                                direction = 1)

  leaderboard_data$PAColor <- spec_color2_scale(leaderboard_data$PA,
                                                scale_from = c(min(leaderboard_data$PA), max(leaderboard_data$PA)),
                                                direction = -1)

  leaderboard_data <- leaderboard_data %>% left_join(team_photos, by = c("Team" = "team_name"))

  json_data <- jsonlite::toJSON(leaderboard_data, pretty = TRUE)

  return(json_data)
}

power_rankings_to_json <- function(matchup_data, max_week, number_of_teams) {
  power_ranking_data <- create_power_rankings(matchup_data, max_week, number_of_teams)

  power_ranking_data$TaColor <- spec_color2_scale(
    power_ranking_data$`Team Ability`,
    scale_from = c(
      min(power_ranking_data$`Team Ability`),
      max(power_ranking_data$`Team Ability`)
    ),
    direction = 1
  )

  power_ranking_data$SosColor <- spec_color2_scale(
    power_ranking_data$`Str of Sched`,
    scale_from = c(
      min(power_ranking_data$`Str of Sched`),
      max(power_ranking_data$`Str of Sched`)
    ),
    direction = -1
  )

  json_data <- jsonlite::toJSON(power_ranking_data, pretty = TRUE)

  return(json_data)
}

median_lb_to_json <- function(matchup_data, max_week) {
  median_lb_data <- create_median_leaderboard(matchup_data, max_week)

  median_lb_data$PFColor <- spec_color2_scale(median_lb_data$PF,
                                              scale_from = c(min(median_lb_data$PF), max(median_lb_data$PF)),
                                              direction = 1)

  median_lb_data$PAColor <- spec_color2_scale(median_lb_data$PA,
                                              scale_from = c(min(median_lb_data$PA), max(median_lb_data$PA)),
                                              direction = -1)

  json_data <- jsonlite::toJSON(median_lb_data, pretty = TRUE)

  return(json_data)
}

best_ball_lb_to_json <- function(matchup_data, player_data, max_week) {
  best_ball_lineups <-
    calc_best_ball_lineups(player_data, max_week)

  best_ball_matchups <-
    create_best_ball_matchups(optimal_lineups = best_ball_lineups)

  best_ball_leaderboard <-
    create_best_ball_leaderboard(matchup_data, best_ball_matchups, max_week)

  best_ball_leaderboard$PFColor <- spec_color2_scale(
    best_ball_leaderboard$PF,
    scale_from = c(
      min(best_ball_leaderboard$PF),
      max(best_ball_leaderboard$PF)
    ),
    direction = 1
  )

  best_ball_leaderboard$PAColor <- spec_color2_scale(
    best_ball_leaderboard$PA,
    scale_from = c(
      min(best_ball_leaderboard$PA),
      max(best_ball_leaderboard$PA)
    ),
    direction = -1
  )

  json_data <- jsonlite::toJSON(best_ball_leaderboard, pretty = TRUE)

  return(json_data)
}

schedule_comparison_to_json <- function(matchup_data, team_photos) {
  matchup_data <- matchup_data %>%
    left_join(team_photos) %>%
    select(week, team_name, image_or_text, matchup_id, team_points)

  your_data <- matchup_data %>%
    select(week, team_name, image_or_text, matchup_id, team_points) %>%
    group_by(week, matchup_id) %>%
    arrange(team_name) %>%
    mutate(other_team_points = case_when(
      team_name == unique(team_name)[1] ~ lead(team_points, order_by = team_name),
      team_name != unique(team_name)[1] ~ lag(team_points, order_by = team_name)
    )) %>%
    ungroup()

  team_list <- unique(your_data$team_name)

  records_list <- list()

  # Initialize team_records data frame
  team_records <- data.frame(
    team1 = character(),
    team2 = character(),
    wins = numeric(),
    losses = numeric(),
    ties = numeric(),
    stringsAsFactors = FALSE
  )

  for (team in team_list) {
    # get team scores for each week
    team_scores <- your_data %>%
      filter(team_name == team) %>%
      select(week, team_points, matchup_id)

    for (team2 in team_list) {
      # Initialize wins, losses, ties inside the inner loop
      wins <- 0
      losses <- 0
      ties <- 0

      opponent_scores <- your_data %>%
        filter(team_name == team2) %>%
        select(week, team_points, other_team_points, matchup_id)

      merged_scores <- merge(team_scores, opponent_scores, by = "week")

      for (i in 1:nrow(merged_scores)) {
        if ((merged_scores$matchup_id.x[i] == merged_scores$matchup_id.y[i]) &
            (team != team2)) {
          if (merged_scores$team_points.x[i] > merged_scores$team_points.y[i]) {
            wins <- wins + 1
          } else if (merged_scores$team_points.x[i] < merged_scores$team_points.y[i]) {
            losses <- losses + 1
          } else {
            ties <- ties + 1
          }
        } else {
          if (merged_scores$team_points.x[i] > merged_scores$other_team_points[i]) {
            wins <- wins + 1
          } else if (merged_scores$team_points.x[i] < merged_scores$other_team_points[i]) {
            losses <- losses + 1
          } else {
            ties <- ties + 1
          }
        }
      }
      # Append record to list of records as a data frame
      record <- data.frame(
        team1 = as.character(team),
        team2 = as.character(team2),
        wins = wins,
        losses = losses,
        ties = ties
      )
      records_list <- c(records_list, list(record))
    }
  }

  # Convert the list of records to a data frame using bind_rows
  team_records <- bind_rows(records_list)

  best_records <- team_records %>%
    group_by(team1) %>%
    filter(wins == max(wins)) %>%
    summarize(
      team2_list = list(team2),
      wins = max(wins),
      losses = max(losses),
      ties = max(ties)
    )

  worst_records <- team_records %>%
    group_by(team1) %>%
    filter(wins == min(wins)) %>%
    summarize(
      team2_list = list(team2),
      wins = max(wins),
      losses = max(losses),
      ties = max(ties)
    )

  current_records <- team_records %>%
    filter(team1 == team2) %>%
    select(team1, wins, losses, ties)

  # Combine records into a list
  combined_list <- list(
    best_records = best_records,
    worst_records = worst_records,
    current_records = current_records
  )

  json_data <- jsonlite::toJSON(combined_list, pretty = TRUE)

  return(json_data)
}

generate_file_path <- function(base_path = "C:/Users/Trevor/Documents/Fantasy Football/theoffensiveline-site/src/newsletters/20",
                               current_year,
                               current_week,
                               file_name) {
  paste0(base_path, current_year, " Week ", current_week, "/", file_name)
}

# Define a function to write JSON data to a file
write_json_to_file <- function(json_data, file_path) {
  writeLines(json_data, file_path)
  cat("Data has been successfully written to", file_path, "\n")
}
