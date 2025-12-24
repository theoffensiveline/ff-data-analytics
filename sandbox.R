library(sleeperapi)
library(offensiveline)
sleeper_players_csv <- "sleeper_players.csv"



league_id <- 1253779168802377728 # main league

league <- get_league(league_id)

old_league_id <- as.numeric(league$previous_league_id) # main league 2024

old_league <- get_league(old_league_id)

older_league_id <- as.numeric(old_league$previous_league_id)

# get all player data for each matchup
old_all_players <- get_all_matchups_data(17, old_league_id, sleeper_players_csv)


# summarize to the team level
old_all_matchups <- get_team_matchups(player_data = old_all_players)



users <- get_league_users(league_id)
filtered_users <- users %>% select(team_name, avatar)

# Define a function to fetch image or use team_name as text
get_image_or_text <- function(avatar, team_name) {
  return(ifelse(is.na(avatar), team_name, avatar))
}

# Apply the function to create a new column with image URLs or team names
filtered_users$image_or_text <-
  mapply(get_image_or_text,
         filtered_users$avatar,
         filtered_users$team_name)

return(filtered_users)


library(jsonlite)

# Function to build tree based on the matchups
build_tree <- function(df, champion) {
  # Find the rows where the given champion is listed
  next_matchups <- df[df$Champion == champion, ]
  
  # Create a list where the champion is the key and the values are their possible opponents
  if (nrow(next_matchups) == 0) {
    return(champion)  # Leaf node, return the champion
  }
  
  # Recursively build the tree for each opponent
  tree <- list()
  for (i in 1:nrow(next_matchups)) {
    opponent <- next_matchups$Opponent[i]
    tree[[opponent]] <- build_tree(df, opponent)
  }
  
  return(tree)
}


# Step 1: Read the CSV file
df <- read.csv("currentMotW/matchup_tree.csv", stringsAsFactors = FALSE)

# Filter the dataframe for weeks less than 6
df_filtered <- df %>% filter(Week < 7)

# Build the tree starting with the first champion
motw_tree <- build_tree(df_filtered, df_filtered$Champion[1])
print(motw_tree)

# Convert to JSON
json_data <- toJSON(df_filtered, pretty = TRUE)

write_json_to_file(
  json_data,
  generate_file_path(
    current_year = current_year,
    current_week = current_week,
    file_name = "motwTree.json"
  )
)






library(dplyr)
yahoo_df <- read.csv("~/Fantasy Football/matchups.csv")
yahoo_df %>%
  arrange(points)


# Define the target manager and the specific group
target_manager <- "Who\x92s Trevor??"
specific_group <- c("Caught on Kamara", "Gay Bills", "Tucker Right in the Kelce")

# Filter for Trevor's games
trevor_games <- yahoo_df[yahoo_df$manager == target_manager, ]

# 1. Overall win/loss record
overall_wins <- sum(trevor_games$win == "True")
overall_losses <- sum(trevor_games$win == "False")

cat("Trevor's Overall Record:\n")
cat(sprintf("Wins: %d, Losses: %d\n\n", overall_wins, overall_losses))

# 2. Win/loss record against specific group
trevor_vs_group <- trevor_games[trevor_games$opponent %in% specific_group, ]
group_wins <- sum(trevor_vs_group$win == "True")
group_losses <- sum(trevor_vs_group$win == "False")

cat("Trevor's Record vs Caught on Kamara, Gay Bills, and Tucker Right in the Kelce:\n")
cat(sprintf("Wins: %d, Losses: %d\n\n", group_wins, group_losses))

# 3. Win/loss record against everyone else
trevor_vs_others <- trevor_games[!(trevor_games$opponent %in% specific_group), ]
others_wins <- sum(trevor_vs_others$win == "True")
others_losses <- sum(trevor_vs_others$win == "False")

cat("Trevor's Record vs Everyone Else:\n")
cat(sprintf("Wins: %d, Losses: %d\n", others_wins, others_losses))

# Optional: Create a summary data frame
summary_df <- data.frame(
  Category = c("Overall", "vs Specific Group", "vs Others"),
  Wins = c(overall_wins, group_wins, others_wins),
  Losses = c(overall_losses, group_losses, others_losses)
)

print(summary_df)






all_matchups %>%
  group_by(week) %>%
  arrange(desc(team_points)) %>%
  mutate(rank = row_number()) %>%
  filter(rank <= 3) %>%
  ungroup() %>%
  count(team_name, name = "top_3_finishes") %>%
  arrange(desc(top_3_finishes))




all_matchups %>%
  # Step 1: Identify top 3 teams per week
  group_by(week) %>%
  arrange(desc(team_points)) %>%
  mutate(rank = row_number()) %>%
  filter(rank <= 3) %>%
  select(week, matchup_id, team_name_top3 = team_name) %>%
  ungroup() %>%
  
  # Step 2: Join back to find matchups involving top 3 teams
  inner_join(all_matchups, by = c("week", "matchup_id")) %>%
  
  # Step 3: Count appearances of non-top-3 teams playing top-3 teams
  filter(team_name != team_name_top3) %>%
  count(team_name, name = "times_against_top_3") %>%
  
  # Step 4: Arrange results
  arrange(desc(times_against_top_3))


all_matchups %>%
  filter(winner == 0, team_points > 100) %>%
  count(team_name, name = "losses_over_100") %>%
  arrange(desc(losses_over_100))


all_matchups %>%
  group_by(week, matchup_id) %>%
  summarise(total_points = sum(team_points)) %>%
  arrange(desc(total_points))


wk7_trans <- get_all_transaction_data(league_id, 7)

wk7_trades <- wk7_trans %>%
  filter(type == "trade")


# position tables
print(
  starter_ppg %>%
    filter(position == "K") %>%
    filter(games_played >= 8) %>%
    arrange(desc(ppg)),
  n = 40
)

print(
  starter_ppg %>%
    filter(position == "K") %>%
    filter(games_played < 8) %>%
    arrange(desc(ppg)),
  n = 25
)

print(starter_ppg %>%
        filter(position == "K") %>%
        filter(games_played < 8) %>%
        arrange(ppg),
      n = 25)





get_all_historical_matchups <- function(league_id,
                                        sleeper_players_csv,
                                        max_weeks = 17) {
  # Initialize list to store all matchups
  all_historical_matchups <- list()
  
  # Start with the current league
  current_league_id <- league_id
  season_counter <- 1
  
  # Loop through all previous seasons
  while (!is.null(current_league_id)) {
    cat(sprintf("Processing league ID: %s\n", current_league_id))
    
    # Get league info
    tryCatch({
      league <- get_league(current_league_id)
      season_year <- league$season
      
      cat(sprintf("Season: %s\n", season_year))
      
      # Get all player data for each matchup
      all_players <- get_all_matchups_data(max_weeks, current_league_id, sleeper_players_csv)
      
      # Summarize to the team level
      matchups <- get_team_matchups(player_data = all_players)
      
      # Add season identifier from league
      matchups$season <- season_year
      matchups$league_id <- current_league_id
      
      # Store in list
      all_historical_matchups[[season_counter]] <- matchups
      
      # Move to previous season (will be NULL when no more previous leagues)
      current_league_id <- league$previous_league_id
      if (!is.null(current_league_id)) {
        current_league_id <- as.numeric(current_league_id)
      }
      season_counter <- season_counter + 1
      
    }, error = function(e) {
      cat(sprintf(
        "Error processing league %s: %s\n",
        current_league_id,
        e$message
      ))
      current_league_id <<- NULL  # Stop the loop on error
    })
  }
  
  # Combine all seasons into one dataframe
  if (length(all_historical_matchups) > 0) {
    combined_matchups <- do.call(rbind, all_historical_matchups)
    rownames(combined_matchups) <- NULL
    
    cat(sprintf(
      "\nSuccessfully processed %d seasons\n",
      length(all_historical_matchups)
    ))
    cat(sprintf("Total matchups: %d\n", nrow(combined_matchups)))
    
    return(combined_matchups)
  } else {
    cat("No matchups found\n")
    return(NULL)
  }
}

# Usage:
all_historical_matchups <- get_all_historical_matchups(
  league_id = league_id,
  sleeper_players_csv = sleeper_players_csv,
  max_weeks = 17
)


# Define the target manager and the specific group
target_manager_id <- 11
specific_group_ids <- c(2, 8, 4)

# Filter for Trevor's games (manager_id 11)
trevor_games <- all_historical_matchups[all_historical_matchups$manager_id == target_manager_id, ]

# Get Trevor's opponent IDs for each matchup
# First, create a helper to find opponents
get_opponent_id <- function(week, matchup_id, manager_id, df) {
  # Find the other team in the same week and matchup
  opponent <- df[df$week == week &
                   df$matchup_id == matchup_id &
                   df$manager_id != manager_id, ]
  if (nrow(opponent) > 0) {
    return(opponent$manager_id[1])
  } else {
    return(NA)
  }
}

# Add opponent_id column to trevor_games
trevor_games$opponent_id <- mapply(
  get_opponent_id,
  trevor_games$week,
  trevor_games$matchup_id,
  trevor_games$manager_id,
  MoreArgs = list(df = all_historical_matchups)
)

# 1. Overall win/loss record
overall_wins <- sum(trevor_games$winner == 1)
overall_losses <- sum(trevor_games$winner == 0)

cat("Trevor's Overall Record:\n")
cat(sprintf("Wins: %d, Losses: %d\n\n", overall_wins, overall_losses))

# 2. Win/loss record against specific group (IDs 2, 8, 4)
trevor_vs_group <- trevor_games[trevor_games$opponent_id %in% specific_group_ids, ]
group_wins <- sum(trevor_vs_group$winner == 1, na.rm = TRUE)
group_losses <- sum(trevor_vs_group$winner == 0, na.rm = TRUE)

cat("Trevor's Record vs Manager IDs 2, 8, and 4:\n")
cat(sprintf("Wins: %d, Losses: %d\n\n", group_wins, group_losses))

# 3. Win/loss record against everyone else
trevor_vs_others <- trevor_games[!(trevor_games$opponent_id %in% specific_group_ids), ]
others_wins <- sum(trevor_vs_others$winner == 1, na.rm = TRUE)
others_losses <- sum(trevor_vs_others$winner == 0, na.rm = TRUE)

cat("Trevor's Record vs Everyone Else:\n")
cat(sprintf("Wins: %d, Losses: %d\n", others_wins, others_losses))

# Optional: Create a summary data frame
summary_df <- data.frame(
  Category = c("Overall", "vs Specific Group", "vs Others"),
  Wins = c(overall_wins, group_wins, others_wins),
  Losses = c(overall_losses, group_losses, others_losses)
)

print(summary_df)








library(dplyr)

# Average points per team per week
avg_points_per_week <- all_matchups %>%
  group_by(manager_id, team_name) %>%
  summarise(
    avg_points = mean(team_points, na.rm = TRUE),
    weeks_played = n(),
    .groups = "drop"
  )

opponent_comparison_summary <- all_matchups %>%
  group_by(week, matchup_id) %>%
  mutate(opponent_points = if_else(row_number() == 1, last(team_points), first(team_points))) %>%
  ungroup() %>%
  left_join(all_matchups %>%
              group_by(manager_id) %>%
              summarise(team_avg = mean(team_points, na.rm = TRUE)),
            by = "manager_id") %>%
  mutate(opponent_diff_from_avg = opponent_points - team_avg) %>%
  group_by(manager_id, team_name) %>%
  summarise(
    avg_opponent_diff = mean(opponent_diff_from_avg, na.rm = TRUE),
    games_played = n(),
    .groups = "drop"
  ) %>%
  left_join(team_photos, by = "team_name")

opponent_comparison_json <- jsonlite::toJSON(opponent_comparison_summary, pretty = TRUE)
opponent_comparison_file_path <- generate_file_path(current_year = current_year,
                                                    current_week = current_week,
                                                    file_name = "opponentComparison.json")
write_json_to_file(opponent_comparison_json, opponent_comparison_file_path)





###### Divisions ######

library(dplyr)

divisions <- data.frame(
  manager_id = c(2, 4, 8, 11, 3, 6, 7, 9, 1, 5, 10, 12),
  division = c(rep("Hubbell", 4), rep("Glizzy", 4), rep("Avon", 4))
)

# Calculate records against each division
division_records <- all_matchups %>%
  # Add division info for each team
  left_join(divisions, by = "manager_id") %>%
  rename(team_division = division) %>%
  # For each matchup, find the opponent's division
  group_by(week, matchup_id) %>%
  mutate(
    opponent_manager_id = rev(manager_id),
    opponent_division = rev(team_division)
  ) %>%
  ungroup() %>%
  # Group by team and opponent division to calculate records
  group_by(manager_id, team_name, team_division, opponent_division) %>%
  summarise(
    games = n(),
    wins = sum(winner),
    losses = games - wins,
    .groups = "drop"
  ) %>%
  arrange(manager_id, opponent_division)

# View the results
print(division_records)

division_records_json <- jsonlite::toJSON(division_records, pretty = TRUE)
division_records_file_path <- generate_file_path(current_year = current_year,
                                                 current_week = current_week,
                                                 file_name = "divisionRecords.json")
write_json_to_file(division_records_json, division_records_file_path)



# Calculate each division's overall record (excluding intra-division games)
division_overall_records <- all_matchups %>%
  # Add division info for each team
  left_join(divisions, by = "manager_id") %>%
  rename(team_division = division) %>%
  # For each matchup, find the opponent's division
  group_by(week, matchup_id) %>%
  mutate(
    opponent_manager_id = rev(manager_id),
    opponent_division = rev(team_division)
  ) %>%
  ungroup() %>%
  # Exclude intra-division games
  filter(team_division != opponent_division) %>%
  # Group by division to sum up wins and losses
  group_by(team_division) %>%
  summarise(
    teams = n_distinct(manager_id),
    games = n(),
    wins = sum(winner),
    losses = games - wins,
    win_pct = wins / games,
    .groups = "drop"
  ) %>%
  arrange(desc(win_pct))

# View the results
print(division_overall_records)

division_overall_records_json <- jsonlite::toJSON(division_overall_records, pretty = TRUE)
division_overall_records_file_path <- generate_file_path(current_year = current_year,
                                                         current_week = current_week,
                                                         file_name = "divisionOverallRecords.json")
write_json_to_file(division_overall_records_json,
                   division_overall_records_file_path)









# Calculate metrics per team
starting_lineup_data <- all_players %>%
  filter(!is.na(starter_id)) %>%
  group_by(manager_id, team_name) %>%
  summarize(
    strength = mean(points, na.rm = TRUE),
    cv = (sd(points, na.rm = TRUE) / mean(points, na.rm = TRUE)) * 100,
    balance = 100 / (1 + cv),  # Inverse transform so higher = more balanced
    .groups = 'drop'
  ) %>%
  left_join(team_photos, by = "team_name")

print(starting_lineup_data)

starting_lineup_data_json <- jsonlite::toJSON(starting_lineup_data, pretty = TRUE)
starting_lineup_data_file_path <- generate_file_path(current_year = current_year,
                                                         current_week = current_week,
                                                         file_name = "startingLineupData.json")
write_json_to_file(starting_lineup_data_json,
                   starting_lineup_data_file_path)



# Scatter plot
ggplot(team_metrics, aes(x = strength, y = balance, label = team_name)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_text(hjust = -0.1, size = 3) +
  geom_vline(xintercept = median(team_metrics$strength), 
             linetype = "dashed", alpha = 0.5) +
  geom_hline(yintercept = median(team_metrics$balance), 
             linetype = "dashed", alpha = 0.5) +
  labs(
    title = "Starting Lineup: Strength vs Balance",
    x = "Strength (Mean Points per Starter)",
    y = "Balance (Lower CV = Higher Balance)",
    caption = "Top-right quadrant = Strong & Balanced (ideal)"
  ) +
  theme_minimal()




library(dplyr)

all_matchups %>%
  group_by(week) %>%
  mutate(weekly_median = median(team_points)) %>%
  ungroup() %>%
  filter(winner == 0) %>%
  mutate(above_median = team_points > weekly_median) %>%
  group_by(team_name) %>%
  summarise(
    losses_above_median = sum(above_median),
    losses_below_median = sum(!above_median),
    total_losses = n()
  ) %>%
  arrange(desc(losses_above_median))



library(dplyr)

# Step 1: Identify Chris Olave and Ladd McConkey in the data
# First, let's find their player IDs
chris_olave_data <- all_players %>%
  filter(full_name == "Chris Olave")

ladd_mcconkey_data <- all_players %>%
  filter(full_name == "Ladd McConkey")

# Check their player IDs
unique(chris_olave_data$players)
unique(ladd_mcconkey_data$players)

# Step 2: Track their original and current teams by week
chris_original_manager <- 8
ladd_original_manager <- 5

# Identify when trades happened
chris_trades <- chris_olave_data %>%
  arrange(week) %>%
  select(week, manager_id, points, matchup_id)

ladd_trades <- ladd_mcconkey_data %>%
  arrange(week) %>%
  select(week, manager_id, points, matchup_id)

print("Chris Olave by week:")
print(chris_trades)
print("Ladd McConkey by week:")
print(ladd_trades)

# Step 3: Calculate point adjustments needed
# For weeks where they were on different teams than original
point_adjustments <- bind_rows(
  chris_olave_data %>%
    filter(manager_id != chris_original_manager) %>%
    mutate(
      original_manager = chris_original_manager,
      current_manager = manager_id,
      player_name = "Chris Olave"
    ),
  ladd_mcconkey_data %>%
    filter(manager_id != ladd_original_manager) %>%
    mutate(
      original_manager = ladd_original_manager,
      current_manager = manager_id,
      player_name = "Ladd McConkey"
    )
) %>%
  select(week, player_name, points, original_manager, current_manager, matchup_id)

print("Point adjustments needed:")
print(point_adjustments)

# Step 4: Create adjusted matchups dataset
all_matchups_adjusted <- all_matchups

# Apply point adjustments
for(i in 1:nrow(point_adjustments)) {
  adj_week <- point_adjustments$week[i]
  adj_points <- point_adjustments$points[i]
  original_mgr <- point_adjustments$original_manager[i]
  current_mgr <- point_adjustments$current_manager[i]
  
  # Subtract points from current manager
  all_matchups_adjusted <- all_matchups_adjusted %>%
    mutate(team_points = ifelse(week == adj_week & manager_id == current_mgr,
                                team_points - adj_points,
                                team_points))
  
  # Add points to original manager
  all_matchups_adjusted <- all_matchups_adjusted %>%
    mutate(team_points = ifelse(week == adj_week & manager_id == original_mgr,
                                team_points + adj_points,
                                team_points))
}

# Step 5: Recalculate winners for each matchup
all_matchups_adjusted <- all_matchups_adjusted %>%
  group_by(week, matchup_id) %>%
  mutate(winner = ifelse(team_points == max(team_points), 1, 0)) %>%
  ungroup()

# Step 6: Compare original vs adjusted
comparison <- all_matchups %>%
  select(week, manager_id, team_name, matchup_id, 
         original_points = team_points, original_winner = winner) %>%
  left_join(
    all_matchups_adjusted %>%
      select(week, manager_id, adjusted_points = team_points, adjusted_winner = winner),
    by = c("week", "manager_id")
  ) %>%
  mutate(
    points_diff = adjusted_points - original_points,
    outcome_changed = original_winner != adjusted_winner
  )

# Show matchups where outcomes changed
changed_matchups <- comparison %>%
  filter(outcome_changed == TRUE | points_diff != 0) %>%
  arrange(week, matchup_id)

print("Matchups with changed outcomes or points:")
print(changed_matchups)

# Summary of changes
summary_changes <- comparison %>%
  filter(outcome_changed == TRUE) %>%
  group_by(team_name, manager_id) %>%
  summarize(
    games_outcome_changed = n(),
    wins_gained = sum(adjusted_winner == 1 & original_winner == 0),
    wins_lost = sum(adjusted_winner == 0 & original_winner == 1),
    .groups = "drop"
  ) %>%
  arrange(desc(games_outcome_changed))

print("Summary of outcome changes by team:")
print(summary_changes)

# Final adjusted dataset
print("Adjusted all_matchups dataset ready!")
head(all_matchups_adjusted)

