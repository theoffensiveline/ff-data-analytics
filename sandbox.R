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
