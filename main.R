# Load necessary libraries
library(tidyverse)
library(sleeperapi)
library(dplyr)
library(offensiveline)

# Define the league ID and current week
league_id <- 968890022248103936
current_week <- 2
current_year <- 2023
sleeper_players_csv <- "sleeper_players.csv"

# Retrieve league details using the Sleeper API
league <- get_league(league_id)

# Get the previous league ID for this league
league_id_2 <- league$previous_league_id

# get all player data for each matchup
all_players <- get_all_matchups_data(current_week,
                                     league_id_2,
                                     sleeper_players_csv)

# summarize to the team level
all_matchups <- get_team_matchups(player_data = all_players)

awards_table <- create_awards_table(player_data = all_players,
                                    matchup_data = all_matchups)

