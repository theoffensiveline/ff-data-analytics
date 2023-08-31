# Load necessary libraries
library(tidyverse)
library(sleeperapi)
library(dplyr)
library(offensiveline)
library(ggplot2)
library(reshape2)

# Define the league ID, sleeper players file, and get NFL state
league_id <- 968890022248103936
sleeper_players_csv <- "sleeper_players.csv"
NFL_state <- get_sport_state('nfl')
current_week <- 5#NFL_state$week

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

# get MotW data
motw_data <- add_motw_to_matchups(
  matchup_data = all_matchups,
  week_1_matchup_id = 1,
  max_week = current_week
)

# create the motw output table
motw_table <- create_motw_table(motw_data)

# create the awards table
awards_table <- create_awards_table(player_data = all_players,
                                    matchup_data = all_matchups)

# create the scoring distribution
scoring_dist <- create_scoring_dist(
  matchup_data = all_matchups,
  max_week = current_week,
  color_1 = "#22763FFF",
  color_2 = "#5E4FA2FF"
)

# create weekly scoring chart
weekly_scores <-
  create_weekly_scoring_chart(matchup_data = all_matchups,
                              max_week = current_week)
