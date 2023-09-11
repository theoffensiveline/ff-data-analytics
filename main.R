# Load necessary libraries
library(tidyverse)
library(sleeperapi)
library(dplyr)
library(offensiveline)
library(ggplot2)
library(reshape2)

# Define the league ID, sleeper players file, and get NFL state
# league_id <- 968890022248103936
league_id <- 996445270105714688
sleeper_players_csv <- "sleeper_players.csv"
NFL_state <- get_sport_state('nfl')
current_week <- NFL_state$week

# Retrieve league details using the Sleeper API
#league <- get_league(league_id)

# Get the previous league ID for this league
#league_id_2 <- league$previous_league_id

# get all player data for each matchup
all_players <- get_all_matchups_data(current_week,
                                     league_id,
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

# create leaderboard
leaderboard <- create_leaderboard(matchup_data = all_matchups,
                                  max_week = current_week)

# create power rankings
power_rankings <- create_power_rankings(matchup_data = all_matchups,
                                        max_week = current_week,
                                        number_of_teams = length(unique(all_matchups$manager_id)))

# create best ball data and outputs
best_ball_lineups <- calc_best_ball_lineups(player_data = all_players, max_week = current_week)

best_ball_matchups <- create_best_ball_matchups(optimal_lineups = best_ball_lineups)

best_ball_leaderboard <- create_leaderboard(matchup_data = best_ball_matchups,
                                            max_week = current_week)

# create the awards table
awards_table <- create_awards_table(player_data = all_players,
                                    matchup_data = all_matchups,
                                    best_ball_matchups = best_ball_matchups)
