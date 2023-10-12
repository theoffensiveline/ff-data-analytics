# Load necessary libraries
library(tidyverse)
library(sleeperapi)
library(dplyr)
library(offensiveline)
library(ggplot2)
library(reshape2)
library(kableExtra)
library(paletteer)
library(scales)
library(readr)
library(ggrepel)
library(png)
library(ggimage)
library(ggtext)
library(reticulate)

# Define the league ID, sleeper players file, and get NFL state
# league_id <- 968890022248103936 # Walter league
league_id <- 996445270105714688 # main league
sleeper_players_csv <- "sleeper_players.csv"
NFL_state <- get_sport_state('nfl')
current_week <- 5 #NFL_state$display_week
current_year <- 23

# team photos
team_photos <- get_team_photos(league_id)

# get all player data for each matchup
all_players <- get_all_matchups_data(current_week,
                                     league_id,
                                     sleeper_players_csv)

# summarize to the team level
all_matchups <- get_team_matchups(player_data = all_players)

# get MotW data
motw_data <- add_motw_to_matchups(
  matchup_data = all_matchups,
  week_1_matchup_id = 2,
  max_week = current_week,
  player_data = all_players
)

# create the motw output table
motw_table <- create_motw_table(motw_data)

# create the scoring distribution
scoring_dist <- create_scoring_dist(
  matchup_data = all_matchups,
  max_week = current_week,
  color_1 = "#20A4F4",
  color_2 = "#7D8491"
)

# create weekly scoring chart
weekly_scores <-
  create_weekly_scoring_chart(matchup_data = all_matchups,
                              max_week = current_week)

# create leaderboard
leaderboard <- create_leaderboard(matchup_data = all_matchups,
                                  max_week = current_week)

# create PF PA scatter
PF_PA_chart <- create_PF_PA_scatter(leaderboard = leaderboard,
                                    team_photos = team_photos)

# create rank chart
rank_chart <- create_rank_chart(matchup_data = all_matchups,
                                team_photos = team_photos)

# create power rankings
power_rankings <- create_power_rankings(
  matchup_data = all_matchups,
  max_week = current_week,
  number_of_teams = length(unique(all_matchups$manager_id))
)

# create best ball data and outputs
best_ball_lineups <-
  calc_best_ball_lineups(player_data = all_players,
                         max_week = current_week)

best_ball_matchups <-
  create_best_ball_matchups(optimal_lineups = best_ball_lineups)

best_ball_leaderboard <-
  create_leaderboard(matchup_data = best_ball_matchups,
                     max_week = current_week)

# create the awards table
awards_table <- create_awards_table(
  player_data = all_players,
  matchup_data = all_matchups,
  best_ball_matchups = best_ball_matchups
)

# chart from Sleeper about manager efficiency
efficiency_plot <-
  create_efficiency_plot(
    best_ball_matchups = best_ball_matchups,
    matchup_data = all_matchups,
    max_week = current_week
  )


# create motw shot history
shots_dist <- create_shots_dist(
  motw_data = motw_data,
  max_week = current_week,
  color_1 = "#20A4F4",
  color_2 = "#7D8491"
)

# motw data for python scripts - need to clean up
full_season_player_data <-
  get_all_matchups_data(14, league_id, sleeper_players_csv)
full_season_schedule <- get_team_matchups(full_season_player_data)
full_season_motw_schedule <-
  add_motw_to_matchups(
    full_season_schedule,
    week_1_matchup_id = 2,
    max_week = 2,
    full_season_player_data
  )

motw_schedule_output <-
  full_season_motw_schedule %>% select(week, team_name, matchup_id) %>%
  group_by(matchup_id, week) %>%
  mutate(team_order = row_number()) %>%
  pivot_wider(names_from = team_order,
              values_from = team_name,
              names_prefix = "team") %>%
  ungroup() %>%
  select(-matchup_id)

motw_schedule_output

write_csv(motw_schedule_output, "currentMotW/schedule23.csv", col_names = FALSE)

# run python script for MotW data - don't forget to edit first
py_run_file("motw.py")

danger_table <- create_danger_table()

danger_chart <- create_danger_chart()

