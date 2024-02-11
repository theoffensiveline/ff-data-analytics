##### Data prep #####
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
library(rcartocolor)

# Define the league ID, sleeper players file, and get NFL state
# league_id <- 968890022248103936 # Walter league
league_id <- 996445270105714688 # main league
sleeper_players_csv <- "sleeper_players.csv"
NFL_state <- get_sport_state('nfl')
current_week <- 17 #NFL_state$display_week
current_year <- 23

# team photos
team_photos <- get_team_photos(league_id)

# get all player data for each matchup
all_players <- get_all_matchups_data(current_week,
                                     league_id,
                                     sleeper_players_csv)

# get rid of NA
all_players <- all_players[complete.cases(all_players$matchup_id), ]

# summarize to the team level
all_matchups <- get_team_matchups(player_data = all_players)

# get rid of NA
all_matchups <- all_matchups[complete.cases(all_matchups$matchup_id), ]

# get MotW data
motw_data <- add_motw_to_matchups(
  matchup_data = all_matchups,
  week_1_matchup_id = 2,
  max_week = current_week,
  player_data = all_players
)

##### data outputs #####

# awardsTable.json
awards_json <- awards_to_json(all_matchups, all_players, current_week)
awards_file_path <- generate_file_path(
  current_year = current_year, 
  current_week = current_week, 
  file_name = "awardsTable.json"
)
write_json_to_file(awards_json, awards_file_path)

# bestBallLb.json
best_ball_json <- best_ball_lb_to_json(all_matchups, all_players, current_week)
best_ball_file_path <- generate_file_path(
  current_year = current_year, 
  current_week = current_week, 
  file_name = "bestBallLb.json"
)
write_json_to_file(best_ball_json, best_ball_file_path)

# efficiencyData.json
efficiency_json <- efficiency_to_json(all_matchups, all_players, current_week)
efficiency_file_path <- generate_file_path(
  current_year = current_year, 
  current_week = current_week, 
  file_name = "efficiencyData.json"
)
write_json_to_file(efficiency_json, efficiency_file_path)

# leaderboard.json
leaderboard_json <- leaderboard_to_json(all_matchups, current_week, team_photos)
leaderboard_file_path <- generate_file_path(
  current_year = current_year, 
  current_week = current_week, 
  file_name = "leaderboard.json"
)
write_json_to_file(leaderboard_json, leaderboard_file_path)

# matchupData.json
matchup_info_json <- matchup_info_to_json(all_matchups)
matchup_info_file_path <- generate_file_path(  
  current_year = current_year, 
  current_week = current_week, 
  file_name = "matchupData.json")
write_json_to_file(matchup_info_json, matchup_info_file_path)

# medianLb.json
median_lb_json <- median_lb_to_json(all_matchups, current_week)
median_lb_file_path <- generate_file_path(  
  current_year = current_year, 
  current_week = current_week, 
  file_name = "medianLb.json")
write_json_to_file(median_lb_json, median_lb_file_path)

# motwTable.json
motw_json <- motw_table_to_json(motw_data)
motw_file_path <- generate_file_path(
  current_year = current_year, 
  current_week = current_week,
  file_name = "motwTable.json"
)
write_json_to_file(motw_json, motw_file_path)

# playoffTable.json

# powerRankings.json
power_rankings_json <- power_rankings_to_json(all_matchups, current_week)
power_rankings_file_path <- generate_file_path(
  current_year = current_year, 
  current_week = current_week,
  file_name = "powerRankings.json"
)
write_json_to_file(power_rankings_json, power_rankings_file_path)

# scheduleData.json
schedule_json <- schedule_comparison_to_json(all_matchups, team_photos)
schedule_file_path  <- generate_file_path(
  current_year = current_year, 
  current_week = current_week,
  file_name = "scheduleData.json"
)
write_json_to_file(schedule_json, schedule_file_path)

# shotsDist.json
shots_json <- shots_dist_to_json(motw_data)
shots_file_path  <- generate_file_path(
  current_year = current_year, 
  current_week = current_week,
  file_name = "shotsDist.json"
)
write_json_to_file(shots_json, shots_file_path)

# starters.json
starters_json <- matchup_plot_to_json(all_players, current_week)
starters_file_path <- generate_file_path(
  current_year = current_year, 
  current_week = current_week,
  file_name = "starters.json"
)
write_json_to_file(starters_json, starters_file_path)

# # create weekly scoring chart
# weekly_scores <-
#   create_weekly_scoring_chart(matchup_data = all_matchups,
#                               max_week = current_week)
# 
# # create leaderboard
# leaderboard <- create_leaderboard(matchup_data = all_matchups,
#                                   max_week = current_week)
# 
# # create PF PA scatter
# PF_PA_chart <- create_PF_PA_scatter(leaderboard = leaderboard,
#                                     team_photos = team_photos)
# 
# # create rank chart
# rank_chart <- create_rank_chart(matchup_data = all_matchups,
#                                 team_photos = team_photos)
# 
# # create power rankings
# power_rankings <- create_power_rankings(
#   matchup_data = all_matchups,
#   max_week = current_week,
#   number_of_teams = length(unique(all_matchups$manager_id))
# )
# 
# 
# test <- calc_bench_points(all_matchups, best_ball_lineups)
# 
# # create best ball data and outputs
# best_ball_lineups <-
#   calc_best_ball_lineups(player_data = all_players,
#                          max_week = current_week)
# 
# best_ball_matchups <-
#   create_best_ball_matchups(optimal_lineups = best_ball_lineups)
# 
# best_ball_leaderboard <-
#   create_best_ball_leaderboard(matchup_data = all_matchups,
#     best_ball_matchup_data = best_ball_matchups,
#                      max_week = current_week)
# 
# # create the awards table
# awards_table <- create_awards_table(
#   player_data = all_players,
#   matchup_data = all_matchups,
#   best_ball_matchups = best_ball_matchups
# )
# 
# # chart from Sleeper about manager efficiency
# efficiency_plot <-
#   create_efficiency_plot(
#     best_ball_matchups = best_ball_matchups,
#     matchup_data = all_matchups,
#     max_week = current_week
#   )
# 
# # create motw shot history
# shots_dist <- create_shots_dist(
#   motw_data = motw_data,
#   max_week = current_week,
#   color_1 = "#20A4F4",
#   color_2 = "#7D8491"
# )
# 
# # motw data for python scripts - need to clean up
# full_season_player_data <-
#   get_all_matchups_data(14, league_id, sleeper_players_csv)
# full_season_schedule <- get_team_matchups(full_season_player_data)
# full_season_motw_schedule <-
#   add_motw_to_matchups(
#     full_season_schedule,
#     week_1_matchup_id = 2,
#     max_week = 2,
#     full_season_player_data
#   )
# 
# motw_schedule_output <-
#   full_season_motw_schedule %>% select(week, team_name, matchup_id) %>%
#   group_by(matchup_id, week) %>%
#   mutate(team_order = row_number()) %>%
#   pivot_wider(names_from = team_order,
#               values_from = team_name,
#               names_prefix = "team") %>%
#   ungroup() %>%
#   select(-matchup_id)
# 
# motw_schedule_output
# 
# write_csv(motw_schedule_output,
#           "currentMotW/schedule23.csv",
#           col_names = FALSE)
# 
# # run python script for MotW data - don't forget to edit first
# py_run_file("motw.py")
# 
# danger_table <- create_danger_table()
# 
# danger_chart <- create_danger_chart()
# 
# 
# ##### need to clean up #####
# 
# ###### Create Playoff Board ######
# playoff_output <- leaderboard[, c(1, 3, 4, 5)]
# playoff_output$last_losses <- max(playoff_output$L)
# playoff_output$max_losses_top6 <- playoff_output[6, 4]
# playoff_output$max_losses_bottom6 <- playoff_output[7, 4]
# playoff_output$`Play-off #`[7:12] <-
#   (14 + 1 - playoff_output$W - playoff_output$max_losses_bottom6)[7:12, ]
# playoff_output$`Play-off #`[1:6] <-
#   (14 + 1 - playoff_output$W - playoff_output$max_losses_top6)[1:6, ]
# playoff_output$`Last #` <-
#   (14 - current_week) - (playoff_output$last_losses - playoff_output$L - 1)
# 
# playoff_output <- playoff_output[, c(1, 2, 3, 4, 8, 9)]
# 
# playoff_output[12, 6] <-
#   playoff_output[12, 6] + (playoff_output[12, 4] - playoff_output[11, 4])
# playoff_output$`Play-off #` <- as.character(playoff_output$`Play-off #`)
# playoff_output[playoff_output[, 5] > 2 * (14 - current_week) + 1, 5] <- 'ELIMINATED'
# playoff_output[playoff_output[, 5] <= 0, 5] <- 'CLINCHED'
# playoff_output$`Last #` <- as.character(playoff_output$`Last #`)
# playoff_output[playoff_output[, 6] <= 0, 6] <- 'SAFE'
# # playoff_output[playoff_output[, 6] > 2 * (14 - current_week) + 1, 6] <- NA
# 
# ###### Manual edit for playoff chances ######
# playoff_chances <- leaderboard[, 3]
# playoff_chances$`Play-off %` <- c(100,
#                                   100,
#                                   100,
#                                   100,
#                                   100,
#                                   62,
#                                   21,
#                                   17,
#                                   0,
#                                   0,
#                                   0,
#                                   0)
# playoff_chances$`Last %` <- c(0, 0, 0, 0, 0, 0, 0, 0, 1.87, 3.43, 19.16, 75.54)
# 
# playoff_output <- merge(playoff_output, playoff_chances)
# 
# playoff_output <-
#   playoff_output[order(playoff_output$Rank), c(2, 1, 3, 4, 7, 5, 8, 6)]
# 
# row.names(playoff_output) <- NULL
# 
# # extra game against median each week
# median_leaderboard <- create_median_leaderboard(all_matchups, current_week)
# 
# 
# 
# # transactions <- get_transactions(league_id, 11)
# # 
# # transactions[transactions$type == 'trade', c('type', 'status')]
# # 
# # yahoo_trans <- read.csv("C:/Users/Trevor/Documents/Fantasy Football/transactions.csv")
# # 
# # yahoo_trades <- yahoo_trans[yahoo_trans$type == 'trade',]
# # 
# # yahoo_trades$week <- yahoo_trades$week_idx - 35
# # 
# # yahoo_trades %>%
# #   mutate(veto_id = case_when(ts %in% 
# #                                c('11/10/2022, 20:05:26',
# #                                  '10/25/2022, 20:28:13',
# #                                  '10/13/2022, 15:15:37',
# #                                  '10/13/2022, 13:48:34',
# #                                  '10/13/2022, 09:54:58',
# #                                  '10/12/2022, 08:49:01',
# #                                  '10/11/2022, 19:14:50') ~ 1,
# #                              TRUE ~ 0)) %>%
# #   group_by(type, week, veto_id) %>%
# #   summarize(count_distinct_ts = n_distinct(ts))
# # 
# # 
# # trades_by_week <- data.frame(week = c(1:12), 
# #                                      sleeper_trades = c(0,1,1,1,1,0,1,0,3,0,4,0),
# #                                      yahoo_trades = c(0,0,1,1,0,2,0,3,3,3,3,1),
# #                                      vetoed_yahoo_trades = c(0,0,0,0,0,5,0,1,0,0,1,0)
# #                                      )
# # 
# # # Assuming your data frame is named 'trades_by_week'
# # trades_by_week_plot <- ggplot(trades_by_week, aes(x = week)) +
# #   geom_line(aes(y = cumsum(yahoo_trades), color = "Yahoo Trades"), size = 1.2) +
# #   geom_line(aes(y = cumsum(vetoed_yahoo_trades), color = "Vetoed Yahoo Trades"), size = 1.2) +
# #   geom_line(aes(y = cumsum(sleeper_trades), color = "Sleeper Trades"), size = 1.2) +
# #   scale_color_manual(values = c("Yahoo Trades" = "#410093", "Vetoed Yahoo Trades" = "#FF3366", "Sleeper Trades" = "#20A4F4")) +
# #   labs(title = "Cumulative Trades by Week",
# #        x = "Week",
# #        y = "Cumulative Trades") +
# #   scale_x_continuous(breaks = seq(1, 12, 1)) +
# #   scale_y_continuous(breaks = seq(0, 20, 2)) +
# #   theme_classic()
# # 
# # trades_by_week_plot
# # 
# # # output chart
# # png(
# #   file = paste0(
# #     "C:\\Users\\Trevor\\Documents\\Website\\public\\FantasyFootball",
# #     current_year,
# #     "\\Week",
# #     current_week,
# #     "\\Trades by Week.png"
# #   ),
# #   width = 900,
# #   height = 600
# # )
# # trades_by_week_plot
# # dev.off()
