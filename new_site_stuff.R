library(jsonlite)
library(clipr)
library(dplyr)
library(tidyr)
library(offensiveline)

##### Leaderboard ######
leaderboard_data <- create_leaderboard(all_matchups, current_week)

###### Create Playoff Board ######
# Start with relevant columns
playoff_output <- leaderboard_data[, c("Rank", "Team", "W", "L")]

# Extract key values
last_losses <- max(playoff_output$L)
max_losses_top6 <- playoff_output$L[7]
max_losses_bottom6 <- playoff_output$L[6]

# Calculate Play-off # (magic number for clinching playoff spot)
playoff_output$`Play-off #` <- c(
  # Top 6: games needed to clinch vs 7th place
  15 - playoff_output$W[1:6] - max_losses_top6,
  # Bottom 6: games needed to catch 6th place
  15 - playoff_output$W[7:12] - max_losses_bottom6
)

# Calculate Last # (magic number for avoiding last place)
playoff_output$`Last #` <- (14 - current_week) - (last_losses - playoff_output$L - 1)

# Adjust last place team's Last # based on gap to 11th place
playoff_output$`Last #`[12] <- playoff_output$`Last #`[12] + 
  (playoff_output$L[12] - playoff_output$L[11])

# Add status indicators
playoff_output <- playoff_output %>%
  mutate(
    `Play-off #` = case_when(
      `Play-off #` <= 0 ~ "CLINCHED",
      `Play-off #` > 2 * (14 - current_week) + 1 ~ "ELIMINATED",
      TRUE ~ as.character(`Play-off #`)
    ),
    `Last #` = case_when(
      `Last #` <= 0 ~ "SAFE",
      TRUE ~ as.character(`Last #`)
    )
  )


###### Manual edit for playoff chances ######
playoff_chances <- leaderboard_data[,c('Team')]
playoff_chances
playoff_chances$`Play-off %` <- c(100,
                                  100,
                                  100,
                                  100,
                                  98.74,
                                  80.03,
                                  21.11,
                                  0.12,
                                  0,
                                  0,
                                  0,
                                  0)
playoff_chances$`WP Playoff %` <- c(100, 100, 100, 100, 98, 97, 3, 0.1, 0, 0, 0, 0)
playoff_chances$`Last %` <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 25.84, 74.16)

playoff_output <- merge(playoff_output, playoff_chances)

playoff_output <-
  playoff_output[order(playoff_output$Rank), c(2, 1, 3, 4, 7, 8, 5, 9, 6)]

row.names(playoff_output) <- NULL

playoff_output$PlayoffColor <- spec_color2_scale(
  playoff_output$`Play-off %`,
  scale_from = c(
    min(playoff_output$`Play-off %`),
    max(playoff_output$`Play-off %`)
  ),
  direction = 1
)

playoff_output$WPPlayoffColor <- spec_color2_scale(
  playoff_output$`WP Playoff %`,
  scale_from = c(
    min(playoff_output$`WP Playoff %`),
    max(playoff_output$`WP Playoff %`)
  ),
  direction = 1
)

playoff_output$LastColor <- spec_color2_scale(playoff_output$`Last %`,
                                              scale_from = c(min(playoff_output$`Last %`), max(playoff_output$`Last %`)),
                                              direction = -1)

playoff_output$PlayoffMagicColor <- ifelse(
  playoff_output$`Play-off #` == "CLINCHED",
  '#227740',
  ifelse(playoff_output$`Play-off #` == "ELIMINATED", '#bc293d', NA)
)

playoff_output$LastMagicColor <- ifelse(playoff_output$`Last #` == "SAFE",
                                        '#227740',
                                        ifelse(is.na(playoff_output$`Last #`), '#bc293d', NA))

write_clip(toJSON(playoff_output, pretty = TRUE))

# ###### median and best ball ######
# median_lb_data <- create_median_leaderboard(all_matchups, current_week)
#
# median_lb_data$PFColor <- spec_color2_scale(
#   median_lb_data$PF,
#   scale_from = c(
#     min(median_lb_data$PF),
#     max(median_lb_data$PF)
#   ),
#   direction = 1
# )
#
# median_lb_data$PAColor <- spec_color2_scale(
#   median_lb_data$PA,
#   scale_from = c(
#     min(median_lb_data$PA),
#     max(median_lb_data$PA)
#   ),
#   direction = -1
# )
#
# write_clip(toJSON(median_lb_data, pretty = TRUE))
#
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
#                                best_ball_matchup_data = best_ball_matchups,
#                                max_week = current_week)
#
# best_ball_leaderboard$PFColor <- spec_color2_scale(
#   best_ball_leaderboard$PF,
#   scale_from = c(
#     min(best_ball_leaderboard$PF),
#     max(best_ball_leaderboard$PF)
#   ),
#   direction = 1
# )
#
# best_ball_leaderboard$PAColor <- spec_color2_scale(
#   best_ball_leaderboard$PA,
#   scale_from = c(
#     min(best_ball_leaderboard$PA),
#     max(best_ball_leaderboard$PA)
#   ),
#   direction = -1
# )
#
# write_clip(toJSON(best_ball_leaderboard, pretty = TRUE))
#
#
#
# ###### schedule comparison ######
#
# your_data <- matchup_data %>%
#   select(week, team_name, image_or_text, matchup_id, team_points) %>%
#   group_by(week, matchup_id) %>%
#   arrange(team_name) %>%
#   mutate(other_team_points = case_when(
#     team_name == unique(team_name)[1] ~ lead(team_points, order_by = team_name),
#     team_name != unique(team_name)[1] ~ lag(team_points, order_by = team_name)
#   )) %>%
#   ungroup()
#
# team_list <- unique(your_data$team_name)
#
# records_list <- list()
#
# # Initialize team_records data frame
# team_records <- data.frame(team1 = character(), team2 = character(), wins = numeric(), losses = numeric(), ties = numeric(), stringsAsFactors = FALSE)
#
# for (team in team_list) {
#   # get team scores for each week
#   team_scores <- your_data %>%
#     filter(team_name == team) %>%
#     select(week, team_points, matchup_id)
#
#   for (team2 in team_list) {
#     # Initialize wins, losses, ties inside the inner loop
#     wins <- 0
#     losses <- 0
#     ties <- 0
#
#     opponent_scores <- your_data %>%
#       filter(team_name == team2) %>%
#       select(week, team_points, other_team_points, matchup_id)
#
#     merged_scores <- merge(team_scores, opponent_scores, by = "week")
#
#     print(merged_scores)
#
#     for (i in 1:nrow(merged_scores)) {
#       if ((merged_scores$matchup_id.x[i] == merged_scores$matchup_id.y[i]) &
#           (team != team2)
#           ) {
#         if (merged_scores$team_points.x[i] > merged_scores$team_points.y[i]) {
#           wins <- wins + 1
#         } else if (merged_scores$team_points.x[i] < merged_scores$team_points.y[i]) {
#           losses <- losses + 1
#         } else {
#           ties <- ties + 1
#         }
#       } else {
#         if (merged_scores$team_points.x[i] > merged_scores$other_team_points[i]) {
#           wins <- wins + 1
#         } else if (merged_scores$team_points.x[i] < merged_scores$other_team_points[i]) {
#           losses <- losses + 1
#         } else {
#           ties <- ties + 1
#         }
#       }
#     }
#     # Append record to list of records as a data frame
#     record <- data.frame(team1 = as.character(team), team2 = as.character(team2), wins = wins, losses = losses, ties = ties)
#     records_list <- c(records_list, list(record))
#   }
# }
#
# # Convert the list of records to a data frame using bind_rows
# team_records <- bind_rows(records_list)
#
# # Display the resulting data frame
# team_records
#
# best_records <- team_records %>%
#   group_by(team1) %>%
#   filter(wins == max(wins)) %>%
#   summarize(team2_list = list(team2),
#             wins = max(wins),
#             losses = max(losses),
#             ties = max(ties))
#
# worst_records <- team_records %>%
#   group_by(team1) %>%
#   filter(wins == min(wins)) %>%
#   summarize(team2_list = list(team2),
#             wins = max(wins),
#             losses = max(losses),
#             ties = max(ties))
#
# current_records <- team_records %>%
#   filter(team1 == team2) %>%
#   select(team1, wins, losses, ties)
#
# # Combine records into a list
# combined_list <- list(
#   best_records = best_records,
#   worst_records = worst_records,
#   current_records = current_records
# )
#
# write_clip(toJSON(combined_list, pretty = TRUE))
