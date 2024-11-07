library(jsonlite)
library(clipr)
library(dplyr)
library(tidyr)
library(offensiveline)

##### leaderboard ######
leaderboard_data <- create_leaderboard(all_matchups, current_week)

##### need to clean up #####

###### Create Playoff Board ######
playoff_output <- leaderboard_data[, c(1, 3, 4, 5)]
playoff_output$last_losses <- max(playoff_output$L)
playoff_output$max_losses_top6 <- playoff_output[6, 4]
playoff_output$max_losses_bottom6 <- playoff_output[7, 4]

# Helper Columns for Play-off #
playoff_output$`Play-off #` <- NA  # Initialize with NA

# Calculate Play-off # for top 6
playoff_output$`Play-off #`[1:6] <- (14 + 1 - playoff_output$W - playoff_output$max_losses_top6)[1:6,]

# Calculate Play-off # for bottom 6
playoff_output$`Play-off #`[7:12] <- (14 + 1 - playoff_output$W - playoff_output$max_losses_bottom6)[7:12,]

# Helper Column for Last #
playoff_output$`Last #` <- (14 - current_week) - (playoff_output$last_losses - playoff_output$L - 1)

# Finalizing Data Frame
playoff_output <- playoff_output[, c(1, 2, 3, 4, 8, 9)]

# Adjusting specific cells
playoff_output[12, 6] <- playoff_output[12, 6] + (playoff_output[12, 4] - playoff_output[11, 4])

# Using mutate for clearer assignments
playoff_output <- playoff_output %>%
  mutate(
    playoff_eliminated = ifelse(playoff_output[, 5] > 2 * (14 - current_week) + 1, 'ELIMINATED', NA),
    playoff_clinched = ifelse(playoff_output[, 5] <= 0, 'CLINCHED', NA),
    last_safe = ifelse(playoff_output[, 6] <= 0, 'SAFE', NA)
  )

# Coalesce final results into the desired columns
playoff_output$`Play-off #` <- coalesce(playoff_output$playoff_eliminated, playoff_output$`Play-off #`)
playoff_output$`Last #` <- coalesce(playoff_output$last_safe, playoff_output$`Last #`)

# Optionally drop helper columns if no longer needed
playoff_output <- playoff_output %>%
  select(-playoff_eliminated, -playoff_clinched, -last_safe)


###### Manual edit for playoff chances ######
playoff_chances <- leaderboard_data[, 3]
playoff_chances
playoff_chances$`Play-off %` <- c(98.46,
                                  95.79,
                                  84.17,
                                  79.98,
                                  75.51,
                                  30.05,
                                  53.10,
                                  36.22,
                                  25.40,
                                  5.75,
                                  7.23,
                                  8.34)
playoff_chances$`Last %` <- c(0.01, 0.02, 0.22, 0.18, 0.51, 2.93, 2.99, 4.29, 7.42, 27.37, 32.87, 21.20)

playoff_output <- merge(playoff_output, playoff_chances)

playoff_output <-
  playoff_output[order(playoff_output$Rank), c(2, 1, 3, 4, 7, 5, 8, 6)]

row.names(playoff_output) <- NULL

playoff_output$PlayoffColor <- spec_color2_scale(
  playoff_output$`Play-off %`,
  scale_from = c(
    min(playoff_output$`Play-off %`),
    max(playoff_output$`Play-off %`)
  ),
  direction = 1
)

playoff_output$LastColor <- spec_color2_scale(
  playoff_output$`Last %`,
  scale_from = c(
    min(playoff_output$`Last %`),
    max(playoff_output$`Last %`)
  ),
  direction = -1
)

playoff_output$PlayoffMagicColor <- ifelse(
  playoff_output$`Play-off #` == "CLINCHED",
  '#227740',
  ifelse(playoff_output$`Play-off #` == "ELIMINATED", '#bc293d', NULL))

playoff_output$LastMagicColor <- ifelse(
  playoff_output$`Last #` == "SAFE",
  '#227740',
  ifelse(is.na(playoff_output$`Last #`), '#bc293d', NULL))

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
