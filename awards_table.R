##### Data Setup #####
# Load necessary libraries
library(tidyverse)
library(sleeperapi)
library(dplyr)
source('sleeper-etl.R')

# Define the league ID and current week
league_id <- 968890022248103936
current_week <- 2
current_year <- 2023

# Retrieve league details using the Sleeper API
league <- get_league(league_id)

# Get the previous league ID for this league
league_id_2 <- league$previous_league_id

# get all player data for each matchup
all_players <- get_matchup_data_data(current_week,league_id_2)

# summarize to the team level
all_matchups <- get_team_matchups(all_players)

create_awards_table <- function(player_data, matchup_data) {
  ##### Awards Table #####
  ###### Best and Worst Managers ######
  # find the best manager and points scored
  best_manager <- matchup_data %>% 
    mutate(team_points_rank = rank(-team_points)) %>%
    filter(week == current_week) %>% 
    slice_max(order_by = team_points)
  
  best_manager_desc <- sprintf(
    '%s - %s points (#%s this season)',
    best_manager$team_name,
    best_manager$team_points,
    best_manager$team_points_rank
  )
  
  awards <- data.frame(
    "Superlative" = "Best Managed Team",
    "Description" = best_manager_desc
  )
  
  # rename the columns of the table to be what is desired
  # can't do Winner + Description because + isn't allowed
  names(awards) <- c("Superlative", "Winner + Description")
  
  # find the worst manager and points scored
  worst_manager <- matchup_data %>% 
    mutate(team_points_rank = rank(team_points)) %>%
    filter(week == current_week) %>% 
    slice_max(order_by = -team_points)
  
  worst_manager_desc <- sprintf(
    '%s - %s points (#%s this season)',
    worst_manager$team_name,
    worst_manager$team_points,
    worst_manager$team_points_rank
  )
  
  awards <- rbind(awards, c(
    "Superlative" = "Most Mismanaged Team",
    "Winner + Description" = worst_manager_desc
  ))
  
  ###### Best Players ######
  # find the best QB this week
  awards <- rbind(awards, 
                  top_player_award(pos = "QB", 
                                   data = player_data,
                                   current_week = current_week,
                                   award_name = "Anti-Russ"))
  
  # find the best RB this week
  awards <- rbind(awards, 
                  top_player_award(pos = "RB", 
                                   data = player_data, 
                                   current_week = current_week,
                                   award_name = "Running Wild"))
  
  # find the best WR this week
  awards <- rbind(awards, 
                  top_player_award(pos = "WR", 
                                   data = player_data, 
                                   current_week = current_week,
                                   award_name = "Widest Receiver"))
  
  # find the best TE this week
  awards <- rbind(awards, 
                  top_player_award(pos = "TE", 
                                   data = player_data, 
                                   current_week = current_week,
                                   award_name = "Tighest End"))
  
  
  # # find the best K this week
  # awards <- rbind(awards, 
  #                 top_player_award(pos = "K", 
  #                                  data = player_data, 
  # current_week = current_week,
  #                                  award_name = "Das Boot"))
  # 
  # 
  # # find the best DEF this week
  # awards <- rbind(awards, 
  #                 top_player_award(pos = "DEF", 
  #                                  data = player_data, 
  # current_week = current_week,
  #                                  award_name = "Biggest D"))
  
  ###### MVP Award ######
  # get MVP data and award entry
  mvp_data <- player_data %>%
    filter(winner == 1, starter_id == 1) %>%
    mutate(percent_output = round((100 * points / team_points), 2)) %>%
    arrange(-percent_output) %>%
    mutate(mvp_rank = rank(-percent_output, ties.method = "min")) %>%
    filter(week == current_week)
  
  best_player <- mvp_data[which.max(mvp_data$percent_output), ]
  
  mvp_desc <- sprintf('%s played %s - %s%% of points (#%s this season)',
                      best_player$team_name,
                      best_player$full_name,
                      max(best_player$percent_output),
                      best_player$mvp_rank
                      )
  
  awards <- rbind(awards, c(
    "Superlative" = "MVP",
    "Winner + Description" = mvp_desc
  ))
  
  ###### Worst Winner and Best Loser ######
  # find the worst winner and points scored
  worst_winner <- matchup_data %>% 
    filter(winner == 1) %>%
    mutate(team_points_rank = rank(team_points)) %>%
    filter(week == current_week) %>% 
    slice_max(order_by = -team_points)
  
  worst_winner_desc <- sprintf(
    '%s - %s points (#%s this season)',
    worst_winner$team_name,
    worst_winner$team_points,
    worst_winner$team_points_rank
  )
  
  awards <- rbind(awards, c(
    "Superlative" = "Worst Winner",
    "Winner + Description" = worst_winner_desc
  ))
  
  # find the worst winner and points scored
  best_loser <- matchup_data %>% 
    filter(winner != 1) %>%
    mutate(team_points_rank = rank(-team_points)) %>%
    filter(week == current_week) %>% 
    slice_max(order_by = team_points)
  
  best_loser_desc <- sprintf(
    '%s - %s points (#%s this season)',
    best_loser$team_name,
    best_loser$team_points,
    best_loser$team_points_rank
  )
  
  awards <- rbind(awards, c(
    "Superlative" = "Best Loser",
    "Winner + Description" = best_loser_desc
  ))
  
  ###### Biggest Blowout and Closest Game ######
  game_margins <- matchup_data %>% group_by(week, matchup_id) %>%
    mutate(game_margin = team_points[winner == 1] - team_points[winner == 0]) %>%
    ungroup() %>%
    mutate(blowout_rank = dense_rank(-game_margin), 
           closest_rank = dense_rank(game_margin))
  
  biggest_blowout <- game_margins %>%
    filter(week == current_week) %>%
    slice_max(order_by = game_margin)
  
  biggest_blowout_desc <- sprintf(
    '%s defeated %s by %s points (#%s this season)',
    biggest_blowout$team_name[biggest_blowout$winner == 1],
    biggest_blowout$team_name[biggest_blowout$winner == 0],
    max(biggest_blowout$game_margin),
    max(biggest_blowout$blowout_rank)
  )
  
  awards <-  rbind(awards, c(
    "Superlative" = "Deadest Horse",
    "Winner + Description" = biggest_blowout_desc
  ))
  
  closest_game <- game_margins %>%
    filter(week == current_week) %>%
    slice_max(order_by = -game_margin)
  
  closest_game_desc <- sprintf(
    '%s defeated %s by %s points (#%s this season)',
    closest_game$team_name[closest_game$winner == 1],
    closest_game$team_name[closest_game$winner == 0],
    max(closest_game$game_margin),
    max(closest_game$closest_rank)
  )
  
  awards <-  rbind(awards, c(
    "Superlative" = "Photo Finish",
    "Winner + Description" = closest_game_desc
  ))
  
  ###### Best and Worst Benches - Leave this for once we have lineup info ######
  # optimal_lineups <- calc_optimal_lineups(player_data) 
  # 
  # 
  # awards <- rbind(awards, c(
  #   'Best Benchwarmers',
  #   paste0(
  #     last_week_optimize_scores$manager[which.max(last_week_optimize_scores$Points.on.Bench)],
  #     ' left ',
  #     max(last_week_optimize_scores$Points.on.Bench),
  #     ' points on the bench ',
  #     '(#',
  #     last_week_optimize_scores$high_rank[which.max(last_week_optimize_scores$Points.on.Bench)],
  #     ' this season)'
  #   )
  # ))
  # 
  # low_depth_managers <- last_week_optimize_scores$manager[which(
  #   last_week_optimize_scores$Points.on.Bench ==
  #     min(last_week_optimize_scores$Points.on.Bench)
  # )]
  # 
  # awards <- rbind(awards, c(
  #   'Least Depth',
  #   paste0(
  #     paste(low_depth_managers, collapse = ' and '),
  #     ' left ',
  #     min(last_week_optimize_scores$Points.on.Bench),
  #     ' points on the bench ',
  #     '(#',
  #     last_week_optimize_scores$low_rank[which.min(last_week_optimize_scores$Points.on.Bench)],
  #     ' this season)'
  #   )
  # ))
  return(awards)
}

##### Old Output from last season #####
# kable(awards,
#       "html",
#       booktabs = T,
#       escape = F,
#       align = 'l') %>%
#   #column_spec(1, width_min = '40px', width_max = "50%") %>%
#   #column_spec(2, width = "30%") %>%
#   cat(
#     .,
#     file = paste0(
#       "C:\\Users\\Trevor\\Documents\\Website\\public\\FantasyFootball",
#       current_year,
#       "\\Week",
#       current_week,
#       "\\awards_kable.html"
#     )
#   )
