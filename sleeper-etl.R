# this file contains functions to manipulate the sleeper league data
# Load necessary libraries
library(tidyverse)
library(sleeperapi)
library(dplyr)

# This should not be run frequently, especially not more than once a day per sleeper docs
update_player_data <- function(file_path) {
  # retrieve all NFL player data
  players <- get_all_nfl_players(TRUE)
  
  # write players data to csv
  write.csv(players, file = file_path)
  
  # clear environment
  rm(list = ls())
}

# get player data from csv
get_players_data <- function(update_players = FALSE, file_path) {
  # update player data if needed
  if (update_players) {
    update_player_data()
  }
  # Read player data from a CSV file into 'players' data frame
  players <- read.csv(file = file_path)
  
  # Select only the columns 'player_id', 'full_name', and 'position' from 'players' data frame
  # and store it in 'players_important'
  players_important <-
    players[, c("player_id", "full_name", "position")]
  
  return(players_important)
}

# function to get matchup data for a given week
get_week_matchup_data <- function(week_number, league_id) {
  # Get the matchups for the current week
  matchups <- get_matchups(league_id, week_number)
  
  # Get the rosters for the league
  rosters <- get_rosters(league_id)
  
  # select relevant columns
  filtered_rosters <- rosters[,c('roster_id', 'owner_id')]
  
  # get user info for the league
  users <- get_league_users(league_id)
  
  # select relevant columns and fill null teams names with display names
  filtered_users <- data.frame(
    owner_id = users$user_id,
    team_name = ifelse(is.na(users$metadata$team_name), users$display_name, users$metadata$team_name)
  )
  
  # merge roster data and team name data
  team_names_by_id <- merge(x = filtered_rosters, y = filtered_users, by = 'owner_id')
  
  # add a column to identify the winner of each matchup
  cleaned_matchups <- matchups %>%
    arrange(matchup_id, points) %>%
    group_by(matchup_id) %>%
    mutate(winner = case_when(
      points == max(points) ~ 1,
      TRUE ~ 0
    ))
  
  # join team names to matchup data
  cleaned_matchups <- merge(x = cleaned_matchups, y = team_names_by_id, by = 'roster_id')
  
  # add a column that has the week
  cleaned_matchups$week <- week_number
  
  return(cleaned_matchups)
}

get_all_starters_data <- function(max_week, league_id) {
  all_data <- data.frame() # Initialize an empty data frame
  
  for (week in 1:max_week) {
    week_data <- get_week_matchup_data(week, league_id) # Collect data for the current week
    
    week_data <- data.frame(
      week = rep(
        week_data$week,
        sapply(week_data$starters, length)
      ),
      manager_id = rep(
        week_data$roster_id,
        sapply(week_data$starters, length)
      ),
      starters = unlist(week_data$starters)
    )
    
    all_data <- rbind(all_data, week_data) # Concatenate the data
  }
  
  all_data$starter_id <- 1
  
  return(all_data)
}

get_all_matchups_data <- function(max_week, league_id) {
  all_data <- data.frame()
  
  player_data <- get_players_data()
  
  for (week in 1:max_week) {
    week_data <- get_week_matchup_data(week, league_id) # Collect data for the current week
    
    week_data_clean <- data.frame(
      week = rep(
        week_data$week,
        sapply(week_data$players, length)
      ),
      matchup_id = rep(
        week_data$matchup_id,
        sapply(week_data$players, length)
      ),
      manager_id = rep(
        week_data$roster_id,
        sapply(week_data$players, length)
      ),
      team_name = rep(
        week_data$team_name,
        sapply(week_data$players, length)
      ),
      winner = rep(
        week_data$winner,
        sapply(week_data$players, length)
      ),
      team_points = rep(
        week_data$points,
        sapply(week_data$players, length)
      ),
      players = unlist(week_data$players)
    )
    
    player_pts <- gather(week_data$players_points, key = 'player_id', value = 'points')
    
    player_pts <- player_pts %>%
      filter(!is.na(points))
    
    week_data_clean <- merge(week_data_clean, player_pts, by.x = 'players', by.y = 'player_id')
    
    all_data <- rbind(all_data, week_data_clean) # Concatenate the data
  }
    
  all_data <- merge(x = all_data, y = player_data,
                    by.x = "players", by.y = "player_id")
  
  starter_data <- get_all_starters_data(max_week, league_id)
  
  all_data <- merge(x = all_data, y = starter_data, 
                    by.x = c("players", "manager_id", "week"), by.y = c("starters", "manager_id", "week"),
                    all.x = TRUE)
  
  return(all_data)
}

get_team_matchups <- function(all_matchup_data) {
  data <- all_matchup_data %>% 
    group_by(week, manager_id, team_name, winner, matchup_id) %>% 
    summarise(team_points = max(team_points)) %>% ungroup()
  
  return(data)
}

find_top_player <- function(pos, data, current_week) {
  result <- data %>%
    filter(position == pos & starter_id == 1) %>%
    mutate(rank = rank(-points)) %>%
    filter(week == current_week) %>%
    slice_max(order_by = points)
  
  player_name <- result$full_name
  manager <- result$team_name
  player_points <- result$points
  player_rank <- result$rank
  
  return(list(player_name = player_name, 
              manager = manager, 
              player_points = player_points,
              player_rank = player_rank))
}

top_player_award <- function(pos, data, current_week, award_name) {
  top_player <- find_top_player(pos, data, current_week)
  
  award_text <- c(
    award_name,
    sprintf(
    '%s played %s - %s points (#%s this season)',
    paste(top_player$manager, collapse = " and "),
    paste(top_player$player_name, collapse = " and "),
    top_player$player_points,
    top_player$player_rank
  )
  )
  
  return(award_text)
}

