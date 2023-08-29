# this file contains functions to manipulate the sleeper league data

# This should not be run frequently, especially not more than once a day per sleeper docs
#' Update Player Data
#'
#' @param file_path a file path to save the csv file of player information to
#'
#' @return
#' @export
#'
#' @examples update_player_data("sleeper_players.csv")
update_player_data <- function(file_path) {
  # retrieve all NFL player data
  players <- get_all_nfl_players(TRUE)

  # write players data to csv
  write.csv(players, file = file_path)

  # clear environment
  rm(list = ls())
}

#' Title Get Player Data From Sleeper
#'
#' @param update_players whether or not to update players
#' @param file_path a file path to read/write the csv file of player information
#'
#' @return
#' @export
#'
#' @examples get_players_data("sleeper_players.csv")
#' @examples get_players_data(update_players = TRUE, "sleeper_players.csv")
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
#' Title Get Matchup Data for a Given Week
#'
#' @param week_number NFL week to get matchup data for
#' @param league_id Sleeper league id to get data from
#'
#' @return
#' @export
#'
#' @examples get_week_matchup_data(1, 996445270105714688)
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

#' Title Get Weekly Starters
#'
#' @param max_week the most recent week of the NFL season
#' @param league_id Sleeper league id to get data from
#'
#' @return
#' @export
#'
#' @examples get_all_starters_data(2, 996445270105714688)
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

#' Title Get All Matchup Data up to the Current Week
#'
#' @param max_week the most recent week of the NFL season
#' @param league_id Sleeper league id to get data from
#' @param file_path a file path to read the csv file of player information
#'
#' @return
#' @export
#'
#' @examples get_all_matchups_data(2, 996445270105714688, "sleeper_players.csv")
get_all_matchups_data <- function(max_week, league_id, file_path) {
  all_data <- data.frame()

  player_data <- get_players_data(update_players = FALSE, file_path = file_path)

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

#' Title Get Matchups for Each Team Each Week
#'
#' @param player_data result of get_all_matchups_data
#'
#' @return
#' @export
#'
#' @examples get_team_matchups(get_all_matchups_data(2, 996445270105714688, "sleeper_players.csv"))
get_team_matchups <- function(player_data) {
  data <- player_data %>%
    group_by(week, manager_id, team_name, winner, matchup_id) %>%
    summarise(team_points = max(team_points)) %>% ungroup()

  return(data)
}

