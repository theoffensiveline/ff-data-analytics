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
  filtered_rosters <- rosters[, c('roster_id', 'owner_id')]

  # get user info for the league
  users <- get_league_users(league_id)

  # select relevant columns and fill null teams names with display names
  filtered_users <- data.frame(
    owner_id = users$user_id,
    team_name = ifelse(
      is.na(users$metadata$team_name),
      users$display_name,
      users$metadata$team_name
    )
  )

  # merge roster data and team name data
  team_names_by_id <-
    merge(x = filtered_rosters, y = filtered_users, by = 'owner_id')

  # add a column to identify the winner of each matchup
  cleaned_matchups <- matchups %>%
    arrange(matchup_id, points) %>%
    group_by(matchup_id) %>%
    mutate(winner = case_when(points == max(points) ~ 1,
                              TRUE ~ 0))

  # join team names to matchup data
  cleaned_matchups <-
    merge(x = cleaned_matchups, y = team_names_by_id, by = 'roster_id')

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
    week_data <-
      get_week_matchup_data(week, league_id) # Collect data for the current week

    week_data <- data.frame(
      week = rep(week_data$week,
                 sapply(week_data$starters, length)),
      manager_id = rep(week_data$roster_id,
                       sapply(week_data$starters, length)),
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

  player_data <-
    get_players_data(update_players = FALSE, file_path = file_path)

  for (week in 1:max_week) {
    week_data <-
      get_week_matchup_data(week, league_id) # Collect data for the current week

    week_data_clean <- data.frame(
      week = rep(week_data$week,
                 sapply(week_data$players, length)),
      matchup_id = rep(week_data$matchup_id,
                       sapply(week_data$players, length)),
      manager_id = rep(week_data$roster_id,
                       sapply(week_data$players, length)),
      team_name = rep(week_data$team_name,
                      sapply(week_data$players, length)),
      winner = rep(week_data$winner,
                   sapply(week_data$players, length)),
      team_points = rep(week_data$points,
                        sapply(week_data$players, length)),
      players = unlist(week_data$players)
    )

    player_pts <-
      gather(week_data$players_points,
             key = 'player_id',
             value = 'points')

    player_pts <- player_pts %>%
      filter(!is.na(points))

    week_data_clean <-
      merge(week_data_clean,
            player_pts,
            by.x = 'players',
            by.y = 'player_id')

    all_data <-
      rbind(all_data, week_data_clean) # Concatenate the data
  }

  all_data <- merge(
    x = all_data,
    y = player_data,
    by.x = "players",
    by.y = "player_id"
  )

  starter_data <- get_all_starters_data(max_week, league_id)

  all_data <- merge(
    x = all_data,
    y = starter_data,
    by.x = c("players", "manager_id", "week"),
    by.y = c("starters", "manager_id", "week"),
    all.x = TRUE
  )

  all_data$full_name <-
    ifelse(is.na(all_data$full_name),
           all_data$players,
           all_data$full_name)

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

get_team_photos <- function(league_id) {
  # get user info for the league
  users <- get_league_users(league_id)
  filtered_users <- users$metadata %>% select(team_name, avatar)

  # Define a function to fetch image or use team_name as text
  get_image_or_text <- function(avatar, team_name) {
    return(ifelse(is.na(avatar), team_name, avatar))
  }

  # Apply the function to create a new column with image URLs or team names
  filtered_users$image_or_text <-
    mapply(get_image_or_text,
           filtered_users$avatar,
           filtered_users$team_name)

  return(filtered_users)
}


get_all_transaction_data <- function(max_week, league_id) {
  all_data <- data.frame() # Initialize an empty data frame

  for (week in 1:max_week) {
    week_data <-
      get_transactions(league_id, week) # Collect data for the current week

    result_df <- data.frame()

    # Iterate through each row of the transactions dataframe
    for (i in seq_len(nrow(week_data))) {
      # Extract information from the adds dataframe
      adds_df <- week_data$adds[i, ]

      # If adds_df is not NULL and not an empty dataframe, proceed with iteration
      if (!is.null(adds_df) &&
          ncol(adds_df) > 0 && nrow(adds_df) > 0) {
        # Iterate through each column of the adds dataframe
        for (player_id in intersect(names(adds_df), colnames(adds_df))) {
          # Ignore NA values
          if (!is.na(adds_df[[player_id]])) {
            # Create a new row with player_id, manager_id, type, and status
            new_row <- data.frame(
              week = week,
              trans_id = week_data$transaction_id[i],
              player_id = as.integer(player_id),
              # Convert player_id to integer
              manager_id = adds_df[[player_id]],
              type = week_data$type[i],
              status = week_data$status[i],
              add_drop = 'add'
            )

            # Append the new row to the result dataframe
            result_df <- rbind(result_df, new_row)
          }
        }
      }

      # Extract information from the drops dataframe
      drops_df <- week_data$drops[i, ]

      # If drops_df is not NULL and not an empty dataframe, proceed with iteration
      if (!is.null(drops_df) &&
          ncol(drops_df) > 0 && nrow(drops_df) > 0) {
        # Iterate through each column of the drops dataframe
        for (player_id in intersect(names(drops_df), colnames(drops_df))) {
          # Ignore NA values
          if (!is.na(drops_df[[player_id]])) {
            # Create a new row with player_id, manager_id, type, and status
            new_row <- data.frame(
              week = week,
              trans_id = week_data$transaction_id[i],
              player_id = as.integer(player_id),
              # Convert player_id to integer
              manager_id = drops_df[[player_id]],
              type = week_data$type[i],
              status = week_data$status[i],
              add_drop = 'drop'
            )

            # Append the new row to the result dataframe
            result_df <- rbind(result_df, new_row)
          }
        }
      }
    }

    all_data <- rbind(all_data, result_df) # Concatenate the data
  }

  return(all_data)
}

