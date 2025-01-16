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
      is.na(users$team_name),
      users$display_name,
      users$team_name
    )
  )

  # merge roster data and team name data
  team_names_by_id <-
    merge(x = filtered_rosters, y = filtered_users, by = 'owner_id')

  # add a column to identify the winner of each matchup
  cleaned_matchups <- matchups %>%
    arrange(matchup_id, points) %>%
    group_by(matchup_id) %>%
    mutate(winner = case_when(points == max(points) ~ 1, TRUE ~ 0))

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
      week = rep(week_data$week, sapply(week_data$starters, length)),
      manager_id = rep(week_data$roster_id, sapply(week_data$starters, length)),
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

  # Get the rosters for the league
  rosters <- get_rosters(league_id)

  # get player nicknames from rosters
  player_nicknames <- rosters %>%
    pivot_longer(
      cols = starts_with("p_nick_"),
      names_to = "player_id",
      values_to = "nickname",
      names_transform = list(player_id = ~ sub("p_nick_", "", .))  # Remove the prefix
    ) %>%
    select(owner_id, player_id, nickname) %>%
    filter(!is.na(nickname)) %>%
    filter(nickname != "")

  for (week in 1:max_week) {
    week_data <-
      get_week_matchup_data(week, league_id) # Collect data for the current week

    week_data_clean <- data.frame(
      week = rep(week_data$week, sapply(week_data$players, length)),
      matchup_id = rep(week_data$matchup_id, sapply(week_data$players, length)),
      manager_id = rep(week_data$roster_id, sapply(week_data$players, length)),
      team_name = rep(week_data$team_name, sapply(week_data$players, length)),
      winner = rep(week_data$winner, sapply(week_data$players, length)),
      team_points = rep(week_data$points, sapply(week_data$players, length)),
      players = unlist(week_data$players)
    )

    player_pts <-
      pivot_longer(
        week_data,
        cols = c(
          -roster_id,
          -points,
          -players,
          -custom_points,
          -matchup_id,
          -starters,
          -starters_points,
          -winner,
          -owner_id,
          -team_name,
          -week
        ),
        names_to = 'player_id',
        values_to = 'player_points'
      ) %>%
      select(owner_id, player_id, points = player_points)

    player_pts <- player_pts %>%
      filter(!is.na(points)) %>%
      left_join(player_nicknames, by = c('player_id', 'owner_id'))

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
  filtered_users <- users %>% select(team_name, avatar_upload)

  # Define a function to fetch image or use team_name as text
  get_image_or_text <- function(avatar_upload, team_name) {
    return(ifelse(is.na(avatar_upload), team_name, avatar_upload))
  }

  # Apply the function to create a new column with image URLs or team names
  filtered_users$image_or_text <-
    mapply(get_image_or_text,
           filtered_users$avatar_upload,
           filtered_users$team_name)

  return(filtered_users)
}

get_transactions_sleeper <- function(league_id, round) {
  # Check if class of round parameter is numeric
  if (!is.numeric(round)) {
    # If not numeric, inform user and halt function
    stop("round parameter must be of type numeric")
  }
  # Query results from API given league ID and round specified
  x <- jsonlite::fromJSON(httr::content(httr::GET(
    paste0(
      "https://api.sleeper.app/v1/league/",
      league_id,
      "/transactions/",
      round
    )
  ), as = "text"))
  # Check if returned object is a list
  if (inherits(x, "list")) {
    # If returned object is a list, inform user and return nothing
    message("No data found. Were the league ID and round entered correctly?")
  } else {
    # If returned object is not a list, return object (which is a data frame)
    return(x)
  }
}

get_all_transaction_data <- function(league_id, max_week) {
  all_data_list <- list()  # Initialize an empty list to store data

  for (week in 1:max_week) {
    week_data <- get_transactions_sleeper(league_id, week)  # Collect data for the current week

    result_df <- data.frame()  # Temporary data frame for each week's transactions

    # Iterate through each row of the transactions dataframe for player adds/drops and FAAB trades
    for (i in seq_len(nrow(week_data))) {
      # Extract information from the adds dataframe
      adds_df <- week_data$adds[i, ]

      # If adds_df is not NULL and not an empty dataframe, proceed with iteration
      if (!is.null(adds_df) &&
          ncol(adds_df) > 0 && nrow(adds_df) > 0) {
        # Iterate through each column of the adds dataframe
        for (player_id in intersect(names(adds_df), colnames(adds_df))) {
          if (!is.na(adds_df[[player_id]])) {
            # Ignore NA values
            waiver_bid <- NA
            if ("settings" %in% names(week_data) &&
                "waiver_bid" %in% names(week_data$settings)) {
              waiver_bid <- week_data$settings$waiver_bid[i]
            }

            new_row <- data.frame(
              week = week,
              trans_id = week_data$transaction_id[i],
              player_id = player_id,
              manager_id = adds_df[[player_id]],
              type = week_data$type[i],
              status = week_data$status[i],
              add_drop = 'add',
              waiver_bid = waiver_bid
            )

            result_df <- rbind(result_df, new_row)
          }
        }
      }

      # Extract information from the drops dataframe
      drops_df <- week_data$drops[i, ]
      if (!is.null(drops_df) &&
          ncol(drops_df) > 0 && nrow(drops_df) > 0) {
        for (player_id in intersect(names(drops_df), colnames(drops_df))) {
          if (!is.na(drops_df[[player_id]])) {
            new_row <- data.frame(
              week = week,
              trans_id = week_data$transaction_id[i],
              player_id = player_id,
              manager_id = drops_df[[player_id]],
              type = week_data$type[i],
              status = week_data$status[i],
              add_drop = 'drop',
              waiver_bid = NA
            )

            result_df <- rbind(result_df, new_row)
          }
        }
      }

      # Handle waiver budget transactions (FAAB trades)
      if (length(week_data$waiver_budget) > 0 &&
          !is.null(week_data$waiver_budget[[i]]) &&
          length(week_data$waiver_budget[[i]]) > 0) {
        waiver_data <- week_data$waiver_budget[[i]]

        if (nrow(waiver_data) > 0) {
          for (j in 1:nrow(waiver_data)) {
            # Receiver gets FAAB
            new_row <- data.frame(
              week = week,
              trans_id = week_data$transaction_id[i],
              player_id = NA,
              # No player involved in the FAAB trade
              manager_id = waiver_data$receiver[j],
              type = "trade",
              status = week_data$status[i],
              add_drop = "add",
              waiver_bid = waiver_data$amount[j]
            )

            result_df <- rbind(result_df, new_row)

            # Sender loses FAAB
            new_row_sender <- data.frame(
              week = week,
              trans_id = week_data$transaction_id[i],
              player_id = NA,
              # No player involved in the FAAB trade
              manager_id = waiver_data$sender[j],
              type = "trade",
              status = week_data$status[i],
              add_drop = "drop",
              waiver_bid = waiver_data$amount[j]
            )

            result_df <- rbind(result_df, new_row_sender)
          }
        }
      }
    }

    all_data_list[[week]] <- result_df  # Store the week's data in the list
  }

  # Combine all data at the end of the loop
  all_data <- do.call(rbind, all_data_list)

  return(all_data)
}
