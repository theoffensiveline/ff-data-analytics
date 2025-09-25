##### Data Setup #####
#' Title Find the Top Player at a Position in a Week
#'
#' @param pos Position to look for
#' @param data player data
#' @param current_week Week to look at
#'
#' @return
#' @export
#'
#' @examples find_top_player("QB", player_data, 2)
find_top_player <- function(pos, data, current_week) {
  result <- data %>%
    filter(position == pos & starter_id == 1) %>%
    mutate(rank = rank(-points, ties.method = "min")) %>%
    filter(week == current_week) %>%
    slice_max(order_by = points)

  # Vectorized handling of multiple players
  player_name <- if (pos == 'DEF') {
    result$players
  } else {
    result$full_name
  }

  manager <- result$team_name
  player_points <- result$points
  player_rank <- result$rank

  # Construct player photo URLs, vectorized
  player_photo <- if (pos == "DEF") {
    paste0("https://sleepercdn.com/images/team_logos/nfl/", tolower(result$players), ".png")
  } else {
    paste0("https://sleepercdn.com/content/nfl/players/", result$players, ".jpg")
  }

  # Handle nicknames vectorized
  player_nickname <- result$nickname
  player_name <- ifelse(
    !is.na(player_nickname) & player_nickname != "",
    paste(player_name, " (", player_nickname, ")", sep = ""),
    player_name
  )

  # Return a list of all tied players
  return(
    list(
      player_name = player_name,
      manager = manager,
      player_points = player_points,
      player_rank = player_rank,
      player_photo = player_photo
    )
  )
}

#' Title Create the Top Player Award
#'
#' @param pos position for award
#' @param data player data
#' @param current_week week of award
#' @param award_name what to name the award
#'
#' @return
#' @export
#'
#' @examples top_player_award("QB", player_data, 2, "Anti-Russ")
top_player_award <- function(pos, data, current_week, award_name) {
  top_players <- find_top_player(pos, data, current_week)

  # Create a list of awards for each player (to handle ties)
  awards <- mapply(
    function(player_name, player_photo, player_points, player_rank, manager) {
      list(
        award = award_name,
        photo = player_photo,
        name = player_name,
        value = sprintf('Scored %s points for %s', player_points, manager),
        description = sprintf('This is the #%s %s performance of the season', player_rank, pos)
      )
    },
    player_name = top_players$player_name,
    player_photo = top_players$player_photo,
    player_points = top_players$player_points,
    player_rank = top_players$player_rank,
    manager = top_players$manager,
    SIMPLIFY = FALSE  # Return a list of awards, not a matrix
  )

  return(awards)
}

create_awards_table <-
  function(player_data,
           matchup_data,
           best_ball_matchups,
           team_photos) {
    ##### Awards Table #####
    ###### Best and Worst Managers ######
    # find the best manager and points scored
    best_manager <- matchup_data %>%
      mutate(team_points_rank = rank(-team_points, ties.method = "min")) %>%
      filter(week == current_week) %>%
      slice_max(order_by = team_points)

    awards <- data.frame(award = "Best Managed Team",
                         photo = team_photos[team_photos$team_name == best_manager$team_name, "image_or_text"],
                         name = best_manager$team_name,
                         value = sprintf('Scored %s points', best_manager$team_points),
                         description = sprintf('This is the #%s score this season', best_manager$team_points_rank))

    # find the worst manager and points scored
    worst_manager <- matchup_data %>%
      mutate(team_points_rank = rank(team_points, ties.method = "min")) %>%
      filter(week == current_week) %>%
      slice_max(order_by = -team_points)

    awards <- rbind(
      awards,
      c(award = "Most Mismanaged Team",
      photo = team_photos[team_photos$team_name == worst_manager$team_name, "image_or_text"],
      name = worst_manager$team_name,
      value = sprintf('Scored %s points', worst_manager$team_points),
      description = sprintf('This is the #%s lowest score this season', worst_manager$team_points_rank)))

    ###### Worst Winner and Best Loser ######
    # find the worst winner and points scored
    worst_winner <- matchup_data %>%
      filter(winner == 1) %>%
      mutate(team_points_rank = rank(team_points, ties.method = "min")) %>%
      filter(week == current_week) %>%
      slice_max(order_by = -team_points)

    awards <- rbind(
      awards,
      c(award = "Worst Winner",
        photo = team_photos[team_photos$team_name == worst_winner$team_name, "image_or_text"],
        name = worst_winner$team_name,
        value = sprintf('Scored %s points in their win', worst_winner$team_points),
        description = sprintf('This is the #%s worst winner this season', worst_winner$team_points_rank)))

    # find the worst winner and points scored
    best_loser <- matchup_data %>%
      filter(winner != 1) %>%
      mutate(team_points_rank = rank(-team_points, ties.method = "min")) %>%
      filter(week == current_week) %>%
      slice_max(order_by = team_points)

    awards <- rbind(
      awards,
      c(award = "Best Loser",
        photo = team_photos[team_photos$team_name == best_loser$team_name, "image_or_text"],
        name = best_loser$team_name,
        value = sprintf('Scored %s points in their loss', best_loser$team_points),
        description = sprintf('This is the #%s best loser this season', best_loser$team_points_rank)))

    ###### Biggest Blowout and Closest Game ######
    game_margins <- matchup_data %>% group_by(week, matchup_id) %>%
      mutate(game_margin = round(team_points[winner == 1] - team_points[winner == 0], 2)) %>% ungroup() %>%
      mutate(blowout_rank = dense_rank(-game_margin),
             closest_rank = dense_rank(game_margin))

    biggest_blowout <- game_margins %>%
      filter(week == current_week) %>%
      slice_max(order_by = game_margin)

    awards <- rbind(
      awards,
      c(award = "Deadest Horse",
        photo = team_photos[team_photos$team_name == biggest_blowout$team_name[biggest_blowout$winner == 1], "image_or_text"],
        name = biggest_blowout$team_name[biggest_blowout$winner == 1],
        value = sprintf('Defeated %s by %s points', biggest_blowout$team_name[biggest_blowout$winner == 0],max(biggest_blowout$game_margin)),
        description = sprintf('This is the #%s biggest blowout this season', biggest_blowout$blowout_rank)))

    closest_game <- game_margins %>%
      filter(week == current_week) %>%
      slice_max(order_by = -game_margin)

    awards <- rbind(
      awards,
      c(award = "Photo Finish",
        photo = team_photos[team_photos$team_name == closest_game$team_name[closest_game$winner == 1], "image_or_text"],
        name = closest_game$team_name[closest_game$winner == 1],
        value = sprintf('Defeated %s by %s points', closest_game$team_name[closest_game$winner == 0],max(closest_game$game_margin)),
        description = sprintf('This is the #%s closest game this season', closest_game$closest_rank)))

    ###### Best and Worst Benches - Leave this for once we have lineup info ######
    bench_points <- matchup_data %>%
      left_join(best_ball_matchups,
                by = c('week', 'manager_id', 'team_name', 'matchup_id')) %>%
      mutate(points_on_bench = round(team_points.y - team_points.x, 1)) %>%
      select(-team_points.x,-team_points.y,-winner.x,-winner.y) %>%
      mutate(
        most_bench_pts_rank = rank(-points_on_bench, ties.method = "min"),
        least_bench_pts_rank = rank(points_on_bench, ties.method = "min")
      )

    most_bench_points <- bench_points %>%
      filter(week == current_week) %>%
      slice_max(order_by = points_on_bench)

    awards <- rbind(
      awards,
      c(award = "Warmest Bench",
        photo = team_photos[team_photos$team_name == most_bench_points$team_name, "image_or_text"],
        name = most_bench_points$team_name,
        value = sprintf('Left %s points on the bench', most_bench_points$points_on_bench),
        description = sprintf('This is the #%s warmest bench this season', most_bench_points$most_bench_pts_rank)))

    least_bench_points <- bench_points %>%
      filter(week == current_week) %>%
      slice_max(order_by = -points_on_bench)

    awards <- rbind(
      awards,
      c(award = "Heaviest Top",
        photo = team_photos[team_photos$team_name == least_bench_points$team_name, "image_or_text"],
        name = least_bench_points$team_name,
        value = sprintf('Left %s points on the bench', least_bench_points$points_on_bench),
        description = sprintf('This is the #%s optimized lineup this season', least_bench_points$least_bench_pts_rank)))

    ###### MVP and Bench MVP Award ######
    # get MVP data and award entry
    mvp_data <- player_data %>%
      filter(winner == 1, starter_id == 1) %>%
      mutate(percent_output = round((100 * points / team_points), 2)) %>%
      arrange(-percent_output) %>%
      mutate(mvp_rank = rank(-percent_output, ties.method = "min")) %>%
      filter(week == current_week)

    best_player <- mvp_data[which.max(mvp_data$percent_output), ]

    best_player$name <- if (!is.null(best_player$nickname) && !is.na(best_player$nickname) && best_player$nickname != "") {
      paste(best_player$full_name, " (", best_player$nickname, ")", sep = "")
    } else {
      best_player$full_name
    }

    awards <- rbind(
      awards,
      c(award = "MVP",
        photo = if (best_player$position == "DEF") {
          paste0("https://sleepercdn.com/images/team_logos/nfl/", tolower(best_player$players), ".png")
        } else {
          paste0("https://sleepercdn.com/content/nfl/players/", best_player$players, ".jpg")
        },
        name = best_player$name,
        value = sprintf('Scored %s points for %s which was %s%% of the team total', best_player$points, best_player$team_name, best_player$percent_output),
        description = sprintf('This is the #%s MVP performance this season', best_player$mvp_rank)))

    # get best bench player
    bench_players <- player_data %>%
      filter(is.na(starter_id)) %>%
      mutate(bench_mvp_rank = rank(-points, ties.method = "min")) %>%
      filter(week == current_week)

    best_bench_player <- bench_players[which.max(bench_players$points), ]

    best_bench_player$name <- if (!is.null(best_bench_player$nickname) && !is.na(best_bench_player$nickname) && best_bench_player$nickname != "") {
      paste(best_bench_player$full_name, " (", best_bench_player$nickname, ")", sep = "")
    } else {
      best_bench_player$full_name
    }

    awards <- rbind(
      awards,
      c(award = "Bench MVP",
        photo = if (best_bench_player$position == "DEF") {
          paste0("https://sleepercdn.com/images/team_logos/nfl/", tolower(best_bench_player$players), ".png")
        } else {
          paste0("https://sleepercdn.com/content/nfl/players/", best_bench_player$players, ".jpg")
        },
        name = best_bench_player$name,
        value = sprintf('Scored %s points on the bench for %s', best_bench_player$points, best_bench_player$team_name),
        description = sprintf('This is the #%s bench performance this season', best_bench_player$bench_mvp_rank)))


    ###### Best Players ######
    # Define the positions and their corresponding award names
    positions_awards <- list(
      QB = "Literally Throwing",
      RB = "Running Wild",
      WR = "Widest Receiver",
      TE = "Tightest End",
      K  = "Das Boot",
      DEF = "Biggest D"
    )

    # Loop through each position and apply the `top_player_award` function
    for (pos in names(positions_awards)) {
      pos_awards <- top_player_award(
        pos = pos,
        data = player_data,
        current_week = current_week,
        award_name = positions_awards[[pos]]
      )

      # If there are multiple awards for a position, append numbers (1, 2, 3, etc.) to the name
      for (i in seq_along(pos_awards)) {
        award <- pos_awards[[i]]

        # Append a number to the award name if there are multiple awards for the position
        if (length(pos_awards) > 1) {
          award$award <- paste(award$award, i)
        }

        # Add the award to the main awards list
        awards <- rbind(awards, award)
      }
    }

    return(awards)
  }
