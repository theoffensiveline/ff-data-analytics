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

  player_name <-
    if (pos == 'DEF') {
      result$players
    } else {
      result$full_name
    }
  manager <- result$team_name
  player_points <- result$points
  player_rank <- result$rank

  return(
    list(
      player_name = player_name,
      manager = manager,
      player_points = player_points,
      player_rank = player_rank
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

create_awards_table <-
  function(player_data,
           matchup_data,
           best_ball_matchups) {
    ##### Awards Table #####
    ###### Best and Worst Managers ######
    # find the best manager and points scored
    best_manager <- matchup_data %>%
      mutate(team_points_rank = rank(-team_points, ties.method = "min")) %>%
      filter(week == current_week) %>%
      slice_max(order_by = team_points)

    best_manager_desc <- sprintf(
      '%s - %s points (#%s this season)',
      best_manager$team_name,
      best_manager$team_points,
      best_manager$team_points_rank
    )

    awards <- data.frame("Superlative" = "Best Managed Team",
                         "Description" = best_manager_desc)

    # rename the columns of the table to be what is desired
    # can't do Winner + Description because + isn't allowed
    names(awards) <- c("Superlative", "Winner + Description")

    # find the worst manager and points scored
    worst_manager <- matchup_data %>%
      mutate(team_points_rank = rank(team_points, ties.method = "min")) %>%
      filter(week == current_week) %>%
      slice_max(order_by = -team_points)

    worst_manager_desc <- sprintf(
      '%s - %s points (#%s this season)',
      worst_manager$team_name,
      worst_manager$team_points,
      worst_manager$team_points_rank
    )

    awards <- rbind(
      awards,
      c("Superlative" = "Most Mismanaged Team",
        "Winner + Description" = worst_manager_desc)
    )

    ###### Best Players ######
    # find the best QB this week
    awards <- rbind(
      awards,
      top_player_award(
        pos = "QB",
        data = player_data,
        current_week = current_week,
        award_name = "Literally Throwing"
      )
    )

    # find the best RB this week
    awards <- rbind(
      awards,
      top_player_award(
        pos = "RB",
        data = player_data,
        current_week = current_week,
        award_name = "Running Wild"
      )
    )

    # find the best WR this week
    awards <- rbind(
      awards,
      top_player_award(
        pos = "WR",
        data = player_data,
        current_week = current_week,
        award_name = "Widest Receiver"
      )
    )

    # find the best TE this week
    awards <- rbind(
      awards,
      top_player_award(
        pos = "TE",
        data = player_data,
        current_week = current_week,
        award_name = "Tighest End"
      )
    )


    # find the best K this week
    awards <- rbind(
      awards,
      top_player_award(
        pos = "K",
        data = player_data,
        current_week = current_week,
        award_name = "Das Boot"
      )
    )


    # find the best DEF this week
    awards <- rbind(
      awards,
      top_player_award(
        pos = "DEF",
        data = player_data,
        current_week = current_week,
        award_name = "Biggest D"
      )
    )

    ###### MVP Award ######
    # get MVP data and award entry
    mvp_data <- player_data %>%
      filter(winner == 1, starter_id == 1) %>%
      mutate(percent_output = round((100 * points / team_points), 2)) %>%
      arrange(-percent_output) %>%
      mutate(mvp_rank = rank(-percent_output, ties.method = "min")) %>%
      filter(week == current_week)

    best_player <- mvp_data[which.max(mvp_data$percent_output), ]

    mvp_desc <-
      sprintf(
        '%s played %s - %s%% of points (#%s this season)',
        best_player$team_name,
        best_player$full_name,
        max(best_player$percent_output),
        best_player$mvp_rank
      )

    awards <- rbind(awards,
                    c("Superlative" = "MVP",
                      "Winner + Description" = mvp_desc))

    ###### Worst Winner and Best Loser ######
    # find the worst winner and points scored
    worst_winner <- matchup_data %>%
      filter(winner == 1) %>%
      mutate(team_points_rank = rank(team_points, ties.method = "min")) %>%
      filter(week == current_week) %>%
      slice_max(order_by = -team_points)

    worst_winner_desc <- sprintf(
      '%s - %s points (#%s this season)',
      worst_winner$team_name,
      worst_winner$team_points,
      worst_winner$team_points_rank
    )

    awards <- rbind(awards,
                    c("Superlative" = "Worst Winner",
                      "Winner + Description" = worst_winner_desc))

    # find the worst winner and points scored
    best_loser <- matchup_data %>%
      filter(winner != 1) %>%
      mutate(team_points_rank = rank(-team_points, ties.method = "min")) %>%
      filter(week == current_week) %>%
      slice_max(order_by = team_points)

    best_loser_desc <- sprintf(
      '%s - %s points (#%s this season)',
      best_loser$team_name,
      best_loser$team_points,
      best_loser$team_points_rank
    )

    awards <- rbind(awards,
                    c("Superlative" = "Best Loser",
                      "Winner + Description" = best_loser_desc))

    ###### Biggest Blowout and Closest Game ######
    game_margins <- matchup_data %>% group_by(week, matchup_id) %>%
      mutate(game_margin = round(team_points[winner == 1] - team_points[winner == 0], 2)) %>% ungroup() %>%
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

    awards <-  rbind(
      awards,
      c("Superlative" = "Deadest Horse",
        "Winner + Description" = biggest_blowout_desc)
    )

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

    awards <-  rbind(awards,
                     c("Superlative" = "Photo Finish",
                       "Winner + Description" = closest_game_desc))

    ###### Best and Worst Benches - Leave this for once we have lineup info ######
    bench_points <- matchup_data %>%
      left_join(best_ball_matchups,
                by = c('week', 'manager_id', 'team_name', 'matchup_id')) %>%
      mutate(points_on_bench = team_points.y - team_points.x) %>%
      select(-team_points.x,-team_points.y,-winner.x,-winner.y) %>%
      mutate(
        most_bench_pts_rank = rank(-points_on_bench, ties.method = "min"),
        least_bench_pts_rank = rank(points_on_bench, ties.method = "min")
      )

    most_bench_points <- bench_points %>%
      filter(week == current_week) %>%
      slice_max(order_by = points_on_bench)

    most_bench_points_desc <- sprintf(
      '%s left %s points on the bench (#%s this season)',
      most_bench_points$team_name,
      most_bench_points$points_on_bench,
      most_bench_points$most_bench_pts_rank
    )

    awards <-  rbind(
      awards,
      c("Superlative" = "Warmest Bench",
        "Winner + Description" = most_bench_points_desc)
    )

    least_bench_points <- bench_points %>%
      filter(week == current_week) %>%
      slice_max(order_by = -points_on_bench)

    least_bench_points_desc <- sprintf(
      '%s left %s points on the bench (#%s this season)',
      paste(least_bench_points$team_name, collapse = " and "),
      round(least_bench_points$points_on_bench, 2),
      least_bench_points$least_bench_pts_rank
    )

    awards <-  rbind(
      awards,
      c("Superlative" = "Optimizer",
        "Winner + Description" = least_bench_points_desc)
    )

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
