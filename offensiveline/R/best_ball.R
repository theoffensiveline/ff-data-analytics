#' Calculate Optimal (Best-Ball) Lineups
#'
#' For each team and week, determines the highest-scoring valid lineup by
#' assigning players to slots (QB, RB x2, WR x2, TE, FLEX W/R/T, DEF, K) that
#' maximize total points.
#'
#' @param player_data player-level data from \code{get_all_matchups_data()}
#' @param max_week the current week number (only weeks up to this are used)
#'
#' @return the input \code{player_data} with an added \code{optimal_slot} column
#'   (\code{"BENCH"} for players not in the optimal lineup)
#' @export
calc_best_ball_lineups <- function(player_data, max_week) {
  # create dict of all slots that different positions can play in
  possible_slots <-
    tibble::tibble(
      position = c("QB", "RB", "RB", "WR", "WR", "TE", "TE", "DEF", "K"),
      possible_slot = c("QB", "RB", "W/R/T", "WR", "W/R/T", "TE", "W/R/T", "DEF", "K")
    )
  # create dict of how many players can play in each slot
  slot_counts <-
    tibble::tibble(
      possible_slot = c("QB", "RB", "WR", "TE", "DEF", "K"),
      n_slots = c(1, 2, 2, 1, 1, 1)
    )
  # create a record for each player and each slot they could play at
  all_possible_slots <- player_data %>%
    left_join(possible_slots, by = "position") %>%
    left_join(slot_counts, by = "possible_slot") %>%
    #For each team, week and possible slot, rank the possibility by points (descending)
    group_by(team_name, week, possible_slot) %>%
    #Note the use of ties.method = first to ensure only one record per rank
    mutate(rank = rank(-points, ties.method = "first")) %>%
    ungroup()

  #Now limit just to those records where the rank is <= the number of available slots
  optimal_slots <- all_possible_slots %>%
    filter(rank <= n_slots)

  #The FLEX slot is special case, we need to select the best REMAINING player,
  #i.e. one not selected in optimal_slots
  flex_slot <- all_possible_slots %>%
    #limit just to the FLEX slot and remove anyone selected in optimal_slots
    filter(possible_slot == "W/R/T") %>%
    anti_join(optimal_slots %>% select(team_name, week, players)) %>%
    #Among those remaining, take the highest scoring
    group_by(team_name, week) %>%
    filter(rank(-points, ties.method = "first") == 1) %>%
    ungroup()

  #Now let's bring it all together with the original data.
  optimal_lineups <- player_data %>%
    #append the optimal slot (including flex)
    left_join({
      optimal_slots %>%
        bind_rows(flex_slot) %>%
        #concatenate the slot and the rank in that slot
        mutate(optimal_slot = paste0(possible_slot,
                                     ifelse(
                                       possible_slot %in% c("RB", "WR", "TE"), rank, ""
                                     ))) %>%
        select(team_name, week, players, optimal_slot)
    }, by = c("players", "week", "team_name")) %>%
    #Anyone without an optimal slot should be benched
    mutate(optimal_slot = coalesce(optimal_slot, "BENCH"))

  return(optimal_lineups)
}

#' Aggregate Optimal Lineups to Matchup Level
#'
#' Sums each team's optimal starter points per week and recomputes winners based
#' on best-ball scores.
#'
#' @param optimal_lineups result of \code{calc_best_ball_lineups()}
#'
#' @return a data frame with columns \code{week}, \code{manager_id},
#'   \code{team_name}, \code{winner}, \code{matchup_id}, \code{team_points}
#'   (optimal total)
#' @export
create_best_ball_matchups <-
  function(optimal_lineups) {
    matchups <- optimal_lineups %>%
      group_by(week, manager_id, team_name, winner, matchup_id) %>%
      dplyr::summarize("team_points" = sum(points[optimal_slot != "BENCH"]))

    matchups <- matchups %>%
      arrange(week, matchup_id, team_points) %>%
      group_by(matchup_id, week) %>%
      mutate(winner = case_when(
        team_points == max(team_points) ~ 1,
        TRUE ~ 0
      ))

    return(matchups)
  }

#' Calculate Points Left on the Bench
#'
#' Computes the difference between each team's optimal (best-ball) score and
#' their actual score, and ranks teams by how many points they left on the bench.
#'
#' @param matchup_data team-level matchup data from \code{get_team_matchups()}
#' @param optimal_lineups result of \code{calc_best_ball_lineups()}
#'
#' @return a data frame with columns from \code{matchup_data} plus
#'   \code{points_on_bench}, \code{most_bench_pts_rank},
#'   \code{least_bench_pts_rank}
#' @export
calc_bench_points <- function(matchup_data, optimal_lineups) {
  best_ball_matchups <- create_best_ball_matchups(optimal_lineups)

  bench_points <- matchup_data %>%
    left_join(best_ball_matchups,
              by = c('week', 'manager_id', 'team_name', 'matchup_id')) %>%
    mutate(points_on_bench = team_points.y - team_points.x) %>%
    select(-team_points.x, -team_points.y, -winner.x, -winner.y) %>%
    mutate(
      most_bench_pts_rank = rank(-points_on_bench, ties.method = "min"),
      least_bench_pts_rank = rank(points_on_bench, ties.method = "min")
    )

  return(bench_points)
}
