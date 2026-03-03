#' Assign winner/loser to each side of a trade
#'
#' Takes a pre-processed trade data frame (one row per player per team per
#' transaction, with \code{total_points} already summed per team per trans_id)
#' and assigns a \code{winner} column via the case_when logic.
#'
#' @param trades_data data frame with columns \code{trans_id}, \code{team_name},
#'   \code{total_points}, \code{waiver_bid}
#' @return same data frame with \code{winner} column added
#' @export
evaluate_trades <- function(trades_data) {
  trades_data %>%
    dplyr::group_by(trans_id) %>%
    dplyr::mutate(
      winner = dplyr::case_when(
        max(total_points) > 0 & total_points == max(total_points) ~ 1L,
        max(total_points) == 0 &
          sum(!is.na(waiver_bid)) == 1 & !is.na(waiver_bid) ~ 1L,
        max(total_points) == 0 &
          sum(!is.na(waiver_bid)) == 1 & is.na(waiver_bid) ~ 0L,
        max(total_points) == 0 &
          length(unique(waiver_bid[!is.na(waiver_bid)])) == 2 ~ dplyr::case_when(
            waiver_bid == max(waiver_bid, na.rm = TRUE) ~ 1L,
            waiver_bid == min(waiver_bid, na.rm = TRUE) ~ 0L
          ),
        max(total_points) == 0 &
          length(unique(waiver_bid[!is.na(waiver_bid)])) == 1 ~ NA_integer_,
        max(total_points) == 0 &
          sum(waiver_bid == max(waiver_bid, na.rm = TRUE), na.rm = TRUE) > 1 ~ NA_integer_,
        TRUE ~ 0L
      )
    ) %>%
    dplyr::ungroup()
}


#' Summarize best and worst win/loss scores per team
#'
#' @param all_matchups matchup data
#' @return data frame with one row per team: \code{manager_id}, \code{team_name},
#'   score extremes, and associated color columns
#' @export
summarize_win_loss_extremes <- function(all_matchups) {
  all_matchups %>%
    dplyr::group_by(manager_id, team_name, winner) %>%
    dplyr::summarise(
      most_points  = max(team_points),
      least_points = min(team_points),
      .groups = "drop"
    ) %>%
    tidyr::pivot_wider(
      names_from  = winner,
      values_from = c(most_points, least_points)
    ) %>%
    dplyr::mutate(
      best_win_color = spec_color2_scale(
        most_points_1,
        scale_from = c(min(least_points_0, na.rm = TRUE),
                       max(most_points_1, na.rm = TRUE)),
        direction  = 1
      ),
      worst_win_color = spec_color2_scale(
        least_points_1,
        scale_from = c(min(least_points_0, na.rm = TRUE),
                       max(most_points_1, na.rm = TRUE)),
        direction  = 1
      ),
      best_loss_color = spec_color2_scale(
        most_points_0,
        scale_from = c(min(least_points_0, na.rm = TRUE),
                       max(most_points_1, na.rm = TRUE)),
        direction  = 1
      ),
      worst_loss_color = spec_color2_scale(
        least_points_0,
        scale_from = c(min(least_points_0, na.rm = TRUE),
                       max(most_points_1, na.rm = TRUE)),
        direction  = 1
      )
    )
}


#' Summarize MotW appearances, record, and shots taken per team
#'
#' @param motw_data result of \code{add_motw_to_matchups}
#' @return data frame with one row per team: \code{team_name},
#'   \code{motw_count}, \code{motw_wins}, \code{motw_losses}, \code{shots_dogs}
#' @export
summarize_motw_record <- function(motw_data) {
  motw_data %>%
    dplyr::filter(motw == 1) %>%
    dplyr::group_by(team_name) %>%
    dplyr::summarise(
      motw_count  = dplyr::n(),
      motw_wins   = sum(winner),
      motw_losses = dplyr::n() - sum(winner),
      shots_dogs  = sum(ifelse(winner != 1, `# of Shots`, 0)),
      .groups     = "drop"
    )
}


#' Summarize shots/dogs given out by each MotW winner
#'
#' @param motw_data result of \code{add_motw_to_matchups}
#' @return data frame with one row per team: \code{team_name},
#'   \code{shots_dogs_given_out}
#' @export
summarize_motw_given <- function(motw_data) {
  motw_data %>%
    dplyr::filter(motw == 1) %>%
    dplyr::left_join(
      motw_data %>%
        dplyr::filter(motw == 1) %>%
        dplyr::select(week, matchup_id, team_points, `# of Shots`),
      by           = c("matchup_id", "week"),
      relationship = "many-to-many"
    ) %>%
    dplyr::filter(team_points.x != team_points.y) %>%
    dplyr::group_by(team_name) %>%
    dplyr::filter(winner == 1) %>%
    dplyr::summarise(shots_dogs_given_out = sum(`# of Shots.y`), .groups = "drop")
}


#' Count weeks each team was the best/worst scorer in the league
#'
#' @param all_matchups matchup data
#' @return data frame with one row per \code{manager_id}: best/worst team week
#'   counts and associated color columns
#' @export
summarize_weekly_rank <- function(all_matchups) {
  all_matchups %>%
    dplyr::group_by(week, matchup_id) %>%
    dplyr::arrange(manager_id) %>%
    dplyr::mutate(other_team_points = dplyr::case_when(
      manager_id == unique(manager_id)[1] ~ dplyr::lead(team_points, order_by = manager_id),
      manager_id != unique(manager_id)[1] ~ dplyr::lag(team_points,  order_by = manager_id)
    )) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(week) %>%
    dplyr::mutate(
      pts_rank            = rank(-team_points),
      other_team_pts_rank = rank(-other_team_points),
      number_of_teams     = dplyr::n()
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(manager_id) %>%
    dplyr::select(pts_rank, other_team_pts_rank, number_of_teams) %>%
    dplyr::summarise(
      worst_team_week_count       = sum(pts_rank == number_of_teams),
      other_team_worst_week_count = sum(other_team_pts_rank == number_of_teams),
      best_team_week_count        = sum(pts_rank == 1),
      other_team_best_week_count  = sum(other_team_pts_rank == 1),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      best_team_color = spec_color2_scale(
        best_team_week_count,
        scale_from = c(min(best_team_week_count), max(best_team_week_count)),
        direction  = 1
      ),
      worst_team_color = spec_color2_scale(
        worst_team_week_count,
        scale_from = c(min(worst_team_week_count), max(worst_team_week_count)),
        direction  = -1
      ),
      best_other_team_color = spec_color2_scale(
        other_team_best_week_count,
        scale_from = c(min(other_team_best_week_count), max(other_team_best_week_count)),
        direction  = -1
      ),
      worst_other_team_color = spec_color2_scale(
        other_team_worst_week_count,
        scale_from = c(min(other_team_worst_week_count), max(other_team_worst_week_count)),
        direction  = 1
      )
    )
}


#' Summarize average point differential, close games, and blowouts per team
#'
#' @param all_matchups matchup data
#' @return data frame with one row per \code{manager_id}: \code{pt_diff_1},
#'   \code{pt_diff_0}, \code{close_games_1}, \code{close_games_0},
#'   \code{blowouts_1}, \code{blowouts_0}, and associated color columns
#' @export
summarize_pt_diff <- function(all_matchups) {
  all_matchups %>%
    dplyr::group_by(week, matchup_id) %>%
    dplyr::arrange(manager_id) %>%
    dplyr::mutate(other_team_points = dplyr::case_when(
      manager_id == unique(manager_id)[1] ~ dplyr::lead(team_points, order_by = manager_id),
      manager_id != unique(manager_id)[1] ~ dplyr::lag(team_points,  order_by = manager_id)
    )) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(manager_id, winner) %>%
    dplyr::summarize(
      pt_diff     = round(mean(abs(team_points - other_team_points)), 2),
      close_games = sum(abs(team_points - other_team_points) < 10),
      blowouts    = sum(abs(team_points - other_team_points) > 40),
      .groups     = "drop"
    ) %>%
    tidyr::pivot_wider(
      names_from  = winner,
      values_from = c(close_games, blowouts, pt_diff)
    ) %>%
    replace(is.na(.), 0) %>%
    dplyr::mutate(
      pt_diff_1_color = spec_color2_scale(
        pt_diff_1,
        scale_from = c(min(pt_diff_1), max(pt_diff_1)),
        direction  = 1
      ),
      pt_diff_0_color = spec_color2_scale(
        pt_diff_0,
        scale_from = c(min(pt_diff_0), max(pt_diff_0)),
        direction  = -1
      ),
      blowouts_1_color = spec_color2_scale(
        blowouts_1,
        scale_from = c(min(blowouts_1), max(blowouts_1)),
        direction  = 1
      ),
      blowouts_0_color = spec_color2_scale(
        blowouts_0,
        scale_from = c(min(blowouts_0), max(blowouts_0)),
        direction  = -1
      ),
      close_games_1_color = spec_color2_scale(
        close_games_1,
        scale_from = c(min(close_games_1), max(close_games_1)),
        direction  = 1
      ),
      close_games_0_color = spec_color2_scale(
        close_games_0,
        scale_from = c(min(close_games_0), max(close_games_0)),
        direction  = -1
      )
    )
}


#' Build the end-of-season recap data frame
#'
#' Orchestrates all recap summary helpers into a single wide data frame
#' suitable for JSON serialization.
#'
#' @param all_matchups matchup data (all weeks including playoffs)
#' @param motw_data result of \code{add_motw_to_matchups}
#' @param all_transactions transaction data from \code{get_all_transaction_data}
#' @param all_players player-level data from \code{get_all_matchups_data}
#' @param sleeper_players data frame with columns \code{player_id} and
#'   \code{full_name}; if \code{NULL}, read from \code{"sleeper_players.csv"}
#' @param regular_season_weeks last week of the regular season (default 14)
#' @return a wide data frame with one row per team
#' @export
build_recap_data <- function(all_matchups,
                             motw_data,
                             all_transactions,
                             all_players,
                             sleeper_players      = NULL,
                             regular_season_weeks = 14L) {
  if (is.null(sleeper_players)) {
    sleeper_players <- read.csv("sleeper_players.csv") %>%
      dplyr::select(player_id, full_name) %>%
      dplyr::mutate(full_name = dplyr::coalesce(full_name, player_id))
  }

  max_week     <- max(all_players$week)
  all_starters <- all_players[!is.na(all_players$starter_id), ]

  starter_ppg <- all_starters %>%
    dplyr::group_by(team_name, full_name, position, players, manager_id) %>%
    dplyr::summarize(
      games_played = dplyr::n(),
      ppg          = round(mean(points), 2),
      points       = sum(points),
      .groups      = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(ppg))

  team_names <- all_players %>%
    dplyr::group_by(manager_id) %>%
    dplyr::summarise(team_name = unique(team_name), .groups = "drop")

  # ── Seed from regular-season leaderboard ────────────────────────────────
  leaderboard <- create_leaderboard(all_matchups, regular_season_weeks)
  recap_data  <- leaderboard %>% dplyr::select(-Trend, -Rank)

  # ── Win/loss extremes (colors computed inside helper) ───────────────────
  recap_data <- summarize_win_loss_extremes(all_matchups) %>%
    dplyr::right_join(recap_data, by = c("team_name" = "Team"))

  # ── MotW record ──────────────────────────────────────────────────────────
  recap_data <- summarize_motw_record(motw_data) %>%
    dplyr::right_join(recap_data, by = "team_name") %>%
    replace(is.na(.), 0) %>%
    dplyr::mutate(
      shots_dogs_taken_color = spec_color2_scale(
        shots_dogs,
        scale_from = c(min(shots_dogs), max(shots_dogs)),
        direction  = -1
      ),
      motw_wins_color = spec_color2_scale(
        motw_wins,
        scale_from = c(min(motw_wins), max(motw_wins)),
        direction  = 1
      ),
      motw_losses_color = spec_color2_scale(
        motw_losses,
        scale_from = c(min(motw_losses), max(motw_losses)),
        direction  = -1
      )
    )

  # ── MotW shots given ─────────────────────────────────────────────────────
  recap_data <- summarize_motw_given(motw_data) %>%
    dplyr::right_join(recap_data, by = "team_name") %>%
    replace(is.na(.), 0) %>%
    dplyr::mutate(
      shots_dogs_given_out_color = spec_color2_scale(
        shots_dogs_given_out,
        scale_from = c(min(shots_dogs_given_out), max(shots_dogs_given_out)),
        direction  = 1
      )
    )

  # ── Weekly rank (best/worst team) ────────────────────────────────────────
  # Colors computed inside summarize_weekly_rank()
  recap_data <- summarize_weekly_rank(all_matchups) %>%
    dplyr::right_join(recap_data, by = "manager_id")

  # ── Point differential / close games / blowouts ──────────────────────────
  # Colors computed inside summarize_pt_diff()
  recap_data <- summarize_pt_diff(all_matchups) %>%
    dplyr::right_join(recap_data, by = "manager_id")

  # ── Distinct starters ────────────────────────────────────────────────────
  recap_data <- all_starters %>%
    dplyr::group_by(manager_id) %>%
    dplyr::summarize(distinct_starters = dplyr::n_distinct(full_name), .groups = "drop") %>%
    dplyr::mutate(
      distinct_starters_color = spec_color2_scale(
        distinct_starters,
        scale_from = c(min(distinct_starters), max(distinct_starters)),
        direction  = -1
      )
    ) %>%
    dplyr::right_join(recap_data, by = "manager_id")

  # ── Transaction summary ──────────────────────────────────────────────────
  recap_data <- all_transactions %>%
    dplyr::group_by(manager_id) %>%
    dplyr::summarize(
      completed_waivers   = sum((type == "waiver") & (status == "complete")),
      total_faab_spent    = sum(waiver_bid[(type == "waiver") & (status == "complete")], na.rm = TRUE),
      failed_waivers      = sum((type == "waiver") & (status == "failed")),
      total_faab_failed   = sum(waiver_bid[(type == "waiver") & (status == "failed")], na.rm = TRUE),
      free_agent_adds     = sum((type == "free_agent") & (status == "complete") & (add_drop == "add"), na.rm = TRUE),
      drops               = sum(((type == "free_agent") | (type == "waiver")) & (status == "complete") & (add_drop == "drop"), na.rm = TRUE),
      players_traded_for  = sum((type == "trade") & (status == "complete") & (is.na(player_id)) & (add_drop == "add"), na.rm = TRUE),
      players_traded_away = sum((type == "trade") & (status == "complete") & (!is.na(player_id)) & (add_drop == "drop"), na.rm = TRUE),
      trades              = dplyr::n_distinct(trans_id[type == "trade" & status == "complete"], na.rm = TRUE),
      faab_traded_for     = sum(waiver_bid[(type == "trade") & status == "complete" & add_drop == "add"], na.rm = TRUE),
      faab_traded_away    = sum(waiver_bid[(type == "trade") & status == "complete" & add_drop == "drop"], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      completed_waivers_color   = spec_color2_scale(completed_waivers,   scale_from = c(min(completed_waivers),   max(completed_waivers)),   direction = 1),
      failed_waivers_color      = spec_color2_scale(failed_waivers,      scale_from = c(min(failed_waivers),      max(failed_waivers)),      direction = 1),
      total_faab_spent_color    = spec_color2_scale(total_faab_spent,    scale_from = c(min(total_faab_spent),    max(total_faab_spent)),    direction = 1),
      total_faab_failed_color   = spec_color2_scale(total_faab_failed,   scale_from = c(min(total_faab_failed),   max(total_faab_failed)),   direction = 1),
      free_agent_adds_color     = spec_color2_scale(free_agent_adds,     scale_from = c(min(free_agent_adds),     max(free_agent_adds)),     direction = 1),
      drops_color               = spec_color2_scale(drops,               scale_from = c(min(drops),               max(drops)),               direction = 1),
      players_traded_for_color  = spec_color2_scale(players_traded_for,  scale_from = c(min(players_traded_for),  max(players_traded_for)),  direction = 1),
      players_traded_away_color = spec_color2_scale(players_traded_away, scale_from = c(min(players_traded_away), max(players_traded_away)), direction = 1),
      trades_color              = spec_color2_scale(trades,              scale_from = c(min(trades),              max(trades)),              direction = 1)
    ) %>%
    dplyr::right_join(recap_data, by = "manager_id")

  # ── Best ball bench (wrong start/sits) ───────────────────────────────────
  possible_slots <- tibble::tibble(
    position      = c("QB", "RB", "RB", "WR", "WR", "TE", "DEF", "K"),
    possible_slot = c("QB", "RB", "W/R/T", "WR", "W/R/T", "TE", "DEF", "K")
  )

  best_ball_lineups <- calc_best_ball_lineups(all_players, max_week) %>%
    dplyr::left_join(possible_slots, by = "position", relationship = "many-to-many")

  actual_starters_bb <- best_ball_lineups %>%
    dplyr::filter(!is.na(starter_id)) %>%
    dplyr::select(full_name, points, week, possible_slot, manager_id)

  best_ball_bench <- best_ball_lineups %>%
    dplyr::filter(is.na(starter_id)) %>%
    dplyr::left_join(
      actual_starters_bb,
      by           = c("possible_slot", "manager_id", "week"),
      suffix       = c("", "_starter"),
      relationship = "many-to-many"
    ) %>%
    dplyr::mutate(points_over_starter = points - points_starter) %>%
    dplyr::group_by(full_name, manager_id, week) %>%
    dplyr::arrange(dplyr::desc(points_over_starter)) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::filter(optimal_slot != "BENCH") %>%
    dplyr::filter(points_over_starter > 0) %>%
    dplyr::arrange(dplyr::desc(points_over_starter))

  recap_data <- best_ball_bench %>%
    dplyr::group_by(manager_id) %>%
    dplyr::summarise(
      wrong_start_sits                  = dplyr::n(),
      points_lost_from_wrong_start_sits = sum(points_over_starter),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      wrong_start_sits_color = spec_color2_scale(
        wrong_start_sits,
        scale_from = c(min(wrong_start_sits), max(wrong_start_sits)),
        direction  = -1
      ),
      points_lost_from_wrong_start_sits_color = spec_color2_scale(
        points_lost_from_wrong_start_sits,
        scale_from = c(min(points_lost_from_wrong_start_sits), max(points_lost_from_wrong_start_sits)),
        direction  = -1
      )
    ) %>%
    dplyr::right_join(recap_data, by = "manager_id")

  # ── Trade record ─────────────────────────────────────────────────────────
  all_trades <- all_transactions %>%
    dplyr::filter(type == "trade" & status == "complete") %>%
    dplyr::mutate(player_id = as.character(player_id)) %>%
    dplyr::left_join(starter_ppg,    by = c("manager_id" = "manager_id", "player_id" = "players")) %>%
    dplyr::left_join(sleeper_players, by = "player_id") %>%
    dplyr::mutate(full_name = dplyr::coalesce(full_name.x, full_name.y)) %>%
    dplyr::select(-dplyr::starts_with("full_name.")) %>%
    dplyr::left_join(team_names, by = "manager_id") %>%
    dplyr::mutate(team_name = dplyr::coalesce(team_name.x, team_name.y)) %>%
    dplyr::select(-dplyr::starts_with("team_name.")) %>%
    dplyr::mutate(
      points       = ifelse(is.na(points), 0, points),
      ppg          = ifelse(is.na(ppg), 0, ppg),
      games_played = ifelse(is.na(games_played), 0, games_played)
    ) %>%
    dplyr::filter(add_drop == "add") %>%
    dplyr::select(week, trans_id, team_name, manager_id, games_played, ppg, points, full_name, waiver_bid) %>%
    dplyr::mutate(
      total_points = ave(points, trans_id, team_name, FUN = sum),
      full_name    = ifelse(is.na(full_name), paste0("$", waiver_bid, " FAAB"), full_name)
    ) %>%
    evaluate_trades()

  recap_data <- all_trades %>%
    dplyr::distinct(trans_id, manager_id, .keep_all = TRUE) %>%
    dplyr::left_join(
      all_trades %>%
        dplyr::group_by(trans_id, manager_id) %>%
        dplyr::summarise(total_points_opponent = sum(points), .groups = "drop"),
      by           = "trans_id",
      relationship = "many-to-many"
    ) %>%
    dplyr::filter(manager_id.x != manager_id.y) %>%
    dplyr::select(
      manager_id    = manager_id.x,
      winner,
      team_name,
      total_points,
      total_points_opponent
    ) %>%
    dplyr::group_by(manager_id, team_name) %>%
    dplyr::summarise(
      trade_wins              = sum(winner == 1, na.rm = TRUE),
      trade_losses            = sum(winner == 0, na.rm = TRUE),
      total_trade_for_points  = sum(total_points),
      total_trade_away_points = sum(total_points_opponent),
      .groups = "drop"
    ) %>%
    dplyr::select(team_name, trade_wins, trade_losses, total_trade_for_points, total_trade_away_points) %>%
    dplyr::right_join(recap_data, by = "team_name") %>%
    replace(is.na(.), 0) %>%
    dplyr::mutate(
      trade_wins_color = spec_color2_scale(
        trade_wins,
        scale_from = c(min(trade_wins), max(trade_wins)),
        direction  = 1
      ),
      trade_losses_color = spec_color2_scale(
        trade_losses,
        scale_from = c(min(trade_losses), max(trade_losses)),
        direction  = -1
      ),
      total_trade_for_points_color = spec_color2_scale(
        total_trade_for_points,
        scale_from = c(min(total_trade_for_points), max(total_trade_for_points)),
        direction  = 1
      ),
      total_trade_away_points_color = spec_color2_scale(
        total_trade_away_points,
        scale_from = c(min(total_trade_away_points), max(total_trade_away_points)),
        direction  = -1
      )
    )

  # ── Final cleanup ────────────────────────────────────────────────────────
  recap_data %>%
    replace(is.na(.), 0) %>%
    dplyr::arrange(team_name)
}
