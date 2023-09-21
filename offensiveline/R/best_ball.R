calc_best_ball_lineups <- function(player_data, max_week) {
  # create dict of all slots that different positions can play in
  possible_slots <-
    tibble(
      position = c("QB", "RB", "RB", "WR", "WR", "TE", "TE", "DEF", "K"),
      possible_slot = c("QB", "RB", "W/R/T", "WR", "W/R/T", "TE", "W/R/T", "DEF", "K")
    )
  # create dict of how many players can play in each slot
  slot_counts <-
    tibble(
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

  return (optimal_lineups)
}

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

calc_bench_points <- function(matchup_data, optimal_lineups) {
  bench_points <- matchup_data %>% left_join(optimal_lineups)

  return(bench_points)
}
