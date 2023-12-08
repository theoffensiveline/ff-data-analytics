create_leaderboard <- function(matchup_data, max_week) {
  # create last week leaderboard
  last_week_leaderboard <- matchup_data %>%
    group_by(week, matchup_id) %>%
    arrange(manager_id) %>%
    mutate(other_team_points = case_when(
      manager_id == unique(manager_id)[1] ~ lead(team_points, order_by = manager_id),
      manager_id != unique(manager_id)[1] ~ lag(team_points, order_by = manager_id)
    )) %>%
    ungroup() %>%
    filter(week < max_week) %>%
    group_by(team_name) %>%
    reframe(Team = team_name,
            W = sum(winner),
            PF = sum(team_points)) %>%
    distinct() %>%
    arrange(desc(W), desc(PF)) %>%
    mutate(LastWeekRank = row_number()) %>%
    select(LastWeekRank, Team)

  # create current leaderboard
  leaderboard <- matchup_data %>%
    group_by(week, matchup_id) %>%
    arrange(manager_id) %>%
    mutate(other_team_points = case_when(
      manager_id == unique(manager_id)[1] ~ lead(team_points, order_by = manager_id),
      manager_id != unique(manager_id)[1] ~ lag(team_points, order_by = manager_id)
    )) %>%
    ungroup() %>%
    filter(week <= max_week) %>%
    group_by(team_name) %>%
    reframe(
      Team = team_name,
      W = sum(winner),
      L = n() - sum(winner),
      PF = sum(team_points),
      PA = sum(other_team_points)
    ) %>%
    distinct() %>%
    arrange(desc(W), desc(PF)) %>%
    mutate(Rank = row_number()) %>%
    left_join(last_week_leaderboard, by = 'Team') %>%
    mutate(Trend = LastWeekRank - Rank) %>%
    select(Rank, Trend, Team, W, L, PF, PA)

  return(leaderboard)
}


create_power_rankings <-
  function(matchup_data, max_week, number_of_teams) {
    # create last week power_rankings
    last_week_power_rankings <- matchup_data %>%
      filter(week < max_week) %>%
      arrange(week, team_points) %>%
      group_by(week) %>%
      mutate(rank = rank(-team_points, ties.method = "min")) %>%
      mutate(all_ws = number_of_teams - rank, all_ls = rank - 1) %>%
      mutate(
        week_weight = case_when(
          week == max_week ~ 2,
          week == max_week - 1 ~ 1.8,
          week == max_week - 2 ~ 1.6,
          week == max_week - 3 ~ 1.4,
          week == max_week - 4 ~ 1.2,
          TRUE ~ 1  # Default value if none of the conditions are met
        )
      ) %>% group_by(week, matchup_id) %>%
      mutate(opponent_rank = ifelse(manager_id == first(manager_id), last(rank), first(rank))) %>%
      ungroup() %>%
      group_by(team_name) %>%
      summarize(
        play_all_w = sum(all_ws),
        play_all_l = sum(all_ls),
        weighted_w = sum(all_ws * week_weight),
        total_opp_rank = mean(number_of_teams - opponent_rank)
      ) %>%
      mutate(
        team_ability = rescale(
          round(play_all_w / max_week, 2),
          to = c(1, 100),
          from = c(0, number_of_teams - 1)
        ),
        strength_of_schedule = rescale(
          round(total_opp_rank, 2),
          to = c(1, 100),
          from = c(0, number_of_teams - 1)
        ),
        last_p_rank = order(order(-weighted_w,-strength_of_schedule))
      ) %>% arrange(last_p_rank) %>% select(team_name, last_p_rank)


    # create current power_rankings
    power_rankings <- matchup_data %>%
      arrange(week, team_points) %>%
      group_by(week) %>%
      mutate(rank = rank(-team_points, ties.method = "min")) %>%
      mutate(all_ws = number_of_teams - rank, all_ls = rank - 1) %>%
      mutate(
        week_weight = case_when(
          week == max_week ~ 2,
          week == max_week - 1 ~ 1.8,
          week == max_week - 2 ~ 1.6,
          week == max_week - 3 ~ 1.4,
          week == max_week - 4 ~ 1.2,
          TRUE ~ 1  # Default value if none of the conditions are met
        )
      ) %>% group_by(week, matchup_id) %>%
      mutate(opponent_rank = ifelse(manager_id == first(manager_id), last(rank), first(rank))) %>%
      ungroup() %>%
      group_by(team_name) %>%
      reframe(
        Team = team_name,
        `Play All W` = sum(all_ws),
        `Play All L` = sum(all_ls),
        weighted_w = sum(all_ws * week_weight),
        total_opp_rank = mean(number_of_teams - opponent_rank)
      ) %>%
      distinct() %>%
      mutate(
        `Team Ability` = rescale(
          round(`Play All W` / max_week, 2),
          to = c(1, 100),
          from = c(0, number_of_teams - 1)
        ),
        `Str of Sched` = rescale(
          round(total_opp_rank, 2),
          to = c(1, 100),
          from = c(0, number_of_teams - 1)
        ),
        `P Rank` = order(order(-weighted_w,-`Str of Sched`))
      ) %>% arrange(`P Rank`) %>%
      left_join(last_week_power_rankings, by = 'team_name') %>%
      mutate(Trend = last_p_rank - `P Rank`) %>%
      select(`P Rank`,
             Trend,
             Team,
             `Play All W`,
             `Play All L`,
             `Team Ability`,
             `Str of Sched`)

    return(power_rankings)
  }

create_median_leaderboard <- function(matchup_data, max_week) {
  # get current leaderboard
  leaderboard <- create_leaderboard(matchup_data, max_week)

  leaderboard <- leaderboard %>% rename(Rank_L = Rank) %>% select(Team, Rank_L)

  # create current leaderboard
  median_leaderboard <- matchup_data %>%
    group_by(week, matchup_id) %>%
    arrange(manager_id) %>%
    mutate(other_team_points = case_when(
      manager_id == unique(manager_id)[1] ~ lead(team_points, order_by = manager_id),
      manager_id != unique(manager_id)[1] ~ lag(team_points, order_by = manager_id)
    )) %>%
    ungroup() %>%
    filter(week <= max_week) %>%
    group_by(week) %>%
    mutate(week_pts_rank = rank(-team_points)) %>%
    mutate(week_pts_win = case_when(week_pts_rank <= 6 ~ 1, week_pts_rank > 6 ~ 0)) %>%
    group_by(team_name) %>%
    reframe(
      Team = team_name,
      W = sum(winner) + sum(week_pts_win),
      L = 2*n() - sum(winner) - sum(week_pts_win),
      PF = sum(team_points),
      PA = sum(other_team_points)
    ) %>%
    distinct() %>%
    arrange(desc(W), desc(PF)) %>%
    mutate(Rank = row_number()) %>%
    left_join(leaderboard, by = 'Team') %>%
    mutate(Diff = Rank_L - Rank) %>%
    select(Rank, Diff, Team, W, L, PF, PA)

  return(median_leaderboard)
}

create_best_ball_leaderboard <- function(matchup_data, best_ball_matchup_data, max_week) {
  # get current leaderboard
  leaderboard <- create_leaderboard(matchup_data, max_week)

  leaderboard <- leaderboard %>% rename(Rank_L = Rank) %>% select(Team, Rank_L)

  # create leaderboard
  leaderboard <- best_ball_matchup_data %>%
    group_by(week, matchup_id) %>%
    arrange(manager_id) %>%
    mutate(other_team_points = case_when(
      manager_id == unique(manager_id)[1] ~ lead(team_points, order_by = manager_id),
      manager_id != unique(manager_id)[1] ~ lag(team_points, order_by = manager_id)
    )) %>%
    ungroup() %>%
    filter(week <= max_week) %>%
    group_by(team_name) %>%
    reframe(
      Team = team_name,
      W = sum(winner),
      L = n() - sum(winner),
      PF = sum(team_points),
      PA = sum(other_team_points)
    ) %>%
    distinct() %>%
    arrange(desc(W), desc(PF)) %>%
    mutate(Rank = row_number()) %>%
    left_join(leaderboard, by = 'Team') %>%
    mutate(Diff = Rank_L - Rank) %>%
    select(Rank, Diff, Team, W, L, PF, PA)
}
