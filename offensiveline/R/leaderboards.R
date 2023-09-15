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
      group_by(week) %>%
      arrange(desc(team_points)) %>%
      mutate(points_rank = row_number()) %>%
      mutate(other_team_points_rank = ifelse(
        manager_id == unique(manager_id)[1],
        lead(points_rank, order_by = manager_id),
        lag(points_rank, order_by = manager_id)
      )) %>%
      mutate(`Play All W` = number_of_teams - points_rank,
             `Play All L` = points_rank - 1)

    # create current power_rankings
    power_rankings <- matchup_data %>%
      filter(week <= max_week) %>%
      group_by(week) %>%
      arrange(desc(team_points)) %>%
      mutate(points_rank = row_number()) %>%
      mutate(other_team_points_rank = ifelse(
        manager_id == unique(manager_id)[1],
        lead(points_rank, order_by = manager_id),
        lag(points_rank, order_by = manager_id)
      )) %>%
      mutate(`Play All W` = number_of_teams - points_rank,
             `Play All L` = points_rank - 1) %>%
      left_join(last_week_power_rankings, by = 'Team') %>%
      mutate(Trend = LastWeekRank - Rank) %>%
      select(Rank, Trend, Team, W, L, PF, PA)

    return(power_rankings)
  }
