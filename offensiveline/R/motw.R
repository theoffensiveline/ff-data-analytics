#' Title Matchup of the Week
#'
#' @param matchup_data matchup data from sleeper
#' @param week_1_matchup_id matchup id for the MotW for week 1
#' @param max_week the final week to calculate MotW for
#'
#' @return
#' @export
#'
#' @examples add_motw_to_matchups(matchup_data = all_matchups, week_1_matchup_id = 1, max_week = current_week)
add_motw_to_matchups <-
  function(matchup_data,
           week_1_matchup_id,
           max_week) {
    # add MotW starting value to matchups
    matchup_data$motw <- NA
    matchup_data[(matchup_data$matchup_id == week_1_matchup_id) &
                   (matchup_data$week == 1), 'motw'] <- 1

    # add remaining MotW values to matchups
    if (max_week > 1) {
      for (week_num in 2:max_week) {
        # get previous week
        prev_week <- week_num - 1

        # get last loser of motw
        prev_motw_losers <- matchup_data %>%
          filter(week == prev_week, winner == 0, motw == 1) %>%
          pull(manager_id)

        # Iterate over each previous week's loser and assign motw
        for (prev_motw_loser in prev_motw_losers) {
          matchup_data <- matchup_data %>%
            mutate(motw = ifelse(week == week_num &
                                   manager_id == prev_motw_loser, 1, motw))
        }

        # get the matchup id of the losers game
        this_week_matchup_id <- matchup_data %>%
          filter(week == week_num, motw == 1) %>%
          pull(matchup_id)

        # assign motw to the losers opponent
        matchup_data <- matchup_data %>%
          mutate(motw = ifelse(
            week == week_num & matchup_id == this_week_matchup_id,
            1,
            motw
          ))
      }
    }
    return(matchup_data)
  }


#' Title Create MotW Table
#'
#' @param motw_data result of add_motw_to_matchups
#'
#' @return
#' @export
#'
#' @examples create_motw_table(motw_data)
create_motw_table <- function(motw_data) {
  motw_table <- motw_data %>%
    filter(motw == 1) %>%
    group_by(week) %>%
    arrange(matchup_id) %>%
    reframe(
      Week = week,
      `Winner Team` = team_name[which.max(winner)],
      `Winner Score` = max(team_points),
      `Loser Score` = min(team_points),
      `Loser Team` = team_name[which.min(winner)]
    ) %>%
    distinct() %>%
    select(-week)

  return(motw_table)
}
