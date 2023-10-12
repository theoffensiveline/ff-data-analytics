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
           max_week,
           player_data) {
    # add MotW starting value to matchups
    matchup_data$motw <- NA
    matchup_data[(matchup_data$matchup_id == week_1_matchup_id) &
                   (matchup_data$week == 1), 'motw'] <- 1

    # add remaining MotW values to matchups
    if (max_week > 1) {
      for (week_num in 2:max_week) {
        # get previous week
        prev_week <- week_num - 1

        # get last winner of motw
        prev_motw_winners <- matchup_data %>%
          filter(week == prev_week, winner == 1, motw == 1) %>%
          pull(manager_id)

        # Iterate over each previous week's loser and assign motw
        for (prev_motw_winner in prev_motw_winners) {
          matchup_data <- matchup_data %>%
            mutate(motw = ifelse(week == week_num &
                                   manager_id == prev_motw_winner, 1, motw))
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
    shots_data <- player_data %>%
      filter(starter_id == 1) %>%
      group_by(week, manager_id) %>%
      summarise(`# of Shots` = sum(points < 10) + sum(points < 0))

    output_data <-
      matchup_data %>% left_join(shots_data, by = c("week", "manager_id"))
    return(output_data)
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
      `Loser Team` = team_name[which.min(winner)],
      `# of Shots/Dogs` = `# of Shots`[which.min(winner)]
    ) %>%
    distinct() %>%
    select(-week)

  return(motw_table)
}


create_danger_table <- function() {
  # create danger table and visual
  tree <-
    read.csv('currentMotW/matchup_tree.csv')
  left_tree <-
    read.csv('currentMotW/matchup_tree_left.csv')
  right_tree <-
    read.csv('currentMotW/matchup_tree_right.csv')
  tree$Count <- 0.5
  left_tree$Count <- 0.5
  right_tree$Count <- 0.5

  tree_champ <- tree[, c(1, 2, 4)]
  tree_opp <- tree[, c(1, 3, 4)]
  names(tree_opp) <- names(tree_champ)
  tree <- rbind(tree_champ, tree_opp)
  names(tree) <- c('Week', 'Team', 'Count')

  left_tree_champ <- left_tree[, c(1, 2, 4)]
  left_tree_opp <- left_tree[, c(1, 3, 4)]
  names(left_tree_opp) <- names(left_tree_champ)
  left_tree <- rbind(left_tree_champ, left_tree_opp)
  names(left_tree) <- c('Week', 'Team', 'Count')

  right_tree_champ <- right_tree[, c(1, 2, 4)]
  right_tree_opp <- right_tree[, c(1, 3, 4)]
  names(right_tree_opp) <- names(right_tree_champ)
  right_tree <- rbind(right_tree_champ, right_tree_opp)
  names(right_tree) <- c('Week', 'Team', 'Count')

  df <- tree %>% group_by(Team) %>%
    dplyr::summarize('NPG' = min(Week),
                     'RPA' = n(),)

  tree$Week <- as.factor(tree$Week)
  left_tree$Week <- as.factor(left_tree$Week)
  right_tree$Week <- as.factor(right_tree$Week)

  dt <- tree %>%
    dplyr::group_by(Week, Team) %>%
    dplyr::tally() %>%
    dplyr::mutate(percent = n / sum(n))

  safety <- dt %>% dplyr::group_by(Team) %>%
    dplyr::summarize(`DM` = round(100 * sum(percent)))

  left_dt <- left_tree %>%
    dplyr::group_by(Week, Team) %>%
    dplyr::tally() %>%
    dplyr::mutate(percent = n / sum(n))

  left_safety <- left_dt %>% dplyr::group_by(Team) %>%
    dplyr::summarize("{Current_Champion} Win DM" := round(100 * sum(percent)))

  right_dt <- right_tree %>%
    dplyr::group_by(Week, Team) %>%
    dplyr::tally() %>%
    dplyr::mutate(percent = n / sum(n))

  right_safety <- right_dt %>% dplyr::group_by(Team) %>%
    dplyr::summarize("{Current_Opponent} Win DM" := round(100 * sum(percent)))

  df <- merge(x = df,
              y = safety,
              by = "Team",
              all.x = TRUE)
  df <- merge(x = df,
              y = left_safety,
              by = "Team",
              all.x = TRUE)
  df <- merge(x = df,
              y = right_safety,
              by = "Team",
              all.x = TRUE)

  df[, 3:6][is.na(df[, 3:6])] <- 0

  df <- df %>% arrange(NPG, -DM)

  return(df)
}

create_danger_chart <- function() {
  tree <-
    read.csv('currentMotW/matchup_tree.csv')
  tree$Count <- 0.5

  tree_champ <- tree[, c(1, 2, 4)]
  tree_opp <- tree[, c(1, 3, 4)]
  names(tree_opp) <- names(tree_champ)
  tree <- rbind(tree_champ, tree_opp)
  names(tree) <- c('Week', 'Team', 'Count')

  tree$Week <- as.factor(tree$Week)

  dt <- tree %>%
    dplyr::group_by(Week, Team) %>%
    dplyr::tally() %>%
    dplyr::mutate(percent = n / sum(n))

  week_plot <-
    ggplot(data = dt, aes(x = Week, y = percent, fill = Team)) +
    geom_bar(stat = 'identity', position = "fill") +
    geom_text(
      aes(label = paste0(sprintf(
        "%1.1f", percent * 100
      ), "%")),
      position = position_fill(vjust = 0.5),
      colour = "white",
      size = 3
    ) +
    coord_flip() +
    scale_fill_manual(values = carto_pal(12, "Safe")) +
    theme_classic() +
    scale_x_discrete(limits = rev) +
    labs(x = 'Week', y = 'Chance to Lose MotW',
         title = 'Who Will Have to Take Shots?')

  return(week_plot)
}
