#' Title Scoring Distribution Chart
#'
#' @param matchup_data output from get_team_matchups
#' @param max_week maximum week in chart
#' @param color_1
#' @param color_2
#'
#' @return
#' @export
#'
#' @examples
create_scoring_dist <-
  function(matchup_data, max_week, color_1, color_2) {
    matchup_data$Group <-
      ifelse(matchup_data$week < max_week, 'Historic', 'Last Week')

    matchup_data$Group <-
      factor(matchup_data$Group, levels = c('Last Week', 'Historic'))

    matchup_data$Score <- matchup_data$team_points

    scoring_dist <-
      ggplot(matchup_data, aes(x = Score, fill = Group)) +
      geom_histogram(
        position = 'stack',
        color = '#000000',
        breaks = seq(
          plyr::round_any(min(matchup_data$Score), 5, floor),
          plyr::round_any(max(matchup_data$Score), 5, ceiling),
          by = 5
        )
      ) +
      theme_classic() +
      theme(legend.position = "top", text = element_text(size = 20)) + scale_fill_manual(values =
                                                                                           c(color_1, color_2)) +
      scale_x_continuous(breaks = seq(
        plyr::round_any(min(matchup_data$Score), 5, floor),
        plyr::round_any(max(matchup_data$Score), 5, ceiling),
        by = 15
      )) +
      scale_y_continuous(breaks = seq(0, 100, 2)) +
      labs(title = 'Team Scoring Distribution', x = 'Score', y = 'Count')

    return(scoring_dist)
  }


create_weekly_scoring_chart <- function(matchup_data, max_week) {
  stats_df <- matchup_data %>% group_by(week) %>%
    dplyr::summarize(
      'Average' = mean(team_points),
      'Median' = median(team_points),
      'Maximum' = max(team_points),
      'Minimum' = min(team_points),
    )

  stats_df <- melt(stats_df, "week")

  stats_df$value <- round(stats_df$value , digit = 2)

  stats_df$variable <-
    factor(stats_df$variable,
           levels = c("Maximum", 'Average', 'Median', 'Minimum'))

  names(stats_df) <- c("Week", "Stat", "Score")

  weekly_scoring_plot <-
    ggplot(matchup_data, aes(x = week, y = team_points)) +
    geom_line(size = 1,
              data = stats_df,
              aes(x = Week, y = Score, color = Stat)) +
    geom_point(size = 2) +
    theme_classic() +
    theme(
      legend.position = "top",
      text = element_text(size = 20),
      panel.grid.major.y = element_line()
    ) +
    scale_x_continuous(breaks = seq(1, max_week, 1)) +
    scale_y_continuous(breaks = seq(
      plyr::round_any(min(matchup_data$team_points), 5, floor),
      plyr::round_any(max(matchup_data$team_points), 5, ceiling),
      by = 15
    )) +
    labs(title = 'Weekly Scoring', x = 'Week', y = 'Points') +
    scale_colour_manual(values = c("#22763FFF", "#5E4FA2FF", "#F46D43FF", "#BE2A3EFF"))

  return(weekly_scoring_plot)
}
