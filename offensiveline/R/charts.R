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
      ifelse(matchup_data$week < max_week, 'Historic', 'This Week')

    matchup_data$Group <-
      factor(matchup_data$Group, levels = c('This Week', 'Historic'))

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

create_matchup_plot <- function(player_data, matchup_id, week) {
  matchup <- player_data[!is.na(player_data$starter_id) &
                           player_data$matchup_id == matchup_id &
                           player_data$week == week,]

  # Calculate the total team points for each team
  team_total_points <-
    aggregate(points ~ team_name, data = matchup, sum)

  # Order team names by total points in descending order
  ordered_teams <-
    team_total_points[order(-team_total_points$points), "team_name"]

  matchup$position <-
    factor(matchup$position, levels = c("QB", "RB", "WR", "TE", "K", "DEF"))

  # Reorder team_name based on total team_points
  matchup$team_name <-
    factor(matchup$team_name, levels = ordered_teams)

  final_plot <-
    ggplot(matchup, aes(fill = position, y = points, x = team_name)) +
    geom_bar(position = "stack", stat = "identity") +
    scale_fill_manual(values = colorsByPosition) +
    theme_classic() +
    geom_text(
      aes(
        x = team_name,
        y = points,
        label = paste0(full_name, " - ", points),
        group = position
      ),
      position = position_stack(vjust = 0.5),
      colour = "black"
    ) +
    geom_bar(
      aes(x = team_name, y = points, fill = position),
      position = "stack",
      stat = "identity",
      alpha = 0,
      color = "white",
      linewidth = 0.1
    ) +
    geom_text(
      aes(
        x = team_name,
        y = team_points + 5,
        label = paste0(team_points)
      ),
      colour = "black",
      size = 4
    ) +
    labs(x = 'Team', y = 'Points') +
    scale_y_continuous(breaks = seq(0,
                                    plyr::round_any(
                                      max(player_data$team_points), 5, ceiling
                                    ), by = 15)) + theme(legend.position = "none")

  return(final_plot)
}


create_efficiency_plot <-
  function(best_ball_matchups, matchup_data, max_week) {
    chart_data <- best_ball_matchups %>%
      left_join(matchup_data, by = c('week', 'manager_id')) %>%
      mutate(percentage = team_points.y / team_points.x * 100) %>%
      filter(week == max_week) %>%
      arrange(desc(percentage)) # Sort by percentage in descending order

    final_chart <-
      ggplot(data = chart_data, aes(x = reorder(team_name.x, team_points.y))) +
      geom_bar(
        aes(y = team_points.x),
        stat = "identity",
        position = "identity",
        alpha = .3,
        fill = '#65645A',
        color = '#65645A'
      ) +
      geom_bar(
        aes(y = team_points.y),
        stat = "identity",
        position = "identity",
        alpha = .8,
        fill = '#000DFF',
        color = '#000DFF'
      ) +
      labs(x = 'Team', y = 'Points') +
      coord_flip() +
      geom_text(
        aes(x = team_name.x, y = 1, label = team_name.x),
        hjust = 'left',
        # Align the label to the left
        vjust = 'middle',
        color = 'white',
        # Center the label vertically
        size = 4        # Adjust the size of labels if needed
      ) +
      geom_text(aes(y = team_points.x, label = paste0(round(percentage, 2), "%")),
                hjust = -0.1,
                size = 3)  +    # Adjust the size of labels if needed)
      scale_y_continuous(breaks = seq(0,
                                      plyr::round_any(
                                        max(chart_data$team_points.x), 5, ceiling
                                      ), by = 15)) +
      theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "null"),
        panel.background = element_blank(),
        panel.grid = element_blank()
      )

    return(final_chart)
  }

colorsByPosition <- c(
  QB = '#E1676F',
  RB = '#11D677',
  WR = '#4DB6F0',
  TE = '#E9AC53',
  K = '#D959FF',
  DEF = '#65645A'
)

spec_color2 <- function(x,
                        alpha = 1,
                        begin = 0,
                        end = 1,
                        direction = 1,
                        option = "D",
                        na_color = "#BBBBBB",
                        scale_from = NULL,
                        n = 12,
                        palette = paletteer_c("ggthemes::Blue", n, direction)) {
  n <- length(palette)
  if (is.null(scale_from)) {
    x <- round(scales::rescale(x, c(1, n)))
  } else {
    x <- round(scales::rescale(x, to = c(1, n),
                               from = scale_from))
  }

  color_code <- palette[x]
  color_code[is.na(color_code)] <- na_color
  return(color_code)
}

create_shots_dist <- function(motw_data, max_week, color_1, color_2) {
  motw_data$Group <-
    ifelse(is.na(motw_data$motw), 'Not MotW', 'MotW')

  shots_dist <- ggplot(motw_data[motw_data$winner == 0,], aes(x = `# of Shots`, fill = Group)) +
    geom_bar(position = 'stack', color = '#000000') +
    theme_classic() +
    theme(legend.position = "top", text = element_text(size = 20)) + scale_fill_manual(values =
                                                                                         c(color_1, color_2)) +
    scale_x_continuous(breaks = seq(0, 9, 1)) +
    scale_y_continuous(breaks = seq(0, 100, 1)) +
    labs(title = 'Losing Teams Potential Shots Distribution', x = 'Shots', y = 'Count')

  return(shots_dist)
}
