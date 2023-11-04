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
      factor(matchup_data$Group, levels = c('Historic', 'This Week'))

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
                                                                                           c(color_2, color_1)) +
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
    scale_colour_manual(values = c("#20A4F4", "#668F80", "#7E6551", "#FF3366"))

  return(weekly_scoring_plot)
}

create_matchup_plot <- function(player_data, matchup_id, week) {
  matchup <- player_data[!is.na(player_data$starter_id) &
                           player_data$matchup_id == matchup_id &
                           player_data$week == week, ]

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
  function(best_ball_matchups,
           matchup_data,
           max_week) {
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
        fill = '#7D8491',
        color = '#7D8491'
      ) +
      geom_bar(
        aes(y = team_points.y),
        stat = "identity",
        position = "identity",
        alpha = .8,
        fill = '#20A4F4',
        color = '#20A4F4'
      ) +
      labs(x = 'Team', y = 'Points') +
      coord_flip() +
      geom_text(
        aes(x = team_name.x, y = 1, label = team_name.x),
        hjust = 'left',
        # Align the label to the left
        vjust = 'middle',
        color = '#2E2E2E',
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

custom_palette12 <-
  c(
    "#ff3366",
    "#fd3880",
    "#f74398",
    "#ed50af",
    "#df5ec3",
    "#cd6cd5",
    "#b878e3",
    "#a083ed",
    "#858ef4",
    "#6996f7",
    "#499ef7",
    "#20a4f3"
  )

custom_palette36 <- c(
  "#ff3366",
  "#ff346e",
  "#fe3576",
  "#fd387e",
  "#fc3a86",
  "#fa3e8e",
  "#f84195",
  "#f5459d",
  "#f249a4",
  "#ef4eab",
  "#eb52b2",
  "#e756b8",
  "#e25bbf",
  "#dd5fc5",
  "#d863ca",
  "#d268d0",
  "#cc6cd5",
  "#c670da",
  "#bf74de",
  "#b878e2",
  "#b17ce6",
  "#aa7fea",
  "#a283ed",
  "#9a86ef",
  "#9289f1",
  "#898cf3",
  "#818ff5",
  "#7892f6",
  "#6f95f7",
  "#6597f7",
  "#5c9af7",
  "#529cf7",
  "#479ef6",
  "#3ca0f6",
  "#2fa2f4",
  "#20a4f3"
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
                        palette = custom_palette36) {
  if (direction == -1) {
    custom_palette36 <- rev(custom_palette36)
  }

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


create_shots_dist <-
  function(motw_data, max_week, color_1, color_2) {
    motw_data$Group <-
      ifelse(is.na(motw_data$motw), 'Not MotW', 'MotW')

    motw_data$Group <-
      factor(motw_data$Group, levels = c('Not MotW', 'MotW'))

    shots_dist <-
      ggplot(motw_data[motw_data$winner == 0, ], aes(x = `# of Shots`, fill = Group)) +
      geom_bar(position = 'stack', color = '#000000') +
      theme_classic() +
      theme(legend.position = "top", text = element_text(size = 20)) + scale_fill_manual(values =
                                                                                           c(color_2, color_1)) +
      scale_x_continuous(breaks = seq(0, 9, 1)) +
      scale_y_continuous(breaks = seq(0, 100, 1)) +
      labs(title = 'Losing Teams Potential Shots Distribution', x = 'Shots', y = 'Count')

    return(shots_dist)
  }

create_PF_PA_scatter <- function(leaderboard, team_photos) {
  df <-
    leaderboard %>% left_join(team_photos, by = c("Team" = "team_name"))

  scatterplot <- ggplot(df, aes(x = PF, y = PA)) +
    geom_abline(slope = 1,
                intercept = 0,
                linetype = 'dotted') +
    geom_image(data = subset(df,!is.na(avatar)),
               aes(image = avatar),
               size = 0.1) +
    geom_text(data = subset(df, is.na(avatar)), aes(label = Team)) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    labs(x = "Points For",
         y = "Points Against") +
    scale_x_continuous(
      limits = c(
        plyr::round_any(min(df$PF), 50, floor),
        plyr::round_any(max(df$PF), 50, ceiling)
      ),
      breaks = seq(
        plyr::round_any(min(df$PF), 50, floor),
        plyr::round_any(max(df$PF), 50, ceiling),
        by = 50
      )
    ) +
    scale_y_continuous(
      limits = c(
        plyr::round_any(min(df$PA), 50, floor),
        plyr::round_any(max(df$PA), 50, ceiling)
      ),
      breaks = seq(
        plyr::round_any(min(df$PA), 50, floor),
        plyr::round_any(max(df$PA), 50, ceiling),
        by = 50
      )
    ) +
    annotate(
      geom = "richtext",
      x = plyr::round_any(min(df$PF), 50, floor),
      y = plyr::round_any(min(df$PA), 50, floor),
      label = "<span style='color: #20A4F4;'>Lucky</span> and <span style='color: #FF3366;'>Bad</span>",
      hjust = 0,
      vjust = 0
    ) +
    annotate(
      geom = "richtext",
      x = plyr::round_any(max(df$PF), 50, ceiling),
      y = plyr::round_any(min(df$PA), 50, floor),
      label = "<span style='color: #20A4F4;'>Lucky</span> and <span style='color: #20A4F4;'>Good</span>",
      hjust = 1,
      vjust = 0
    ) +
    annotate(
      geom = "richtext",
      x = plyr::round_any(min(df$PF), 50, floor),
      y = plyr::round_any(max(df$PA), 50, ceiling),
      label = "<span style='color: #FF3366;'>Unlucky</span> and <span style='color: #FF3366;'>Bad</span>",
      hjust = 0,
      vjust = 1
    ) +
    annotate(
      geom = "richtext",
      x = plyr::round_any(max(df$PF), 50, ceiling),
      y = plyr::round_any(max(df$PA), 50, ceiling),
      label = "<span style='color: #FF3366;'>Unlucky</span> and <span style='color: #20A4F4;'>Good</span>",
      hjust = 1,
      vjust = 1
    )

  return(scatterplot)
}

create_rank_chart <- function(matchup_data, team_photos) {
  df <- matchup_data %>%
    arrange(week, team_points) %>%
    group_by(week) %>%
    mutate(rank = rank(-team_points, ties.method = "min")) %>% left_join(team_photos, by = "team_name")

  # Convert rank to a factor with reversed levels
  df$rank <-
    factor(df$rank, levels = rev(unique(df$rank)))

  weekly_rank_plot <- ggplot(df, aes(x = week, y = rank)) +
    #geom_line(aes(group = team_name), linewidth = 0.5) +
    geom_image(data = subset(df, !is.na(avatar)),
               aes(image = avatar),
               size = 0.09) +
    geom_text(
      data = subset(df, winner == 1),
      # Add green tiles for winner == 1
      aes(x = week, y = rank, label = 'W'),
      color = "#20A4F4",
      size = 4,
      nudge_x = 0.3
    ) +
    geom_text(
      data = subset(df, winner == 0),
      # Add red tiles for winner == 0
      aes(x = week, y = rank, label = 'L'),
      color = "#FF3366",
      size = 4,
      nudge_x = 0.3
    ) +
    geom_text(data = subset(df, is.na(avatar)),
              aes(label = team_name),
              size = 2) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    scale_y_discrete(limits = rev) +
    scale_x_discrete() +
    labs(x = 'Week', y = 'Rank')

  return(weekly_rank_plot)
}
