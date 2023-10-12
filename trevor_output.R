# load objects
source("main.R")

# output awards table
kable(
  awards_table,
  "html",
  booktabs = T,
  escape = F,
  align = 'l'
) %>%
  #column_spec(1, width_min = '40px', width_max = "50%") %>%
  #column_spec(2, width = "30%") %>%
  cat(
    .,
    file = paste0(
      "C:\\Users\\Trevor\\Documents\\Website\\public\\FantasyFootball",
      current_year,
      "\\Week",
      current_week,
      "\\awards_kable.html"
    )
  )

# output manager skill assessment
png(
  file = paste0(
    "C:\\Users\\Trevor\\Documents\\Website\\public\\FantasyFootball",
    current_year,
    "\\Week",
    current_week,
    "\\Efficiency.png"
  ),
  width = 900,
  height = 600
)
efficiency_plot
dev.off()

# output shots dist
png(
  file = paste0(
    "C:\\Users\\Trevor\\Documents\\Website\\public\\FantasyFootball",
    current_year,
    "\\Week",
    current_week,
    "\\Shots Distribution.png"
  ),
  width = 900,
  height = 600
)
shots_dist
dev.off()

# output leaderboard
kable(
  leaderboard,
  "html",
  booktabs = TRUE,
  escape = FALSE,
  align = 'c'
) %>%
  column_spec(6,
              color = "white",
              background = spec_color2(leaderboard$`PF`,
                                       direction = 1)) %>%
  column_spec(7,
              color = "white",
              background = spec_color2(leaderboard$`PA`,
                                       direction = -1)) %>%
  column_spec(
    2,
    color = ifelse(
      is.na(leaderboard$Trend),
      'white',
      ifelse(
        leaderboard$Trend == 0,
        '#f9f7f1',
        ifelse(leaderboard$Trend > 0, '#22763FFF', '#BE2A3EFF')
      )
    ),
    background = ifelse(is.na(leaderboard$Trend), 'white', 'transparent')
  ) %>%
  cat(
    .,
    file = paste0(
      "C:\\Users\\Trevor\\Documents\\Website\\public\\FantasyFootball",
      current_year,
      "\\Week",
      current_week,
      "\\actual_lb_kable.html"
    )
  )

# output power rankings
kable(
  power_rankings,
  "html",
  booktabs = TRUE,
  escape = FALSE,
  align = 'c'
) %>%
  column_spec(6,
              color = "white",
              background = spec_color2(power_rankings$`Team Ability`,
                                       direction = 1)) %>%
  column_spec(7,
              color = "white",
              background = spec_color2(power_rankings$`Str of Sched`,
                                       direction = -1)) %>%
  column_spec(
    2,
    color = ifelse(
      is.na(power_rankings$Trend),
      'white',
      ifelse(
        power_rankings$Trend == 0,
        '#f9f7f1',
        ifelse(power_rankings$Trend > 0, '#22763FFF', '#BE2A3EFF')
      )
    ),
    background = ifelse(is.na(power_rankings$Trend), 'white', 'transparent')
  ) %>%
  cat(
    .,
    file = paste0(
      "C:\\Users\\Trevor\\Documents\\Website\\public\\FantasyFootball",
      current_year,
      "\\Week",
      current_week,
      "\\power_rank_kable.html"
    )
  )

# output scoring dist
png(
  file = paste0(
    "C:\\Users\\Trevor\\Documents\\Website\\public\\FantasyFootball",
    current_year,
    "\\Week",
    current_week,
    "\\Scoring Distribution.png"
  ),
  width = 900,
  height = 600
)
scoring_dist
dev.off()

# output scoring chart
png(
  file = paste0(
    "C:\\Users\\Trevor\\Documents\\Website\\public\\FantasyFootball",
    current_year,
    "\\Week",
    current_week,
    "\\Scoring Plot.png"
  ),
  width = 900,
  height = 600
)
weekly_scores
dev.off()

# output motw table
kable(
  motw_table,
  "html",
  booktabs = T,
  escape = F,
  align = 'c'
) %>%
  cat(
    .,
    file = paste0(
      "C:\\Users\\Trevor\\Documents\\Website\\public\\FantasyFootball",
      current_year,
      "\\Week",
      current_week,
      "\\MotW_kable.html"
    )
  )

# output best ball leaderboard
kable(
  best_ball_leaderboard,
  "html",
  booktabs = TRUE,
  escape = FALSE,
  align = 'c'
) %>%
  column_spec(6,
              color = "white",
              background = spec_color2(best_ball_leaderboard$`PF`,
                                       direction = 1)) %>%
  column_spec(7,
              color = "white",
              background = spec_color2(best_ball_leaderboard$`PA`,
                                       direction = -1)) %>%
  column_spec(
    2,
    color = ifelse(
      is.na(best_ball_leaderboard$Trend),
      'white',
      ifelse(
        best_ball_leaderboard$Trend == 0,
        '#f9f7f1',
        ifelse(best_ball_leaderboard$Trend > 0, '#22763FFF', '#BE2A3EFF')
      )
    ),
    background = ifelse(is.na(best_ball_leaderboard$Trend), 'white', 'transparent')
  ) %>%
  cat(
    .,
    file = paste0(
      "C:\\Users\\Trevor\\Documents\\Website\\public\\FantasyFootball",
      current_year,
      "\\Week",
      current_week,
      "\\bb_lb_kable.html"
    )
  )


# output PF PA chart
png(
  file = paste0(
    "C:\\Users\\Trevor\\Documents\\Website\\public\\FantasyFootball",
    current_year,
    "\\Week",
    current_week,
    "\\PF PA Plot.png"
  ),
  width = 900,
  height = 600
)
PF_PA_chart
dev.off()

# output rank chart
png(
  file = paste0(
    "C:\\Users\\Trevor\\Documents\\Website\\public\\FantasyFootball",
    current_year,
    "\\Week",
    current_week,
    "\\Rank Plot.png"
  ),
  width = 900,
  height = 600
)
rank_chart
dev.off()


# Define a function to generate matchup plots
generate_matchup_plots <- function(matchup_id, week, current_year) {
  match_plot <- create_matchup_plot(player_data = all_players,
                                    matchup_id = matchup_id,
                                    week = week)
  
  png(
    file = paste0(
      "C:\\Users\\Trevor\\Documents\\Website\\public\\FantasyFootball",
      current_year,
      "\\Week",
      week,
      "\\Match",
      matchup_id,
      "Plot.png"
    ),
    width = 900,
    height = 600
  )
  print(match_plot)
  dev.off()
}

# Loop through matchup_id values from 1 to 6 and generate plots
for (matchup_id in 1:6) {
  generate_matchup_plots(matchup_id, current_week, current_year)
}
