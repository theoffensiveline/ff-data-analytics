# average_score_by_week.R
# Average team score for every week across the full history of a Sleeper league.
# Follows previous_league_id back through every prior season on Sleeper.

library(sleeperapi)
library(dplyr)
library(tidyr)

# Sleeper league ids exceed R's integer precision - keep as a string
league_id <- "1382521746292219904"

##### 1. Walk the league chain to find every season #####

league_history <- data.frame()
current_league_id <- league_id

while (!is.null(current_league_id) && !is.na(current_league_id)) {
  league <- get_league(current_league_id)

  # last_scored_leg = most recent week with real scores (handles in-progress seasons)
  max_week <- league$last_scored_leg
  if (is.null(max_week) || is.na(max_week)) {
    max_week <- 18
  }

  league_history <- rbind(league_history, data.frame(
    league_id = current_league_id,
    season = as.numeric(league$season),
    playoff_week_start = league$playoff_week_start,
    max_week = as.numeric(max_week)
  ))

  current_league_id <- league$previous_league_id
}

print(league_history)

##### 2. Pull matchup scores for every week of every season #####

all_scores <- data.frame()

for (i in seq_len(nrow(league_history))) {
  season <- league_history$season[i]
  lid <- league_history$league_id[i]

  for (week in seq_len(league_history$max_week[i])) {
    matchups <- get_matchups(lid, week)

    if (is.null(matchups) || nrow(matchups) == 0) next

    all_scores <- rbind(all_scores, data.frame(
      season = season,
      week = week,
      roster_id = matchups$roster_id,
      matchup_id = matchups$matchup_id,
      points = matchups$points
    ))
  }
}

# drop unplayed rosters (0 points = matchup hasn't happened yet)
all_scores <- all_scores %>% filter(points > 0)

##### 3. Average score per week #####

# average team score for each week of each season
avg_by_season_week <- all_scores %>%
  group_by(season, week) %>%
  summarise(
    avg_score = round(mean(points), 2),
    teams = n(),
    .groups = "drop"
  ) %>%
  left_join(league_history %>% select(season, playoff_week_start), by = "season") %>%
  mutate(type = ifelse(week >= playoff_week_start, "playoffs", "regular")) %>%
  arrange(season, week)

# wide version: one row per season, one column per week
avg_score_wide <- avg_by_season_week %>%
  select(season, week, avg_score) %>%
  pivot_wider(names_from = week,
              values_from = avg_score,
              names_prefix = "week_")

# average team score for each week number, pooled across all seasons
avg_by_week <- all_scores %>%
  group_by(week) %>%
  summarise(
    avg_score = round(mean(points), 2),
    seasons = n_distinct(season),
    team_games = n(),
    .groups = "drop"
  )

# every season-week ranked by average score, highest first
weekly_ranking <- avg_by_season_week %>%
  arrange(desc(avg_score)) %>%
  mutate(rank = row_number()) %>%
  select(rank, season, week, avg_score, type)

print(avg_by_season_week, n = Inf)
print(as.data.frame(avg_score_wide))
print(avg_by_week, n = Inf)
print(weekly_ranking, n = Inf)

cat(sprintf(
  "\nOverall average team score across %d seasons: %.2f\n",
  n_distinct(all_scores$season),
  mean(all_scores$points)
))
