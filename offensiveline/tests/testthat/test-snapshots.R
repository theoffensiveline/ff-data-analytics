# Regression tests against golden JSON files (Phase 3).
# Each test calls a package function with fixture data and compares the output
# to the golden file captured in Phase 2. If output ever diverges from the
# golden, the test fails and shows exactly what changed.
#
# The recap_data test is skipped until build_recap_data() is implemented (Phase 5).

compare_to_golden <- function(actual_json, golden_file) {
  golden_path <- test_path("snapshots", golden_file)
  golden <- jsonlite::fromJSON(readLines(golden_path, warn = FALSE))
  actual <- jsonlite::fromJSON(actual_json)
  expect_equal(actual, golden)
}

# Load fixtures once when the file is sourced
all_players_raw  <- readRDS(test_path("fixtures", "players_full.rds"))
all_matchups_raw <- readRDS(test_path("fixtures", "matchups_full.rds"))
all_players  <- all_players_raw[complete.cases(all_players_raw$matchup_id), ]
all_matchups <- all_matchups_raw[complete.cases(all_matchups_raw$matchup_id), ]
motw_data    <- readRDS(test_path("fixtures", "motw_full.rds"))
team_photos  <- readRDS(test_path("fixtures", "team_photos.rds"))
max_week     <- 17L

# Two package functions reference globals leaked from main.R's calling environment:
#   - create_awards_table()  uses `current_week` (awards_table.R:103)
#   - motw_table_to_json()   uses `all_matchups`  (stuff_to_json.R:157)
# Assign them to .GlobalEnv so package functions can find them during tests.
# Phase 5 will fix these functions to accept the values as parameters instead.
assign("current_week", max_week,     envir = .GlobalEnv)
assign("all_matchups", all_matchups, envir = .GlobalEnv)

# ── main.R outputs ────────────────────────────────────────────────────────────

test_that("leaderboard_to_json matches golden", {
  compare_to_golden(
    leaderboard_to_json(all_matchups, max_week, team_photos),
    "leaderboard.json"
  )
})

test_that("awards_to_json matches golden", {
  compare_to_golden(
    awards_to_json(all_matchups, all_players, max_week, team_photos),
    "awards.json"
  )
})

test_that("motw_table_to_json matches golden", {
  compare_to_golden(
    motw_table_to_json(motw_data),
    "motw_table.json"
  )
})

test_that("efficiency_to_json matches golden", {
  compare_to_golden(
    efficiency_to_json(all_matchups, all_players, max_week),
    "efficiency.json"
  )
})

test_that("power_rankings_to_json matches golden", {
  compare_to_golden(
    power_rankings_to_json(all_matchups, max_week, number_of_teams = 12),
    "power_rankings.json"
  )
})

test_that("schedule_comparison_to_json matches golden", {
  compare_to_golden(
    schedule_comparison_to_json(all_matchups, team_photos),
    "schedule.json"
  )
})

test_that("best_ball_lb_to_json matches golden", {
  compare_to_golden(
    best_ball_lb_to_json(all_matchups, all_players, max_week),
    "best_ball_lb.json"
  )
})

test_that("shots_dist_to_json matches golden", {
  compare_to_golden(
    shots_dist_to_json(motw_data),
    "shots_dist.json"
  )
})

# ── end_of_season_recap.R outputs (Phase 5) ──────────────────────────────────

test_that("recap_data matches golden", {
  skip("build_recap_data() not yet extracted — implement in Phase 5")
  all_transactions <- readRDS(test_path("fixtures", "transactions_full.rds"))
  compare_to_golden(
    jsonlite::toJSON(build_recap_data(all_matchups, motw_data, all_transactions, all_players)),
    "recap_data.json"
  )
})
