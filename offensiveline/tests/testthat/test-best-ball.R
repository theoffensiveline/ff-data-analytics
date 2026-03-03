library(dplyr)

mock_m <- readRDS(test_path("fixtures", "matchups_w3.rds"))
mock_p <- readRDS(test_path("fixtures", "players_w3.rds"))

# --- calc_best_ball_lineups ---

test_that("optimal lineup score is always >= actual lineup score", {
  lineups <- calc_best_ball_lineups(mock_p, max_week = 3)

  optimal_by_team <- lineups %>%
    group_by(manager_id, week) %>%
    summarise(optimal = sum(points[optimal_slot != "BENCH"]), .groups = "drop")

  actual_by_team <- mock_p %>%
    filter(!is.na(starter_id)) %>%
    group_by(manager_id, week) %>%
    summarise(actual = sum(points), .groups = "drop")

  comparison <- inner_join(optimal_by_team, actual_by_team, by = c("manager_id", "week"))
  expect_true(all(comparison$optimal >= comparison$actual - 0.001))
})

test_that("every player appears exactly once per team per week", {
  result <- calc_best_ball_lineups(mock_p, max_week = 3)
  counts <- result %>% count(manager_id, week, players)
  expect_true(all(counts$n == 1))
})

test_that("optimal_slot is never NA", {
  result <- calc_best_ball_lineups(mock_p, max_week = 3)
  expect_false(any(is.na(result$optimal_slot)))
})

test_that("number of non-BENCH slots matches expected starter count per team per week", {
  result <- calc_best_ball_lineups(mock_p, max_week = 3)
  starters_per_team <- result %>%
    filter(optimal_slot != "BENCH") %>%
    count(manager_id, week)
  # 9 starters: QB, RB, RB, WR, WR, TE, FLEX(W/R/T), DEF, K
  expect_true(all(starters_per_team$n == 9))
})

# --- create_best_ball_matchups ---

test_that("best ball matchups return one row per team per week", {
  lineups <- calc_best_ball_lineups(mock_p, max_week = 3)
  result  <- create_best_ball_matchups(lineups)
  counts  <- result %>% count(manager_id, week)
  expect_true(all(counts$n == 1))
})

test_that("best ball team score >= actual team score in all matchups", {
  lineups     <- calc_best_ball_lineups(mock_p, max_week = 3)
  bb_matchups <- create_best_ball_matchups(lineups)
  joined <- inner_join(
    mock_m %>% select(week, manager_id, actual = team_points),
    bb_matchups %>% select(week, manager_id, optimal = team_points),
    by = c("week", "manager_id")
  )
  expect_true(all(joined$optimal >= joined$actual - 0.001))
})
