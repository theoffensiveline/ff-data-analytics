library(dplyr)

mock_m <- readRDS(test_path("fixtures", "matchups_w3.rds"))

# --- create_leaderboard ---

test_that("create_leaderboard returns one row per team", {
  result <- create_leaderboard(mock_m, max_week = 3)
  expect_equal(nrow(result), length(unique(mock_m$manager_id)))
})

test_that("W + L equals weeks played for every team", {
  result <- create_leaderboard(mock_m, max_week = 3)
  expect_true(all(result$W + result$L == 3))
})

test_that("PF equals sum of team_points from raw matchup data", {
  result <- create_leaderboard(mock_m, max_week = 3)
  expected_pf <- mock_m %>%
    filter(week <= 3) %>%
    group_by(manager_id) %>%
    summarise(PF = sum(team_points))
  expect_equal(sort(result$PF), sort(expected_pf$PF))
})

test_that("teams are ranked 1 through N with no gaps or duplicates", {
  result <- create_leaderboard(mock_m, max_week = 3)
  expect_equal(sort(result$Rank), seq_len(nrow(result)))
})

test_that("team with more wins ranks above team with fewer wins", {
  result <- create_leaderboard(mock_m, max_week = 3)
  result_sorted <- result[order(result$Rank), ]
  expect_true(all(diff(result_sorted$W) <= 0))
})

# --- create_power_rankings ---

test_that("power rankings return one row per team", {
  result <- create_power_rankings(mock_m, max_week = 3, number_of_teams = 12)
  expect_equal(nrow(result), length(unique(mock_m$manager_id)))
})

test_that("power ranking scores are numeric and non-negative", {
  result <- create_power_rankings(mock_m, max_week = 3, number_of_teams = 12)
  expect_true(is.numeric(result$`Team Ability`))
  expect_true(all(result$`Team Ability` >= 0))
})
