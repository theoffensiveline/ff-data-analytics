library(dplyr)

mock_m  <- readRDS(test_path("fixtures", "matchups_w3.rds"))
mock_p  <- readRDS(test_path("fixtures", "players_w3.rds"))
photos  <- readRDS(test_path("fixtures", "team_photos.rds"))

# create_awards_table uses `current_week` as a global variable (Phase 5 will fix this).
assign("current_week", 3L, envir = .GlobalEnv)

lineups     <- calc_best_ball_lineups(mock_p, max_week = 3)
bb_matchups <- create_best_ball_matchups(lineups)

test_that("awards table has at least one row", {
  result <- create_awards_table(mock_p, mock_m, bb_matchups, photos)
  expect_true(nrow(result) > 0)
})

test_that("awards table has no duplicate award names", {
  result <- create_awards_table(mock_p, mock_m, bb_matchups, photos)
  expect_false(any(duplicated(result$award)))
})

test_that("awards table has required columns", {
  result <- create_awards_table(mock_p, mock_m, bb_matchups, photos)
  expect_true(all(c("award", "name", "value", "photo", "description") %in% names(result)))
})

# --- find_top_player ---

test_that("find_top_player returns a character player name", {
  result <- find_top_player("QB", mock_p, current_week = 3)
  expect_type(result$player_name, "character")
  expect_true(length(result$player_name) >= 1)
})

test_that("find_top_player top QB is actually a QB starter", {
  result <- find_top_player("QB", mock_p, current_week = 3)
  qb_starters <- mock_p[mock_p$position == "QB" &
                           !is.na(mock_p$starter_id) &
                           mock_p$week == 3, ]
  # strip any nickname suffix before matching
  base_name <- sub(" \\(.*\\)$", "", result$player_name)
  expect_true(all(base_name %in% qb_starters$full_name))
})
