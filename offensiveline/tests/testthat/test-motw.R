library(dplyr)

mock_m <- readRDS(test_path("fixtures", "matchups_w3.rds"))
mock_p <- readRDS(test_path("fixtures", "players_w3.rds"))

test_that("motw flag is set on exactly 2 rows (1 matchup) per week", {
  result <- add_motw_to_matchups(mock_m, week_1_matchup_id = 1, max_week = 3, mock_p)
  motw_per_week <- result %>% filter(motw == 1) %>% count(week)
  expect_true(all(motw_per_week$n == 2))
})

test_that("every week has a motw matchup", {
  result <- add_motw_to_matchups(mock_m, week_1_matchup_id = 1, max_week = 3, mock_p)
  expect_equal(sort(unique(result$week[result$motw == 1])), 1:3)
})

test_that("motw winner in week N is in the motw matchup in week N+1", {
  result <- add_motw_to_matchups(mock_m, week_1_matchup_id = 1, max_week = 3, mock_p)
  for (w in 1:2) {
    winner_id <- result %>%
      filter(motw == 1, week == w, winner == 1) %>%
      pull(manager_id)
    next_week_motw_ids <- result %>%
      filter(motw == 1, week == w + 1) %>%
      pull(manager_id)
    expect_true(winner_id %in% next_week_motw_ids)
  }
})

test_that("# of Shots is a non-negative integer for motw rows", {
  result <- add_motw_to_matchups(mock_m, week_1_matchup_id = 1, max_week = 3, mock_p)
  shots <- result %>% filter(motw == 1) %>% pull(`# of Shots`)
  expect_true(all(shots >= 0))
  expect_true(all(shots == floor(shots)))
})

test_that("winner column is 0 or 1 for motw rows", {
  result <- add_motw_to_matchups(mock_m, week_1_matchup_id = 1, max_week = 3, mock_p)
  motw_winners <- result %>% filter(motw == 1) %>% pull(winner)
  expect_true(all(motw_winners %in% c(0, 1)))
})

test_that("each motw matchup has exactly one winner and one loser", {
  result <- add_motw_to_matchups(mock_m, week_1_matchup_id = 1, max_week = 3, mock_p)
  for (w in 1:3) {
    week_motw <- result %>% filter(motw == 1, week == w)
    expect_equal(sum(week_motw$winner), 1)
  }
})
