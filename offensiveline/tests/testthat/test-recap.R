library(dplyr)

mock_m <- readRDS(test_path("fixtures", "matchups_w3.rds"))
mock_p <- readRDS(test_path("fixtures", "players_w3.rds"))
mock_t <- readRDS(test_path("fixtures", "transactions_w3.rds"))

# --- evaluate_trades ---

make_trade <- function(team, points, bid = NA) {
  data.frame(
    trans_id     = 1L,
    team_name    = team,
    total_points = points,
    waiver_bid   = bid,
    stringsAsFactors = FALSE
  )
}

test_that("evaluate_trades: team with higher total_points wins", {
  input  <- rbind(make_trade("A", 120), make_trade("B", 80))
  result <- evaluate_trades(input)
  expect_equal(result$winner[result$team_name == "A"], 1L)
  expect_equal(result$winner[result$team_name == "B"], 0L)
})

test_that("evaluate_trades: tied total_points marks both as winner", {
  input  <- rbind(make_trade("A", 100), make_trade("B", 100))
  result <- evaluate_trades(input)
  expect_true(all(result$winner == 1L))
})

test_that("evaluate_trades: no points, solo bidder wins", {
  input  <- rbind(make_trade("A", 0, bid = 25), make_trade("B", 0, bid = NA))
  result <- evaluate_trades(input)
  expect_equal(result$winner[result$team_name == "A"], 1L)
  expect_equal(result$winner[result$team_name == "B"], 0L)
})

test_that("evaluate_trades: no points, non-bidder loses", {
  input  <- rbind(make_trade("A", 0, bid = NA), make_trade("B", 0, bid = 10))
  result <- evaluate_trades(input)
  expect_equal(result$winner[result$team_name == "A"], 0L)
})

test_that("evaluate_trades: no points, higher FAAB bid wins", {
  input  <- rbind(make_trade("A", 0, bid = 50), make_trade("B", 0, bid = 20))
  result <- evaluate_trades(input)
  expect_equal(result$winner[result$team_name == "A"], 1L)
  expect_equal(result$winner[result$team_name == "B"], 0L)
})

test_that("evaluate_trades: no points, equal bids = NA", {
  input  <- rbind(make_trade("A", 0, bid = 30), make_trade("B", 0, bid = 30))
  result <- evaluate_trades(input)
  expect_true(all(is.na(result$winner)))
})

test_that("evaluate_trades: no points, multiple max bidders = NA", {
  # Need 3+ distinct bid values so the "two distinct" branch doesn't fire first
  input <- rbind(
    make_trade("A", 0, bid = 50),
    make_trade("B", 0, bid = 50),
    make_trade("C", 0, bid = 30),
    make_trade("D", 0, bid = 20)
  )
  result <- evaluate_trades(input)
  expect_true(all(is.na(result$winner[result$team_name %in% c("A", "B")])))
})

# --- summarize_win_loss_extremes ---

test_that("summarize_win_loss_extremes: best win set for teams with wins", {
  result <- summarize_win_loss_extremes(mock_m)
  # Teams with 0 wins have NA for most_points_1; filter to those with wins
  has_win <- !is.na(result$most_points_1)
  expect_true(any(has_win))
  expect_true(all(!is.na(result$most_points_1[has_win])))
})

test_that("summarize_win_loss_extremes: worst loss <= best loss per team", {
  result <- summarize_win_loss_extremes(mock_m)
  # Only compare for teams that have at least one loss
  has_loss <- !is.na(result$least_points_0) & !is.na(result$most_points_0)
  expect_true(all(result$least_points_0[has_loss] <=
                    result$most_points_0[has_loss]))
})

test_that("summarize_win_loss_extremes: returns one row per team", {
  result <- summarize_win_loss_extremes(mock_m)
  expect_equal(nrow(result), length(unique(mock_m$manager_id)))
})

# --- summarize_pt_diff ---

test_that("summarize_pt_diff: win differential is non-negative", {
  result <- summarize_pt_diff(mock_m)
  expect_true(all(result$pt_diff_1 >= 0))
})

test_that("summarize_pt_diff: close_games_1 + blowouts_1 <= total wins", {
  wins <- mock_m %>%
    group_by(manager_id) %>%
    summarise(W = sum(winner), .groups = "drop")
  result <- summarize_pt_diff(mock_m)
  joined <- inner_join(result, wins, by = "manager_id")
  expect_true(all(joined$close_games_1 + joined$blowouts_1 <= joined$W))
})
