# Testing & Refactoring Plan

## Goal

Add a `testthat` test suite so we can confidently refactor `main.R`, `end_of_season_recap.R`,
and `new_site_stuff.R` without breaking the JSON outputs consumed by the website.

---

## Current State

- **No tests exist** — no `/tests` directory anywhere in the project
- Logic lives in two places:
  - **`offensiveline/` package** — ~40 exported functions across 8 R files
  - **Root scripts** — `main.R`, `end_of_season_recap.R`, `new_site_stuff.R` contain long
    inline pipelines that call package functions and do additional data shaping
- External API calls (`get_all_matchups_data`, `get_all_transaction_data`, etc.) are mixed
  in with transformation logic, making testing difficult without mocking

---

## Recommended Approach

### Where to put tests

Tests belong inside the `offensiveline` package — it already has the right structure (`R/`,
`man/`, `DESCRIPTION`). We just need to add a `tests/` directory:

```
offensiveline/
├── R/
├── man/
├── tests/
│   ├── testthat.R              # entry point (one line)
│   └── testthat/
│       ├── fixtures/           # .rds data files generated from real Sleeper API
│       ├── snapshots/          # golden JSON files captured from current code
│       ├── test-awards.R
│       ├── test-best-ball.R
│       ├── test-leaderboards.R
│       ├── test-motw.R
│       ├── test-recap.R        # tests for end_of_season_recap.R logic
│       └── test-snapshots.R    # regression tests against golden files
```

For the root scripts (`main.R`, `end_of_season_recap.R`), the strategy is to **capture
golden JSON output first**, then extract inline pipelines into named functions, then
verify the new functions produce identical output to the goldens before deleting old code.

---

## Phase 1 — Infrastructure Setup (do this first)

1. **Install `testthat`** in the `offensiveline` package:

   ```r
   usethis::use_testthat()   # creates tests/ dir, adds testthat to Suggests in DESCRIPTION
   ```

2. **Generate fixtures from the real Sleeper API (one-time setup)** — pull full real data
   from your league, then save slices as `.rds` files. The tests themselves never hit the
   API; they just load these files. This means fixtures reflect actual data shapes, column
   names, and edge cases from your league rather than hand-crafted mocks that might diverge.

   ```r
   # fixture_setup.R — run once, then commit the .rds files to git
   library(offensiveline)

   league_id          <- 1253779168802377728
   sleeper_players_csv <- "sleeper_players.csv"

   # Pull full season (week 1–17) so fixtures capture the whole range
   all_players_full   <- get_all_matchups_data(17, league_id, sleeper_players_csv)
   all_matchups_full  <- get_team_matchups(all_players_full)
   motw_full          <- add_motw_to_matchups(all_matchups_full, week_1_matchup_id = 1,
                                              max_week = 17, all_players_full)
   all_transactions_full <- get_all_transaction_data(as.character(league_id), max_week = 17)
   team_photos_full   <- get_team_photos(league_id)

   # Save full-season fixtures (used for snapshot/integration tests)
   saveRDS(all_matchups_full,     "offensiveline/tests/testthat/fixtures/matchups_full.rds")
   saveRDS(all_players_full,      "offensiveline/tests/testthat/fixtures/players_full.rds")
   saveRDS(motw_full,             "offensiveline/tests/testthat/fixtures/motw_full.rds")
   saveRDS(all_transactions_full, "offensiveline/tests/testthat/fixtures/transactions_full.rds")
   saveRDS(team_photos_full,      "offensiveline/tests/testthat/fixtures/team_photos.rds")

   # Save small slices (weeks 1–3) for fast unit tests
   saveRDS(all_matchups_full[all_matchups_full$week <= 3, ],
           "offensiveline/tests/testthat/fixtures/matchups_w3.rds")
   saveRDS(all_players_full[all_players_full$week <= 3, ],
           "offensiveline/tests/testthat/fixtures/players_w3.rds")
   saveRDS(motw_full[motw_full$week <= 3, ],
           "offensiveline/tests/testthat/fixtures/motw_w3.rds")
   saveRDS(all_transactions_full[all_transactions_full$week <= 3, ],
           "offensiveline/tests/testthat/fixtures/transactions_w3.rds")
   ```

   Unit tests use the `_w3` slices (fast). Snapshot/integration tests use `_full` (thorough).
   Re-run `fixture_setup.R` at the start of each new season to refresh.

3. **Add `testthat.R` entry point**:
   ```r
   # offensiveline/tests/testthat.R
   library(testthat)
   library(offensiveline)
   test_check("offensiveline")
   ```

---

## Phase 2 — Capture Golden Files from Current Code

**Do this before any refactoring.** Run the existing scripts as-is with real data and
write every JSON output to disk. These become the golden record — if your refactored
functions produce identical output, you're done. If they diverge, the test fails and
shows you exactly what changed.

### Step A — Add `writeLines` calls to `end_of_season_recap.R`

Add these alongside the existing `clipr::write_clip()` calls, run the script once with
real data, then commit the `.json` files to git. Remove these lines afterward.

```r
# Temporarily added to end_of_season_recap.R — run once, then remove
dir.create("offensiveline/tests/testthat/snapshots", recursive = TRUE, showWarnings = FALSE)

writeLines(starter_json,         "offensiveline/tests/testthat/snapshots/starter_ppg.json")
writeLines(kicker_def_json,      "offensiveline/tests/testthat/snapshots/kicker_def.json")
writeLines(all_trades_json,      "offensiveline/tests/testthat/snapshots/all_trades.json")
writeLines(all_free_agents_json, "offensiveline/tests/testthat/snapshots/all_free_agents.json")
writeLines(recap_data_json,      "offensiveline/tests/testthat/snapshots/recap_data.json")
writeLines(slut_meter_json,      "offensiveline/tests/testthat/snapshots/slut_meter.json")
writeLines(best_ball_bench_json, "offensiveline/tests/testthat/snapshots/best_ball_bench.json")
```

### Step B — Copy `main.R` outputs (they already write to files)

```r
# main.R already calls write_json_to_file(), so just copy those outputs to snapshots
file.copy(leaderboard_file_path,    "offensiveline/tests/testthat/snapshots/leaderboard.json")
file.copy(motw_file_path,           "offensiveline/tests/testthat/snapshots/motw_table.json")
file.copy(awards_file_path,         "offensiveline/tests/testthat/snapshots/awards.json")
file.copy(efficiency_file_path,     "offensiveline/tests/testthat/snapshots/efficiency.json")
file.copy(best_ball_file_path,      "offensiveline/tests/testthat/snapshots/best_ball_lb.json")
file.copy(power_rankings_file_path, "offensiveline/tests/testthat/snapshots/power_rankings.json")
file.copy(schedule_file_path,       "offensiveline/tests/testthat/snapshots/schedule.json")
file.copy(shots_file_path,          "offensiveline/tests/testthat/snapshots/shots_dist.json")
```

Commit all `.json` files in `snapshots/`. These are the source of truth for Phase 3.

---

## Phase 3 — Write Regression Tests Against Golden Files

With golden files in place, write tests that run the refactored functions and compare
their output against the golden files. Write these tests now, before Phase 5, so they
exist as the gate that controls when refactoring is complete.

```r
# test-snapshots.R

compare_to_golden <- function(actual_json, golden_file) {
  golden <- jsonlite::fromJSON(readLines(golden_file))
  actual <- jsonlite::fromJSON(actual_json)
  expect_equal(actual, golden)
}

# --- main.R outputs ---
test_that("leaderboard_to_json matches golden", {
  mock_m <- readRDS("fixtures/matchups_full.rds")
  photos <- readRDS("fixtures/team_photos.rds")
  compare_to_golden(leaderboard_to_json(mock_m, max_week = 17, photos),
                    "snapshots/leaderboard.json")
})

test_that("awards_to_json matches golden", {
  mock_m <- readRDS("fixtures/matchups_full.rds")
  mock_p <- readRDS("fixtures/players_full.rds")
  photos <- readRDS("fixtures/team_photos.rds")
  compare_to_golden(awards_to_json(mock_m, mock_p, max_week = 17, photos),
                    "snapshots/awards.json")
})

test_that("motw_table_to_json matches golden", {
  mock_motw <- readRDS("fixtures/motw_full.rds")
  compare_to_golden(motw_table_to_json(mock_motw), "snapshots/motw_table.json")
})

test_that("efficiency_to_json matches golden", {
  mock_m <- readRDS("fixtures/matchups_full.rds")
  mock_p <- readRDS("fixtures/players_full.rds")
  compare_to_golden(efficiency_to_json(mock_m, mock_p, max_week = 17),
                    "snapshots/efficiency.json")
})

test_that("power_rankings_to_json matches golden", {
  mock_m <- readRDS("fixtures/matchups_full.rds")
  compare_to_golden(power_rankings_to_json(mock_m, max_week = 17, number_of_teams = 12),
                    "snapshots/power_rankings.json")
})

test_that("schedule_comparison_to_json matches golden", {
  mock_m <- readRDS("fixtures/matchups_full.rds")
  photos <- readRDS("fixtures/team_photos.rds")
  compare_to_golden(schedule_comparison_to_json(mock_m, photos),
                    "snapshots/schedule.json")
})

test_that("best_ball_lb_to_json matches golden", {
  mock_m <- readRDS("fixtures/matchups_full.rds")
  mock_p <- readRDS("fixtures/players_full.rds")
  compare_to_golden(best_ball_lb_to_json(mock_m, mock_p, max_week = 17),
                    "snapshots/best_ball_lb.json")
})

test_that("shots_dist_to_json matches golden", {
  mock_motw <- readRDS("fixtures/motw_full.rds")
  compare_to_golden(shots_dist_to_json(mock_motw), "snapshots/shots_dist.json")
})

# --- end_of_season_recap.R outputs (once refactored into build_recap_data()) ---
test_that("recap_data matches golden", {
  mock_m    <- readRDS("fixtures/matchups_full.rds")
  mock_motw <- readRDS("fixtures/motw_full.rds")
  mock_tx   <- readRDS("fixtures/transactions_full.rds")
  mock_p    <- readRDS("fixtures/players_full.rds")
  compare_to_golden(
    jsonlite::toJSON(build_recap_data(mock_m, mock_motw, mock_tx, mock_p)),
    "snapshots/recap_data.json"
  )
})
```

These tests will **fail until Phase 5 is complete** — that's expected and intentional.
They are the finish line for the refactor.

---

## Phase 4 — Unit Test the `offensiveline` Package Functions

These functions are already extracted — write tests for them directly. The examples below
are the full intended test list for each function, not just a starting point.

### Priority 1: `stuff_to_json.R` — `spec_color2_scale`

These tests have limited standalone value — the function is simple and the golden file
tests in Phase 3 will catch any regressions implicitly since color values appear in every
output. If you write them at all, trim to just the two meaningful cases below and skip the
rest.

```r
# test-colors.R
test_that("returns a hex color string for each input value", {
  result <- spec_color2_scale(c(0, 50, 100), scale_from = c(0, 100), direction = 1)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", result)))
})

test_that("output length matches input length", {
  result <- spec_color2_scale(1:12, scale_from = c(1, 12), direction = 1)
  expect_length(result, 12)
})

test_that("direction = -1 flips the gradient", {
  asc  <- spec_color2_scale(c(0, 100), scale_from = c(0, 100), direction =  1)
  desc <- spec_color2_scale(c(0, 100), scale_from = c(0, 100), direction = -1)
  expect_equal(asc[1], desc[2])
  expect_equal(asc[2], desc[1])
})

test_that("all-same-value input returns identical colors", {
  result <- spec_color2_scale(c(50, 50, 50), scale_from = c(50, 50), direction = 1)
  expect_true(length(unique(result)) == 1)
})

test_that("scale_from clamps values outside range without erroring", {
  expect_no_error(
    spec_color2_scale(c(0, 150), scale_from = c(0, 100), direction = 1)
  )
})
```

### Priority 2: `leaderboards.R` — `create_leaderboard`, `create_power_rankings`, `create_median_leaderboard`

These feed almost everything downstream.

```r
# test-leaderboards.R

# --- create_leaderboard ---
test_that("returns one row per team", {
  mock <- readRDS("fixtures/matchups_w3.rds")
  result <- create_leaderboard(mock, max_week = 3)
  expect_equal(nrow(result), length(unique(mock$manager_id)))
})

test_that("W + L equals weeks played for every team", {
  mock <- readRDS("fixtures/matchups_w3.rds")
  result <- create_leaderboard(mock, max_week = 3)
  expect_true(all(result$W + result$L == 3))
})

test_that("PF equals sum of team_points from raw matchup data", {
  mock <- readRDS("fixtures/matchups_w3.rds")
  result <- create_leaderboard(mock, max_week = 3)
  expected_pf <- mock %>% group_by(manager_id) %>% summarise(PF = sum(team_points))
  expect_equal(sort(result$PF), sort(expected_pf$PF))
})

test_that("teams are ranked 1 through N with no gaps or duplicates", {
  mock <- readRDS("fixtures/matchups_w3.rds")
  result <- create_leaderboard(mock, max_week = 3)
  expect_equal(sort(result$Rank), seq_len(nrow(result)))
})

test_that("winner with more wins ranks above team with fewer wins", {
  mock <- readRDS("fixtures/matchups_w3.rds")
  result <- create_leaderboard(mock, max_week = 3)
  # Check rank is monotonically non-increasing with wins
  result_sorted <- result[order(result$Rank), ]
  expect_true(all(diff(result_sorted$W) <= 0))
})

# --- create_power_rankings ---
test_that("power rankings return one row per team", {
  mock <- readRDS("fixtures/matchups_w3.rds")
  result <- create_power_rankings(mock, max_week = 3, number_of_teams = 12)
  expect_equal(nrow(result), length(unique(mock$manager_id)))
})

test_that("power ranking scores are numeric and non-negative", {
  mock <- readRDS("fixtures/matchups_w3.rds")
  result <- create_power_rankings(mock, max_week = 3, number_of_teams = 12)
  expect_true(all(is.numeric(result$power_rank_score)))
  expect_true(all(result$power_rank_score >= 0))
})
```

### Priority 3: `motw.R` — `add_motw_to_matchups`, `create_motw_table`

MotW logic is stateful — the loser of week N carries the matchup into week N+1 — and
is the most likely to produce subtle bugs after refactoring.

```r
# test-motw.R

# --- add_motw_to_matchups ---
test_that("motw flag is set on exactly 2 rows (1 matchup) per week", {
  mock_m <- readRDS("fixtures/matchups_w3.rds")
  mock_p <- readRDS("fixtures/players_w3.rds")
  result <- add_motw_to_matchups(mock_m, week_1_matchup_id = 1, max_week = 3, mock_p)
  motw_per_week <- result %>% filter(motw == 1) %>% count(week)
  expect_true(all(motw_per_week$n == 2))
})

test_that("every week has a motw matchup", {
  mock_m <- readRDS("fixtures/matchups_w3.rds")
  mock_p <- readRDS("fixtures/players_w3.rds")
  result <- add_motw_to_matchups(mock_m, week_1_matchup_id = 1, max_week = 3, mock_p)
  expect_equal(sort(unique(result$week[result$motw == 1])), 1:3)
})

test_that("motw loser in week N is in the motw matchup in week N+1", {
  mock_m <- readRDS("fixtures/matchups_w3.rds")
  mock_p <- readRDS("fixtures/players_w3.rds")
  result <- add_motw_to_matchups(mock_m, week_1_matchup_id = 1, max_week = 3, mock_p)

  for (w in 1:2) {
    loser_id <- result %>%
      filter(motw == 1, week == w, winner == 0) %>%
      pull(manager_id)
    next_week_motw_ids <- result %>%
      filter(motw == 1, week == w + 1) %>%
      pull(manager_id)
    expect_true(loser_id %in% next_week_motw_ids)
  }
})

test_that("# of Shots is a positive integer for motw rows", {
  mock_m <- readRDS("fixtures/matchups_w3.rds")
  mock_p <- readRDS("fixtures/players_w3.rds")
  result <- add_motw_to_matchups(mock_m, week_1_matchup_id = 1, max_week = 3, mock_p)
  shots <- result %>% filter(motw == 1) %>% pull(`# of Shots`)
  expect_true(all(shots > 0))
  expect_true(all(shots == floor(shots)))
})

test_that("winner column is 0 or 1 for motw rows", {
  mock_m <- readRDS("fixtures/matchups_w3.rds")
  mock_p <- readRDS("fixtures/players_w3.rds")
  result <- add_motw_to_matchups(mock_m, week_1_matchup_id = 1, max_week = 3, mock_p)
  motw_winners <- result %>% filter(motw == 1) %>% pull(winner)
  expect_true(all(motw_winners %in% c(0, 1)))
})

test_that("each motw matchup has exactly one winner and one loser", {
  mock_m <- readRDS("fixtures/matchups_w3.rds")
  mock_p <- readRDS("fixtures/players_w3.rds")
  result <- add_motw_to_matchups(mock_m, week_1_matchup_id = 1, max_week = 3, mock_p)
  for (w in 1:3) {
    week_motw <- result %>% filter(motw == 1, week == w)
    expect_equal(sum(week_motw$winner), 1)
  }
})
```

### Priority 4: `best_ball.R` — `calc_best_ball_lineups`, `create_best_ball_matchups`

```r
# test-best-ball.R

# --- calc_best_ball_lineups ---
test_that("optimal lineup score is always >= actual lineup score", {
  mock <- readRDS("fixtures/players_w3.rds")
  result <- calc_best_ball_lineups(mock, max_week = 3)
  comparison <- result %>%
    group_by(manager_id, week) %>%
    summarise(
      optimal = sum(points[optimal_slot != "BENCH"]),
      actual  = sum(points[!is.na(starter_id)])
    )
  expect_true(all(comparison$optimal >= comparison$actual))
})

test_that("every player appears exactly once per team per week", {
  mock <- readRDS("fixtures/players_w3.rds")
  result <- calc_best_ball_lineups(mock, max_week = 3)
  counts <- result %>% count(manager_id, week, full_name)
  expect_true(all(counts$n == 1))
})

test_that("optimal_slot is never NA", {
  mock <- readRDS("fixtures/players_w3.rds")
  result <- calc_best_ball_lineups(mock, max_week = 3)
  expect_false(any(is.na(result$optimal_slot)))
})

test_that("number of non-BENCH slots matches expected starter count", {
  mock <- readRDS("fixtures/players_w3.rds")
  result <- calc_best_ball_lineups(mock, max_week = 3)
  starters_per_team <- result %>%
    filter(optimal_slot != "BENCH") %>%
    count(manager_id, week)
  # Assuming 8 starter slots (QB, RB, RB, WR, WR, TE, DEF, K)
  expect_true(all(starters_per_team$n == 8))
})

# --- create_best_ball_matchups ---
test_that("best ball matchups return one row per team per week", {
  mock <- readRDS("fixtures/players_w3.rds")
  lineups <- calc_best_ball_lineups(mock, max_week = 3)
  result  <- create_best_ball_matchups(lineups)
  counts  <- result %>% count(manager_id, week)
  expect_true(all(counts$n == 1))
})

test_that("best ball team score >= actual team score in all matchups", {
  mock_m <- readRDS("fixtures/matchups_w3.rds")
  mock_p <- readRDS("fixtures/players_w3.rds")
  lineups <- calc_best_ball_lineups(mock_p, max_week = 3)
  bb_matchups <- create_best_ball_matchups(lineups)
  joined <- inner_join(mock_m, bb_matchups, by = c("manager_id", "week"))
  expect_true(all(joined$best_ball_points >= joined$team_points))
})
```

### Priority 5: `awards_table.R` — `create_awards_table`, `find_top_player`

```r
# test-awards.R
test_that("awards table has one row per award category", {
  mock_m <- readRDS("fixtures/matchups_w3.rds")
  mock_p <- readRDS("fixtures/players_w3.rds")
  lineups <- calc_best_ball_lineups(mock_p, max_week = 3)
  bb_matchups <- create_best_ball_matchups(lineups)
  photos  <- readRDS("fixtures/team_photos.rds")
  result  <- create_awards_table(mock_p, mock_m, bb_matchups, photos)
  expect_true(nrow(result) > 0)
  expect_false(any(duplicated(result$award)))
})

test_that("find_top_player returns a single player name", {
  mock_p <- readRDS("fixtures/players_w3.rds")
  result <- find_top_player("QB", mock_p, current_week = 3)
  expect_length(result, 1)
  expect_type(result, "character")
})

test_that("find_top_player top QB is actually in the QB position", {
  mock_p <- readRDS("fixtures/players_w3.rds")
  top_qb <- find_top_player("QB", mock_p, current_week = 3)
  expect_true(top_qb %in% mock_p$full_name[mock_p$position == "QB"])
})
```

---

## Phase 5 — Extract & Test Root Script Logic

The inline pipelines in `end_of_season_recap.R` need to become testable functions.
The goal is to move them into `offensiveline/R/recap.R`.

The regression tests from Phase 3 are the gate: once `build_recap_data()` and the
individual helper functions produce output that matches every golden file, delete the
old inline code. Do not delete until all Phase 3 tests pass green.

### Functions to extract from `end_of_season_recap.R`

| Lines   | Current inline logic                   | New function name               |
| ------- | -------------------------------------- | ------------------------------- |
| 125–154 | best/worst win/loss scores             | `summarize_win_loss_extremes()` |
| 158–185 | MotW counts + shots taken              | `summarize_motw_record()`       |
| 188–207 | shots/dogs given out                   | `summarize_motw_given()`        |
| 210–261 | best/worst scoring weeks               | `summarize_weekly_rank()`       |
| 264–316 | win/loss point differential            | `summarize_pt_diff()`           |
| 566–641 | trade winner logic (the big case_when) | `evaluate_trades()`             |

### `evaluate_trades()` — test every branch of the `case_when`

This is the highest-value test target. The 7 branches need individual test cases built
from small hand-crafted data frames (not fixtures), so each case is unambiguous:

```r
# test-recap.R

# Helper: minimal trade data frame for two teams in one transaction
make_trade <- function(team, points, bid = NA) {
  data.frame(
    trans_id   = 1L,
    team_name  = team,
    total_points = points,
    waiver_bid = bid,
    stringsAsFactors = FALSE
  )
}

# Branch 1: team with more total_points wins
test_that("evaluate_trades: team with higher total_points wins", {
  input <- rbind(make_trade("A", 120), make_trade("B", 80))
  result <- evaluate_trades(input)
  expect_equal(result$winner[result$team_name == "A"], 1L)
  expect_equal(result$winner[result$team_name == "B"], 0L)
})

# Branch 2: tied total_points -> both teams are winners (max(total_points) applies to both)
test_that("evaluate_trades: tied total_points marks both as winner", {
  input <- rbind(make_trade("A", 100), make_trade("B", 100))
  result <- evaluate_trades(input)
  expect_true(all(result$winner == 1L))
})

# Branch 3: no points scored, only one team bid — that team wins
test_that("evaluate_trades: no points, solo bidder wins", {
  input <- rbind(make_trade("A", 0, bid = 25), make_trade("B", 0, bid = NA))
  result <- evaluate_trades(input)
  expect_equal(result$winner[result$team_name == "A"], 1L)
  expect_equal(result$winner[result$team_name == "B"], 0L)
})

# Branch 4: no points, only one team bid — non-bidder loses
test_that("evaluate_trades: no points, non-bidder is marked 0", {
  input <- rbind(make_trade("A", 0, bid = NA), make_trade("B", 0, bid = 10))
  result <- evaluate_trades(input)
  expect_equal(result$winner[result$team_name == "A"], 0L)
})

# Branch 5: no points, two distinct bids — higher bid wins
test_that("evaluate_trades: no points, higher FAAB bid wins", {
  input <- rbind(make_trade("A", 0, bid = 50), make_trade("B", 0, bid = 20))
  result <- evaluate_trades(input)
  expect_equal(result$winner[result$team_name == "A"], 1L)
  expect_equal(result$winner[result$team_name == "B"], 0L)
})

# Branch 6: no points, identical bids — both marked NA
test_that("evaluate_trades: no points, equal bids = NA", {
  input <- rbind(make_trade("A", 0, bid = 30), make_trade("B", 0, bid = 30))
  result <- evaluate_trades(input)
  expect_true(all(is.na(result$winner)))
})

# Branch 7: no points, multiple rows with the max bid — NA (tied top bids)
test_that("evaluate_trades: no points, multiple max bidders = NA", {
  input <- rbind(
    make_trade("A", 0, bid = 50),
    make_trade("B", 0, bid = 50),
    make_trade("C", 0, bid = 30)
  )
  result <- evaluate_trades(input)
  expect_true(all(is.na(result$winner[result$team_name %in% c("A", "B")])))
})
```

### `summarize_win_loss_extremes()` tests

```r
test_that("best win is the highest scoring game that was a win", {
  mock <- readRDS("fixtures/matchups_full.rds")
  result <- summarize_win_loss_extremes(mock)
  expect_true(all(!is.na(result$most_points_1)))  # every team has at least one win
})

test_that("worst loss is the lowest scoring game that was a loss", {
  mock <- readRDS("fixtures/matchups_full.rds")
  result <- summarize_win_loss_extremes(mock)
  expect_true(all(result$least_points_0 < result$most_points_0))
})

test_that("returns one row per team", {
  mock <- readRDS("fixtures/matchups_full.rds")
  result <- summarize_win_loss_extremes(mock)
  expect_equal(nrow(result), length(unique(mock$manager_id)))
})
```

### `summarize_pt_diff()` tests

```r
test_that("win differential is always positive", {
  mock <- readRDS("fixtures/matchups_full.rds")
  result <- summarize_pt_diff(mock)
  expect_true(all(result$pt_diff_1 >= 0))
})

test_that("close_games_1 + blowouts_1 <= total wins", {
  mock <- readRDS("fixtures/matchups_full.rds")
  lb   <- create_leaderboard(mock, max_week = 17)
  result <- summarize_pt_diff(mock)
  joined <- inner_join(result, lb %>% select(manager_id, W), by = "manager_id")
  expect_true(all(joined$close_games_1 + joined$blowouts_1 <= joined$W))
})
```

---

---

## Running Tests

```r
# From RStudio or the R console, with working dir set to offensiveline/
devtools::test()

# Run a single file
testthat::test_file("tests/testthat/test-motw.R")

# From the root project directory
devtools::test("offensiveline")
```

---

## Phased Summary

| Phase | Work                                                           | Value                                     |
| ----- | -------------------------------------------------------------- | ----------------------------------------- |
| 1     | Install testthat, generate fixtures from Sleeper API           | Foundation — enables everything else      |
| 2     | Capture golden JSON files from current scripts                 | Safety net before any code changes        |
| 3     | Write regression tests against golden files                    | Tests that define "done" for the refactor |
| 4     | Unit test existing `offensiveline` package functions           | Confidence in the foundations             |
| 5     | Extract inline recap logic to functions; pass regression tests | Safe deletion of old inline code          |

Phases 2–3 are the safety net and should be done **before touching any code**.
Phase 4 can run in parallel with Phase 5 as you work through the refactor.
Phase 5 is complete only when all golden file regression tests pass green.

---

## Todo List

### Phase 1 — Infrastructure

- [x] Run `usethis::use_testthat()` from inside `offensiveline/`
- [x] Verify `testthat` is added to `Suggests` in `offensiveline/DESCRIPTION`
- [x] Create `fixture_setup.R` in the project root using the Phase 1 code block
- [x] Run `fixture_setup.R` with real data to generate all `.rds` files
- [x] Verify all 9 fixture files exist in `offensiveline/tests/testthat/fixtures/`
  - [x] `matchups_full.rds`
  - [x] `players_full.rds`
  - [x] `motw_full.rds`
  - [x] `transactions_full.rds`
  - [x] `team_photos.rds`
  - [x] `matchups_w3.rds`
  - [x] `players_w3.rds`
  - [x] `motw_w3.rds`
  - [x] `transactions_w3.rds`
- [x] Add `offensiveline/tests/testthat.R` entry point file
- [x] Commit all fixture `.rds` files and the entry point to git

---

### Phase 2 — Capture Golden Files

> **Note**: Instead of running `main.R` and `end_of_season_recap.R` directly (which require
> live API calls, Python scripts, and interactive clipboard access), a `snapshot_setup.R`
> script was created that loads the `.rds` fixtures and runs the same logic to produce
> identical golden files deterministically.

- [x] Create `snapshot_setup.R` using fixture data (replaces Steps A and B)
- [x] Run `snapshot_setup.R` to generate all golden files
- [x] Verify all 7 recap golden files exist in `offensiveline/tests/testthat/snapshots/`
  - [x] `starter_ppg.json`
  - [x] `kicker_def.json`
  - [x] `all_trades.json`
  - [x] `all_free_agents.json`
  - [x] `recap_data.json`
  - [x] `slut_meter.json`
  - [x] `best_ball_bench.json`
- [x] Verify all 8 main.R golden files exist in `snapshots/`
  - [x] `leaderboard.json`
  - [x] `motw_table.json`
  - [x] `awards.json`
  - [x] `efficiency.json`
  - [x] `best_ball_lb.json`
  - [x] `power_rankings.json`
  - [x] `schedule.json`
  - [x] `shots_dist.json`
- [x] Commit all 15 golden `.json` files to git

---

### Phase 3 — Regression Tests Against Golden Files

- [x] Create `offensiveline/tests/testthat/test-snapshots.R`
- [x] Add the `compare_to_golden()` helper function
- [x] Write regression test for `leaderboard_to_json`
- [x] Write regression test for `awards_to_json`
- [x] Write regression test for `motw_table_to_json`
- [x] Write regression test for `efficiency_to_json`
- [x] Write regression test for `power_rankings_to_json`
- [x] Write regression test for `schedule_comparison_to_json`
- [x] Write regression test for `best_ball_lb_to_json`
- [x] Write regression test for `shots_dist_to_json`
- [x] Write regression test for `recap_data` / `build_recap_data()` *(skipped until Phase 5)*
- [x] Run `devtools::test("offensiveline")` — **FAIL 0 | WARN 5 | SKIP 1 | PASS 8** ✓

---

### Phase 4 — Unit Tests for Package Functions

- [ ] Create `offensiveline/tests/testthat/test-colors.R` *(optional — see Phase 4 note)*
  - [ ] `direction = -1 flips the gradient`
  - [ ] `all-same-value input returns identical colors`
- [ ] Create `offensiveline/tests/testthat/test-leaderboards.R`
  - [ ] `returns one row per team`
  - [ ] `W + L equals weeks played for every team`
  - [ ] `PF equals sum of team_points from raw matchup data`
  - [ ] `teams are ranked 1 through N with no gaps or duplicates`
  - [ ] `winner with more wins ranks above team with fewer wins`
  - [ ] `power rankings return one row per team`
  - [ ] `power ranking scores are numeric and non-negative`
- [ ] Create `offensiveline/tests/testthat/test-motw.R`
  - [ ] `motw flag is set on exactly 2 rows (1 matchup) per week`
  - [ ] `every week has a motw matchup`
  - [ ] `motw loser in week N is in the motw matchup in week N+1`
  - [ ] `# of Shots is a positive integer for motw rows`
  - [ ] `winner column is 0 or 1 for motw rows`
  - [ ] `each motw matchup has exactly one winner and one loser`
- [ ] Create `offensiveline/tests/testthat/test-best-ball.R`
  - [ ] `optimal lineup score is always >= actual lineup score`
  - [ ] `every player appears exactly once per team per week`
  - [ ] `optimal_slot is never NA`
  - [ ] `number of non-BENCH slots matches expected starter count`
  - [ ] `best ball matchups return one row per team per week`
  - [ ] `best ball team score >= actual team score in all matchups`
- [ ] Create `offensiveline/tests/testthat/test-awards.R`
  - [ ] `awards table has one row per award category`
  - [ ] `find_top_player returns a single player name`
  - [ ] `find_top_player top QB is actually in the QB position`
- [ ] Run `devtools::test("offensiveline")` and confirm all Phase 4 tests pass

---

### Phase 5 — Extract Recap Logic & Pass Regression Tests

- [ ] Create `offensiveline/R/recap.R`
- [ ] Extract `summarize_win_loss_extremes()` from `end_of_season_recap.R` lines 125–154
- [ ] Extract `summarize_motw_record()` from lines 158–185
- [ ] Extract `summarize_motw_given()` from lines 188–207
- [ ] Extract `summarize_weekly_rank()` from lines 210–261
- [ ] Extract `summarize_pt_diff()` from lines 264–316
- [ ] Extract `evaluate_trades()` from lines 566–641
- [ ] Create `build_recap_data()` that orchestrates all of the above into a single call
- [ ] Export all new functions via `@export` roxygen tags and rebuild `NAMESPACE`
- [ ] Create `offensiveline/tests/testthat/test-recap.R`
  - [ ] Add `make_trade()` helper
  - [ ] `evaluate_trades: team with higher total_points wins` (Branch 1)
  - [ ] `evaluate_trades: tied total_points marks both as winner` (Branch 2)
  - [ ] `evaluate_trades: no points, solo bidder wins` (Branch 3)
  - [ ] `evaluate_trades: no points, non-bidder is marked 0` (Branch 4)
  - [ ] `evaluate_trades: no points, higher FAAB bid wins` (Branch 5)
  - [ ] `evaluate_trades: no points, equal bids = NA` (Branch 6)
  - [ ] `evaluate_trades: no points, multiple max bidders = NA` (Branch 7)
  - [ ] `summarize_win_loss_extremes: best win is highest scoring win`
  - [ ] `summarize_win_loss_extremes: worst loss is lowest scoring loss`
  - [ ] `summarize_win_loss_extremes: returns one row per team`
  - [ ] `summarize_pt_diff: win differential is always positive`
  - [ ] `summarize_pt_diff: close_games_1 + blowouts_1 <= total wins`
- [ ] Run `devtools::test("offensiveline")` — confirm Phase 3 `recap_data` test now passes
- [ ] Run full test suite and confirm everything is green
- [ ] Update `end_of_season_recap.R` to call `build_recap_data()` instead of inline code
- [ ] Delete the extracted inline pipelines from `end_of_season_recap.R`
- [ ] Final run of full test suite — confirm still all green
