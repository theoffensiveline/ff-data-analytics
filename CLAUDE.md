# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This repo analyzes a Sleeper fantasy football league. It has two parts:
1. **`offensiveline/` R package** — all reusable functions (ETL, analytics, JSON serialization)
2. **Root scripts** (`main.R`, `end_of_season_recap.R`, `new_site_stuff.R`) — orchestration scripts that call package functions and write JSON outputs consumed by a separate website repo at `../theoffensiveline-site/`

The package depends on `sleeperapi` (a separate R package wrapping the Sleeper API) and `dplyr`/`tidyr` for data manipulation.

## Commands

### Running Tests

```r
# All tests (run from RStudio or R console with working dir = offensiveline/)
devtools::test()

# From the root project directory
devtools::test("offensiveline")

# Single test file
testthat::test_file("tests/testthat/test-motw.R")
```

### Installing / Reloading the Package

```r
# From project root
devtools::load_all("offensiveline")

# Or from inside offensiveline/
devtools::load_all()
```

### Rebuilding the NAMESPACE After Adding @export Tags

```r
devtools::document("offensiveline")
```

### Refreshing Fixtures (Start of Each New Season)

Run `fixture_setup.R` from the project root with a valid Sleeper connection. This regenerates the `.rds` fixture files in `offensiveline/tests/testthat/fixtures/`.

### Regenerating Golden Snapshot Files

Run `snapshot_setup.R` from the project root (uses fixtures, no live API calls) to regenerate the `.json` golden files in `offensiveline/tests/testthat/snapshots/`.

## Architecture

### Data Flow

```
Sleeper API
    ↓
sleeper-etl.R  (get_all_matchups_data → get_team_matchups → add_motw_to_matchups)
    ↓
all_players (player-level, one row per player per team per week)
all_matchups (team-level, one row per team per week)
motw_data   (all_matchups with MotW flag and shot counts added)
    ↓
*_to_json() functions in stuff_to_json.R + leaderboards/awards/motw/best_ball/recap.R
    ↓
JSON files → ../theoffensiveline-site/src/newsletters/20YY/20YY Week WW/
```

### Key Data Structures

- **`all_players`**: player-level data. Columns include `week`, `matchup_id`, `manager_id`, `team_name`, `winner`, `team_points`, `players` (player_id), `points` (individual), `full_name`, `position`, `starter_id` (NA = bench).
- **`all_matchups`**: team-level summary. One row per team per week. Produced by `get_team_matchups(all_players)`.
- **`motw_data`**: `all_matchups` augmented with `motw` flag and `# of Shots` column from `add_motw_to_matchups()`. The MotW loser each week faces that week's MotW winner the following week.

### Package Structure (`offensiveline/R/`)

| File | Responsibility |
|------|---------------|
| `sleeper-etl.R` | Sleeper API calls → `all_players`, `all_matchups`, transactions |
| `leaderboards.R` | `create_leaderboard()`, `create_power_rankings()`, `create_median_leaderboard()` |
| `motw.R` | `add_motw_to_matchups()`, `create_motw_table()` |
| `best_ball.R` | `calc_best_ball_lineups()`, `create_best_ball_matchups()`, `create_best_ball_leaderboard()` |
| `awards_table.R` | `create_awards_table()`, `find_top_player()` |
| `recap.R` | `build_recap_data()` and its helper summarizers (end-of-season stats) |
| `stuff_to_json.R` | `*_to_json()` serializers, `spec_color2_scale()`, `generate_file_path()`, `write_json_to_file()` |
| `playoffOdds.R` | Playoff odds / danger table logic |

### Color Scaling

`spec_color2_scale(x, scale_from, direction)` in `stuff_to_json.R` maps numeric values to hex colors using a custom 36-color palette (`custom_palette36`). Every JSON output includes `*_color` columns alongside data columns — these are consumed directly by the website for table cell backgrounds.

### Testing Architecture

Tests live in `offensiveline/tests/testthat/`. Two categories:

- **Unit tests** (`test-leaderboards.R`, `test-motw.R`, `test-best-ball.R`, `test-awards.R`, `test-recap.R`): use `_w3` fixture slices (weeks 1–3) for speed
- **Regression/snapshot tests** (`test-snapshots.R`): use `_full` fixtures (all 17 weeks) and compare `*_to_json()` output against golden `.json` files in `snapshots/`

Fixtures are `.rds` files captured from the real Sleeper API once and committed to git. Tests never hit the live API.

### Output File Paths

`generate_file_path()` writes to `../theoffensiveline-site/src/newsletters/20YY/20YY Week WW/<file_name>`. The site repo must be cloned as a sibling of this repo.

### League IDs

- Main league: `1253779168802377728`
- Walter league: `1223730601350135814` (secondary, commented out in scripts)

### Season Configuration

`current_week` and `current_year` are set manually at the top of `main.R` before running. `current_year` is a 2-digit year (e.g., `25` for 2025). The `NFL_state` API call is available but often overridden manually.
