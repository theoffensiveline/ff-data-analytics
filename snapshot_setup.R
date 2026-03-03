# snapshot_setup.R — run once after fixture_setup.R, then commit the .json files
# Generates golden JSON files from fixtures without hitting the Sleeper API

library(sleeperapi)
devtools::load_all("offensiveline")  # load source code, not installed copy
library(dplyr)
library(tidyr)
library(scales)

snapshots_dir <- "offensiveline/tests/testthat/snapshots"
fixtures_dir  <- "offensiveline/tests/testthat/fixtures"

dir.create(snapshots_dir, recursive = TRUE, showWarnings = FALSE)

# ── Load fixtures ────────────────────────────────────────────────────────────
all_players_raw  <- readRDS(file.path(fixtures_dir, "players_full.rds"))
all_matchups_raw <- readRDS(file.path(fixtures_dir, "matchups_full.rds"))
motw_data        <- readRDS(file.path(fixtures_dir, "motw_full.rds"))
all_transactions <- readRDS(file.path(fixtures_dir, "transactions_full.rds"))
team_photos      <- readRDS(file.path(fixtures_dir, "team_photos.rds"))

# Apply same filters main.R / end_of_season_recap.R apply
all_players  <- all_players_raw[complete.cases(all_players_raw$matchup_id), ]
all_matchups <- all_matchups_raw[complete.cases(all_matchups_raw$matchup_id), ]

current_week <- 17

# ── main.R golden files ──────────────────────────────────────────────────────
writeLines(awards_to_json(all_matchups, all_players, current_week, team_photos),
           file.path(snapshots_dir, "awards.json"))

writeLines(best_ball_lb_to_json(all_matchups, all_players, current_week),
           file.path(snapshots_dir, "best_ball_lb.json"))

writeLines(efficiency_to_json(all_matchups, all_players, current_week),
           file.path(snapshots_dir, "efficiency.json"))

writeLines(leaderboard_to_json(all_matchups, current_week, team_photos),
           file.path(snapshots_dir, "leaderboard.json"))

writeLines(motw_table_to_json(motw_data),
           file.path(snapshots_dir, "motw_table.json"))

writeLines(power_rankings_to_json(all_matchups, current_week, 12),
           file.path(snapshots_dir, "power_rankings.json"))

writeLines(schedule_comparison_to_json(all_matchups, team_photos),
           file.path(snapshots_dir, "schedule.json"))

writeLines(shots_dist_to_json(motw_data),
           file.path(snapshots_dir, "shots_dist.json"))

message("main.R golden files written.")

# ── end_of_season_recap.R golden files ──────────────────────────────────────
# Derived variables needed by recap inline logic
all_starters <- all_players[!is.na(all_players$starter_id), ]

starter_ppg <- all_starters %>%
  group_by(team_name, full_name, position, players, manager_id) %>%
  summarize(
    games_played = n(),
    ppg = round(mean(points), 2),
    points = sum(points)
  ) %>%
  ungroup() %>%
  arrange(desc(ppg))

starter_ppg$color <- spec_color2_scale(starter_ppg$ppg,
                                       scale_from = c(min(starter_ppg$ppg), max(starter_ppg$ppg)),
                                       direction = 1)

starter_json <-
  jsonlite::toJSON(starter_ppg %>% select(team_name, full_name, position, games_played, ppg, color))

writeLines(starter_json, file.path(snapshots_dir, "starter_ppg.json"))


kicker_def <- all_starters %>%
  filter(position == c('K', 'DEF')) %>%
  group_by(team_name, position) %>%
  summarize(count = n_distinct(full_name), ppg = round(mean(points), 2)) %>%
  ungroup() %>%
  arrange(position, team_name) %>%
  left_join(team_photos, by = "team_name")

kicker_def_json <- jsonlite::toJSON(kicker_def)

writeLines(kicker_def_json, file.path(snapshots_dir, "kicker_def.json"))


sleeper_players_df <- read.csv("sleeper_players.csv") %>%
  select(player_id, full_name) %>%
  mutate(full_name = coalesce(full_name, player_id))

team_names <- all_players %>%
  group_by(manager_id) %>%
  summarise(team_name = unique(team_name))

all_trades <- all_transactions %>%
  filter(type == 'trade' & status == 'complete') %>%
  mutate(player_id = as.character(player_id)) %>%
  left_join(starter_ppg,
            by = c('manager_id' = 'manager_id', 'player_id' = 'players')) %>%
  left_join(sleeper_players_df, by = 'player_id') %>%
  mutate(full_name = coalesce(full_name.x, full_name.y)) %>%
  select(-starts_with("full_name.")) %>%
  left_join(team_names, by = 'manager_id') %>%
  mutate(team_name = coalesce(team_name.x, team_name.y)) %>%
  select(-starts_with("team_name.")) %>%
  mutate(
    points = ifelse(is.na(points), 0, points),
    ppg = ifelse(is.na(ppg), 0, ppg),
    games_played = ifelse(is.na(games_played), 0, games_played)
  ) %>%
  filter(add_drop == 'add') %>%
  select(
    week,
    trans_id,
    team_name,
    manager_id,
    games_played,
    ppg,
    points,
    full_name,
    waiver_bid
  ) %>%
  mutate(total_points = ave(points, trans_id, team_name, FUN = sum)) %>%
  mutate(full_name = ifelse(is.na(full_name), paste0("$", waiver_bid, " FAAB"), full_name)) %>%
  group_by(trans_id) %>%
  mutate(
    winner = case_when(
      max(total_points) > 0 & total_points == max(total_points) ~ 1L,
      max(total_points) == 0 &
        sum(!is.na(waiver_bid)) == 1 & !is.na(waiver_bid) ~ 1L,
      max(total_points) == 0 &
        sum(!is.na(waiver_bid)) == 1 & is.na(waiver_bid) ~ 0L,
      max(total_points) == 0 &
        length(unique(waiver_bid[!is.na(waiver_bid)])) == 2 ~ case_when(
          waiver_bid == max(waiver_bid, na.rm = TRUE) ~ 1L,
          waiver_bid == min(waiver_bid, na.rm = TRUE) ~ 0L
        ),
      max(total_points) == 0 &
        length(unique(waiver_bid[!is.na(waiver_bid)])) == 1 ~ NA_integer_,
      max(total_points) == 0 &
        sum(waiver_bid == max(waiver_bid, na.rm = TRUE), na.rm = TRUE) > 1 ~ NA_integer_,
      TRUE ~ 0L
    )
  ) %>%
  ungroup() %>%
  mutate(
    ppg_color = spec_color2_scale(ppg, scale_from = c(min(ppg), max(ppg)), direction = 1),
    points_color = spec_color2_scale(
      points,
      scale_from = c(min(points), max(points)),
      direction = 1
    ),
    games_played_color = spec_color2_scale(
      games_played,
      scale_from = c(min(games_played), max(games_played)),
      direction = 1
    ),
  )

all_trades_json <- jsonlite::toJSON(all_trades)

writeLines(all_trades_json, file.path(snapshots_dir, "all_trades.json"))


all_free_agents <- all_transactions %>%
  filter((type == 'free_agent' |
            type == 'waiver') &
           status == 'complete' &
           add_drop == 'add' & !is.na(player_id)
  ) %>%
  mutate(player_id = as.character(player_id)) %>%
  left_join(starter_ppg,
            by = c('manager_id' = 'manager_id', 'player_id' = 'players'))  %>%
  left_join(sleeper_players_df, by = 'player_id') %>%
  mutate(full_name = coalesce(full_name.x, full_name.y)) %>%
  select(-starts_with("full_name.")) %>%
  left_join(team_names, by = 'manager_id') %>%
  mutate(team_name = coalesce(team_name.x, team_name.y)) %>%
  select(-starts_with("team_name.")) %>%
  group_by(team_name, manager_id, full_name) %>%
  summarize(
    week = min(week),
    points = max(points),
    ppg = max(ppg),
    games_played = max(games_played)
  ) %>%
  replace(is.na(.), 0) %>%
  arrange(-points, -ppg, -games_played) %>%
  select(week,
         team_name,
         manager_id,
         games_played,
         ppg,
         points,
         full_name) %>%
  ungroup() %>%
  mutate(
    games_played_color = spec_color2_scale(
      games_played,
      scale_from = c(min(games_played), max(games_played)),
      direction = 1
    ),
    ppg_color = spec_color2_scale(ppg, scale_from = c(min(ppg), max(ppg)), direction = 1),
    points_color = spec_color2_scale(
      points,
      scale_from = c(min(points), max(points)),
      direction = 1
    ),
  )

all_free_agents_json <- jsonlite::toJSON(all_free_agents)

writeLines(all_free_agents_json, file.path(snapshots_dir, "all_free_agents.json"))


slut_meter_data <- all_transactions %>%
  filter(!is.na(player_id)) %>%
  filter(status == 'complete') %>%
  group_by(player_id) %>%
  summarize(adds = sum(add_drop == 'drop'),
            unique_owners = n_distinct(manager_id)) %>%
  arrange(-adds) %>%
  left_join(sleeper_players_df, by = 'player_id') %>%
  mutate(
    player_photo = if_else(
      grepl("^[0-9]+$", player_id),
      paste0("https://sleepercdn.com/content/nfl/players/", player_id, ".jpg"),
      paste0("https://sleepercdn.com/images/team_logos/nfl/", tolower(player_id), ".png")
    )) %>%
  mutate(
    adds_color = spec_color2_scale(adds, scale_from = c(min(adds), max(adds)), direction = 1),
    unique_owners_color = spec_color2_scale(
      unique_owners,
      scale_from = c(min(unique_owners), max(unique_owners)),
      direction = 1
    ),
  )

slut_meter_json <- jsonlite::toJSON(slut_meter_data)

writeLines(slut_meter_json, file.path(snapshots_dir, "slut_meter.json"))


possible_slots <-
  tibble(
    position = c("QB", "RB", "RB", "WR", "WR", "TE", "DEF", "K"),
    possible_slot = c("QB", "RB", "W/R/T", "WR", "W/R/T", "TE", "DEF", "K")
  )

best_ball_lineups <-
  calc_best_ball_lineups(all_players, current_week) %>%
  left_join(possible_slots, by = 'position', relationship = "many-to-many")

actual_starters <- best_ball_lineups %>%
  filter(!is.na(starter_id)) %>%
  select(full_name, points, week, possible_slot, manager_id)

best_ball_bench <- best_ball_lineups %>%
  filter(is.na(starter_id)) %>%
  left_join(
    actual_starters,
    by = c('possible_slot', 'manager_id', 'week'),
    suffix = c("", "_starter"),
    relationship = "many-to-many"
  ) %>%
  mutate(points_over_starter = points - points_starter) %>%
  group_by(full_name, manager_id, week) %>%
  arrange(desc(points_over_starter)) %>%
  slice(1) %>%
  ungroup() %>%
  filter(optimal_slot != 'BENCH') %>%
  filter(points_over_starter > 0) %>%
  arrange(desc(points_over_starter))

best_ball_bench$color_bench <-
  spec_color2_scale(best_ball_bench$points,
                    scale_from = c(0, 44.3),
                    direction = 1)

best_ball_bench$color_starter <-
  spec_color2_scale(
    best_ball_bench$points_starter,
    scale_from = c(-4, 26.28),
    direction = 1
  )

best_ball_bench$color_points_over_starter <-
  spec_color2_scale(
    best_ball_bench$points_over_starter,
    scale_from = c(0, 33.3),
    direction = -1
  )

best_ball_bench_json <- jsonlite::toJSON(best_ball_bench)

writeLines(best_ball_bench_json, file.path(snapshots_dir, "best_ball_bench.json"))


# recap_data: progressive joins matching end_of_season_recap.R exactly
leaderboard <- create_leaderboard(all_matchups, 14)
recap_data <- leaderboard %>% select(-Trend, -Rank)

recap_data <- all_matchups %>%
  group_by(manager_id, team_name, winner) %>%
  summarise(most_points = max(team_points),
            least_points = min(team_points)) %>%
  pivot_wider(names_from = winner,
              values_from = c(most_points, least_points)) %>%
  ungroup() %>%
  mutate(
    best_win_color = spec_color2_scale(
      most_points_1,
      scale_from = c(min(least_points_0), max(most_points_1)),
      direction = 1
    ),
    worst_win_color = spec_color2_scale(
      least_points_1,
      scale_from = c(min(least_points_0), max(most_points_1)),
      direction = 1
    ),
    best_loss_color = spec_color2_scale(
      most_points_0,
      scale_from = c(min(least_points_0), max(most_points_1)),
      direction = 1
    ),
    worst_loss_color = spec_color2_scale(
      least_points_0,
      scale_from = c(min(least_points_0), max(most_points_1)),
      direction = 1
    )
  ) %>%
  right_join(recap_data, by = c('team_name' = 'Team'))

recap_data <- motw_data %>%
  filter(motw == 1) %>%
  group_by(team_name) %>%
  summarise(
    motw_count = n(),
    motw_wins = sum(winner),
    motw_losses = n() - sum(winner),
    shots_dogs = sum(ifelse(winner != 1, `# of Shots`, 0))
  ) %>%
  right_join(recap_data, by = 'team_name') %>%
  replace(is.na(.), 0) %>%
  mutate(
    shots_dogs_taken_color = spec_color2_scale(
      shots_dogs,
      scale_from = c(min(shots_dogs), max(shots_dogs)),
      direction = -1
    ),
    motw_wins_color = spec_color2_scale(
      motw_wins,
      scale_from = c(min(motw_wins), max(motw_wins)),
      direction = 1
    ),
    motw_losses_color = spec_color2_scale(
      motw_losses,
      scale_from = c(min(motw_losses), max(motw_losses)),
      direction = -1
    ),
  )

recap_data <- motw_data %>%
  filter(motw == 1) %>%
  left_join(
    motw_data %>%
      filter(motw == 1) %>% select(week, matchup_id, team_points, `# of Shots`),
    by = c('matchup_id', 'week'),
    relationship = 'many-to-many'
  ) %>%
  filter(team_points.x != team_points.y) %>%
  group_by(team_name) %>%
  filter(winner == 1) %>%
  summarise(shots_dogs_given_out = sum(`# of Shots.y`)) %>%
  right_join(recap_data, by = 'team_name')  %>%
  replace(is.na(.), 0) %>%
  mutate(shots_dogs_given_out_color = spec_color2_scale(
    shots_dogs_given_out,
    scale_from = c(min(shots_dogs_given_out), max(shots_dogs_given_out)),
    direction = 1
  ),
  )

recap_data <- all_matchups %>%
  group_by(week, matchup_id) %>%
  arrange(manager_id) %>%
  mutate(other_team_points = case_when(
    manager_id == unique(manager_id)[1] ~ lead(team_points, order_by = manager_id),
    manager_id != unique(manager_id)[1] ~ lag(team_points, order_by = manager_id)
  )) %>%
  ungroup() %>%
  group_by(week) %>%
  mutate(
    pts_rank = rank(-team_points),
    other_team_pts_rank = rank(-other_team_points),
    number_of_teams = n()
  ) %>%
  ungroup() %>%
  group_by(manager_id) %>%
  select(pts_rank, other_team_pts_rank, number_of_teams) %>%
  summarise(
    worst_team_week_count = sum(pts_rank == number_of_teams),
    other_team_worst_week_count = sum(other_team_pts_rank == number_of_teams),
    best_team_week_count = sum(pts_rank == 1),
    other_team_best_week_count = sum(other_team_pts_rank == 1)
  ) %>%
  mutate(
    best_team_color = spec_color2_scale(
      best_team_week_count,
      scale_from = c(min(best_team_week_count), max(best_team_week_count)),
      direction = 1
    ),
    worst_team_color = spec_color2_scale(
      worst_team_week_count,
      scale_from = c(min(worst_team_week_count), max(worst_team_week_count)),
      direction = -1
    ),
    best_other_team_color = spec_color2_scale(
      other_team_best_week_count,
      scale_from = c(
        min(other_team_best_week_count),
        max(other_team_best_week_count)
      ),
      direction = -1
    ),
    worst_other_team_color = spec_color2_scale(
      other_team_worst_week_count,
      scale_from = c(
        min(other_team_worst_week_count),
        max(other_team_worst_week_count)
      ),
      direction = 1
    )
  ) %>%
  right_join(recap_data, by = 'manager_id')

recap_data <- all_matchups %>%
  group_by(week, matchup_id) %>%
  arrange(manager_id) %>%
  mutate(other_team_points = case_when(
    manager_id == unique(manager_id)[1] ~ lead(team_points, order_by = manager_id),
    manager_id != unique(manager_id)[1] ~ lag(team_points, order_by = manager_id)
  )) %>%
  ungroup()  %>%
  group_by(manager_id, winner) %>%
  summarize(
    pt_diff = round(mean(abs(
      team_points - other_team_points
    )), 2),
    close_games = sum(abs(team_points - other_team_points) < 10),
    blowouts = sum(abs(team_points - other_team_points) > 40)
  ) %>%
  pivot_wider(names_from = winner,
              values_from = c(close_games, blowouts, pt_diff)) %>%
  replace(is.na(.), 0) %>%
  ungroup() %>%
  mutate(
    pt_diff_1_color = spec_color2_scale(
      pt_diff_1,
      scale_from = c(min(pt_diff_1), max(pt_diff_1)),
      direction = 1
    ),
    pt_diff_0_color = spec_color2_scale(
      pt_diff_0,
      scale_from = c(min(pt_diff_0), max(pt_diff_0)),
      direction = -1
    ),
    blowouts_1_color = spec_color2_scale(
      blowouts_1,
      scale_from = c(min(blowouts_1), max(blowouts_1)),
      direction = 1
    ),
    blowouts_0_color = spec_color2_scale(
      blowouts_0,
      scale_from = c(min(blowouts_0), max(blowouts_0)),
      direction = -1
    ),
    close_games_1_color = spec_color2_scale(
      close_games_1,
      scale_from = c(min(close_games_1), max(close_games_1)),
      direction = 1
    ),
    close_games_0_color = spec_color2_scale(
      close_games_0,
      scale_from = c(min(close_games_0), max(close_games_0)),
      direction = -1
    )
  ) %>%
  right_join(recap_data, by = 'manager_id')

recap_data <- all_starters %>%
  group_by(manager_id) %>%
  summarize(distinct_starters = n_distinct(full_name)) %>%
  mutate(distinct_starters_color = spec_color2_scale(
    distinct_starters,
    scale_from = c(min(distinct_starters), max(distinct_starters)),
    direction = -1
  )) %>%
  right_join(recap_data, by = 'manager_id')

recap_data <- all_transactions %>%
  group_by(manager_id) %>%
  summarize(
    completed_waivers = sum((type == "waiver") &
                              (status == "complete")),
    total_faab_spent = sum(waiver_bid[(type == "waiver") &
                                        (status == "complete")], na.rm = TRUE),
    failed_waivers = sum((type == "waiver") &
                           (status == "failed")),
    total_faab_failed = sum(waiver_bid[(type == "waiver") &
                                         (status == "failed")], na.rm = TRUE),
    free_agent_adds = sum((type == "free_agent") &
                            (status == "complete") &
                            (add_drop == "add"),
                          na.rm = TRUE
    ),
    drops = sum(((type == "free_agent") |
                   (type == "waiver")) &
                  (status == "complete") &
                  (add_drop == "drop"), na.rm = TRUE),
    players_traded_for = sum((type == "trade") &
                               (status == "complete") &
                               (is.na(player_id)) &
                               (add_drop == "add"),
                             na.rm = TRUE
    ),
    players_traded_away = sum((type == "trade") &
                                (status == "complete") &
                                (!is.na(player_id)) &
                                (add_drop == "drop"),
                              na.rm = TRUE
    ),
    trades = n_distinct(trans_id[type == "trade" &
                                   status == "complete"], na.rm = TRUE),
    faab_traded_for = sum(waiver_bid[(type == "trade") &
                                       status == "complete" &
                                       add_drop == "add"], na.rm = TRUE),
    faab_traded_away = sum(waiver_bid[(type == "trade") &
                                        status == "complete" &
                                        add_drop == "drop"], na.rm = TRUE),
  ) %>% ungroup() %>%
  mutate(
    completed_waivers_color = spec_color2_scale(
      completed_waivers,
      scale_from = c(min(completed_waivers), max(completed_waivers)),
      direction = 1
    ),
    failed_waivers_color = spec_color2_scale(
      failed_waivers,
      scale_from = c(min(failed_waivers), max(failed_waivers)),
      direction = 1
    ),
    total_faab_spent_color = spec_color2_scale(
      total_faab_spent,
      scale_from = c(min(total_faab_spent), max(total_faab_spent)),
      direction = 1
    ),
    total_faab_failed_color = spec_color2_scale(
      total_faab_failed,
      scale_from = c(min(total_faab_failed), max(total_faab_failed)),
      direction = 1
    ),
    free_agent_adds_color = spec_color2_scale(
      free_agent_adds,
      scale_from = c(min(free_agent_adds), max(free_agent_adds)),
      direction = 1
    ),
    drops_color = spec_color2_scale(
      drops,
      scale_from = c(min(drops), max(drops)),
      direction = 1
    ),
    players_traded_for_color = spec_color2_scale(
      players_traded_for,
      scale_from = c(min(players_traded_for), max(players_traded_for)),
      direction = 1
    ),
    players_traded_away_color = spec_color2_scale(
      players_traded_away,
      scale_from = c(min(players_traded_away), max(players_traded_away)),
      direction = 1
    ),
    trades_color = spec_color2_scale(
      trades,
      scale_from = c(min(trades), max(trades)),
      direction = 1
    ),
  ) %>%
  right_join(recap_data, by = 'manager_id')

recap_data <- best_ball_bench %>%
  group_by(manager_id) %>%
  summarise(
    wrong_start_sits = n(),
    points_lost_from_wrong_start_sits = sum(points_over_starter)
  ) %>% mutate(
    wrong_start_sits_color = spec_color2_scale(
      wrong_start_sits,
      scale_from = c(min(wrong_start_sits), max(wrong_start_sits)),
      direction = -1
    ),
    points_lost_from_wrong_start_sits_color = spec_color2_scale(
      points_lost_from_wrong_start_sits,
      scale_from = c(
        min(points_lost_from_wrong_start_sits),
        max(points_lost_from_wrong_start_sits)
      ),
      direction = -1
    )
  ) %>% right_join(recap_data, by = 'manager_id')

recap_data <- all_trades %>%
  distinct(trans_id, manager_id, .keep_all = TRUE) %>%
  left_join(
    all_trades %>%
      group_by(trans_id, manager_id) %>%
      summarise(total_points_opponent = sum(points)),
    by = "trans_id",
    relationship = 'many-to-many'
  ) %>%
  filter(manager_id.x != manager_id.y) %>%
  select(manager_id = manager_id.x,
         winner,
         team_name,
         total_points,
         total_points_opponent) %>%
  group_by(manager_id, team_name) %>%
  summarise(
    trade_wins = sum(winner == 1, na.rm = TRUE),
    trade_losses = sum(winner == 0, na.rm = TRUE),
    total_trade_for_points = sum(total_points),
    total_trade_away_points = sum(total_points_opponent)
  ) %>%
  ungroup() %>%
  select(
    team_name,
    trade_wins,
    trade_losses,
    total_trade_for_points,
    total_trade_away_points
  ) %>%
  right_join(recap_data, by = 'team_name') %>%
  replace(is.na(.), 0) %>%
  mutate(
    trade_wins_color = spec_color2_scale(
      trade_wins,
      scale_from = c(min(trade_wins), max(trade_wins)),
      direction = 1
    ),
    trade_losses_color = spec_color2_scale(
      trade_losses,
      scale_from = c(min(trade_losses), max(trade_losses)),
      direction = -1
    ),
    total_trade_for_points_color = spec_color2_scale(
      total_trade_for_points,
      scale_from = c(min(total_trade_for_points), max(total_trade_for_points)),
      direction = 1
    ),
    total_trade_away_points_color = spec_color2_scale(
      total_trade_away_points,
      scale_from = c(min(total_trade_away_points), max(total_trade_away_points)),
      direction = -1
    ),
  )

recap_data <- recap_data %>%
  replace(is.na(.), 0) %>%
  arrange(team_name)

recap_data_json <- jsonlite::toJSON(recap_data)

writeLines(recap_data_json, file.path(snapshots_dir, "recap_data.json"))

message("end_of_season_recap.R golden files written.")
message("All snapshots written to ", snapshots_dir)
