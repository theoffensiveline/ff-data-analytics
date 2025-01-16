##### Data prep #####
# Load necessary libraries
library(tidyverse)
library(sleeperapi)
library(dplyr)
library(offensiveline)
library(ggplot2)
library(reshape2)
library(kableExtra)
library(paletteer)
library(scales)
library(readr)
library(ggrepel)
library(png)
library(ggimage)
library(ggtext)
library(reticulate)
library(rcartocolor)

# Define the league ID, sleeper players file, and get NFL state
# league_id <- 968890022248103936 # Walter league
league_id <- 1124831356770058240 # main league
sleeper_players_csv <- "sleeper_players.csv"
NFL_state <- get_sport_state('nfl')
current_week <- 17 #NFL_state$display_week
current_year <- 24

# team photos
team_photos <- get_team_photos(league_id)

# get all player data for each matchup
all_players <- get_all_matchups_data(current_week, league_id, sleeper_players_csv)

# get rid of NA
all_players <- all_players[complete.cases(all_players$matchup_id), ]

# summarize to the team level
all_matchups <- get_team_matchups(player_data = all_players)

# get rid of NA
all_matchups <-
  all_matchups[complete.cases(all_matchups$matchup_id), ]

# get MotW data
motw_data <- add_motw_to_matchups(
  matchup_data = all_matchups,
  week_1_matchup_id = 2,
  max_week = current_week,
  player_data = all_players
)


all_starters <- all_players[!is.na(all_players$starter_id), ]

all_bench <- all_players[is.na(all_players$starter_id), ]

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

clipr::write_clip(starter_json)



# number of K and DEF played and team ppg
kicker_def <- all_starters %>%
  filter(position == c('K', 'DEF')) %>%
  group_by(team_name, position) %>%
  summarize(count = n_distinct(full_name), ppg = round(mean(points), 2)) %>%
  ungroup() %>%
  arrange(position, team_name) %>%
  left_join(team_photos, by = "team_name")

kicker_def_json <- jsonlite::toJSON(kicker_def)

clipr::write_clip(kicker_def_json)

# Get data for all managers with opponent information
all_managers_opps <- all_matchups %>%
  left_join(all_matchups,
            by = c('week', 'matchup_id'),
            relationship = 'many-to-many') %>%
  filter(manager_id.x != manager_id.y) %>%
  mutate(
    points_color = spec_color2_scale(
      team_points.y,
      scale_from = c(min(team_points.y), max(team_points.y)),
      direction = 1
    ),
    winner_color = spec_color2_scale(
      winner.y,
      scale_from = c(min(winner.y), max(winner.y)),
      direction = 1
    )
  ) %>%
  arrange(-team_points.y) %>%
  rename(team_name = team_name.y)


all_managers_opps_json <- jsonlite::toJSON(all_managers_opps)

clipr::write_clip(all_managers_opps_json)


##### wrapped data #####
# regular season record
leaderboard <- create_leaderboard(all_matchups, 14)

recap_data <- leaderboard %>% select(-Trend, -Rank)

# best/worst wins/losses
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


# motw count
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

# motw dogs given out
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

# top team
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

# average win/loss differential & close games/blowout counts
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

# players started

recap_data <- all_starters %>%
  group_by(manager_id) %>%
  summarize(distinct_starters = n_distinct(full_name)) %>%
  mutate(distinct_starters_color = spec_color2_scale(
    distinct_starters,
    scale_from = c(min(distinct_starters), max(distinct_starters)),
    direction = -1
  )) %>%
  right_join(recap_data, by = 'manager_id')

# transactions
all_transactions <- get_all_transaction_data(league_id = as.character(league_id), max_week = current_week)

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


# bench stars
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
                    scale_from = c(0, 51.5),
                    direction = 1)

best_ball_bench$color_starter <-
  spec_color2_scale(
    best_ball_bench$points_starter,
    scale_from = c(-2, 24.18),
    direction = 1
  )

best_ball_bench$color_points_over_starter <-
  spec_color2_scale(
    best_ball_bench$points_over_starter,
    scale_from = c(0, 51.5),
    direction = -1
  )

best_ball_bench_json <- jsonlite::toJSON(best_ball_bench)

clipr::write_clip(best_ball_bench_json)

test <- best_ball_bench %>%
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



# draft info
draft_id <- "1124831356770058241"

draft_data <- get_draft_picks(draft_id = draft_id)

draft_picks <- draft_data %>%
  select(
    pick_no = pick_no,
    manager_id = roster_id,
    player_id = player_id,
    position = position
  ) %>%
  group_by(position) %>%
  mutate(pos_pick = rank(pick_no))

# player totals
started_player_totals <- all_starters %>%
  group_by(players, full_name, position) %>%
  summarise(points = sum(points), ppg = sum(points) / n()) %>%
  ungroup() %>%
  group_by(position) %>%
  mutate(pts_pos_rank = rank(-points),
         ppg_pos_rank = rank(-ppg))

draft_w_totals <- left_join(x = draft_picks,
                            y = started_player_totals,
                            by = c('player_id' = 'players')) %>%
  mutate(
    diff_pts_rank_draft = pos_pick - pts_pos_rank,
    diff_ppg_rank_draft = pos_pick - ppg_pos_rank
  )

sleeper_players_csv <- read.csv("sleeper_players.csv") %>%
  select(player_id, full_name) %>%
  mutate(full_name = coalesce(full_name, player_id))

# fill NA full_name values in draft_w_totals with values from sleeper_players_csv
draft_w_totals <-
  left_join(draft_w_totals, sleeper_players_csv, by = 'player_id') %>%
  mutate(full_name = coalesce(full_name.x, full_name.y)) %>%
  select(-starts_with("full_name."))

team_names <- all_players %>%
  group_by(manager_id) %>%
  summarise(team_name = unique(team_name))

# join in team names
draft_w_totals <-
  left_join(draft_w_totals, team_names, by = 'manager_id') %>%
  select(
    pick_no,
    position = position.x,
    pos_pick,
    points,
    ppg,
    pts_pos_rank,
    ppg_pos_rank,
    diff_pts_rank_draft,
    diff_ppg_rank_draft,
    full_name,
    team_name
  )

# trade evaluation
all_trades <- all_transactions %>%
  filter(type == 'trade' & status == 'complete') %>%
  mutate(player_id = as.character(player_id)) %>%
  left_join(starter_ppg,
            by = c('manager_id' = 'manager_id', 'player_id' = 'players')) %>%
  left_join(sleeper_players_csv, by = 'player_id') %>%
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
      max(total_points) > 0 &
        total_points == max(total_points) ~ 1L,
      # Team scored maximum points
      
      max(total_points) == 0 &
        n_distinct(waiver_bid[!is.na(waiver_bid)]) == 1 &
        !is.na(waiver_bid) ~ 1L,
      # No points but only one team bid
      
      max(total_points) == 0 &
        n_distinct(waiver_bid[!is.na(waiver_bid)]) == 1 &
        is.na(waiver_bid) ~ 0L,
      # No points, one team bid but this team didn't
      
      max(total_points) == 0 &
        length(waiver_bid[!is.na(waiver_bid)]) > 1 &
        sum(waiver_bid == max(waiver_bid, na.rm = TRUE), na.rm = TRUE) > 1 ~ NA_integer_,
      # No points and tied bids (multiple rows with the max bid)
      
      max(total_points) == 0 &
        waiver_bid == max(waiver_bid, na.rm = TRUE) &
        sum(waiver_bid == max(waiver_bid, na.rm = TRUE), na.rm = TRUE) == 1 ~ 1L,
      # No points but unique highest bid (only one row with the max bid)
      
      TRUE ~ 0L  # All other cases
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

clipr::write_clip(all_trades_json)

# trading record W/L
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


# good/bad pickups
all_free_agents <- all_transactions %>%
  filter((type == 'free_agent' |
            type == 'waiver') &
           status == 'complete' &
           add_drop == 'add' & !is.na(player_id)
  ) %>%
  mutate(player_id = as.character(player_id)) %>%
  left_join(starter_ppg,
            by = c('manager_id' = 'manager_id', 'player_id' = 'players'))  %>%
  left_join(sleeper_players_csv, by = 'player_id') %>%
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

clipr::write_clip(all_free_agents_json)

# recap data output
recap_data <- recap_data %>%
  replace(is.na(.), 0) %>%
  arrange(team_name)

recap_data_json <- jsonlite::toJSON(recap_data)

clipr::write_clip(recap_data_json)




# end of season awards

recap_awards <- data.frame(
  name = character(),
  photo = character(),
  award = character(),
  value = character(),
  description = character(),
  stringsAsFactors = FALSE  # To prevent automatic conversion of strings to factors
)

recap_awards <- rbind(
  recap_awards,
  list(
    name = "Pink Pony Kupp",
    photo = "https://sleepercdn.com/uploads/3cdeac6447a59d22994c88379ba9bce8",
    award = "League Champion",
    value = "Defeated Costo Guys in the championship",
    description = "Congrats to Pink Pony Kupp on a hard fought victory"
  ),
  list(
    name = "League Loser Prophecy",
    photo = "https://sleepercdn.com/uploads/a245e54037dc499725dcab05fb9ebbad.jpg",
    award = "Sucks At Fantasy Football",
    value = "5-9 Record",
    description = "They have completed 50% of this league's last place punishments"
  ),
  list(
    name = "Joe Burrow",
    photo = "https://sleepercdn.com/content/nfl/players/6770.jpg",
    award = "Finals MVP",
    value = "36.98 points",
    description = "Burrow put up nearly 37 points in the championship win, not bad for a 9th round pick"
  ),
  list(
    name = "Lamar Jackson",
    photo = "https://sleepercdn.com/content/nfl/players/4881.jpg",
    award = "League MVP",
    value = "407.40 points - 25.46 ppg",
    description = "Should this have been Josh Allen? Probably, but we won't get into that debate here"
  ),
  list(
    name = "Just Joshin pt. 2",
    photo = "https://sleepercdn.com/uploads/f266ea37455022ae9a07a2c6644ace5e.jpg",
    award = "Offensive Team of The Year",
    value = "1772.88 PF",
    description = "Scored the most points to earn the #1 seed"
  ),
  list(
    name = "Costo Guys",
    photo = "https://sleepercdn.com/uploads/ed33dc321d46f8acd298bf79617d512e.jpg",
    award = "Defensive Team of The Year",
    value = "1527.96 PA",
    description = "This is 130 more PA than last year's winner, Just Joshin"
  ),
  list(
    name = "Youngster Joey",
    photo = "https://sleepercdn.com/uploads/006bf4340223cac66abcf1f51402875e",
    award = "Best MotW Manager",
    value = "3-1 record",
    description = "7:1 Hot Dog/Shot Given:Taken Ratio"
  ),
  list(
    name = "Twin Bowers",
    photo = "https://sleepercdn.com/uploads/1bd211e3f19b67fc2cad3efb8ca5544a.jpg",
    award = "Worst MotW Manager",
    value = "0-2 record",
    description = "0:10 Hot Dog/Shot Given:Taken Ratio"
  ),
  list(
    name = "Brock Bowers",
    photo = "https://sleepercdn.com/content/nfl/players/11604.jpg",
    award = "Best Draft Pick",
    value = "221.3 points - 15.81 ppg",
    description = "Pick 9.11 by Twin Bowers, finished as our leagues TE1 in total points"
  ),
  list(
    name = "Patrick Mahomes",
    photo = "https://sleepercdn.com/content/nfl/players/4046.jpg",
    award = "Worst Draft Pick",
    value = "238.84 points - 17.06 ppg",
    description = "Pick 3.12 by Giving Me a Chubb, 7 picks ahead of League MVP Lamar Jackson"
  ),
  list(
    name = "Bucky Irving",
    photo = "https://sleepercdn.com/content/nfl/players/11584.jpg",
    award = "Best Waiver Pickup",
    value = "122.8 points - 17.54 ppg",
    description = "Pink Pony Kupp spent $102 FAAB - scored 23 and 16.4 points in playoff games"
  ),
  list(
    name = "Jaleel McLaughlin",
    photo = "https://sleepercdn.com/content/nfl/players/11439.jpg",
    award = "Worst Waiver Pickup",
    value = "33.1 points - 4.73 ppg",
    description = "Lowest ppg of any player started more than 3 times"
  ),
  list(
    name = "First Down Syndrome",
    photo = "https://sleepercdn.com/uploads/8e97327c2904ce15283e72e27977734b.jpg",
    award = "Best Start/Sits",
    value = "19 incorrect decisions - 179.3 points lost",
    description = "Clutched up when it counted to avoid last place"
  ),
  list(
    name = "Just Joshin pt. 2",
    photo = "https://sleepercdn.com/uploads/f266ea37455022ae9a07a2c6644ace5e.jpg",
    award = "Worst Start/Sits",
    value = "31 incorrect decisions - 334.32 points lost",
    description = "Could've won the championship if not for mismanagement"
  ),
  list(
    name = "Kirk Thuggins & The Boys",
    photo = "https://sleepercdn.com/uploads/4a648c2339b3183b3287cbff2364efc3.jpg",
    award = "Best Trader",
    value = "3-0 record in trades - Netted 226 points",
    description = "'Fleeced' to get Jonathan Taylor and Baker Mayfield"
  ),
  list(
    name = "Youngster Joey",
    photo = "https://sleepercdn.com/uploads/006bf4340223cac66abcf1f51402875e",
    award = "Worst Trader",
    value = "3-3 record in trades - Netted -178.7 points",
    description = "Back-to-back winner of this award"
  )
)

recap_awards_json <- jsonlite::toJSON(recap_awards)

clipr::write_clip(recap_awards_json)










###### slut meter ######
slut_meter_data <- all_transactions %>%
  filter(!is.na(player_id)) %>%
  filter(status == 'complete') %>%
  group_by(player_id) %>%
  summarize(adds = sum(add_drop == 'drop'),
            unique_owners = n_distinct(manager_id)) %>%
  arrange(-adds) %>%
  left_join(sleeper_players_csv, by = 'player_id') %>%
  mutate(# Construct player photo URLs, vectorized
    player_photo = if_else(
      grepl("^[0-9]+$", player_id),
      # Check if player_id is all digits
      paste0(
        "https://sleepercdn.com/content/nfl/players/",
        player_id,
        ".jpg"
      ),
      paste0(
        "https://sleepercdn.com/images/team_logos/nfl/",
        tolower(player_id),
        ".png"
      )
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

clipr::write_clip(slut_meter_json)
