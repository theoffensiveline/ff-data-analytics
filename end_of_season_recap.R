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
league_id <- 996445270105714688 # main league
sleeper_players_csv <- "sleeper_players.csv"
NFL_state <- get_sport_state('nfl')
current_week <- 17 #NFL_state$display_week
current_year <- 23

# team photos
team_photos <- get_team_photos(league_id)

# get all player data for each matchup
all_players <- get_all_matchups_data(current_week,
                                     league_id,
                                     sleeper_players_csv)

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


all_starters <- all_players[!is.na(all_players$starter_id),]

all_bench <- all_players[is.na(all_players$starter_id),]

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
                                       scale_from = c(min(starter_ppg$ppg),
                                                      max(starter_ppg$ppg)),
                                       direction = 1)

starter_json <-
  jsonlite::toJSON(starter_ppg %>% select(team_name, full_name, position, games_played, ppg, color))

clipr::write_clip(starter_json)



# number of K and DEF played and team ppg
kicker_def <- all_starters %>%
  filter(position == c('K', 'DEF')) %>%
  group_by(team_name, position) %>%
  summarize(count = n_distinct(full_name),
            ppg = round(mean(points), 2)) %>%
  ungroup() %>%
  arrange(position, team_name) %>%
  left_join(team_photos, by = "team_name")

kicker_def_json <- jsonlite::toJSON(kicker_def)

clipr::write_clip(kicker_def_json)

# points against Jake
jake_opps <- all_matchups %>%
  filter(manager_id != 5) %>%
  semi_join(all_matchups %>% filter(manager_id == 5),
            by = c('week', 'matchup_id')) %>%
  arrange(desc(team_points)) %>%
  select(week, team_name, team_points, winner) %>%
  mutate(
    points_color = spec_color2_scale(
      team_points,
      scale_from = c(min(team_points),
                     max(team_points)),
      direction = 1
    ),
    winner_color = spec_color2_scale(
      winner,
      scale_from = c(min(winner),
                     max(winner)),
      direction = 1
    ),
  )

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

# points against Alec
alec_opps <- all_matchups %>%
  filter(manager_id != 3) %>%
  semi_join(all_matchups %>% filter(manager_id == 3),
            by = c('week', 'matchup_id')) %>%
  arrange(desc(team_points)) %>%
  select(week, team_name, team_points, winner) %>%
  mutate(
    points_color = spec_color2_scale(
      team_points,
      scale_from = c(min(team_points),
                     max(team_points)),
      direction = 1
    ),
    winner_color = spec_color2_scale(
      winner,
      scale_from = c(min(winner),
                     max(winner)),
      direction = 1
    ),
  )

alec_opps_json <- jsonlite::toJSON(alec_opps)

clipr::write_clip(jake_opps_json)

# points against Devan
devan_opps <- all_matchups %>%
  filter(manager_id != 9) %>%
  semi_join(all_matchups %>% filter(manager_id == 9),
            by = c('week', 'matchup_id')) %>%
  arrange(desc(team_points))

# points against Trevor
trevor_opps <- all_matchups %>%
  filter(manager_id != 2) %>%
  semi_join(all_matchups %>% filter(manager_id == 2),
            by = c('week', 'matchup_id')) %>%
  arrange(desc(team_points))





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
      scale_from = c(min(least_points_0),
                     max(most_points_1)),
      direction = 1
    ),
    worst_win_color = spec_color2_scale(
      least_points_1,
      scale_from = c(min(least_points_0),
                     max(most_points_1)),
      direction = 1
    ),
    best_loss_color = spec_color2_scale(
      most_points_0,
      scale_from = c(min(least_points_0),
                     max(most_points_1)),
      direction = 1
    ),
    worst_loss_color = spec_color2_scale(
      least_points_0,
      scale_from = c(min(least_points_0),
                     max(most_points_1)),
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
      scale_from = c(min(shots_dogs),
                     max(shots_dogs)),
      direction = -1
    ),
    motw_wins_color = spec_color2_scale(
      motw_wins,
      scale_from = c(min(motw_wins),
                     max(motw_wins)),
      direction = 1
    ),
    motw_losses_color = spec_color2_scale(
      motw_losses,
      scale_from = c(min(motw_losses),
                     max(motw_losses)),
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
    scale_from = c(min(shots_dogs_given_out),
                   max(shots_dogs_given_out)),
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
      scale_from = c(min(best_team_week_count),
                     max(best_team_week_count)),
      direction = 1
    ),
    worst_team_color = spec_color2_scale(
      worst_team_week_count,
      scale_from = c(min(worst_team_week_count),
                     max(worst_team_week_count)),
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
      scale_from = c(min(pt_diff_1),
                     max(pt_diff_1)),
      direction = 1
    ),
    pt_diff_0_color = spec_color2_scale(
      pt_diff_0,
      scale_from = c(min(pt_diff_0),
                     max(pt_diff_0)),
      direction = -1
    ),
    blowouts_1_color = spec_color2_scale(
      blowouts_1,
      scale_from = c(min(blowouts_1),
                     max(blowouts_1)),
      direction = 1
    ),
    blowouts_0_color = spec_color2_scale(
      blowouts_0,
      scale_from = c(min(blowouts_0),
                     max(blowouts_0)),
      direction = -1
    ),
    close_games_1_color = spec_color2_scale(
      close_games_1,
      scale_from = c(min(close_games_1),
                     max(close_games_1)),
      direction = 1
    ),
    close_games_0_color = spec_color2_scale(
      close_games_0,
      scale_from = c(min(close_games_0),
                     max(close_games_0)),
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
    scale_from = c(min(distinct_starters),
                   max(distinct_starters)),
    direction = -1
  )) %>%
  right_join(recap_data, by = 'manager_id')

# transactions
#all_trans <-
#  get_all_transaction_data(max_week = current_week, league_id = league_id)


recap_data <- all_trans %>%
  group_by(manager_id) %>%
  summarize(
    completed_waivers = sum((type == "waiver") &
                              (status == "complete")),
    failed_waivers = sum((type == "waiver") &
                           (status == "failed")),
    free_agent_adds = sum((type == "free_agent") &
                            (status == "complete") &
                            (add_drop == "add")
    ),
    drops = sum(((type == "free_agent") |
                   (type == "waiver")) &
                  (status == "complete") & (add_drop == "add")),
    players_traded_for = sum((type == "trade") &
                               (status == "complete") &
                               (add_drop == "add")),
    players_traded_away = sum((type == "trade") &
                                (status == "complete") &
                                (add_drop == "drop")),
    trades = n_distinct(trans_id[type == "trade" &
                                   status == "complete"])
  ) %>% ungroup() %>%
  mutate(
    completed_waivers_color = spec_color2_scale(
      completed_waivers,
      scale_from = c(min(completed_waivers),
                     max(completed_waivers)),
      direction = 1
    ),
    failed_waivers_color = spec_color2_scale(
      failed_waivers,
      scale_from = c(min(failed_waivers),
                     max(failed_waivers)),
      direction = 1
    ),
    free_agent_adds_color = spec_color2_scale(
      free_agent_adds,
      scale_from = c(min(free_agent_adds),
                     max(free_agent_adds)),
      direction = 1
    ),
    drops_color = spec_color2_scale(
      drops,
      scale_from = c(min(drops),
                     max(drops)),
      direction = 1
    ),
    players_traded_for_color = spec_color2_scale(
      players_traded_for,
      scale_from = c(min(players_traded_for),
                     max(players_traded_for)),
      direction = 1
    ),
    players_traded_away_color = spec_color2_scale(
      players_traded_away,
      scale_from = c(min(players_traded_away),
                     max(players_traded_away)),
      direction = 1
    ),
    trades_color = spec_color2_scale(
      trades,
      scale_from = c(min(trades),
                     max(trades)),
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

best_ball_starters <- best_ball_lineups %>%
  filter(optimal_slot != 'BENCH') %>%
  select(full_name, points, week, possible_slot, manager_id)

best_ball_bench <- best_ball_lineups %>%
  filter(is.na(starter_id)) %>%
  left_join(
    best_ball_starters,
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
                    scale_from = c(0, 52),
                    direction = 1)

best_ball_bench$color_starter <-
  spec_color2_scale(
    best_ball_bench$points_starter,
    scale_from = c(0, 52),
    direction = 1
  )

best_ball_bench_json <- jsonlite::toJSON(best_ball_bench)

#clipr::write_clip(best_ball_bench_json)

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



# draft info
draft_id <- 996445270843895808

#draft_data <- get_draft_picks(draft_id = draft_id)

draft_picks <- draft_data %>%
  mutate(position = metadata$position) %>%
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
  summarise(points = sum(points),
            ppg = sum(points) / n()) %>%
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

#sleeper_players_csv <- read.csv("sleeper_players.csv") %>%
#  select(player_id, full_name)

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
all_trades <- all_trans %>%
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
  select(week,
         trans_id,
         team_name,
         manager_id,
         games_played,
         ppg,
         points,
         full_name) %>%
  mutate(total_points = ave(points, trans_id, team_name, FUN = sum)) %>%
  group_by(trans_id) %>%
  mutate(winner = as.integer(total_points == max(total_points))) %>%
  ungroup() %>%
  mutate(
    ppg_color = spec_color2_scale(ppg,
                                  scale_from = c(min(ppg),
                                                 max(ppg)),
                                  direction = 1),
    points_color = spec_color2_scale(
      points,
      scale_from = c(min(points),
                     max(points)),
      direction = 1
    ),
    games_played_color = spec_color2_scale(
      games_played,
      scale_from = c(min(games_played),
                     max(games_played)),
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
    trade_wins = sum(winner == 1),
    trade_losses = sum(winner == 0),
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
      scale_from = c(min(trade_wins),
                     max(trade_wins)),
      direction = 1
    ),
    trade_losses_color = spec_color2_scale(
      trade_losses,
      scale_from = c(min(trade_losses),
                     max(trade_losses)),
      direction = -1
    ),
    total_trade_for_points_color = spec_color2_scale(
      total_trade_for_points,
      scale_from = c(min(total_trade_for_points),
                     max(total_trade_for_points)),
      direction = 1
    ),
    total_trade_away_points_color = spec_color2_scale(
      total_trade_away_points,
      scale_from = c(min(total_trade_away_points),
                     max(total_trade_away_points)),
      direction = -1
    ),
  )


# good/bad pickups
all_free_agents <- all_trans %>%
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
  arrange(-points,-ppg,-games_played) %>%
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
      scale_from = c(min(games_played),
                     max(games_played)),
      direction = 1
    ),
    ppg_color = spec_color2_scale(ppg,
                                  scale_from = c(min(ppg),
                                                 max(ppg)),
                                  direction = 1),
    points_color = spec_color2_scale(
      points,
      scale_from = c(min(points),
                     max(points)),
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
    name = "Njigba’s in Paris",
    photo = "https://sleepercdn.com/uploads/bb838332d8a662620f4b0629fc227c3f.jpg",
    award = "League Champion",
    value = "Defeated Just Joshin in the championship",
    description = "Congrats to Njigba's in Paris on a hard fought victory"
  ),
  list(
    name = "E.T.N Phone Home",
    photo = "https://sleepercdn.com/uploads/3f2ce411ee3f87936b4648a58e78ee85.jpg",
    award = "Sucks At Fantasy Football",
    value = "4-10 Record",
    description = "They lost 6 straight games to earn the punishment"
  ),
  list(
    name = "CeeDee Lamb",
    photo = "https://sleepercdn.com/content/nfl/players/6786.jpg",
    award = "Finals MVP",
    value = "40.20 points",
    description = "CeeDee dominated all season long"
  ),
  list(
    name = "CeeDee Lamb",
    photo = "https://sleepercdn.com/content/nfl/players/6786.jpg",
    award = "League MVP",
    value = "349.10 points - 23.27 ppg",
    description = "Would've been CMC if First Down Syndrome made the playoffs"
  ),
  list(
    name = "Hurt Thuggins & the boys",
    photo = "https://sleepercdn.com/uploads/4a648c2339b3183b3287cbff2364efc3.jpg",
    award = "Offensive Team of The Year",
    value = "1756.52 PF (3rd Most)",
    description = "They overcame a tough schedule to finish 9-5"
  ),
  list(
    name = "Just Joshin",
    photo = "https://sleepercdn.com/uploads/f266ea37455022ae9a07a2c6644ace5e.jpg",
    award = "Defensive Team of The Year",
    value = "1397.78 PA",
    description = "This might go down as the best defensive season ever"
  ),
  list(
    name = "Travis Swift",
    photo = "https://sleepercdn.com/uploads/bbec1499b536f4ec2e79a10fb4765a38.jpg",
    award = "Best MotW Manager",
    value = "3-1 record",
    description = "16:1 Hot Dog/Shot Given:Taken Ratio"
  ),
  list(
    name = "Tim's Team",
    photo = "https://sleepercdn.com/uploads/7dbe04325fa93fc3a7577bf21b0a4ebe.jpg",
    award = "Worst MotW Manager",
    value = "0-3 record",
    description = "0:16 Hot Dog/Shot Given:Taken Ratio"
  ),
  list(
    name = "Puka Nacua",
    photo = "https://sleepercdn.com/content/nfl/players/9493.jpg",
    award = "Best Waiver Pickup",
    value = "213.7 points - 16.44 ppg",
    description = "Hurt Thuggins & the boys picked him up after week 1"
  ),
  list(
    name = "Deshaun Watson",
    photo = "https://sleepercdn.com/content/nfl/players/4017.jpg",
    award = "Worst Waiver Pickup",
    value = "-1.8 points",
    description = "Travis Swift started him week 7 - only non-K/DEF started this season to go negative"
  ),
  list(
    name = "Njigba’s in Paris",
    photo = "https://sleepercdn.com/uploads/bb838332d8a662620f4b0629fc227c3f.jpg",
    award = "Best Start/Sits",
    value = "45.7 points lost",
    description = "Coincidence this is our champion?"
  ),
  list(
    name = "E.T.N Phone Home",
    photo = "https://sleepercdn.com/uploads/3f2ce411ee3f87936b4648a58e78ee85.jpg",
    award = "Worst Start/Sits",
    value = "212.6 points lost",
    description = "Coincidence this is our loser?"
  ),
  list(
    name = "WalterFix",
    photo = "https://sleepercdn.com/uploads/b3c7d23a5b2e92457a0070891cb98326",
    award = "Best Trader",
    value = "3-2 record in trades",
    description = "Shoutout WalterPicks Trade Analyzer - traded for 549.26 points to help avoid last place"
  ),
  list(
    name = "The Werbenjägermanjensens",
    photo = "https://sleepercdn.com/uploads/0a4e3f97a2fc3e992a48ada89ccb76a2",
    award = "Worst Trader",
    value = "0-3 record in trades",
    description = "This doesn't even count trading away their 2nd round draft pick"
  )
)

recap_awards_json <- jsonlite::toJSON(recap_awards)

clipr::write_clip(recap_awards_json)
