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
league_id <- 1253779168802377728 # main league
sleeper_players_csv <- "sleeper_players.csv"
NFL_state <- get_sport_state('nfl')
current_week <- 17 #NFL_state$display_week
current_year <- 25

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
  week_1_matchup_id = 1,
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

# Get transactions and player/team lookups used throughout the script
all_transactions <- get_all_transaction_data(
  league_id = as.character(league_id),
  max_week = current_week
)

sleeper_players_data <- read.csv(sleeper_players_csv) %>%
  select(player_id, full_name) %>%
  mutate(full_name = coalesce(full_name, player_id))

team_names <- all_players %>%
  group_by(manager_id) %>%
  summarise(team_name = unique(team_name))


##### wrapped data #####

recap_data <- build_recap_data(
  all_matchups,
  motw_data,
  all_transactions,
  all_players,
  sleeper_players      = sleeper_players_data,
  regular_season_weeks = 14L
)

recap_data_json <- jsonlite::toJSON(recap_data)

clipr::write_clip(recap_data_json)


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

clipr::write_clip(best_ball_bench_json)


# draft info
draft_id <- "1253779168810774528"

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

# fill NA full_name values with values from sleeper_players_data
draft_w_totals <-
  left_join(draft_w_totals, sleeper_players_data, by = 'player_id') %>%
  mutate(full_name = coalesce(full_name.x, full_name.y)) %>%
  select(-starts_with("full_name."))

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
  left_join(sleeper_players_data, by = 'player_id') %>%
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
  evaluate_trades() %>%
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
  left_join(sleeper_players_data, by = 'player_id') %>%
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
    name = "The Barkley Brawlers",
    photo = "https://sleepercdn.com/uploads/f6acb0a5169c52b59e2272361933c805.jpg",
    award = "League Champion",
    value = "Defeated Uncle Rico Went Pro in the championship",
    description = "Congrats to The Barkley Brawlers on their first championship"
  ),
  list(
    name = "Bye Week Curious",
    photo = "https://sleepercdn.com/uploads/3f2ce411ee3f87936b4648a58e78ee85.jpg",
    award = "Sucks At Fantasy Football",
    value = "4-10 Record",
    description = "They are tied for the most losses in league history"
  ),
  list(
    name = "Chris Olave",
    photo = "https://sleepercdn.com/content/nfl/players/8144.jpg",
    award = "Finals MVP",
    value = "25.9 points",
    description = "Olave put up 25.9 in the finals and was instrumental in this team making it there"
  ),
  list(
    name = "Christian McCaffrey",
    photo = "https://sleepercdn.com/content/nfl/players/4034.jpg",
    award = "League MVP",
    value = "376.8 points - 25.1 PPG",
    description = "30 more points and 2.3 PPG more than the next highest player"
  ),
  list(
    name = "BBCU",
    photo = "https://sleepercdn.com/uploads/07e4a08de2c45ee92d7a5498e5234fbb",
    award = "Offensive Team of The Year",
    value = "1852.36 PF",
    description = "Only team ever with 1800+ points for in a season"
  ),
  list(
    name = "The Barkley Brawlers",
    photo = "https://sleepercdn.com/uploads/f6acb0a5169c52b59e2272361933c805.jpg",
    award = "Defensive Team of The Year",
    value = "1425.94 PA",
    description = "This is their second time winning this award, won in 2023"
  ),
  list(
    name = "The Barkley Brawlers",
    photo = "https://sleepercdn.com/uploads/f6acb0a5169c52b59e2272361933c805.jpg",
    award = "Best MotW Manager",
    value = "3-1 record",
    description = "16:3 Hot Dog/Shot Given:Taken Ratio"
  ),
  list(
    name = "2nd Half Team",
    photo = "https://sleepercdn.com/uploads/9c3413a27399623b1a44d5a1d9149edd.jpg",
    award = "Worst MotW Manager",
    value = "0-2 record",
    description = "0:10 Hot Dog/Shot Given:Taken Ratio"
  ),
  list(
    name = "Jaxon Smith-Njigba",
    photo = "https://sleepercdn.com/content/nfl/players/9488.jpg",
    award = "Best Draft Pick",
    value = "327.2 points - 21.81 PPG",
    description = "Pick 3.02 by BBCU, finished as our leagues WR2 in total points"
  ),
  list(
    name = "Justin Jefferson",
    photo = "https://sleepercdn.com/content/nfl/players/6794.jpg",
    award = "Worst Draft Pick",
    value = "178.9 points - 11.93 ppg",
    description = "Pick 1.08 by Worse Management, 4 picks ahead of WR1 Puka Nacua"
  ),
  list(
    name = "Michael Wilson",
    photo = "https://sleepercdn.com/content/nfl/players/10232.jpg",
    award = "Best Waiver Pickup",
    value = "115.1 points - 19.18 ppg",
    description = "The Barkley Brawlers spent $21 FAAB - averaged 20 PPG in weeks 16-17"
  ),
  list(
    name = "Sean Tucker",
    photo = "https://sleepercdn.com/content/nfl/players/9506.jpg",
    award = "Worst Waiver Pickup",
    value = "5.6 points - 5.6 ppg",
    description = "Costco Guys spent $125 FAAB - started him once for 5.6 points"
  ),
  list(
    name = "BBCU",
    photo = "https://sleepercdn.com/uploads/07e4a08de2c45ee92d7a5498e5234fbb",
    award = "Best Start/Sits",
    value = "22 incorrect decisions - 208.98 points lost",
    description = "Having an elite starting lineup helps make decisions easy"
  ),
  list(
    name = "The Barkley Brawlers",
    photo = "https://sleepercdn.com/uploads/f6acb0a5169c52b59e2272361933c805.jpg",
    award = "Worst Start/Sits",
    value = "40 incorrect decisions - 432.6 points lost",
    description = "2nd straight year winning this award, but they won the chip anyway"
  ),
  list(
    name = "The Barkley Brawlers",
    photo = "https://sleepercdn.com/uploads/f6acb0a5169c52b59e2272361933c805.jpg",
    award = "Best Trader",
    value = "1-0 record in trades - Netted 76 points",
    description = "Won the championship because of this one trade"
  ),
  list(
    name = "BBCU",
    photo = "https://sleepercdn.com/uploads/07e4a08de2c45ee92d7a5498e5234fbb",
    award = "Worst Trader",
    value = "1-3 record in trades - Netted -76 points",
    description = "Back-to-back-to-back winner of this award, would've won the championship if they didn't trade"
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
  left_join(sleeper_players_data, by = 'player_id') %>%
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
