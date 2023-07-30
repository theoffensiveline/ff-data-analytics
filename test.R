library(tidyverse)
library(sleeperapi)

league_id <- 968890022248103936
current_week <- 1

get_league(league_id)

matchups <- get_matchups(league_id, current_week)

