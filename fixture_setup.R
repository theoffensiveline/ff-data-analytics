library(sleeperapi)
library(offensiveline)

league_id           <- 1253779168802377728
sleeper_players_csv <- "sleeper_players.csv"

all_players_full      <- get_all_matchups_data(17, league_id, sleeper_players_csv)
all_matchups_full     <- get_team_matchups(all_players_full)
motw_full             <- add_motw_to_matchups(all_matchups_full, week_1_matchup_id = 1,
                                              max_week = 17, all_players_full)
all_transactions_full <- get_all_transaction_data(as.character(league_id), max_week = 17)
team_photos_full      <- get_team_photos(league_id)

dir.create("offensiveline/tests/testthat/fixtures", recursive = TRUE, showWarnings = FALSE)

saveRDS(all_matchups_full,     "offensiveline/tests/testthat/fixtures/matchups_full.rds")
saveRDS(all_players_full,      "offensiveline/tests/testthat/fixtures/players_full.rds")
saveRDS(motw_full,             "offensiveline/tests/testthat/fixtures/motw_full.rds")
saveRDS(all_transactions_full, "offensiveline/tests/testthat/fixtures/transactions_full.rds")
saveRDS(team_photos_full,      "offensiveline/tests/testthat/fixtures/team_photos.rds")

saveRDS(all_matchups_full[all_matchups_full$week <= 3, ],
        "offensiveline/tests/testthat/fixtures/matchups_w3.rds")
saveRDS(all_players_full[all_players_full$week <= 3, ],
        "offensiveline/tests/testthat/fixtures/players_w3.rds")
saveRDS(motw_full[motw_full$week <= 3, ],
        "offensiveline/tests/testthat/fixtures/motw_w3.rds")
saveRDS(all_transactions_full[all_transactions_full$week <= 3, ],
        "offensiveline/tests/testthat/fixtures/transactions_w3.rds")

message("Fixtures written to offensiveline/tests/testthat/fixtures/")
