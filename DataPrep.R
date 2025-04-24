library(tidyverse)

# Loading in Data
misc_stats <- read_csv('/Users/camsmithers/Desktop/Camalytics/NBA/Data-NBA/misc.csv')
general_stats <- read_csv('/Users/camsmithers/Desktop/Camalytics/NBA/Data-NBA/all_general.csv')

advanced_box_stats <- read_csv('/Users/camsmithers/Desktop/Camalytics/NBA/Data-NBA/AdvancedBox.csv')
basic_box_stats <- read_csv('/Users/camsmithers/Desktop/Camalytics/NBA/Data-NBA/BasicBox.csv')

adj_shooting_stats <- read_csv('/Users/camsmithers/Desktop/Camalytics/NBA/Data-NBA/adjshooting.csv')
advanced_stats <- read_csv('/Users/camsmithers/Desktop/Camalytics/NBA/Data-NBA/advanced.csv')
playbyplay_stats <- read_csv('/Users/camsmithers/Desktop/Camalytics/NBA/Data-NBA/pbp.csv')
per36min_stats <- read_csv('/Users/camsmithers/Desktop/Camalytics/NBA/Data-NBA/per36.csv')
per100poss_stats <- read_csv('/Users/camsmithers/Desktop/Camalytics/NBA/Data-NBA/per100.csv')
pergame_stats <- read_csv('/Users/camsmithers/Desktop/Camalytics/NBA/Data-NBA/pgs.csv')
shooting_stats <- read_csv('/Users/camsmithers/Desktop/Camalytics/NBA/Data-NBA/shooting.csv')
totals_stats <- read_csv('/Users/camsmithers/Desktop/Camalytics/NBA/Data-NBA/totals.csv')

playoff_adj_shooting_stats <- read_csv('/Users/camsmithers/Desktop/Camalytics/NBA/Data-NBA/playoff_adjshooting.csv')
playoff_advanced_stats <- read_csv('/Users/camsmithers/Desktop/Camalytics/NBA/Data-NBA/playoff_advanced.csv')
playoff_playbyplay_stats <- read_csv('/Users/camsmithers/Desktop/Camalytics/NBA/Data-NBA/playoff_pbp.csv')
playoff_per36min_stats <- read_csv('/Users/camsmithers/Desktop/Camalytics/NBA/Data-NBA/playoff_per36.csv')
playoff_per100poss_stats <- read_csv('/Users/camsmithers/Desktop/Camalytics/NBA/Data-NBA/playoff_per100.csv')
playoff_pergame_stats <- read_csv('/Users/camsmithers/Desktop/Camalytics/NBA/Data-NBA/playoff_pgs.csv')
playoff_shooting_stats <- read_csv('/Users/camsmithers/Desktop/Camalytics/NBA/Data-NBA/playoff_shooting.csv')
playoff_totals_stats <- read_csv('/Users/camsmithers/Desktop/Camalytics/NBA/Data-NBA/playoff_totals.csv')

# Change Column Names
misc_names <- c("team_league_stats", "win", "loss", "exp_win", "exp_loss", 
                "avg_margin", "sos", "sor", "ortg", "drtg", "pace_of_play", 
                "ftar", "three_par", "off_efg_pct", "off_tov_pct", "orb_pct", 
                "off_ft_per_fga", "def_efg_pct", "def_tov_pct", "drb_pct", 
                "def_ft_per_fga", "arena", "attendance", "file_id")
names(misc_stats) <- misc_names

general_names <- c("obs_num", "obs_type", "games", "minutes", "fg", "fga", 
                   "fg_pct", "threefg", "threefga", "threefg_pct", "twofg", 
                   "twofga", "twofg_pct", "ft", "fta", "ft_pct", "oreb", 
                   "dreb", "treb", "ast", "stl", "blk", "tov", "pf", "pts", 
                   "file_id")
names(general_stats) <- general_names

advanced_box_names <- c("obs_num", "name", "min", "ts_pct", "efg_pct", 
                        "three_par", "ftar", "oreb_pct", "dreb_pct", "treb_pct", 
                        "ast_pct", "stl_pct", "blk_pct", "tov_pct", "usage_rate", 
                        "offrating", "defrating", "box_plusminus", "team_id", 
                        "game_id", "opp_id")
names(advanced_box_stats) <- advanced_box_names

basic_box_names <- c("obs_num", "name", "min", "fg", "fga", "fg_pct", 
                     "threefg", "threefga", "threefg_pct", "ft", "fta", 
                     "ft_pct", "oreb", "dreb", "treb", "ast", "stl", "blk", 
                     "tov", "pf", "pts", "gamescore", "plusminus", "team_id", 
                     "game_id", "opp_id")
names(basic_box_stats) <- basic_box_names

adj_shooting_names <- c("obs_num", "name", "age", "postion", "games", "starts", 
                        "minutes", "fg_pct", "twofg_pct", "threefg_pct", 
                        "efg_pct", "ft_pct", "ts_pct", "ftar", "three_par", 
                        "adj_fg_pct", "adj_2fg_pct", "adj_3fg_pct", "adj_ft_pct", 
                        "adj_ts_pct", "adj_ftar", "adj_3par", "fg_add_pts", 
                        "ts_add_pts", "awards", "team_id", "file_id")
names(adj_shooting_stats) <- adj_shooting_names

advanced_names <- c("obs_num", "name", "age", "postion", "games", "starts", 
                    "minutes", "player_effrtg", "ts_pct", "three_par", "ftar", 
                    "oreb_pct", "dreb_pct", "treb_pct", "ast_pct", "stl_pct", 
                    "blk_pct", "tov_pct", "usage_rate", "off_ws", "def_ws", 
                    "ws", "ws_per48", "off_box_plusminus", "def_box_plusminus", 
                    "box_plusminus", "vorp", "awards", "file_id")
names(advanced_stats) <- advanced_names

playbyplay_names <- c("obs_num", "name", "age", "postion", "games", "starts", 
                      "minutes", "pg_min", "sg_min", "sf_min", "pf_min", "c_min", 
                      "oncourt_per100poss", "on_off_net_per100poss", 
                      "tov_by_badpass", "tov_by_lostball", "pf_shoot", 
                      "pf_offball", "fd_shoot", "fd_offball", 
                      "pts_genby_ast", "and1", "blkd", "awards", "file_id")
names(playbyplay_stats) <- playbyplay_names

per36min_names <- c("obs_num", "name", "age", "postion", "games", "starts", 
                    "minutes", "fg", "fga", "fg_pct", "threefg", "threefga", 
                    "threefg_pct", "twofg", "twofga", "twofg_pct", "efg_pct", 
                    "ft", "fta", "ft_pct", "oreb", "dreb", "treb", "ast", 
                    "stl", "blk", "tov", "pf", "pts", "awards", "file_id")
names(per36min_stats) <- per36min_names

per100poss_names <- c("obs_num", "name", "age", "games", "starts", 
                      "minutes", "fg", "fga", "fg_pct", "threefg", "threefga", 
                      "threefg_pct", "twofg", "twofga", "twofg_pct", "efg_pct", 
                      "ft", "fta", "ft_pct", "oreb", "dreb", "treb", "ast", 
                      "stl", "blk", "tov", "pf", "pts", "offrating", "defrating", 
                      "awards", "file_id")
names(per100poss_stats) <- per100poss_names

pergame_names <- c("obs_num", "name", "age", "postion", "games", "starts", 
                    "minutes", "fg", "fga", "fg_pct", "threefg", "threefga", 
                    "threefg_pct", "twofg", "twofga", "twofg_pct", "efg_pct", 
                    "ft", "fta", "ft_pct", "oreb", "dreb", "treb", "ast", 
                    "stl", "blk", "tov", "pf", "pts", "awards", "file_id")
names(pergame_stats) <- pergame_names

shooting_names <- c("obs_num", "name", "age", "pos", "games", "starts", 
                    "minutes", "fg_pct", "avg_distance", "pct_fga_2pt", 
                    "pct_fga_0_3", "pct_fga_3_10", "pct_fga_10_16", 
                    "pct_fga_16_3pt", "pct_fga_3pt", "fg_pct_2pt", 
                    "fg_pct_0_3", "fg_pct_3_10", "fg_pct_10_16", 
                    "fg_pct_16_3pt", "fg_pct_3pt", "pct_2pt_astd", 
                    "pct_3pt_astd", "pct_fga_dunk", "num_dunk_att", 
                    "pct_3pt_corner", "corner_3pt_pct", "heaves_fga", 
                    "heaves_fg", "awards", "file_id")
names(shooting_stats) <- shooting_names

totals_names <- c("obs_num", "name", "age", "games", "starts", 
                  "minutes", "fg", "fga", "fg_pct", "threefg", "threefga", 
                  "threefg_pct", "twofg", "twofga", "twofg_pct", "efg_pct", 
                  "ft", "fta", "ft_pct", "oreb", "dreb", "treb", "ast", 
                  "stl", "blk", "tov", "pf", "pts", "tri_doub", "awards", 
                  "file_id")
names(totals_stats) <- totals_names

names(playoff_adj_shooting_stats) <- adj_shooting_names
names(playoff_advanced_stats) <- advanced_names
names(playoff_playbyplay_stats) <- playbyplay_names
names(playoff_per36min_stats) <- per36min_names
names(playoff_per100poss_stats) <- per100poss_names
names(playoff_pergame_stats) <- pergame_names
names(playoff_shooting_stats) <- shooting_names
names(playoff_totals_stats) <- totals_names