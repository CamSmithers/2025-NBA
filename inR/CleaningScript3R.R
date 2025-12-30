library(tidyverse)
source("/Users/camsmithers/Desktop/Camalytics/NBA/inR/DataPrep.R")
source("/Users/camsmithers/Desktop/Camalytics/NBA/inR/Functions.R")
#----------------------------------------------------------------------------#
#Team Season Statistics
misc_stats_2 <- misc_stats_2425 %>%
    separate(file_id, into = c("team", "year"), sep = "-") %>%
    select(-c(arena, attendance)) %>%
    mutate(year = sub("\\..*", "", year)) %>%
    select(team, year, everything()) %>%
    mutate(team_league_stats = case_when(
        team_league_stats == "Team" ~ "team",
        team_league_stats == "Lg Rank" ~ "lgrk"
    )) %>%
    pivot_wider(
        id_cols = c(team, year),
        names_from = team_league_stats,
        values_from = -c(team, year, team_league_stats),
        names_glue = "{.value}_{tolower(team_league_stats)}"
    ) %>%
    select(team, year, ends_with("_team"), ends_with("_lgrk"))



playoff_pg_team <- team_data_prep_drop(
    dirty_data = playoff_pergame_stats_2425, 
    cols_to_drop = c(obs_num, age, pos, games, starts, name, awards, minutes), 
    id_column = file_id, rename_str_pre = "ps_", rename_str_post = "")

advanced_team <- team_data_prep_keep(
    dirty_data = advanced_stats_2425, 
    cols_to_keep = c(ts_pct, off_ws, ws, ws_per48, off_box_plusminus, 
                     def_box_plusminus, box_plusminus, vorp), 
    id_column = file_id, rename_str_pre = "", rename_str_post = "")

playoff_advanced_team <- team_data_prep_keep(
    dirty_data = playoff_advanced_stats_2425, 
    cols_to_keep = c(ts_pct, off_ws, ws, ws_per48, off_box_plusminus, 
                     def_box_plusminus, box_plusminus, vorp), 
    id_column = file_id, rename_str_pre = "ps_", rename_str_post = "")

adj_shooting_team <- team_data_prep_keep(
    dirty_data = adj_shooting_stats_2425, 
    cols_to_keep = c(adj_fg_pct, adj_2fg_pct, adj_3fg_pct, adj_efg_pct, 
                     adj_ft_pct, adj_ts_pct, adj_ftar, adj_3par, team_id), 
    id_column = team_id, rename_str_pre = "", rename_str_post = "")

playoff_adj_shooting_team <- team_data_prep_keep(
    dirty_data = playoff_adj_shooting_stats_2425, 
    cols_to_keep = c(adj_fg_pct, adj_2fg_pct, adj_3fg_pct, adj_efg_pct, 
                     adj_ft_pct, adj_ts_pct, adj_ftar, adj_3par, file_id), 
    id_column = file_id, rename_str_pre = "ps_", rename_str_post = "")

shooting_team <- team_data_prep_drop(
    dirty_data = shooting_stats_2425, 
    cols_to_drop = c(obs_num, name, pos, age, games, minutes, awards, starts, 
                     fg_pct, fg_pct_2pt,fg_pct_3pt, heaves_fga, heaves_fg, 
                     awards), 
    id_column = file_id, rename_str_pre = "", rename_str_post = "")

playoff_shooting_team <- team_data_prep_drop(
    dirty_data = playoff_shooting_stats_2425, 
    cols_to_drop = c(obs_num, name, pos, age, games, minutes, awards, starts, 
                     fg_pct, fg_pct_2pt, fg_pct_3pt, heaves_fga, heaves_fg, 
                     awards), 
    id_column = file_id, rename_str_pre = "ps_", rename_str_post = "")

team_season_stats_list <- list(
    misc_stats_2, advanced_team, playoff_advanced_team, 
    adj_shooting_team, playoff_adj_shooting_team, shooting_team, 
    playoff_shooting_team)

team_season_stats <- reduce(team_season_stats_list, left_join,
                            by = c("team", "year"))

team_season_stats <- team_season_stats %>%
    mutate(across(
        -all_of("team"), ~ as.numeric(.)))
#---------------------------------------------------------------------------#
#Team Basic Box Scores
basic_box_team <- basic_box_stats_2425 %>%
    fill(opp_id, .direction = "down") %>%
    filter(name == "Team Totals") %>%
    mutate(
        team = str_extract(team_id, "(?<=box-).*?(?=-game-basic)"),
        opponent = str_extract(opp_id, "(?<=box-).*?(?=-game-basic)"), 
        date_str = substr(game_id, 1, 8),
        gamedate = as.Date(date_str, format = "%Y%m%d")) %>%
    select(-obs_num, -name, -gamescore, -plusminus, -team_id, -game_id,
           -opp_id, 
           -date_str) %>%
    select(gamedate, team, opponent, everything())
#---------------------------------------------------------------------------#
#Team Advanced Box Scores
advanced_box_team <- advanced_box_stats_2425 %>%
    select(-last_col()) %>%
    fill(opp_id, .direction = "down") %>%
    filter(name == "Team Totals") %>%
    mutate(
        team = str_extract(team_id, "(?<=box-).*?(?=-game-advanced)"),
        opponent = str_extract(opp_id, "(?<=box-).*?(?=-game-advanced)"), 
        date_str = substr(game_id, 1, 8),
        gamedate = as.Date(date_str, format = "%Y%m%d")) %>%
    mutate(homeaway = if_else(str_detect(game_id, team), "home", "away")) %>%
    select(-obs_num, -name, -box_plusminus, -team_id,
           -opp_id, -date_str, -game_id) %>%
    select(gamedate, team, opponent, everything())

team_box_scores <- basic_box_team %>%
    left_join(advanced_box_team, 
              by = c("gamedate", "team", "opponent", "min")) %>%
    mutate(across(
        -all_of(c("gamedate", "homeaway", "team", "opponent")),
        ~ as.numeric(.))) %>%
    mutate(season = case_when(
        gamedate >= as.Date("2020-12-22") & 
            gamedate <= as.Date("2021-07-20") ~ 2021,
        gamedate >= as.Date("2021-10-19") & 
            gamedate <= as.Date("2022-06-16") ~ 2022,
        gamedate >= as.Date("2022-10-18") & 
            gamedate <= as.Date("2023-06-12") ~ 2023,
        gamedate >= as.Date("2023-10-24") & 
            gamedate <= as.Date("2024-06-17") ~ 2024))

#saveRDS(
#    team_season_stats,
#    file = 
#    "/Users/camsmithers/Desktop/Camalytics/NBA/Data-NBA/team_season_stats_2425.rds")
#saveRDS(
#    team_box_scores, 
#    file = 
#    "/Users/camsmithers/Desktop/Camalytics/NBA/Data-NBA/team_box_scores_2425.rds")

#---------------------------------------------------------------------------#
#Player Basic Box Scores
basic_box_player <- basic_box_stats_2425 %>%
    fill(opp_id, .direction = "down") %>%
    filter(name != "Team Totals" & name != "Reserves") %>%
    mutate(
        team = str_extract(team_id, "(?<=box-).*?(?=-game-basic)"),
        opponent = str_extract(opp_id, "(?<=box-).*?(?=-game-basic)"), 
        date_str = substr(game_id, 1, 8),
        gamedate = as.Date(date_str, format = "%Y%m%d"),
        homeaway = if_else(str_detect(game_id, team), 1, 0),
        min = as.numeric(sub(":.*", "", min))) %>%
    select(-obs_num, -gamescore, -plusminus, -team_id, -game_id, -opp_id, 
           -date_str) %>%
    select(gamedate, homeaway, everything()) %>%
    mutate(across(-gamedate, ~ if_else(
        .x == "Did Not Play"| .x == "Did Not Dress"|
            .x == "Not With Team"| .x == "Player Suspended", NA, .x)))

#---------------------------------------------------------------------------#
#Player Advanced Box Scores
advanced_box_player <- advanced_box_stats_2425 %>%
    select(-last_col()) %>%
    fill(opp_id, .direction = "down") %>%
    filter(name != "Team Totals" & name != "Reserves") %>%
    mutate(
        team = str_extract(team_id, "(?<=box-).*?(?=-game-advanced)"),
        opponent = str_extract(opp_id, "(?<=box-).*?(?=-game-advanced)"), 
        date_str = substr(game_id, 1, 8),
        gamedate = as.Date(date_str, format = "%Y%m%d"),
        homeaway = if_else(str_detect(game_id, team), 1, 0),
        min = as.numeric(sub(":.*", "", min))) %>%
    select(-obs_num, -team_id, -opp_id, -date_str, -game_id) %>%
    select(gamedate, team, opponent, everything()) %>%
    mutate(across(-gamedate, ~ if_else(
        .x == "Did Not Play"| .x == "Did Not Dress"|
            .x == "Not With Team"| .x == "Player Suspended", NA, .x)))
full_box_player <- basic_box_player %>%
    left_join(advanced_box_player, 
              by = c("gamedate", "homeaway", "name", 
                     "min", "team", "opponent")) %>%
    mutate(season = case_when(
        gamedate >= as.Date("2020-12-22") & 
            gamedate <= as.Date("2021-07-20") ~ 2021,
        gamedate >= as.Date("2021-10-19") & 
            gamedate <= as.Date("2022-06-16") ~ 2022,
        gamedate >= as.Date("2022-10-18") & 
            gamedate <= as.Date("2023-06-12") ~ 2023,
        gamedate >= as.Date("2023-10-24") & 
            gamedate <= as.Date("2024-06-17") ~ 2024)) %>%
    mutate(across(
        -all_of(c("gamedate", "homeaway", "name", "min", "team", "opponent")),
        ~ as.numeric(.))) %>%
    fixplayername()
#Ignore NAs introduced by coercion, those players didn't play
#So the mins should be NA

#saveRDS(
#    full_box_player,
#    file = 
#    "/Users/camsmithers/Desktop/Camalytics/NBA/Data-NBA/full_box_player_2425.rds")

#----------------------------------------------------------------------------#
#Player Season Statistics
adj_shooting_player <- player_data_prep_keep(
    dirty_data = adj_shooting_stats_2425, 
    cols_to_keep = c(team_id, name, adj_2fg_pct, adj_3fg_pct, adj_efg_pct, 
                     adj_ft_pct, adj_ts_pct, adj_ftar, adj_3par), 
    id_column = team_id, rename_str_pre = "", rename_str_post = "")

playoff_adj_shooting_player <- player_data_prep_keep(
    dirty_data = playoff_adj_shooting_stats_2425, 
    cols_to_keep = c(file_id, name, adj_2fg_pct, adj_3fg_pct, adj_efg_pct, 
                     adj_ft_pct, adj_ts_pct, adj_ftar, adj_3par), 
    id_column = file_id, rename_str_pre = "ps_", rename_str_post = "")

shooting_player <- player_data_prep_drop(
    dirty_data = shooting_stats_2425,
    cols_to_drop = c(obs_num, pos, age, games, minutes, awards, starts, fg_pct, 
                     fg_pct_2pt, fg_pct_3pt, heaves_fga, heaves_fg), 
    id_column = file_id, rename_str_pre = "", rename_str_post = "")

playoff_shooting_player <- player_data_prep_drop(
    dirty_data = playoff_shooting_stats_2425, 
    cols_to_drop = c(obs_num, pos, age, games, minutes, awards, starts, fg_pct, 
                     fg_pct_2pt, fg_pct_3pt, heaves_fga, heaves_fg), 
    id_column = file_id, rename_str_pre = "ps_", rename_str_post = "")

advanced_gen_player <- player_data_prep_drop(
    dirty_data = advanced_stats_2425,
    cols_to_drop = c(obs_num, age, pos, games, starts, minutes, awards),
    id_column = file_id, rename_str_pre = "", rename_str_post = "")

playoff_advanced_gen_player <- player_data_prep_drop(
    dirty_data = playoff_advanced_stats_2425,
    cols_to_drop = c(obs_num, age, pos, games, starts, minutes, awards),
    id_column = file_id, rename_str_pre = "ps_", rename_str_post = "")

pergame_player <- player_data_prep_drop(
    dirty_data = pergame_stats_2425,
    cols_to_drop = c(obs_num, age, pos, games, starts, awards),
    id_column = file_id, rename_str_pre = "", rename_str_post = "")

playoff_pergame_player <- player_data_prep_drop(
    dirty_data = playoff_pergame_stats_2425,
    cols_to_drop = c(obs_num, age, pos, games, starts, awards),
    id_column = file_id, rename_str_pre = "ps_", rename_str_post = "")

playbyplay_player <- player_data_prep_drop(
    dirty_data = playbyplay_stats_2425,
    cols_to_drop = c(obs_num, age, pos, games, starts, minutes, awards, and1),
    id_column = file_id, rename_str_pre = "", rename_str_post = "")

playoff_playbyplay_player <- player_data_prep_drop(
    dirty_data = playoff_playbyplay_stats_2425,
    cols_to_drop = c(obs_num, age, pos, games, starts, minutes, awards, and1),
    id_column = file_id, rename_str_pre = "ps_", rename_str_post = "")

player_regseason_stats_list <- list(
    adj_shooting_player, shooting_player, advanced_gen_player, pergame_player, 
    playbyplay_player)

player_postseason_stats_list <- list(
    playoff_adj_shooting_player, playoff_shooting_player, 
    playoff_advanced_gen_player, playoff_pergame_player, 
    playoff_playbyplay_player)

player_regseason_stats <- reduce(player_regseason_stats_list, left_join,
                              by = c("team", "year", "name"))

player_postseason_stats <- reduce(player_postseason_stats_list, left_join,
                                 by = c("team", "year", "name"))

#saveRDS(
#    player_regseason_stats,
#    file = 
#    "/Users/camsmithers/Desktop/Camalytics/NBA/Data-NBA/plyr_regsn_stats_2425.rds")

#saveRDS(
#    player_postseason_stats,
#    file = 
#        "/Users/camsmithers/Desktop/Camalytics/NBA/Data-NBA/plyr_pstsn_stats_2425.rds")