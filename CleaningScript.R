library(tidyverse)
source("/Users/camsmithers/Desktop/Camalytics/NBA/DataPrep.R")
#------------------------------------------------------------------------------#
#Part 1
#------------------------------------------------------------------------------#
#General Statistics
general_team <- general_stats %>%
    select(-obs_num, -games) %>%
    separate(file_id, into = c("team", "year"), sep = "-") %>%
    mutate(
        year = sub("\\..*", "", year),
        row_id = row_number(),
        manipulate = row_id %% 8 %in% c(7, 0),
        obs_type = if_else(manipulate, paste("Opponent", obs_type), obs_type)
    ) %>%
    select(-row_id, -manipulate) %>%
    select(team, year, everything()) %>%
    rename("min"="minutes")

general_total <- general_team %>%
    filter(obs_type == "Team" | obs_type == "Opponent") %>%
    rename_with(~ paste0(.x, "_gen_tot"), .cols = -c(team, year))

general_pergame <- general_team %>%
    filter(obs_type == "Team/G" | obs_type == "Opponent/G") %>%
    mutate(obs_type = case_when(
        obs_type == "Team/G" ~ "Team",
        obs_type == "Opponent/G" ~ "Opponent")) %>%
    rename_with(~ paste0(.x, "_gen_pg"), .cols = -c(team, year))

general_lg_rank <- general_team %>%
    filter(obs_type == "Lg Rank" | obs_type == "Opponent Lg Rank") %>%
    mutate(obs_type = case_when(
        obs_type == "Lg Rank" ~ "Team",
        obs_type == "Opponent Lg Rank" ~ "Opponent")) %>%
    rename_with(~ paste0(.x, "_gen_lgrk"), .cols = -c(team, year))

general_yr2yr <- general_team %>%
    filter(obs_type == "Year/Year" | obs_type == "Opponent Year/Year") %>%
    mutate(across(
        -c(team, year, obs_type, fg_pct, threefg_pct, twofg_pct, ft_pct),
        ~ as.numeric(gsub("%", "", .)))) %>%
    mutate(obs_type = case_when(
        obs_type == "Year/Year" ~ "Team",
        obs_type == "Opponent Year/Year" ~ "Opponent")) %>%
    rename_with(~ paste0(.x, "_gen_yr2yr"), .cols = -c(team, year))

general_team <- general_team %>%
    rename_with(~ paste0(.x, "_gen"), .cols = -c(team, year))
#------------------------------------------------------------------------------#
#Basic Box Scores
basic_box_team <- basic_box_stats %>%
    fill(opp_id, .direction = "down") %>%
    filter(name == "Team Totals") %>%
    mutate(
        team = str_extract(team_id, "(?<=box-).*?(?=-game-basic)"),
        opponent = str_extract(opp_id, "(?<=box-).*?(?=-game-basic)"), 
        date_str = substr(game_id, 1, 8),
        gamedate = as.Date(date_str, format = "%Y%m%d")) %>%
    select(-obs_num, -name, -gamescore, -plusminus, -team_id, -game_id, -opp_id, 
           -date_str) %>%
    select(gamedate, team, opponent, everything()) %>%
    rename_with( ~paste0(.x, "_bsc_box"), .cols = -c("team", "gamedate"))
#------------------------------------------------------------------------------#
#Advanced Box Scores
advanced_box_team <- advanced_box_stats %>%
    select(-last_col()) %>%
    fill(opp_id, .direction = "down") %>%
    filter(name == "Team Totals") %>%
    mutate(
        team = str_extract(team_id, "(?<=box-).*?(?=-game-advanced)"),
        opponent = str_extract(opp_id, "(?<=box-).*?(?=-game-advanced)"), 
        date_str = substr(game_id, 1, 8),
        gamedate = as.Date(date_str, format = "%Y%m%d")) %>%
    select(-obs_num, -name, -box_plusminus, -team_id, -game_id, -opp_id, -date_str) %>%
    select(gamedate, team, opponent, everything()) %>%
    mutate(
        overtime = if_else(min > 240, 1, 0),
        win = if_else(offrating > defrating, 1, 0)) %>%
    rename_with( ~paste0(.x, "_adv_box"), .cols = -c("team", "gamedate"))
#------------------------------------------------------------------------------#
#Miscellaneous Statistics
misc_stats_2 <- misc_stats %>%
    separate(file_id, into = c("team", "year"), sep = "-") %>%
    select(-c(arena, attendance)) %>%
    mutate(year = sub("\\..*", "", year)) %>%
    select(team, year, everything())

misc_team <- misc_stats_2 %>%
    filter(team_league_stats == "Team") %>%
    select(-team_league_stats) %>%
    rename_with(~ paste0(.x, "_misc_team"), .cols = -c(team, year))

misc_rank <- misc_stats_2 %>%
    filter(team_league_stats == "Lg Rank") %>%
    select(-team_league_stats) %>%
    rename_with(~ paste0(.x, "_misc_lgrk"), .cols = -c(team, year))
#------------------------------------------------------------------------------#
#Adjusted Shooting
adj_shooting_team <- adj_shooting_stats %>%
    filter(name == "Team Totals") %>%
    select(-obs_num, -name, -postion, -age, -games, -minutes, -awards) %>%
    select(team_id, everything()) %>%
    separate(team_id, into = c("team", "year"), sep = "-") %>%
    mutate(year = sub("\\..*", "", year)) %>%
    rename_with( ~paste0(.x, "_adj_sht"), .cols = -c("team", "year"))
playoff_adj_shooting_team <- playoff_adj_shooting_stats %>%
    filter(name == "Team Totals") %>%
    select(-obs_num, -name, -pos, -age, -games, -min, -awards, -starts) %>%
    select(file_id, everything()) %>%
    separate(file_id, into = c("team", "year"), sep = "-") %>%
    mutate(year = sub("\\..*", "", year)) %>%
    rename_with( ~paste0("post_", .x, "_adj_sht"), .cols = -c("team", "year"))
#------------------------------------------------------------------------------#
#Shooting
shooting_team <- shooting_stats %>%
    filter(name == "Team Totals") %>%
    select(-c(obs_num, age, pos, games, starts, minutes, awards, name)) %>%
    select(file_id, everything()) %>%
    separate(file_id, into = c("team", "year"), sep = "-") %>%
    mutate(year = sub("\\..*", "", year)) %>%
    rename_with(~ paste0(.x, "_sht"), .cols = -c(team, year))
playoff_shooting_team <- playoff_shooting_stats %>%
    filter(name == "Team Totals") %>%
    select(-c(obs_num, age, pos, games, starts, minutes, awards, name)) %>%
    select(file_id, everything()) %>%
    separate(file_id, into = c("team", "year"), sep = "-") %>%
    mutate(year = sub("\\..*", "", year)) %>%
    rename_with(~ paste0("post_", .x, "_sht"), .cols = -c(team, year))
#------------------------------------------------------------------------------#
#Advanced Statistics
advanced_gen_team <- advanced_stats %>%
    filter(name == "Team Totals") %>%
    select(-c(obs_num, age, postion, games, player_effrtg, oreb_pct, dreb_pct, treb_pct, 
              ast_pct, stl_pct, blk_pct, tov_pct, usage_rate, awards, name, starts, 
              minutes)) %>%
    separate(file_id, into = c("team", "year"), sep = "-") %>%
    mutate(year = sub("\\..*", "", year)) %>%
    select(team, year, everything()) %>%
    rename_with( ~paste0(.x, "_adv"), .cols = -c("team", "year"))
playoff_advanced_gen_team <- playoff_advanced_stats %>%
    filter(name == "Team Totals") %>%
    select(-c(obs_num, age, postion, games, player_effrtg, oreb_pct, dreb_pct, treb_pct, 
              ast_pct, stl_pct, blk_pct, tov_pct, usage_rate, awards, name, starts, 
              minutes)) %>%
    separate(file_id, into = c("team", "year"), sep = "-") %>%
    mutate(year = sub("\\..*", "", year)) %>%
    select(team, year, everything()) %>%
    rename_with( ~paste0("post_", .x, "_adv"), .cols = -c("team", "year"))
#------------------------------------------------------------------------------#
#Per Game Statistics
pergame_team <- pergame_stats %>%
    filter(name == "Team Totals") %>%
    select(-c(obs_num, age, postion, games, starts, name, awards, minutes)) %>%
    select(file_id, everything()) %>%
    separate(file_id, into = c("team", "year"), sep = "-") %>%
    mutate(year = sub("\\..*", "", year)) %>%
    rename_with(~ paste0(.x, "_pg"), .cols = -c(team, year))
playoff_pergame_team <- playoff_pergame_stats %>%
    filter(name == "Team Totals") %>%
    select(-c(obs_num, age, postion, games, starts, name, awards, minutes)) %>%
    select(file_id, everything()) %>%
    separate(file_id, into = c("team", "year"), sep = "-") %>%
    mutate(year = sub("\\..*", "", year))%>%
    rename_with(~ paste0("post_", .x, "_pg"), .cols = -c(team, year))
#------------------------------------------------------------------------------#
#Play by Play
playbyplay_team <- playbyplay_stats %>%
    filter(name == "Team Totals") %>%
    select(-c(obs_num, name, age, postion, games, starts, minutes, pg_min, sg_min, 
              sf_min, pf_min, c_min, awards)) %>%
    separate(file_id, into = c("team", "year"), sep = "-") %>%
    mutate(year = sub("\\..*", "", year)) %>%
    rename_with(~ paste0(.x, "_pbp"), .cols = -c(team, year)) %>%
    select(team, year, everything())
playoff_playbyplay_team <- playoff_playbyplay_stats %>%
    filter(name == "Team Totals") %>%
    select(-c(obs_num, name, age, postion, games, starts, minutes, pg_min, sg_min, 
              sf_min, pf_min, c_min, awards)) %>%
    separate(file_id, into = c("team", "year"), sep = "-") %>%
    mutate(year = sub("\\..*", "", year)) %>%
    rename_with(~ paste0("post_", .x, "_pbp"), .cols = -c(team, year)) %>%
    select(team, year, everything())
#------------------------------------------------------------------------------#
#Totals
totals_team <- totals_stats %>%
    filter(name == "Team Totals") %>%
    select(-c(obs_num, age, games, starts, minutes, awards, name, tri_doub)) %>%
    select(file_id, everything()) %>%
    separate(file_id, into = c("team", "year"), sep = "-") %>%
    mutate(year = sub("\\..*", "", year)) %>%
    rename_with(~ paste0(.x, "_tot"), .cols = -c(team, year))
playoff_totals_team <- playoff_totals_stats %>%
    filter(name == "Team Totals") %>%
    select(-c(obs_num, age, games, starts, minutes, awards, name, tri_doub)) %>%
    select(file_id, everything()) %>%
    separate(file_id, into = c("team", "year"), sep = "-") %>%
    mutate(year = sub("\\..*", "", year)) %>%
    rename_with(~ paste0("post_", .x, "_tot"), .cols = -c(team, year))
#------------------------------------------------------------------------------#
#Part 2
#------------------------------------------------------------------------------#
#Team Data
general_pergame_fnl <- general_pergame %>%
    mutate(
        obs_type_gen_pg = 
            if_else(obs_type_gen_pg == "Opponent", "Opp", obs_type_gen_pg)) %>%
    select(-min_gen_pg) %>%
    pivot_wider(
        id_cols = c(team, year),
        names_from = obs_type_gen_pg,
        values_from = -c(team, year, obs_type_gen_pg),
        names_glue = "{.value}_{tolower(obs_type_gen_pg)}"
    ) %>%
    select(team, year, ends_with("_team"), ends_with("_opp"))
general_lg_rank_fnl <- general_lg_rank %>%
    mutate(
        obs_type_gen_lgrk = 
            if_else(obs_type_gen_lgrk == "Opponent", "Opp", obs_type_gen_lgrk)) %>%
    select(-min_gen_lgrk) %>%
    pivot_wider(
        id_cols = c(team, year),
        names_from = obs_type_gen_lgrk,
        values_from = -c(team, year, obs_type_gen_lgrk),
        names_glue = "{.value}_{tolower(obs_type_gen_lgrk)}"
    ) %>%
    select(team, year, ends_with("_team"), ends_with("_opp"))
general_yr2yr_fnl <- general_yr2yr %>%
    mutate(
        obs_type_gen_yr2yr = 
            if_else(obs_type_gen_yr2yr == "Opponent", "Opp", obs_type_gen_yr2yr)) %>%
    select(-min_gen_yr2yr) %>%
    pivot_wider(
        id_cols = c(team, year),
        names_from = obs_type_gen_yr2yr,
        values_from = -c(team, year, obs_type_gen_yr2yr),
        names_glue = "{.value}_{tolower(obs_type_gen_yr2yr)}"
    ) %>%
    select(team, year, ends_with("_team"), ends_with("_opp"))

both_box_scores <- basic_box_team %>%
    left_join(advanced_box_team, by = c("gamedate", "team",
                                        "opponent_bsc_box"="opponent_adv_box",
                                        "min_bsc_box"="min_adv_box")) %>%
    select(-usage_rate_adv_box) %>%
    rename("win_loss" = "win_adv_box")

misc_full <- misc_team %>%
    left_join(misc_rank, by = c("team", "year"))

adj_shooting_full <- adj_shooting_team %>%
    select(-starts_adj_sht) %>%
    left_join(playoff_adj_shooting_team, by = c("team", "year"))

shooting_full <- shooting_team %>%
    left_join(playoff_shooting_team, by = c("team", "year"))

advanced_gen_full <- advanced_gen_team %>%
    left_join(playoff_advanced_gen_team, by = c("team", "year"))

pergame_full <- pergame_team %>%
    left_join(playoff_pergame_team, by = c("team", "year"))

playbyplay_full <- playbyplay_team %>%
    left_join(playoff_playbyplay_team, by = c("team", "year"))

team_data_list <- list(general_pergame_fnl, general_lg_rank_fnl, 
                       general_yr2yr_fnl, misc_full, adj_shooting_full, 
                       shooting_full, advanced_gen_full, pergame_full, 
                       playbyplay_full)

team_level_data <- reduce(team_data_list, left_join, by = c("team", "year"))