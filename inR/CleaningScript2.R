library(tidyverse)
source("/Users/camsmithers/Desktop/Camalytics/NBA/inR/DataPrep.R")
#Season Statistics
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

general_pergame <- general_team %>%
    filter(obs_type == "Team/G" | obs_type == "Opponent/G") %>%
    mutate(obs_type = case_when(
        obs_type == "Team/G" ~ "Team",
        obs_type == "Opponent/G" ~ "Opponent")) %>%
    rename_with(~ paste0(.x, "_pg"), .cols = -c(team, year))

general_lg_rank <- general_team %>%
    filter(obs_type == "Lg Rank" | obs_type == "Opponent Lg Rank") %>%
    mutate(obs_type = case_when(
        obs_type == "Lg Rank" ~ "Team",
        obs_type == "Opponent Lg Rank" ~ "Opponent")) %>%
    rename_with(~ paste0(.x, "_lgrk"), .cols = -c(team, year))

general_yr2yr <- general_team %>%
    filter(obs_type == "Year/Year" | obs_type == "Opponent Year/Year") %>%
    mutate(across(
        -c(team, year, obs_type, fg_pct, threefg_pct, twofg_pct, ft_pct),
        ~ as.numeric(gsub("%", "", .)))) %>%
    mutate(obs_type = case_when(
        obs_type == "Year/Year" ~ "Team",
        obs_type == "Opponent Year/Year" ~ "Opponent")) %>%
    rename_with(~ paste0(.x, "_yr2yr"), .cols = -c(team, year))

general_pergame_fnl <- general_pergame %>%
    mutate(
        obs_type_pg = 
            if_else(obs_type_pg == "Opponent", "Opp", obs_type_pg)) %>%
    select(-min_pg) %>%
    pivot_wider(
        id_cols = c(team, year),
        names_from = obs_type_pg,
        values_from = -c(team, year, obs_type_pg),
        names_glue = "{.value}_{tolower(obs_type_pg)}"
    ) %>%
    select(team, year, ends_with("_team"), ends_with("_opp"))

general_lg_rank_fnl <- general_lg_rank %>%
    mutate(
        obs_type_lgrk = 
            if_else(obs_type_lgrk == "Opponent", "Opp", obs_type_lgrk)) %>%
    select(-min_lgrk) %>%
    pivot_wider(
        id_cols = c(team, year),
        names_from = obs_type_lgrk,
        values_from = -c(team, year, obs_type_lgrk),
        names_glue = "{.value}_{tolower(obs_type_lgrk)}"
    ) %>%
    select(team, year, ends_with("_team"), ends_with("_opp"))

general_yr2yr_fnl <- general_yr2yr %>%
    mutate(
        obs_type_yr2yr = 
            if_else(obs_type_yr2yr == "Opponent", "Opp", obs_type_yr2yr)) %>%
    select(-min_yr2yr) %>%
    pivot_wider(
        id_cols = c(team, year),
        names_from = obs_type_yr2yr,
        values_from = -c(team, year, obs_type_yr2yr),
        names_glue = "{.value}_{tolower(obs_type_yr2yr)}"
    ) %>%
    select(team, year, ends_with("_team"), ends_with("_opp"))

misc_stats_2 <- misc_stats %>%
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

playoff_pergame_team <- playoff_pergame_stats %>%
    filter(name == "Team Totals") %>%
    select(-c(obs_num, age, pos, games, starts, name, awards, minutes)) %>%
    select(file_id, everything()) %>%
    separate(file_id, into = c("team", "year"), sep = "-") %>%
    mutate(year = sub("\\..*", "", year))%>%
    rename_with(~ paste0("ps_", .x, "_pg_team"), .cols = -c(team, year))

advanced_team <- advanced_stats %>%
    filter(name == "Team Totals") %>%
    select(ts_pct, off_ws, def_ws, ws, ws_per48, off_box_plusminus, 
           def_box_plusminus, 
           box_plusminus, vorp, file_id) %>%
    separate(file_id, into = c("team", "year"), sep = "-") %>%
    mutate(year = sub("\\..*", "", year)) %>%
    select(team, year, everything())

playoff_advanced_team <- playoff_advanced_stats %>%
    filter(name == "Team Totals") %>%
    select(ts_pct, three_par, ftar, off_ws, def_ws, ws, 
           ws_per48, off_box_plusminus, 
           def_box_plusminus, box_plusminus, vorp, file_id) %>%
    separate(file_id, into = c("team", "year"), sep = "-") %>%
    mutate(year = sub("\\..*", "", year)) %>%
    select(team, year, everything()) %>%
    rename_with( ~paste0("ps_", .x), .cols = -c("team", "year"))

adj_shooting_team <- adj_shooting_stats %>%
    filter(name == "Team Totals") %>%
    select(adj_fg_pct, adj_2fg_pct, adj_3fg_pct, adj_efg_pct, 
           adj_ft_pct, adj_ts_pct, 
           adj_ftar, adj_3par, team_id) %>%
    select(team_id, everything()) %>%
    separate(team_id, into = c("team", "year"), sep = "-") %>%
    mutate(year = sub("\\..*", "", year))

playoff_adj_shooting_team <- playoff_adj_shooting_stats %>%
    select(adj_fg_pct, adj_2fg_pct, adj_3fg_pct, adj_efg_pct, 
           adj_ft_pct, adj_ts_pct, 
           adj_ftar, adj_3par, file_id) %>%
    select(file_id, everything()) %>%
    separate(file_id, into = c("team", "year"), sep = "-") %>%
    mutate(year = sub("\\..*", "", year)) %>%
    rename_with( ~paste0("ps_", .x), .cols = -c("team", "year"))

shooting_team <- shooting_stats %>%
    filter(name == "Team Totals") %>%
    select(-c(obs_num, name, pos, age, games, minutes, awards, starts, fg_pct, 
              fg_pct_2pt,
              fg_pct_3pt, heaves_fga, heaves_fg, awards)) %>%
    select(file_id, everything()) %>%
    separate(file_id, into = c("team", "year"), sep = "-") %>%
    mutate(year = sub("\\..*", "", year))

playoff_shooting_team <- playoff_shooting_stats %>%
    filter(name == "Team Totals") %>%
    select(-c(obs_num, name, pos, age, games, minutes, awards, starts, fg_pct, 
              fg_pct_2pt,
              fg_pct_3pt, heaves_fga, heaves_fg, awards)) %>%
    select(file_id, everything()) %>%
    separate(file_id, into = c("team", "year"), sep = "-") %>%
    mutate(year = sub("\\..*", "", year)) %>%
    rename_with( ~paste0("ps_", .x), .cols = -c("team", "year"))

team_season_stats_list <- list(general_pergame_fnl, playoff_pergame_team, 
                               general_lg_rank_fnl, general_yr2yr_fnl, 
                               misc_stats_2, advanced_team, playoff_advanced_team,
                               adj_shooting_team, playoff_adj_shooting_team, 
                               shooting_team, playoff_shooting_team)
team_season_stats <- reduce(team_season_stats_list, left_join,
                            by = c("team", "year"))

team_season_stats <- team_season_stats %>%
    mutate(across(
        -all_of("team"), ~ as.numeric(.)))
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
    select(gamedate, team, opponent, everything())
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
    mutate(homeaway = if_else(str_detect(game_id, team), "home", "away")) %>%
    select(-obs_num, -name, -box_plusminus, -team_id, -opp_id, -date_str, -game_id) %>%
    select(gamedate, team, opponent, everything()) %>%
    mutate(
        overtime = if_else(min > 240, 1, 0),
        win = if_else(offrating > defrating, 1, 0))

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

#saveRDS(team_season_stats,
#        file = "/Users/camsmithers/Desktop/Camalytics/NBA/Data-NBA/team_season_stats.rds")
#saveRDS(team_box_scores, 
#        file = "/Users/camsmithers/Desktop/Camalytics/NBA/Data-NBA/team_box_scores.rds")

#------------------------------------------------------------------------------#
basic_box_player <- basic_box_stats %>%
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

advanced_box_player <- advanced_box_stats %>%
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
              by = c("gamedate", "homeaway", "name", "min", "team", "opponent")) %>%
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
    mutate(name = case_when(
        name == "Alen SmailagiÄ" ~ "Alen Smailagic",
        name == "Alperen ÅengÃ¼n" | name == "Alperen ÃÂengÃÂ¼n" ~ "Alperen Sengun",
        name == "AnÅ¾ejs PaseÄÅiks" ~ "Anžejs Pasečņiks",
        name == "Anderson VarejÃ£o" ~ "Anderson Varejao",
        name == "Boban MarjanoviÄ"~ "Boban Marjanović",
        name == "Bogdan BogdanoviÄ" ~ "Bogdan Bogdanović",
        name == "Bojan BogdanoviÄ" | name == "Bojan BogdanoviÃÂ" ~ "Bojan Bogdanović",
        name == "Cristiano FelÃ­cio" ~ "Cristiano Felicio",
        name == "Dario Å ariÄ" ~ "Dario Šarić", #No Fix
        name == "DÄvis BertÄns" ~ "Dāvis Bertāns",
        name == "Dennis SchrÃ¶der" ~ "Dennis Schröder",
        name == "Ersan Ä°lyasova" ~ "Ersan Ilyasova",
        name == "Filip PetruÅ¡ev" ~ "Filip Petrusev",
        name == "Goran DragiÄ" ~ "Goran Dragic",
        name == "Jonas ValanÄiÅ«nas" | name == "Jonas ValanÃÂiÃÂ«nas" ~ "Jonas Valančiūnas",
        name == "Juancho HernangÃ³mez" ~ "Juancho Hernangomez",
        name == "Jusuf NurkiÄ" | name == "Jusuf NurkiÃÂ" ~ "Jusuf Nurkić",
        name == "Karim ManÃ©" ~ "Karim Mane",
        name == "Kristaps PorziÅÄ£is" | name == "Kristaps PorziÃÂÃÂ£is"~ "Kristaps Porziņģis",
        name == "Lester QuiÃ±ones" ~ "Lester Quinones",
        name == "Luka Å amaniÄ" ~ "Luka Samanic", #No Fix
        name == "Luka DonÄiÄ" | name == "Luka DonÃÂiÃÂ" ~ "Luka Dončić",
        name == "MÃ£ozinha Pereira" ~ "Maozinha Pereira",
        name == "Moussa DiabatÃ©" ~ "Moussa Diabaté",
        name == "NicolÃ² Melli" ~ "Nicolo Melli",
        name == "Nikola JokiÄ" | name == "Nikola JokiÃÂ" ~ "Nikola Jokić",
        name == "Nikola JoviÄ" ~ "Nikola Jović",
        name == "Nikola VuÄeviÄ" | name == "Nikola VuÃÂeviÃÂ" ~ "Nikola Vučević",
        name == "ThÃ©o Maledon" ~ "Theo Maledon",
        name == "TimothÃ© Luwawu-Cabarrot" ~ "Timothe Luwawu-Cabarrot",
        name == "TomÃ¡Å¡ SatoranskÃ½" ~ "Tomas Satoransky",
        name == "Vasilije MiciÄ" ~ "Vasilije Micić",
        name == "Vlatko ÄanÄar" ~ "Vlatko Čančar",
        name == "Willy HernangÃ³mez" ~ "Willy Hernangomez",
        TRUE ~ name
    ))

adj_shooting_player <- adj_shooting_stats %>%
    select(team_id, name, adj_2fg_pct, adj_3fg_pct, adj_efg_pct, adj_ft_pct, 
           adj_ts_pct, adj_ftar, adj_3par) %>%
    separate(team_id, into = c("team", "year"), sep = "-") %>%
    select(year, team, everything()) %>%
    mutate(year = sub("\\..*", "", year)) %>%
    filter(name != "Team Totals")

playoff_adj_shooting_player <- playoff_adj_shooting_stats %>%
    select(file_id, name, adj_2fg_pct, adj_3fg_pct, adj_efg_pct, adj_ft_pct, 
           adj_ts_pct, adj_ftar, adj_3par) %>%
    separate(file_id, into = c("team", "year"), sep = "-") %>%
    select(year, team, everything()) %>%
    mutate(year = sub("\\..*", "", year)) %>%
    filter(name != "Team Totals") %>%
    rename_with( ~paste0("ps_", .x), .cols = -c("team", "year", "name"))

shooting_player <- shooting_stats %>%
    select(-c(obs_num, pos, age, games, minutes, awards, starts, fg_pct, 
              fg_pct_2pt, fg_pct_3pt, heaves_fga, heaves_fg)) %>%
    separate(file_id, into = c("team", "year"), sep = "-") %>%
    select(year, team, everything()) %>%
    mutate(year = sub("\\..*", "", year)) %>%
    filter(name != "Team Totals")

playoff_shooting_player <- playoff_shooting_stats %>%
    select(-c(obs_num, pos, age, games, minutes, awards, starts, fg_pct, 
              fg_pct_2pt, fg_pct_3pt, heaves_fga, heaves_fg)) %>%
    separate(file_id, into = c("team", "year"), sep = "-") %>%
    select(year, team, everything()) %>%
    mutate(year = sub("\\..*", "", year)) %>%
    filter(name != "Team Totals") %>%
    rename_with( ~paste0("ps_", .x), .cols = -c("team", "year", "name"))

advanced_gen_player <- advanced_stats %>%
    select(-c(obs_num, age, pos, games, starts, minutes, awards)) %>%
    separate(file_id, into = c("team", "year"), sep = "-") %>%
    select(year, team, everything()) %>%
    mutate(year = sub("\\..*", "", year)) %>%
    filter(name != "Team Totals")
playoff_advanced_gen_player <- playoff_advanced_stats %>%
    select(-c(obs_num, age, pos, games, starts, minutes, awards)) %>%
    separate(file_id, into = c("team", "year"), sep = "-") %>%
    select(year, team, everything()) %>%
    mutate(year = sub("\\..*", "", year)) %>%
    filter(name != "Team Totals") %>%
    rename_with( ~paste0("ps_", .x), .cols = -c("team", "year", "name"))

pergame_player <- pergame_stats %>%
    select(-c(obs_num, age, pos, games, starts, awards)) %>%
    separate(file_id, into = c("team", "year"), sep = "-") %>%
    select(year, team, everything()) %>%
    mutate(year = sub("\\..*", "", year)) %>%
    filter(name != "Team Totals")

playoff_pergame_player <- playoff_pergame_stats %>%
    select(-c(obs_num, age, pos, games, starts, awards)) %>%
    separate(file_id, into = c("team", "year"), sep = "-") %>%
    select(year, team, everything()) %>%
    mutate(year = sub("\\..*", "", year)) %>%
    filter(name != "Team Totals") %>%
    rename_with( ~paste0("ps_", .x), .cols = -c("team", "year", "name"))

playbyplay_player <- playbyplay_stats %>%
    select(-c(obs_num, age, pos, games, starts, minutes, awards, and1)) %>%
    separate(file_id, into = c("team", "year"), sep = "-") %>%
    select(year, team, everything()) %>%
    mutate(year = sub("\\..*", "", year)) %>%
    filter(name != "Team Totals")

playoff_playbyplay_player <- playoff_playbyplay_stats %>%
    select(-c(obs_num, age, pos, games, starts, minutes, awards, and1)) %>%
    separate(file_id, into = c("team", "year"), sep = "-") %>%
    select(year, team, everything()) %>%
    mutate(year = sub("\\..*", "", year)) %>%
    filter(name != "Team Totals") %>%
    rename_with( ~paste0("ps_", .x), .cols = -c("team", "year", "name"))

player_season_stats_list <- list(pergame_player, playoff_pergame_player, 
                                 advanced_gen_player, playoff_advanced_gen_player, 
                                 shooting_player, playoff_shooting_player, 
                                 adj_shooting_player, playoff_adj_shooting_player, 
                                 playbyplay_player, playoff_playbyplay_player)

player_season_stats <- reduce(player_season_stats_list, left_join,
                              by = c("team", "year", "name"))

#saveRDS(player_season_stats,
#        file = "/Users/camsmithers/Desktop/Camalytics/NBA/Data-NBA/player_season_stats.rds")
#saveRDS(full_box_player, 
#        file = "/Users/camsmithers/Desktop/Camalytics/NBA/Data-NBA/full_box_player.rds")