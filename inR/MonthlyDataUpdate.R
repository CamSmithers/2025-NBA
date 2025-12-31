library(tidyverse)
monthly_adv_box <- read_csv(
    '/Users/camsmithers/Desktop/Camalytics/NBA/Data-NBA/AdvancedBoxOctNov-2526.csv'
)
monthly_basic_box <- read_csv(
    '/Users/camsmithers/Desktop/Camalytics/NBA/Data-NBA/BasicBoxOctNov-2526.csv'
)

monthly_adv_box_names <- c("obs_num", "name", "min", "ts_pct", "efg_pct", 
                        "three_par", "ftar", "oreb_pct", "dreb_pct", "treb_pct", 
                        "ast_pct", "stl_pct", "blk_pct", "tov_pct", "usage_rate", 
                        "offrating", "defrating", "box_plusminus", "team_id", 
                        "game_id", "opp_id")

monthly_basic_box_names <- c("obs_num", "name", "min", "fg", "fga", "fg_pct", 
                                "threefg", "threefga", "threefg_pct", "ft", "fta", 
                                "ft_pct", "oreb", "dreb", "treb", "ast", "stl", "blk", 
                                "tov", "pf", "pts", "gamescore", "plusminus", "team_id", 
                                "game_id", "opp_id")

names(monthly_adv_box) <- monthly_adv_box_names
names(monthly_basic_box) <- monthly_basic_box_names

monthly_basic_box_team <- monthly_basic_box %>%
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

monthly_adv_box_team <- monthly_adv_box %>%
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

monthly_team_box_scores <- monthly_basic_box_team %>%
    left_join(monthly_adv_box_team, 
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
            gamedate <= as.Date("2024-06-17") ~ 2024,
        gamedate >= as.Date("2024-10-15") & 
            gamedate <= as.Date("2025-07-01") ~ 2025,
        gamedate >= as.Date("2025-10-01") & 
            gamedate <= as.Date("2026-07-01") ~ 2026))

saveRDS(
    monthly_team_box_scores,
    file = 
        '/Users/camsmithers/Desktop/Camalytics/NBA/Data-NBA/TeamBoxOctNov-2526.csv'
)
