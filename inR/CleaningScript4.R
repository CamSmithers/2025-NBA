library(tidyverse)
og_player_box <- readRDS(
    '/Users/camsmithers/Desktop/Camalytics/NBA/Data-NBA/Original-2024/full_box_player.rds')
add_player_box_2425 <- readRDS(
    '/Users/camsmithers/Desktop/Camalytics/NBA/Data-NBA/Adding-2425/full_box_player_2425.rds')

add_player_box_2425 <- add_player_box_2425 %>%
    mutate(season = 2025)

full_box_player_updated <- rbind(og_player_box, add_player_box_2425)

saveRDS(
    full_box_player_updated,
    file = 
        '/Users/camsmithers/Desktop/Camalytics/NBA/Data-NBA/Current/full_box_player_updated.rds')

og_team_season_stats <- readRDS(
    '/Users/camsmithers/Desktop/Camalytics/NBA/Data-NBA/Original-2024/team_season_stats_fixed.rds')
add_team_season_stats_2425<- readRDS(
    '/Users/camsmithers/Desktop/Camalytics/NBA/Data-NBA/Adding-2425/team_season_stats_2425.rds')

team_season_stats_updated <- rbind(og_team_season_stats, add_team_season_stats_2425)

saveRDS(
    team_season_stats_updated,
    file = '/Users/camsmithers/Desktop/Camalytics/NBA/Data-NBA/Current/team_season_stats_updated.rds')