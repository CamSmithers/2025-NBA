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

og_player_rgsn_stats <-readRDS(
    '/Users/camsmithers/Desktop/Camalytics/NBA/Data-NBA/Original-2024/plyr_regsn_stats.rds')

og_player_pstsn_stats <- readRDS(
    '/Users/camsmithers/Desktop/Camalytics/NBA/Data-NBA/Original-2024/plyr_pstsn_stats.rds')

add_player_rgsn_stats <- readRDS(
    '/Users/camsmithers/Desktop/Camalytics/NBA/Data-NBA/Adding-2425/plyr_regsn_stats_2425.rds')

add_player_pstsn_stats <- readRDS(
    '/Users/camsmithers/Desktop/Camalytics/NBA/Data-NBA/Adding-2425/plyr_pstsn_stats_2425.rds')

full_og_player_stats <- og_player_rgsn_stats %>%
    left_join(og_player_pstsn_stats, by = c('year','team','name'))

full_add_player_stats <- add_player_rgsn_stats %>%
    left_join(add_player_pstsn_stats, by = c('year','team','name'))

player_season_stats_updated <- rbind(full_og_player_stats, full_add_player_stats)

saveRDS(
    player_season_stats_updated,
    file = '/Users/camsmithers/Desktop/Camalytics/NBA/Data-NBA/Current/player_season_stats_updated.rds')