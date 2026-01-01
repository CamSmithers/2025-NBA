library(tidyverse)

source("/Users/camsmithers/Desktop/Camalytics/NBA/inR/Functions.R")

team_box_scores <- readRDS(
    "/Users/camsmithers/Desktop/Camalytics/NBA/Data-NBA/team_box_scores_updated.rds")
team_box_scores_cy <- readRDS(
    "/Users/camsmithers/Desktop/Camalytics/NBA/Data-NBA/TeamBoxOctNov-2526.rds")

player_box_scores <- readRDS(
    "/Users/camsmithers/Desktop/Camalytics/NBA/Data-NBA/full_box_player.rds")

team_box_scores <- rbind(team_box_scores, team_box_scores_cy)

usdpo <- team_box_scores %>%
    select(gamedate, team, opponent, season, offrating, defrating, pts) %>%
    mutate(
        win = if_else(offrating > defrating, 1, 0),
        netrating = offrating - defrating,
        poss = trunc((100 *pts) / offrating),
        ptsperposs = round(pts/poss, 2)) %>%
    group_by(season) %>%
    mutate(
        ls_avg_offrating = mean(offrating),
        ls_avg_defrating = mean(defrating),
        ls_avg_ptsperposs = mean(ptsperposs),
        ls_sd_offrating = sd(offrating),
        ls_sd_defrating = sd(defrating),
        ls_sd_ptsperposs = sd(ptsperposs)) %>%
    ungroup() %>%
    group_by(season, team) %>%
    mutate(ts_avg_netrating = mean(netrating)) %>%
    ungroup()

usdpo <- usdpo %>%
    mutate(
        offperformance = case_when(
            #Outliers
            offrating < ls_avg_offrating - 2 * ls_sd_offrating ~ "Neg Outlier",
            offrating > ls_avg_offrating + 2 * ls_sd_offrating ~ "Pos Outlier",
            #Within 2SD
            offrating >= ls_avg_offrating - 2 * ls_sd_offrating &
                offrating < ls_avg_offrating - ls_sd_offrating ~ "Within -2SD",
            offrating <= ls_avg_offrating + 2 * ls_sd_offrating &
                offrating > ls_avg_offrating + ls_sd_offrating ~ "Within +2SD",
            #Within 1SD
            offrating >= ls_avg_offrating - ls_sd_offrating &
                offrating < ls_avg_offrating ~ "Within -1SD",
            offrating < ls_avg_offrating + ls_sd_offrating &
                offrating > ls_avg_offrating ~ "Within +1SD"),
        defperformance = case_when(
            #Outliers
            defrating < ls_avg_defrating - 2 * ls_sd_defrating ~ "Neg Outlier",
            defrating > ls_avg_defrating + 2 * ls_sd_defrating ~ "Pos Outlier",
            #Within 2SD
            defrating >= ls_avg_defrating - 2 * ls_sd_defrating &
                defrating < ls_avg_defrating - ls_sd_defrating ~ "Within -2SD",
            defrating <= ls_avg_defrating + 2 * ls_sd_defrating &
                defrating > ls_avg_defrating + ls_sd_defrating ~ "Within +2SD",
            #Within 1SD
            defrating >= ls_avg_defrating - ls_sd_defrating &
                defrating < ls_avg_defrating ~ "Within -1SD",
            defrating < ls_avg_defrating + ls_sd_defrating &
                defrating > ls_avg_defrating ~ "Within +1SD"),
        possperformance = case_when(
            #Outliers
            ptsperposs < ls_avg_ptsperposs - 2 * ls_sd_ptsperposs ~ "Neg Outlier",
            ptsperposs > ls_avg_ptsperposs + 2 * ls_sd_ptsperposs ~ "Pos Outlier",
            #Within 2SD
            ptsperposs >= ls_avg_ptsperposs - 2 * ls_sd_ptsperposs &
                ptsperposs < ls_avg_ptsperposs - ls_sd_ptsperposs ~ "Within -2SD",
            ptsperposs <= ls_avg_ptsperposs + 2 * ls_sd_ptsperposs &
                ptsperposs > ls_avg_ptsperposs + ls_sd_ptsperposs ~ "Within +2SD",
            #Within 1SD
            ptsperposs >= ls_avg_ptsperposs - ls_sd_ptsperposs &
                ptsperposs < ls_avg_ptsperposs ~ "Within -1SD",
            ptsperposs < ls_avg_ptsperposs + ls_sd_ptsperposs &
                ptsperposs > ls_avg_ptsperposs ~ "Within +1SD"
        )
    )

#Offensive
off_nba_df <- data.frame(matrix(ncol = 0, nrow = 0))

nba_teams <- unique(usdpo$team)
nba_years <- unique(usdpo$season)

for (nba_year in nba_years) {
    for (nba_team in nba_teams) {
        offense_performance_sort <- usdpo %>%
            group_by(offperformance) %>%
            filter(team == nba_team, season == nba_year) %>%
            summarize(offperformance_count = n()) %>%
            arrange(desc(offperformance_count)) %>%
            slice_head(n = 3) %>%
            mutate(team = nba_team, season = nba_year)
        off_primary <- offense_performance_sort[1,]
        off_secondary <- offense_performance_sort[2,]
        off_tertiary <- offense_performance_sort[3,]
        names(off_primary) <- c("primary_op", "primary_op_count",
                                "team", "season")
        names(off_secondary) <- c("secondary_op", "secondary_op_count",
                                  "team", "season")
        names(off_tertiary) <- c("tertiary_op", "tertiary_op_count",
                                 "team", "season")
        all_off <- off_primary %>%
            left_join(off_secondary, by = c("team", "season")) %>%
            left_join(off_tertiary, by = c("team", "season"))
        
        off_nba_df <- rbind(off_nba_df, all_off)
    }
}

#Defensive
def_nba_df <- data.frame(matrix(ncol = 0, nrow = 0))

for (nba_year in nba_years) {
    for (nba_team in nba_teams) {
        defense_performance_sort <- usdpo %>%
            group_by(defperformance) %>%
            filter(team == nba_team, season == nba_year) %>%
            summarize(defperformance_count = n()) %>%
            arrange(desc(defperformance_count)) %>%
            slice_head(n = 3) %>%
            mutate(team = nba_team, season = nba_year)
        
        def_primary <- defense_performance_sort[1,]
        def_secondary <- defense_performance_sort[2,]
        def_tertiary <- defense_performance_sort[3,]
        names(def_primary) <- c("primary_dp", "primary_dp_count",
                                "team", "season")
        names(def_secondary) <- c("secondary_dp", "secondary_dp_count",
                                  "team", "season")
        names(def_tertiary) <- c("tertiary_dp", "tertiary_dp_count",
                                 "team", "season")
        all_def <- def_primary %>%
            left_join(def_secondary, by = c("team", "season")) %>%
            left_join(def_tertiary, by = c("team", "season"))
        
        def_nba_df <- rbind(def_nba_df, all_def)
    }
}

#Possessions
poss_nba_df <- data.frame(matrix(ncol = 0, nrow = 0))

for (nba_year in nba_years) {
    for (nba_team in nba_teams) {
        possession_performance_sort <- usdpo %>%
            group_by(possperformance) %>%
            filter(team == nba_team, season == nba_year) %>%
            summarize(possperformance_count = n()) %>%
            arrange(desc(possperformance_count)) %>%
            slice_head(n = 3) %>%
            mutate(team = nba_team, season = nba_year)
        
        poss_primary <- possession_performance_sort[1,]
        poss_secondary <- possession_performance_sort[2,]
        poss_tertiary <- possession_performance_sort[3,]
        names(poss_primary) <- c("primary_pp", "primary_pp_count",
                                 "team", "season")
        names(poss_secondary) <- c("secondary_pp", "secondary_pp_count",
                                   "team", "season")
        names(poss_tertiary) <- c("tertiary_pp", "tertiary_pp_count",
                                  "team", "season")
        all_poss <- poss_primary %>%
            left_join(poss_secondary, by = c("team", "season")) %>%
            left_join(poss_tertiary, by = c("team", "season"))
        
        poss_nba_df <- rbind(poss_nba_df, all_poss)
    }
}

measures_nba_df <- off_nba_df %>%
    left_join(def_nba_df, by = c("team", "season")) %>%
    left_join(poss_nba_df, by = c("team", "season")) %>%
    select(team, season, everything())

usdpo <- usdpo %>%
    left_join(measures_nba_df, by = c("team", "season")) %>%
    mutate(across(where(is.character), as.factor),
           ts_avg_netrating = round(ts_avg_netrating, 1)) %>%
    select(-starts_with("secondary"), -ends_with("_count"), 
           -starts_with("ls_")) %>%
    group_by(season, team) %>%
    mutate(tmsn_avgposs = trunc(mean(poss)),
           tmsn_avgposspts = round(mean(ptsperposs), 2))
    
names(usdpo) <- c("gamedate", "team", "opponent", "season", 
                  "ortg", "drtg", "pts", "win", "netrtg", 
                  "poss", "posspts", "tmsn_netpf", "offpf", 
                  "defpf", "posspf", "pri_op", "tert_op", 
                  "pri_dp", "tert_dp", "pri_pp", "tert_pp", 
                  "tmsn_avgposs", "tmsn_avgposspts")

usdpo <- usdpo %>%
    select(gamedate, season, team, opponent, ortg, drtg, netrtg,
           pts, win, posspts, tmsn_avgposs, tmsn_avgposspts,
           tmsn_netpf, everything())

usdpo_with_opp <- usdpo %>%
    select(gamedate, season, team, pts, tmsn_netpf,
           tmsn_avgposs, tmsn_avgposspts,
           pri_op, pri_dp,pri_pp, 
           tert_op, tert_dp, tert_pp) %>%
    rename_with(~paste0(.x, "_opp"), .cols = -c(gamedate, season))

usdpo <- usdpo %>%
    left_join(usdpo_with_opp, by = c("gamedate", "season", 
                                     "opponent"="team_opp"))

#team_opponent <- player_box_scores %>%
#    distinct(gamedate, team, opponent, season)

#usdpo <- usdpo %>%
#    left_join(team_opponent, by = c("team", "season", "gamedate")) %>%
#    mutate(opponent = as.factor(opponent))

#saveRDS(
#    usdpo,
#    file = 
#        "/Users/camsmithers/Desktop/Camalytics/NBA/Data-NBA/usdpo.rds")