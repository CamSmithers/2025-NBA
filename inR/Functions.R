#Function Script
fixplayername <- function(dataset) {
    dataset %>%
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
}

best_player_metric <- function(dataset, main_col, x, y) {
    new_col <- paste0(as_label(enquo(main_col)), "_player_id")
    
    dataset %>%
        filter(minutes > x & usage > y) %>%
        group_by(year, team) %>%
        slice_max(order_by = {{main_col}}, n = 1, with_ties = FALSE) %>%
        ungroup() %>%
        mutate(!!new_col := sample(1e1:1e3, n(), replace = FALSE)) %>%
        select(year, team, name, {{main_col}}, !!sym(new_col)) %>%
        mutate(year = as.numeric(year))
}

bp_on_court <- function(team_game_dataset, player_game_dataset,
                        bp_dataset, filter_by_col, best_by_id) {
    
    team_with_player <- team_game_dataset %>%
        select(team, gamedate, season, win) %>%
        left_join(bp_dataset, by = c("team", "season"="year"))
    
    only_best_players <- player_game_dataset %>%
        left_join(bp_dataset, by = c("team", "name", "season"="year")) %>%
        filter(!is.na({{filter_by_col}}) | {{filter_by_col}} > 12) %>%
        filter(!is.na({{best_by_id}})) %>%
        mutate(
            bp_ids_keys = 
                   paste(team, gamedate, !!sym(as_label(enquo(best_by_id))))
            ) %>%
        distinct(bp_ids_keys)
    
    team_bp_dataset <- team_with_player %>%
        mutate(best_by_player_avi = ifelse(
            paste(team, gamedate, !!sym(as_label(enquo(best_by_id)))) %in%
                only_best_players$bp_ids_keys, 1, 0))
    
    return(team_bp_dataset)
}

data_prep <- function(dirty_data, cols_to_keep,
                      id_column, rename_str = "") {
    clean_data <- dirty_data %>%
        filter(name == "Team Totals") %>%
        select({{cols_to_keep}}, {{id_column}}) %>%
        separate({{id_column}}, into = c("team", "year"), sep = "-") %>%
        mutate(year = sub("\\..*", "", year)) %>%
        select(team, year, everything()) %>%
        rename_with( ~paste0(rename_str, .x), .cols = -c(team, year))
}