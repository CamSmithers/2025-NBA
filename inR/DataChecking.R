testing_data <- team_box_scores %>%
    group_by(season) %>%
    summarize(
        std = sd(offrating)
    )
