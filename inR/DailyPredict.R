daily.data <- read_csv(
    "/Users/camsmithers/Downloads/dailydata - Sheet1.csv")
daily.data <- daily.data %>%
    mutate(gamedate = as.Date("2026-01-01"))

daily.combined <- rbind(usdpo, daily.data)

daily.combined.fnl <- daily.combined %>%
    group_by(season, team) %>%
    fill(tmsn_netpf, pri_op, pri_dp, pri_pp,
         tmsn_netpf_opp, pri_op_opp, pri_dp_opp, pri_pp_opp)

win_five <- glm(win ~ tmsn_netpf + pri_op + pri_dp + pri_pp +
                    team + opponent + season + tmsn_netpf_opp +
                    pri_op_opp + pri_dp_opp + pri_pp_opp,
                data = usdpo,
                family = "binomial")

model_five_fitted <- fitted(win_five)
model_five_predict <- predict(win_five, newdata = daily.combined.fnl,
                              type = "response")

daily.combined.fnl<- cbind(daily.combined.fnl, model_five_predict)

daily.combined.fnl <- daily.combined.fnl %>%
    rename("model_five_predict" = last_col()) %>%
    mutate(model_five_predict = round(100 * model_five_predict, 1))