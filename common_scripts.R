library(dplyr)

max_time <- 10
x <- seq(0, max_time, by = 0.1)



########################################
## make response table
##
## x: data
##     judgement time: (sec)
##     status: 0 (no response) / 1 (response)
##
## output: tibble
##     Dose / rate / ldose
##
########################################
make_response_table <- function(x) {
    data.frame(x) |>
        group_by(!!sym("Dose")) |>
        summarise(rate = mean(!!sym("response"))) |>
        mutate(logDose = log(!!sym("Dose")))
}



########################################
## probit analysis
##
## x: data
##     logDose
##     response: 0 (no response) / 1 (response)
##     ...: other arguments for glm
##
## output:
##     model by glm (response ~ logDose)
##
########################################
glm_probit <- function(x, ...) {
    glm(response ~ logDose, family = binomial("probit"), data = x, ...)
}



########################################
## calculate TD50/LD50 and 95% CI
##
## model: model by probit analysis (glm)
##
## output:
##     judgement_time
##     Dose
##     lwr, upr: 95% CI of TD50/LD50
##     intercept, slope: estimated parameters for linear predictor
##     Mean, Sigma: estimated parameters for normal distribution
##
########################################
calc_d50_model <- function(model) {
    res <- get_ci(model, p = 0.5)

    coef1 <- coef(model)
    intercept <- coef1[1] |> as.numeric()
    slope <- coef1[2] |> as.numeric()

    return(data.frame(res,
                      intercept = intercept, slope = slope,
                      Mean = -intercept / slope,
                      Sigma = 1 / slope)
          )
}


## Delta method
## https://rion778.hatenablog.com/entry/20100612/1276358244
get_ci <- function(model, p = 0.5, alpha = 0.05) {
    d <- MASS::dose.p(model, p = p)
    log_d <- d |> as.vector()
    dse <- attributes(d)$SE |> as.vector()
    q <- qnorm(1 - alpha / 2)

    .df <- data.frame(
        prob = p,
        logDose = as.vector(log_d),
        log_lwr = log_d - q * dse,
        log_upr = log_d + q * dse,
        Dose = exp(as.vector(log_d)),
        lwr = exp(log_d - q * dse),
        upr = exp(log_d + q * dse)
    )
    return(.df)
}



########################################
##
## get D50 from res_d50
##
## x: res_d50 (list)
##     Time, logDose, Dose
##
## output: tibble (bind rows)
##     reaction, Time, logDose, Dose
##
########################################
get_d50 <- function(.list, labels) {
    lapply(.list, function(x) {
        dplyr::select(x, !!sym("Time"), !!sym("logDose"), !!sym("Dose"))
    }) |>
        bind_rows(.id = "reaction") |>
        mutate(reaction = factor(!!sym("reaction"), labels = labels))
}


########################################
##
## summarize D50 from res_d50
##
## x: d50 (data.frame)
##     reaction, Time, logDose, Dose
##
## output: tibble
##     reaction, Time, n
##     log_mean, log_sd, geomean
##     Min, Q1, Median, Q3, Max
##
########################################
summarise_d50 <- function(d50) {
    d50 |>
        group_by(!!sym("reaction"), !!sym("Time")) |>
        summarize(n = n(),
                  log_mean = mean(!!sym("logDose")),
                  log_sd = sd(!!sym("logDose")),
                  geomean = exp(!!sym("log_mean")),
                  Min = min(!!sym("Dose")),
                  Q1 = quantile(!!sym("Dose"), probs = 0.25),
                  Median = median(!!sym("Dose")),
                  Q3 = quantile(!!sym("Dose"), probs = 0.75),
                  Max = max(!!sym("Dose"))
        ) |>
        ungroup()
}

