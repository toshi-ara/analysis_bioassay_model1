#!/usr/bin/env Rscript

library(dplyr)
library(tidyr)
library(purrr)
library(robustbase)
library(parallel)


## judgement time (sec)
jt <- list(
    seq(180, 600, by = 10),  ## Toxic
    seq(180, 600, by = 10)   ## Lethal
)

## dat_raw:
##   Dose (mg/kg)
##   logDose
##   Time_sec: max 600 sec
##   Time: max 10 min
##   status: 0 (without response) / 1 (with response)
load("rda/animal_data.RData")
load("rda/common_variables.RData")


## dose, log_dose
## x_seq, x_seq_df
## n_mc
load("rda/common_variables.RData")

source("common_scripts.R", local = TRUE)



########################################
## set response within judgement time
##
## args:
##     judgement time: (sec)
##     status: 0 (no response) / 1 (response)
##
## output: add these columns
##     response: 0 (no response) / 1 (response)
##
########################################
set_response <- function(x, judgement_time) {
    x$response <- as.integer(x$Time_sec <= judgement_time & x$status == 1)
    return(x)
}



########################################
# start analysis
########################################

## nls with try-error
safe_nls_try <- function(formula, data, start = NULL, control = nls.control(), ...) {
    fit <- try(
        if (is.null(start)) {
            nls(formula, data = data, control = control, ...)
        } else {
            nls(formula, data = data, start = start, control = control, ...)
        },
        silent = TRUE
    )
    return(fit)
}

## RMSE, Rsquare
## func: modelr::rmse, modelr::rsquare
get_index <- function(model, data, func) {
    ifelse(class(model) == "try-error", NA, func(model, data))
}

myAIC <- function(model) {
    ifelse(class(model) == "try-error", NA, AIC(model))
}

myBIC <- function(model) {
    ifelse(class(model) == "try-error", NA, BIC(model))
}

mycoef <- function(model) {
    # if (class(model) == "try-error") {
    #    NA
    # } else {
       data.frame(t(coef(model)))
    # }
}

weibull_cdf <- function(x, lambda, k) {
    1 - exp(-(x / lambda)^k)
}

myRMSE <- function(model) {
    sqrt(mean(residuals(model)^2))
}



####################
## get TD50/LD50 and 95% CI
####################

##
## add response column (0/1) at each judgement time
##     0: not respond at judgement time
##     1: respond at judgement time

##
## judgement_time: (sec)
## response: tibble
##     Dose (mg/kg)
##     logDose
##     Time_sec: max 600 sec
##     Time: max 10 min
##     status: 0 (without response) / 1 (with response)
##     response: (as above)
##
dat_response <- mclapply(1:2, function(i) {
    tibble(judgement_time = jt[[i]]) |>
        mutate(response = map(judgement_time,
                              ~ set_response(dat_raw[[i]], .)))
}, mc.cores = n_mc)



## statistical model
res_model <- probit <- lapply(1:2, function(i) {
    dat_response[[i]] |>
        mutate(response_tbl = map(response, ~ make_response_table(.)),
               ## probit regression
               model_probit = map(response_tbl, ~
                   glm(rate ~ logDose, family = binomial("probit"),
                       data = .)),
               ## rubust probit regression (M-estimator)
               model_probit_M = map(response_tbl, ~
                   glmrob(rate ~ logDose, family = binomial("probit"),
                          data = .)),
               ## fit CDF of Weibull distribution
               model_weibull = map(response_tbl, ~
                   safe_nls_try(rate ~ weibull_cdf(logDose, lambda, k),
                       start = list(lambda = mean(log_dose[[i]]), k = 5),
                       data = .)),
        )
})

## RMSE
res_RMSE <- lapply(res_model, function(x) {
    x |>
        transmute(
            judgement_time,
            RMSE_probit = map_dbl(model_probit, ~ myRMSE(.)),
            RMSE_probit_M = map_dbl(model_probit_M, ~ myRMSE(.)),
            RMSE_weibull = map_dbl(model_weibull, ~ myRMSE(.)),
        )
})

## parameters
res_params <- lapply(res_model, function(x) {
    x |>
        transmute(
            judgement_time,
            ## probit regression
            coef_probit = map(model_probit, ~ {
                .coef <- coef(.)
                data.frame(
                    Mean = as.numeric(- .coef[1]/.coef[2]),
                    SD = as.numeric(1 / .coef[2])
                )
            }),
            ## rubust probit regression (M-estimator)
            coef_probit_M = map(model_probit_M, ~ {
                .coef <- coef(.)
                data.frame(
                    Mean_M = as.numeric(- .coef[1]/.coef[2]),
                    SD_M = as.numeric(1 / .coef[2])
                )
            }),
            ## fit CDF of Weibull distribution
            coef_weible = map(model_weibull, ~ mycoef(.)),
        ) |>
        unnest(cols = c(coef_probit, coef_probit_M, coef_weible)) |>
        mutate(
            D50_probit = exp(Mean),
            D50_probit_M = exp(Mean_M),
            D50_weibull = exp(lambda * log(2)^(1 / k)), ## exp(median)
            Diff_M = D50_probit_M - D50_probit,
            Rate_M = D50_probit_M / D50_probit,
            Diff_Weibull = D50_probit - D50_weibull,
            Rate_Weibull = D50_probit / D50_weibull
        )
})



########################################
## save analyzed data
########################################

save(
    dat_response,
    res_model, res_RMSE, res_params,
    file = "rda/model1_models.RData"
)

