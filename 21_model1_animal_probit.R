#!/usr/bin/env Rscript

library(dplyr)
library(tidyr)
library(purrr)
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

##
## calculate TD50/LD50 (95% CI) and parameters
##
## judgement_time: (sec)
## response: tibble
## response_tbl: tibble [5 x 3] => Dose, rate, logDose
## logDose
## log_lwr, log_upr: lower and upper limit of 95% CI
## Dose (mg/kg)
## lwr, upr: lower and upper limit of 95% CI
## intercept, slope: parameters estimated by probit analysis
## Mean, Sigma: parameters for normal distribution
##   (for minimal toxic/lethal dose: logDose)
##    Mean = -intercept / slope
##    Sigma = 1 / slope
## Time: max 10 min
##
res_d50 <- mclapply(1:2, function(i) {
    dat_response[[i]] |>
        mutate(model = map(response, ~ glm_probit(.)),
               response_tbl = map(response, ~ make_response_table(.)),
               result = map(model, ~ calc_d50_model(.))) |>
        unnest(col = result) |>
        select(-prob) |>
        mutate(Time = judgement_time / 60)  # convert to minute
}, mc.cores = n_mc)


##
## judgement_time: time (second) set by variable 'jt'
## Time: judgement time (minute)
## Dose: TD50 or LD50
## lwr, upr: 95% CI
## intercept, slope: parameters estimated by probit analysis
## Mean, Sigma: parameters for normal distribution
##   (for minimal toxic/lethal dose: logDose)
##    Mean = -intercept / slope
##    Sigma = 1 / slope
##
res_probit_params <- mclapply(1:2, function(i) {
    res_d50[[i]] |>
        select(-response) |>
        select(judgement_time, Time, Dose, lwr, upr,
               intercept, slope, Mean, Sigma)
}, mc.cores = n_mc)



########################################
## save analyzed data
########################################

save(
    dat_response,
    res_d50,
    res_probit_params,
    file = "rda/model1_animal_probit.RData"
)

