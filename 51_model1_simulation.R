#!/usr/bin/env Rscript

library(dplyr)
library(tidyr)
library(purrr)
library(RcppZiggurat)
library(parallel)
library(jsonlite)


load("rda/common_variables.RData")
source("common_scripts.R", local = TRUE)


## param[[i]]
##   Time: minute
##   Mean / Sigma
param <- read_json("result/parameter.json", simplifyVector = TRUE)



########################################
## Conditions of simulation
########################################

n_group <- 5  # number of dose
n <- 15       # sample size
N <- 100      # number of replication

## judgemnt time
jt <- 4:8

## Dose
Dose <- list(
    c(34.7, 41.7, 50.0, 60.0, 72.0),
    c(102.4, 128.0, 160.0, 200.0, 250.0)
)



########################################
## select parameters (mean, sigma) at jt
########################################

param_jt <- lapply(1:2, function(i) {
    subset(param[[i]], Time == jt)
})



########################################
## Simulation (model 1)
########################################

seed <- 1234

zsetseed(seed)
z <- zrnorm(n * n_group * N * length(jt))

##
## list of tibble:
##   Time (minute)
##   N: replication
##   Dose
##   ID: individual
##   min_dose: minimal toxic/lethal dose
##   response: 0 (not respond) / 1 (respond)
##   logDose
##
dat_sim_response <- mclapply(1:2, function(i) {
    expand_grid(
        Time = jt,
        N = seq_len(N),
        Dose = Dose[[i]],
        ID = seq_len(n)
    ) |>
        ## set parameter (mean, sigma) in each judgement time
        group_nest(Time) |>
        mutate(Mean = subset(param[[i]], Time == Time)[, "Mean"],
               Sigma = subset(param[[i]], Time == Time)[, "Sigma"]) |>
        unnest(cols = data) |>
        ## set individual parameter by random generator
        mutate(z = z,
               min_dose = Mean + Sigma * z) |>
        ## set response (0/1) and logDose
        mutate(response = as.integer(min_dose < log(Dose)),
               logDose = log(Dose)) |>
        ## sort columns
        dplyr::select(Time, N, ID, Dose, logDose, everything())
}, mc.cores = n_mc)



########################################
## probit analysis
########################################

##
## list of tibble:
##   Time (minute)
##   N: replication
##   data: tibble
##   response_tbl: tibble
##   model: glm model by probit analysis
##   prob: 0.5 (for TD50 and LD50)
##   logDose, log_lwr, log_upr
##   Dose, lwr, upr
##   intercept, slope, Mean, Sigma
##
res_d50 <- mclapply(1:2, function(i) {
    dat_sim_response[[i]] |>
        group_nest(Time, N) |>
        mutate(response_tbl = map(data, ~ make_response_table(.)),
               model = map(data, ~ glm_probit(.)),
               result = map(model, ~ calc_d50_model(.))
              ) |>
        unnest(cols = result)
}, mc.cores = n_mc)


##
## output: combined table (TD50 and LD50)
##   reaction, Time, logDose, Dose
##
d50 <- get_d50(res_d50, c("TD50", "LD50"))

##
## output: tibble
##   reaction, Time, n
##   log_mean, log_sd, geomean
##   Min, Q1, Median, Q3, Max
##
d50_summary <- summarise_d50(d50)



########################################
# save analyzed data
########################################

save(
    jt,
    n, n_group, N, seed,
    dat_sim_response,
    res_d50, d50, d50_summary,
    file = "rda/model1_simulation.RData"
)

