#!/usr/bin/env Rscript

library(dplyr)
library(tidyr)
library(purrr)
library(boot)
library(parallel)


########################################
## load analyzed data
########################################

load("rda/model1_animal_probit.RData")
load("rda/common_variables.RData")

source("common_scripts.R")


label_params <- c("Mean", "Sigma")


########################################
## stat function
##     to select the same sample size as original data
##
## args:
##     data including two columns (logDose, response)
##     .index: not used
## output:
##     coefficients of probit analysis (intercept, slope)
##     d50
##
########################################
coef_fun_grouped <- function(data, .index) {
    d <- split(data, data$logDose) |>
        lapply(function(x) {
            x[sample(seq_len(nrow(x)), replace = TRUE), ]
        }) |>
        bind_rows()

    model <- glm_probit(d)
    .coef <- coef(model)
    c(Mean = as.numeric(-.coef[1] / .coef[2]),
      Sigma = as.numeric(1 / .coef[2]))
}

coef_fun <- function(data, index) {
    d <- data[index, ]
    model <- glm_probit(d)

    .coef <- coef(model)
    c(Mean = as.numeric(-.coef[1] / .coef[2]),
      Sigma = as.numeric(1 / .coef[2]))
}


########################################
## perform bootstrap
########################################

R <- 1000
seed <- 123

set.seed(seed)
res_boot <- mclapply(dat_response, function(x) {
    x |>
        mutate(boot = map(response, ~
                          boot(data = ., statistic = coef_fun, R = R))
        )
}, mc.cores = n_mc)



########################################
## save data
########################################

save(res_boot, file = "rda//model1_animal_probit_boot.RData")

