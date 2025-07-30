#!/usr/bin/env Rscript

library(dplyr)
library(tidyr)
library(purrr)
library(parallel)
library(xtable)


########################################
## load analyzed data
########################################

load("rda/model1_animal_probit.RData")
load("rda/animal_data_split_cv.RData")
load("rda/common_variables.RData")

source("common_scripts.R", local = TRUE)


## Judgement time in Table/Figure (minute)
select_time <- 4:8

## label for LaTeX table
labels <- c("Toxic", "Lethal")



########################################
## get D50 ( = - Intercept / slope)
########################################
get_d50 <- function(model) {
    .coef <- coef(model)
    exp(-.coef[1] / .coef[2]) |> as.numeric()
}


########################################
## x: data.frame including two columns (logDose, response)
########################################
get_brier_score <- function(x, model) {
    prob <- predict(model, newdata = x, type = "response")
    mean((x$response - prob)^2)
}


########################################
## chisq test: model vs. observed data
##
## args:
##    data_test: observed frequency of test data
##    newx: logDose
##    model: probit model by train data
## output: tibble
##     chisq, df, pval
########################################
get_chisq_values <- function(data_test, newx, model) {
    prob <- predict(model,
                    newdata = data.frame(logDose = newx),
                    type = "response")

    ## observation / expected table
    obs_table <- data_test |>
        group_by(!!sym("logDose")) |>
        summarize(n = n(),
                  yes = sum(response),
                  no = n - yes)

    exp_table <- obs_table |>
        reframe(logDose = newx,
                n = n,
                yes = n * prob,
                no = n - yes)

    ## chi-squared test
    Obs <- obs_table[, c("yes", "no")]
    Exp <- exp_table[, c("yes", "no")]

    chisq <- sum((Obs - Exp)^2 / Exp)
    .df <- nrow(Obs) - length(coef(model))
    pval <- pchisq(chisq, df = .df, lower.tail = FALSE)

    return(tibble(chisq = chisq, df = .df, pval = pval))
}



########################################
## analysis
########################################

##
## output:
##    judgement_time
##    res: tibble [5 x 7]
##        fold
##        data_train
##        data_test
##        model
##        chisq, df, pval
##
res_score_cv <- mclapply(1:2, function(i) {
    dat_split_cv[[i]] |>
        mutate(model = map(data_train, ~ glm_probit(.)),
               D50 = map_dbl(model, ~ get_d50(.)),
               brier = map2_dbl(data_test, model, get_brier_score),
               res_chisq = pmap(list(data_test = data_test, model = model),
                                get_chisq_values, newx = log_dose[[i]])
        ) |>
        unnest(cols = res_chisq)
}, mc.cores = n_mc)



########################################
## save analyzed data
########################################
save(
    res_score_cv,
    file = "rda/model1_animal_probit_cv.RData"
)



#################
## Table: dose-response in each judgement time
## save Table as LaTeX
#################

tbl_score_cv <- mclapply(1:2, function(i) {
    tbl1 <- res_score_cv[[i]] |>
        mutate(Time = judgement_time / 60) |>
        dplyr::filter(Time %in% select_time)

    tbl1 <- tbl1 |>
        dplyr::select(Time, fold, D50, brier, chisq, df, pval)
    return(tbl1)
}, mc.cores = n_mc)

## merge tables (Toxic and Lethal)
tbl_score_cv_merge <- left_join(
    tbl_score_cv[[1]],
    tbl_score_cv[[2]],
    by = c("Time", "fold")
)

## insert NA in Time
idx <- (seq_len(length(select_time)) - 1) * k + 1
.Time <- tbl_score_cv_merge$Time[idx]
tbl_score_cv_merge$Time <- NA
tbl_score_cv_merge$Time[idx] <- .Time


## cumsum of rows for hline
n_rows_cumsum <- c(idx + k - 1)


## set col labels
colnames(tbl_score_cv_merge) <- c(
    "Time (min)",
    "fold",
    "TD\\textsubscript{50}",
    "Brier score",
    "$\\chi^{2}$",
    "df",
    "\\emph{P} value",
    "LD\\textsubscript{50}",
    "Brier score",
    "$\\chi^{2}$",
    "df",
    "\\emph{P} value"
)


print(file = "Table/Table4.tex",
    xtable(tbl_score_cv_merge,
           digits = c(0, 0, 0, 1, 3, 2, 0, 3, 1, 3, 2, 0, 3)),
    include.rownames = FALSE,
    only.contents = TRUE,
    hline.after = c(0, n_rows_cumsum),
    booktabs = TRUE,
    sanitize.text.function = identity,
    timestamp = NULL
)

