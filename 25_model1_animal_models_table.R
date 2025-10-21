#!/usr/bin/env Rscript

library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(parallel)
library(jsonlite)
library(xtable)


########################################
## load analyzed data
########################################

load("rda/model1_models.RData")
load("rda/common_variables.RData")


## Judgement time in Table/Figure (minute)
select_time <- 4:8

## label for LaTeX table
labels <- c("Toxic", "Lethal")



####################
## save RMSE as CSV
## merge Toxic an Lethal
####################
res_RMSE_all <- lapply(res_RMSE, function(x) {
    mutate(x, across(starts_with("RMSE_"), \(x) round(x, digits = 3)))
}) |>
    bind_rows(.id = "reaction") |>
    mutate(reaction = factor(reaction, labels = labels))

write_csv(res_RMSE_all, file = "csv/model1_parameters_compareRMSE.csv")


####################
## save parameters as CSV
## merge Toxic an Lethal
####################
res_d50 <- lapply(res_params, function(x) {
    mutate(x, across(Mean:Rate_Weibull, \(x) round(x, digits = 3)))
}) |>
    bind_rows(.id = "reaction") |>
    mutate(reaction = factor(reaction, labels = labels))

write_csv(res_d50, file = "csv/model1_parameters_compareD50.csv")



#################
## Table: RMSE
##   only at select time (variable 'select_time')
## save Table as LaTeX
#################
tbl_RMSE <- res_RMSE_all |>
    mutate(Time = judgement_time / 60) |>
    dplyr::filter(Time %in% select_time) |>
    dplyr::select(reaction, Time, starts_with("RMSE_"))

n_rows_cumsum <- tbl_RMSE |>
    group_by(reaction) |>
    count() |>
    pull(n) |>
    cumsum()

## set row names
n_time <- length(select_time)
n_row <- nrow(tbl_RMSE)
bool_reaction <- seq_len(n_row) %% n_time == 1
tbl_RMSE$reaction[!bool_reaction] <- NA

## set column names
colnames(tbl_RMSE) <- c(
    "Reaction",
    "Time (min)",
    "Probit",
    "Robust probit",
    "Weibull"
)

print(file = "Table/TableS1.tex",
    xtable(tbl_RMSE,
           digits = c(0, 0, 0, 3, 3, 3),
           align = rep("c", ncol(tbl_RMSE) + 1)),
    include.rownames = FALSE,
    only.contents = TRUE,
    hline.after = c(0, n_rows_cumsum),
    booktabs = TRUE,
    sanitize.text.function = identity,
    timestamp = NULL
)


#################
## Table: comparison D50
##   only at select time (variable 'select_time')
## save Table as LaTeX
#################
tbl_d50 <- res_d50 |>
    mutate(Time = judgement_time / 60) |>
    dplyr::filter(Time %in% select_time) |>
    dplyr::select(reaction, Time, D50_probit:Rate_Weibull)

n_rows_cumsum <- tbl_d50 |>
    group_by(reaction) |>
    count() |>
    pull(n) |>
    cumsum()

## set row names
n_time <- length(select_time)
n_row <- nrow(tbl_d50)
bool_reaction <- seq_len(n_row) %% n_time == 1
tbl_d50$reaction[!bool_reaction] <- NA

## set column names
colnames(tbl_d50) <- c(
    "Reaction",
    "Time (min)",
    "Probit",
    "Robust probit",
    "Weibull",
    "Difference",
    "Rate",
    "Difference",
    "Rate"
)

print(file = "Table/TableS2.tex",
    xtable(tbl_d50,
           digits = c(0, 0, 0, 1, 1, 1, 3, 3, 3, 3),
           align = rep("c", ncol(tbl_d50) + 1)),
    include.rownames = FALSE,
    only.contents = TRUE,
    hline.after = c(0, n_rows_cumsum),
    booktabs = TRUE,
    sanitize.text.function = identity,
    timestamp = NULL
)

