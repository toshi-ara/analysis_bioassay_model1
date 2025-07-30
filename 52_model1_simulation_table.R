#!/usr/bin/env Rscript

library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(xtable)


########################################
## Simulation by model 1
## load analyzed data
########################################

source("common_scripts.R")
dir.create("Table", recursive = TRUE, showWarnings = FALSE)

##
## jt     : judgement time (4:8)
## n      : sample size
## n_group: number of dose
## N      : number of replication
## seed   : random seed used in simulation (model 1)
## dat_sim_response,
## res_d50
## d50
## d50_summary: tibble
##   reaction, Time, n
##   log_mean, log_sd, geomean
##   Min, Q1, Median, Q3, Max
##
load("rda/model1_simulation.RData")



#################
## save simulation results as CSV
#################

tbl_sim_response <- dat_sim_response |>
    bind_rows(.id = "reaction") |>
    mutate(reaction = factor(reaction, labels = c("TD50", "LD50"))) |>
    dplyr::select(reaction, N, ID, Dose, logDose, min_dose, response)

write_csv(tbl_sim_response, file = "csv/model1_simulation_raw.csv")


tbl_d50_summary <- d50_summary |>
    mutate(across(log_mean:Max, \(x) round(x, digits = 3)))

write_csv(tbl_d50_summary, file = "csv/model1_simulation_summary.csv")



#################
## save Table as LaTeX
#################

## comsum of rows for hline
n_rows_cumsum <- d50_summary |>
    group_by(reaction) |>
    count() |>
    pull(n) |>
    cumsum()

## set row labels
D50_labels <- paste0(c("TD", "LD"), "\\textsubscript{50}")
d50_summary$reaction <- NA
d50_summary$reaction[c(0, n_group) + 1] <- D50_labels

## set col labels
colnames(d50_summary) <- c(
    " ",
    "Time (min)",
    "$n$",
    "Mean$_{\\log \\text{Dose}}$",
    "SD$_{\\log \\text{Dose}}$",
    "geoMean",
    "Min",
    "Q\\textsubscript{1}",
    "Median",
    "Q\\textsubscript{3}",
    "Max"
)

print(file = "Table/Table5.tex",
    xtable(d50_summary,
           digits = c(0, 0, 0, 0, 3, 3, 1, 1, 1, 1, 1, 1)),
    include.rownames = FALSE,
    only.contents = TRUE,
    hline.after = c(0, n_rows_cumsum),
    booktabs = TRUE,
    sanitize.text.function = identity,
    timestamp = NULL
)

