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

load("rda/model1_animal_probit.RData")
load("rda/common_variables.RData")


## Judgement time in Table/Figure (minute)
select_time <- 4:8

## label for LaTeX table
labels <- c("Toxic", "Lethal")



#################
## Table: dose-response in each judgement time
## save Table as LaTeX
#################

##
## input: tibble
##   dat_response[[i]]
##     judgement_time: (second)
##     response: tibble (Dose, Time, status, logDose, response)
## output: tibble
##   Dose
##   n
##   response
##
response2table <- function(x) {
    x |>
        group_by(!!sym("Dose")) |>
        summarise(n = length(!!sym("response")),
                  response = sum(!!sym("response")))
}


##
## list of tibble:
##     Dose
##     n
##     `4` ... `8` (values by select_time)
##     Reaction: "Toxic" or "Lethal" (values by labels)
##
tbl_response_time_list <- mclapply(1:2, function(i) {
    tbl1 <- dat_response[[i]] |>
        mutate(Time = judgement_time / 60) |>
        mutate(res = map(response, ~ response2table(.))) |>
        dplyr::filter(Time %in% select_time) |>
        dplyr::select(Time, res) |>
        unnest(cols = res) |>
        pivot_wider(names_from = "Time", values_from = "response")
    tbl1$Reaction <- ""
    tbl1$Reaction[1] <- labels[i]
    return(tbl1)
}, mc.cores = n_mc)

##
## combined table (toxic and lethal)
## tibble:
##     Dose
##     n
##     `4` ... `8` (values by select_time)
##
tbl_response_time <- tbl_response_time_list |>
    bind_rows() |>
    dplyr::select(Reaction, everything())

## cumsum of rows for hline
n_rows_cumsum <- sapply(tbl_response_time_list, nrow) |>
    cumsum()

colnames(tbl_response_time)[2] <- "Dose (mg/kg)"


print(file = "Table/Table1.tex",
    xtable(tbl_response_time,
           digits = c(0, 0, 1, 0, rep(0, length(select_time))),
           align = c("c", "c", "c", "c", rep("c", length(select_time)))),
    include.rownames = FALSE,
    only.contents = TRUE,
    hline.after = c(0, n_rows_cumsum),
    booktabs = TRUE,
    sanitize.text.function = identity,
    timestamp = NULL
)



#################
## save paramters as JSON
## only at select time (variable 'select_time')
#################

##
## input: res_probit_params[[i]]
##     judgement_time / Time
##     Dose (TD50/LD50), lwr, upr
##     intercept, slope
##     Mean, Sigma
## output: list of tibble
##     Time
##     Mean
##     Sigma
##
param <- mclapply(1:2, function(i) {
    res_probit_params[[i]] |>
        dplyr::select(Time, Mean, Sigma) |>
        dplyr::filter(Time %in% select_time)
}, mc.cores = n_mc)

write_json(param, "result/parameter.json", digits = 3, pretty = TRUE)



####################
## save parameters as CSV
## merge Toxic an Lethal
####################

##
## input res_probit_params[[i]]:
##     judgement_time: time (second) set by variable 'jt'
##     Time: judgement time (minute)
##     Dose: TD50 or LD50
##     lwr, upr: 95% CI
##     intercept, slope: parameters estimated by probit analysis
##     Mean, Sigma: parameters for normal distribution
##
## output: combined table (toxic and lethal)
##     reaction: "TD50" or "LD50"
##     judgement_time (sec)
##     Time (min)
##     Dose, lwr, upper
##     intercept, slope
##     Mean, Sigma
##
res <- lapply(res_probit_params, function(x) {
    mutate(x, across(Time:Sigma, \(x) round(x, digits = 6)))
}) |>
    bind_rows(.id = "reaction") |>
    mutate(reaction = factor(reaction, labels = c("TD50", "LD50")))

write_csv(res, file = "csv/model1_parameters_distribution.csv")



#################
## Table: TD50/LD50 (95% CI)
##   only at select time (variable 'select_time')
## save Table as LaTeX
#################

tmp <- mclapply(1:2, function(i) {
    res_probit_params[[i]] |>
        dplyr::select(Time, Dose, lwr, upr) |>
        dplyr::filter(Time %in% select_time) |>
        mutate(DoseCI = sprintf("%.1f [%.1f, %.1f]", Dose, lwr, upr)) |>
        select(Time, DoseCI)
}, mc.cores = n_mc)

tbl_d50 <- left_join(tmp[[1]], tmp[[2]], by = "Time")

colnames(tbl_d50) <- c(
    "Time (min)",
    "TD\\textsubscript{50} [95\\% CI]",
    "LD\\textsubscript{50} [95\\% CI]"
)

print(file = "Table/Table2.tex",
    xtable(tbl_d50,
           digits = c(0, 0, 0, 0),
           align = c("c", "c", "c", "c")),
    include.rownames = FALSE,
    only.contents = TRUE,
    booktabs = TRUE,
    sanitize.text.function = identity,
    timestamp = NULL
)

