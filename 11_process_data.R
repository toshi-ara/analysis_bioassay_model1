#!/usr/bin/env Rscript

library(readr)
library(dplyr)
library(parallel)


filename <- c("lidocaine_toxic.csv", "lidocaine_lethal.csv")

dir.create("rda", recursive = TRUE, showWarnings = FALSE)
dir.create("csv", recursive = TRUE, showWarnings = FALSE)
dir.create("Table", recursive = TRUE, showWarnings = FALSE)
dir.create("result", recursive = TRUE, showWarnings = FALSE)



########################################
## read raw data
########################################

## Dose (mg/kg)
## logDose
## Time_sec: max 600 sec
## Time: max 10 min
## status: 0 (without response) / 1 (with response)
dat_raw <- lapply(filename, function(x) {
    read_csv(x, show_col_types = FALSE) |>
        rename(Time_sec = Time) |>
        mutate(logDose = log(Dose),
               Time = Time_sec / 60) |>
        dplyr::select(Dose, logDose, Time_sec, Time, status)
})



########################################
## set dose / log_dose
########################################

dose <- list(
    unique(dat_raw[[1]]$Dose),
    unique(dat_raw[[2]]$Dose)
)

log_dose <- list(
    unique(dat_raw[[1]]$logDose),
    unique(dat_raw[[2]]$logDose)
)

## set x for probit curve)
x_seq <- lapply(log_dose, function(.x) {
    d <- (.x[2] - .x[1]) * 0.2
    xmin <- min(.x) - d
    xmax <- max(.x) + d
    seq(xmin, xmax, length.out = 101)
})

x_seq_df <- lapply(x_seq, function(.x) {
    data.frame(logDose = .x)
})



########################################
## CPU core number
########################################

n_mc <- detectCores()



########################################
## save data
########################################

save(dat_raw, file = "rda/animal_data.RData")
save(dose, log_dose, x_seq, x_seq_df, n_mc,
     file = "rda/common_variables.RData")

