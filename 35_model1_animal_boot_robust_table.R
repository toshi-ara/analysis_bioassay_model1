#!/usr/bin/env Rscript

library(dplyr)
library(tidyr)
library(purrr)
library(boot)
library(readr)
library(xtable)



########################################
## load analyzed data
########################################

load("rda/model1_animal_probit_robust_boot.RData")
load("rda/common_variables.RData")

source("common_scripts.R")


label_params <- c("Mean", "Sigma")

## Judgement time in Table/Figure (minute)
select_time <- 4:8

## label for LaTeX table
label_reaction <- c("Toxic", "Lethal")



## output: data.frame
##     Parameter
##     Value
##
get_t0 <- function(x, name_cols) {
    names(x) <- name_cols
    df <- data.frame(t0 = x) |>
        tibble::rownames_to_column(var = "Parameter")
    return(df)
}

## 
## x: matrix
## output: tibble
##
get_t <- function(x, name_cols) {
    df <- data.frame(x)
    colnames(df) <- name_cols
    return(df)
}

##
## 95% CI by BCa
##
## input:
##     x: object by boot function
##
get_ci_boot <- function(x, name_cols) {
    lapply(seq_len(length(name_cols)), function(i) {
        ci <- boot.ci(x, index = i, type = "bca")
        data.frame(
            Parameter = name_cols[i],
            lwr = ci$bca[4],
            upr = ci$bca[5]
        )
    }) |>
        bind_rows()
}


##
## merge t0 and CI
##
merge_t0_ci <- function(t0, ci) {
    merge(t0, ci, by = "Parameter")
}



res_boot_plot <- lapply(res_boot, function(x) {
    x |>
        mutate(t0 = map(boot, ~ get_t0(.$t0, label_params)),
               ci = map(boot, ~ get_ci_boot(., label_params)),
               param = map2(t0, ci, merge_t0_ci),
               t = map(boot, ~ get_t(.$t, label_params))) |>
        dplyr::select(judgement_time, param, t)
})



res_boot_param <- lapply(res_boot_plot, function(x) {
    x |>
        dplyr::select(-t) |>
        unnest(cols = param)
})


## make table
tbl_boot_param <- lapply(res_boot_param, function(x) {
    x |>
        pivot_wider(names_from = Parameter,
                    values_from = c(t0, lwr, upr)) |>
        dplyr::select(judgement_time,
                      ends_with("Mean"), ends_with("Sigma"))
}) |>
    bind_rows(.id = "reaction") |>
    mutate(reaction = factor(reaction, labels = c("Toxic", "Lethal")))



########################################
## save data
########################################

save(res_boot_plot, file = "rda/model1_animal_probit_robust_boot_params.RData")


#################
## save parameters as CSV
## merge Toxic an Lethal
#################

write_csv(tbl_boot_param, file = "csv/model1_parameters_robust_boot_params.csv")


#################
## Table: alpha / beta / D50 (95% CI)
##   only at select time (variable 'select_time')
## save Table as LaTeX
#################

tbl_boot_param <- lapply(res_boot_param, function(x) {
    x |>
        dplyr::filter(judgement_time %in% (select_time * 60)) |>
        mutate(Time = judgement_time / 60) |>
        mutate(param = sprintf("%.3f [%.3f, %.3f]", t0, lwr, upr)) |>
        select(Time, Parameter, param) |>
        pivot_wider(names_from = Parameter, values_from = param)
}) |>
    bind_rows(.id = "reaction") |>
    mutate(reaction = factor(reaction, labels = label_reaction))



n_rows_cumsum <- tbl_boot_param |>
    group_by(reaction) |>
    count() |>
    pull(n) |>
    cumsum()

## set row labels
D50_labels <- label_reaction
tbl_boot_param$reaction <- NA
tbl_boot_param$reaction[c(0, length(select_time)) + 1] <- D50_labels

## set col labels
colnames(tbl_boot_param) <- c(
    " ",
    "Time (min)",
    "$\\mu$ [95\\% CI]",
    "$\\sigma$ [95\\% CI]"
)

print(file = "Table/TableS3.tex",
    xtable(tbl_boot_param,
           digits = c(0, 0, 0, 0, 0)),
    include.rownames = FALSE,
    only.contents = TRUE,
    hline.after = c(0, n_rows_cumsum),
    booktabs = TRUE,
    sanitize.text.function = identity,
    timestamp = NULL
)

