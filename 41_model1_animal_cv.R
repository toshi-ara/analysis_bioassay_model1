#!/usr/bin/env Rscript

library(dplyr)
library(tidyr)
library(purrr)
library(caret)


########################################
## load analyzed data
########################################

load("rda/model1_animal_probit.RData")


########################################
## split data (k-folds)
##
## args:
##    x: original data
##    col: groups (for split)
##    k: number for k-folds
##
## output: tibble
##    fold
##    data_train
##    data_test
########################################
split_data <- function(x, col, k = 10) {
    seq_k <- seq_len(k)
    idx <- createFolds(seq_len(nrow(x)), k = k, list = FALSE)

    data_train <- lapply(seq_k, function(i) {
        subset(x, idx != i)
    })

    data_test <- lapply(seq_k, function(i) {
        subset(x, idx == i)
    })

    return(tibble(
              fold = seq_k,
              data_train = data_train,
              data_test = data_test
           ))
}


########################################
## output:
##    judgement_time
##    fold
##    data_train <tibble>
##    data_test  <tibble>
########################################

k <- 5
seed <- 123

dat_split_cv <- lapply(res_d50, function(x) {
    set.seed(seed)
    x |>
        dplyr::select(judgement_time, response) |>
        group_nest(judgement_time) |>
        mutate(data = map(data, ~ .[[1]][[1]])) |>
        mutate(split_data = map(data, ~ split_data(., .$logDose, k = k))) |>
        dplyr::select(-data) |>
        unnest(cols = split_data)
})



########################################
## save analyzed data
########################################

save(dat_split_cv, k, file = "rda/animal_data_split_cv.RData")

