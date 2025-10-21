#!/usr/bin/env Rscript

library(dplyr)
library(tidyr)
library(purrr)
library(parallel)
library(ggplot2)
library(patchwork)


########################################
# load analyzed data
########################################

load("rda/model1_models.RData")
load("rda/common_variables.RData")

source("common_plots.R")
## label_reaction (TD50/LD50)
## labeler: function for title


select_time <- 4:8
jt <- 5

title_short <- c("TD", "LD")
title_sprintf <- c("Toxic", "Lethal")


weibull_cdf <- function(x, lambda, k) {
    1 - exp(-(x / lambda)^k)
}


####################
## plot and probit curve in each judgement time (sec)
##   judgement_time
##   D50 and 95% CI
####################

newx <- lapply(log_dose, function(x) {
    d <- (x[2] - x[1]) * 0.2
    xmin <- min(x) - d
    xmax <- max(x) + d
    seq(xmin, xmax, length.out = 101)
})

dat_point <- lapply(res_model, function(x) {
    x |>
        dplyr::select(judgement_time, response_tbl) |>
        unnest(cols = response_tbl)
})

dat_line <- lapply(1:2, function(i) {
    res_model[[i]] |>
        dplyr::select(judgement_time, model_probit, model_weibull) |>
        pivot_longer(cols = starts_with("model"),
                     names_to = "model", values_to = "value") |>
        mutate(logDose = list(newx[[i]]),
               rate = map(value, ~
                          predict(.,
                                  newdata = data.frame(logDose = newx[[i]]),
                                  type = "response"))) |>
        dplyr::select(judgement_time, model, logDose, rate) |>
        unnest(cols = c("logDose", "rate")) |>
        mutate(model = factor(model, labels = c("Probit", "Weibull")))
})

dat_d50 <- lapply(res_params, function(x) {
    x |>
        dplyr::select(judgement_time, D50_probit, D50_weibull) |>
        mutate(logD50_probit = log(D50_probit),
               logD50_weibull = log(D50_weibull))
})


labeler <- function(title, judgement_time) {
    str <- sprintf(
        "expression(paste(%s[50], \" (%d sec)\"))", title, judgement_time
    )
    eval(parse(text = str))
}


plot_fit_curve <- function(dat_point, dat_line, dat_d50,
                                  title = NULL, j_time) {
    yval <- seq(0, 1, by = 0.2)

    point <- dat_point |>
        dplyr::filter(judgement_time == j_time)
    fitline <- dat_line |>
        dplyr::filter(judgement_time == j_time)
    d50 <- dat_d50 |>
        dplyr::filter(judgement_time == j_time)

    log_d50 <- max(d50$logD50_probit, d50$logD50_weibull)

    p <- ggplot(fitline, aes(logDose, rate)) +
        geom_line(aes(group = model, linetype = model)) +
        geom_segment(x = -10, y = 0.5,
                     xend = log_d50, yend = 0.5,
                     color = "gray30") +
        geom_segment(x = d50$logD50_probit, y = 0.5,
                     xend = d50$logD50_probit, yend = -10,
                     color = "gray30") +
        geom_segment(x = d50$logD50_weibull, y = 0.5,
                     xend = d50$logD50_weibull, yend = -10,
                     linetype = "dashed", color = "gray30") +
        geom_point(data = point, size = 2) +
        scale_x_continuous(breaks = point$logDose,
                           labels = point$Dose,
                           minor_breaks = NULL) +
        coord_cartesian(
            xlim = c(min(fitline$logDose), max(fitline$logDose)),
            ylim = c(0, 1)
        ) +
        scale_y_continuous(breaks = yval,
                           labels = yval * 100,
                           minor_breaks = NULL) +
        labs(x = "Dose (mg/kg)", y = "Response rate (%)") +
        ggtitle(ifelse(is.null(title), "", labeler(title, j_time))) +
        theme_bw() +
        theme(
            axis.text = element_text(size = 14, color = "black"),
            axis.title = element_text(size = 16),
            plot.title = element_text(size = 16, hjust = 0.5),
            panel.grid.minor = element_blank(),
            legend.text = element_text(size = 11),
            legend.title = element_text(size = 12),
        )

    return(p)
}




## judgement time (sec)
jt <- list(
    seq(180, 600, by = 10),  ## Toxic
    seq(180, 600, by = 10)   ## Lethal
)

p <- mclapply(1:2, function(i) {
    tibble(judgement_time = jt[[i]]) |>
        mutate(plot = pmap(list(j_time = judgement_time),
                           plot_fit_curve,
                           dat_point = dat_point[[i]],
                           dat_line = dat_line[[i]],
                           dat_d50 = dat_d50[[i]],
                           title = c("TD", "LD")[i]
                   )
        )
}, mc.cores = n_mc)


####################
## save as PDF
####################

## Supplemental Figures
## plot fit probit curve in each judgement time
cairo_pdf(file = "result/fig_model1_animal_fitcurve_compare_toxic.pdf",
          width = 6, height = 4, onefile = TRUE)
print(p[[1]]$plot)
dev.off()

cairo_pdf(file = "result/fig_model1_animal_fitcurve_compare_lethal.pdf",
          width = 6, height = 4, onefile = TRUE)
print(p[[2]]$plot)
dev.off()


####################
## archive as zip file
####################

filepattern <- "fig_model1_animal_fitcurve_compare_(toxic|lethal)\\.pdf$"
zipfile <- "FigS2.zip"

if (file.exists(zipfile)) {
    file.remove(zipfile)
}


setwd("result")
filelist <- list.files(".", pattern = filepattern)
zip(zipfile = paste0("../", zipfile), files = filelist)
file.remove(filelist)
setwd("..")

