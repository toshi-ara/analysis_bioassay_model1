#!/usr/bin/env Rscript

library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(patchwork)


########################################
## load analyzed data
########################################

load("rda/model1_animal_probit.RData")
load("rda/animal_data_split_cv.RData")

source("common_plots.R")
dir.create("result", recursive = TRUE, showWarnings = FALSE)


########################################
## get D50 ( = - Intercept / slope)
########################################
get_d50 <- function(model) {
    .coef <- coef(model)
    exp(-.coef[1] / .coef[2]) |> as.numeric()
}


## set Dose/logDose from res_d50
Dose <- list(
    unique(res_d50[[1]]$response[[1]]$Dose),
    unique(res_d50[[2]]$response[[1]]$Dose)
)

logDose <- list(
    unique(res_d50[[1]]$response[[1]]$logDose),
    unique(res_d50[[2]]$response[[1]]$logDose)
)

yval <- seq(0, 1, by = 0.2)


plot_curve_cv_facet <- function(data_probit,
                                tbl_train, tbl_test, judgement_time,
                                axis_logDose, axis_Dose) {
    d <- (axis_logDose[2] - axis_logDose[1]) * 0.2
    xmin <- min(axis_logDose) - d
    xmax <- max(axis_logDose) + d

    p <- ggplot(data_probit, aes(logDose, rate)) +
        geom_line() +
        geom_point(data = tbl_train, size = 1) +
        geom_point(data = tbl_test, pch = 1, size = 2) +
        scale_x_continuous(breaks = axis_logDose,
                           labels = axis_Dose,
                           minor_breaks = NULL) +
        coord_cartesian(xlim = c(xmin, xmax),
                        ylim = c(0, 1)) +
        scale_y_continuous(breaks = yval,
                           labels = yval * 100,
                           minor_breaks = NULL) +
        labs(x = "Dose (mg/kg)", y = "Response rate (%)") +
        ggtitle(sprintf("Judgement time: %d sec", judgement_time)) +
        facet_wrap(~ fold) +
        theme_bw() +
        theme(
            axis.text = element_text(size = 12, color = "black"),
            axis.title = element_text(size = 14),
            plot.title = element_text(size = 14, hjust = 0.5),
            panel.grid.minor = element_blank(),
            strip.background = element_rect(fill = "transparent")
        )
    return(p)
}


plot_curve_cv <- function(data_probit, axis_logDose, axis_Dose,
                          d50 = NULL) {
    d <- (axis_logDose[2] - axis_logDose[1]) * 0.2
    xmin <- min(axis_logDose) - d
    xmax <- max(axis_logDose) + d

    p <- ggplot(data_probit, aes(logDose, rate, group = fold)) +
        geom_line() +
        scale_x_continuous(breaks = axis_logDose,
                           labels = axis_Dose,
                           minor_breaks = NULL) +
        coord_cartesian(xlim = c(xmin, xmax),
                        ylim = c(0, 1)) +
        scale_y_continuous(breaks = yval,
                           labels = yval * 100,
                           minor_breaks = NULL) +
        labs(x = "Dose (mg/kg)", y = "Response rate (%)") +
        theme_bw() +
        theme(
            axis.text = element_text(size = 12, color = "black"),
            axis.title = element_text(size = 14),
            plot.title = element_text(size = 14, hjust = 0.5),
            panel.grid.minor = element_blank()
        )

    if (!is.null(d50)) {
        log_d50 <- log(d50)
        n <- length(log_d50)

        .df <- data.frame(
            x = log_d50,
            xend = log_d50,
            y = rep(0.5, n),
            yend = rep(-10, n)
        )

        p <- p +
            geom_segment(x = -10, y = 0.5,
                         xend = max(log_d50), yend = 0.5,
                         group = 1, color = "gray30", linetype = "dashed") +
            geom_segment(data = .df,
                         aes(x = x, y = y, xend = xend, yend = yend, group = 1),
                         color = "gray30", linetype = "dashed")
    }

    return(p)
}



####################
# analysis
####################

dat_plot <- lapply(1:2, function(i) {
    d <- (logDose[[i]][2] - logDose[[i]][1]) * 0.2
    xmin <- min(logDose[[i]]) - d
    xmax <- max(logDose[[i]]) + d
    newx <- data.frame(logDose = seq(xmin, xmax, length.out = 101))

    dat_d50 <- dat_split_cv[[i]] |>
        mutate(model = map(data_train, ~ glm_probit(.)),
               d50 = map_dbl(model, ~ get_d50(.))
        ) |>
        dplyr::select(judgement_time, d50) |>
        group_nest(judgement_time, .key = "data_D50") |>
        mutate(data_D50 = map(data_D50, ~ pull(., d50)))

    dat_probit <- dat_split_cv[[i]] |>
        mutate(model = map(data_train, ~ glm_probit(.)),
               logDose = list(newx$logDose),
               rate = map(model, ~
                          predict(., newdata = newx, type = "response"))
        ) |>
        dplyr::select(judgement_time, fold, logDose, rate) |>
        unnest(cols = c(logDose, rate)) |>
        group_nest(judgement_time, .key = "data_probit")

    dat_tbl_train <- dat_split_cv[[i]] |>
        mutate(tbl_train = map(data_train, ~ make_response_table(.))) |>
        dplyr::select(judgement_time, fold, tbl_train) |>
        unnest(cols = tbl_train) |>
        group_nest(judgement_time, .key = "tbl_train")

    dat_tbl_test <- dat_split_cv[[i]] |>
        mutate(tbl_test = map(data_test, ~ make_response_table(.))) |>
        dplyr::select(judgement_time, fold, tbl_test) |>
        unnest(cols = tbl_test) |>
        group_nest(judgement_time, .key = "tbl_test")

    dat_d50 |>
        left_join(dat_probit, by = "judgement_time") |>
        left_join(dat_tbl_train, by = "judgement_time") |>
        left_join(dat_tbl_test, by = "judgement_time")
})



########################################
## plot
########################################

design <- "
  AAAAAA
  AAAAAA
  #BBB##
"

p_cv <- lapply(1:2, function(i) {
    dat_plot[[i]] <- dat_plot[[i]] |>
        mutate(p_facet = pmap(list(data_probit = data_probit,
                                   tbl_train = tbl_train,
                                   tbl_test = tbl_test,
                                   judgement_time = judgement_time),
                              plot_curve_cv_facet,
                              axis_logDose = logDose[[i]],
                              axis_Dose = Dose[[i]])) |>
        mutate(p_merge = pmap(list(data_probit = data_probit,
                                   d50 = data_D50),
                         plot_curve_cv,
                         axis_logDose = logDose[[i]],
                         axis_Dose = Dose[[i]])) |>
        mutate(p = map2(p_facet, p_merge, function(x, y) {
                            wrap_plots(A = x,  B = y, design = design) &
                            plot_annotation(tag_levels = "A") &
                            theme(plot.tag = element_text(size = 28))
                        })) |>
        dplyr::select(judgement_time, p)
})


####################
# save as PDF
####################

p1 <- p_cv[[1]] |>
    dplyr::filter(judgement_time == 300)


WIDTH <- 7
HEIGHT <- 8

# multipanel plot
cairo_pdf(file = "Fig4.pdf",
          width = WIDTH, height = HEIGHT)
print(p1$p[[1]] &
      ggtitle(paste("Judgement time:", round(p1$judgement_time / 60, 2), "min")))
dev.off()


# plot fit probit curve in each judgement time
cairo_pdf(file = "result/fig_model1_animal_fitcurve_cv_toxic.pdf",
          width = WIDTH, height = HEIGHT, onefile = TRUE)
print(p_cv[[1]]$p)
dev.off()

cairo_pdf(file = "result/fig_model1_animal_fitcurve_cv_lethal.pdf",
          width = WIDTH, height = HEIGHT, onefile = TRUE)
print(p_cv[[2]]$p)
dev.off()


####################
## archive as zip file
####################

filepattern <- "fig_model1_animal_fitcurve_cv_(toxic|lethal)\\.pdf$"
zipfile <- "FigS3.zip"

if (file.exists(zipfile)) {
    file.remove(zipfile)
}


setwd("result")
filelist <- list.files(".", pattern = filepattern)
zip(zipfile = paste0("../", zipfile), files = filelist)
file.remove(filelist)
setwd("..")

