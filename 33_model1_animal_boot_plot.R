#!/usr/bin/env Rscript

library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(patchwork)



########################################
## load analyzed data
########################################

load("rda/model1_animal_probit_boot_params.RData")
load("rda/common_variables.RData")

source("common_scripts.R")


## Judgement time in Table/Figure (minute)
select_time <- 4:8



########################################
## plot
########################################

.theme <- list(
    theme(
        axis.text = element_text(size = 11, color = "black"),
        axis.title = element_text(size = 14),
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 11),
        strip.background = element_rect(fill = "transparent"),
        plot.title = element_text(size = 16, hjust = 0.5),
        panel.grid.minor = element_blank()
    )
)


p <- lapply(res_boot_plot, function(x) {
    tmp <- x |>
        dplyr::filter(judgement_time %in% (select_time * 60)) |>
        mutate(Time = judgement_time / 60)

    df_boot_plot <- tmp |>
        dplyr::select(Time, t) |>
        mutate(dat = map(t, ~ {
                pivot_longer(., cols = everything(),
                             names_to = "Parameter", values_to = "Value")
        })) |>
        dplyr::select(Time, dat) |>
        unnest(cols = dat) |>
        mutate(Parameter = factor(Parameter,
                                  levels = c("Mean", "Sigma"),
                                  labels = c("mu", "sigma")))

    ci <- tmp |>
        dplyr::select(Time, param) |>
        unnest(cols = param) |>
        mutate(Parameter = factor(Parameter,
                                  levels = c("Mean", "Sigma"),
                                  labels = c("mu", "sigma")))

    p <- ggplot(df_boot_plot, aes(x = Value)) +
        geom_histogram(fill = "gray", color = "black") +
        geom_vline(data = ci, aes(xintercept = t0), linetype = "dashed") +
        geom_vline(data = ci, aes(xintercept = lwr)) +
        geom_vline(data = ci, aes(xintercept = upr)) +
        labs(x = "Parameter Value", y = "Frequency") +
        facet_grid(Time ~ Parameter, labeller = label_parsed, scales = "free") +
        theme_bw() +
        .theme

    return(p)
})

p_comb <- wrap_plots(p) +
    plot_annotation(tag_levels = "A") &
    theme(plot.tag = element_text(size = 28))



WIDTH <- 10
HEIGHT <- 7


cairo_pdf(file = "Fig3.pdf",
          width = WIDTH, height = HEIGHT)
print(p_comb)
dev.off()



####################
## for SFig
####################

plot_param_time <- function(x, param, jt, theme) {
    df_boot_plot <- x |>
        pivot_longer(cols = everything(),
                     names_to = "Parameter",
                     values_to = "Value")

    p <- ggplot(df_boot_plot, aes(x = Value)) +
        geom_histogram(fill = "gray", color = "black") +
        geom_vline(data = param, aes(xintercept = t0), linetype = "dashed") +
        geom_vline(data = param, aes(xintercept = lwr)) +
        geom_vline(data = param, aes(xintercept = upr)) +
        labs(x = "Parameter Value", y = "Frequency",
             title = sprintf("Judgement time: %d sec", jt)) +
        facet_wrap(~ Parameter, scale = "free") +
        theme_bw() +
        theme
    return(p)
}


p_all <- lapply(res_boot_plot, function(x) {
    x |>
        mutate(p = pmap(list(x = t, param = param, jt = judgement_time),
                        plot_param_time,
                        theme = .theme)
        )
})


WIDTH <- 6
HEIGHT <- 3

cairo_pdf(file = "result/fig_model1_animal_parameter_boot_toxic.pdf",
          width = WIDTH, height = HEIGHT, onefile = TRUE)
print(p_all[[1]]$p)
dev.off()

cairo_pdf(file = "result/fig_model1_animal_parameter_boot_lethal.pdf",
          width = WIDTH, height = HEIGHT, onefile = TRUE)
print(p_all[[2]]$p)
dev.off()


####################
## archive as zip file
####################

filepattern <- "fig_model1_animal_parameter_boot_(toxic|lethal)\\.pdf$"
zipfile <- "FigS2.zip"

if (file.exists(zipfile)) {
    file.remove(zipfile)
}


setwd("result")
filelist <- list.files(".", pattern = filepattern)
zip(zipfile = paste0("../", zipfile), files = filelist)
file.remove(filelist)
setwd("..")

