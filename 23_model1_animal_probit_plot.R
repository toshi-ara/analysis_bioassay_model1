#!/usr/bin/env Rscript

library(dplyr)
library(purrr)
library(parallel)
library(ggplot2)
library(patchwork)


########################################
# load analyzed data
########################################

load("rda/model1_animal_probit.RData")
load("rda/common_variables.RData")

source("common_plots.R")
## label_reaction (TD50/LD50)
## labeler: function for title


select_time <- 4:8
jt <- 5

title_short <- c("TD", "LD")
title_sprintf <- c("Toxic", "Lethal")



####################
# plot TD50/LD50 and 95% CI at each judgement time
####################
.theme <- list(
    labs(x = "Judgement time (min)", y = "Dose (mg/kg)"),
    theme_bw(),
    theme(
        axis.text = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 16),
        strip.background = element_blank(),
        plot.title = element_text(size = 18, hjust = 0.5),
        legend.position = "bottom",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12)
    )
)

p <- mclapply(1:2, function(i) {
    ggplot(res_probit_params[[i]], aes(Time, Dose)) +
        geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.3) +
        geom_line() +
        ggtitle(label_reaction[i]) +
        .theme
}, mc.cores = n_mc)



####################
## plot and probit curve in each judgement time (sec)
##   judgement_time
##   D50 and 95% CI
####################
fit_probit_curve <- mclapply(1:2, function(i) {
    res_d50[[i]] |>
        transmute(
            Time = Time,
            plot = pmap(list(model,
                             response_tbl,
                             Dose, lwr, upr,
                             c("TD", "LD")[i],
                             paste(judgement_time, "sec")),
                        plot_fit_probit_curve))
}, mc.cores = n_mc)



####################
## plot and probit curve in specified time (jt)
##   jt (min)
####################
param <- mclapply(1:2, function(i) {
    res_d50[[i]] |>
        dplyr::filter(Time == jt)
})

p_jt <- lapply(1:2, function(i) {
    with(param[[i]],
         plot_fit_probit_curve(model[[1]], response_tbl[[1]],
                               Dose[[1]], lwr[[1]], upr[[1]],
                               title_short[i], 0)
    ) + ggtitle(sprintf("%s reaction (%d min)", title_sprintf[i], jt)) +
        theme(
            plot.title = element_text(size = 18, hjust = 0.5),
        )
})



####################
## multipanel plot
####################

p_comb <- wrap_plots(p_jt) / wrap_plots(p) +
    plot_annotation(tag_levels = "A") &
    theme(plot.tag = element_text(size = 28))



####################
## save as PDF
####################

## multipanel plot
cairo_pdf(file = "Fig2.pdf",
          width = 8, height = 8)
print(p_comb)
dev.off()


## Supplemental Figures
## plot fit probit curve in each judgement time
cairo_pdf(file = "result/fig_model1_animal_fitcurve_toxic.pdf",
          width = 5, height = 4, onefile = TRUE)
print(fit_probit_curve[[1]]$plot)
dev.off()

cairo_pdf(file = "result/fig_model1_animal_fitcurve_lethal.pdf",
          width = 5, height = 4, onefile = TRUE)
print(fit_probit_curve[[2]]$plot)
dev.off()



####################
## archive as zip file
####################

filepattern <- "fig_model1_animal_fitcurve_(toxic|lethal)\\.pdf$"
zipfile <- "FigS1.zip"

if (file.exists(zipfile)) {
    file.remove(zipfile)
}


setwd("result")
filelist <- list.files(".", pattern = filepattern)
zip(zipfile = paste0("../", zipfile), files = filelist)
file.remove(filelist)
setwd("..")

