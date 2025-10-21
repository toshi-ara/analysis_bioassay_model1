#!/usr/bin/env Rscript

library(dplyr)
library(purrr)
library(parallel)
library(ggplot2)


########################################
# load analyzed data
########################################

source("common_plots.R")
load("rda/model1_simulation.RData")


## use subscript in strip of figure
d50 <- d50 |>
    mutate(reaction = factor(reaction, labels = label_reaction))
d50_summary <- d50_summary |>
    mutate(reaction = factor(reaction, labels = label_reaction))


## violin plot of TD50/LD50 at each time
seed_p <- 123  # seed number for sinaplot
p_d50 <- plot_d50_time_violin(d50, d50_summary, seed_p)


## probit analysis / plot of each simulation
res_d50 <- lapply(1:2, function(i) {
    res_d50[[i]] |>
        mutate(plot = pmap(list(model, response_tbl,
                                Dose, lwr, upr,
                                c("TD", "LD")[i],
                                paste(Time, "min")),
                           plot_fit_probit_curve))
})



####################
## save as PDF
####################

cairo_pdf(file = "Fig5.pdf", width = 8, height = 4)
print(p_d50)
dev.off()


## plot fit probit curve in each simulation
for (i in seq_along(jt)) {
    ## TD50
    cairo_pdf(file = sprintf("result/fig_model1_simulation_toxic_%02d.pdf", jt[i]),
              width = 5, height = 4, onefile = TRUE)
        res_d50[[1]] |>
            dplyr::filter(Time == jt[i]) |>
            pull(plot) |>
            print()
    dev.off()

    ## LD50
    cairo_pdf(file = sprintf("result/fig_model1_simulation_lethal_%02d.pdf", jt[i]),
              width = 5, height = 4, onefile = TRUE)
        res_d50[[2]] |>
            dplyr::filter(Time == jt[i]) |>
            pull(plot) |>
            print()
    dev.off()
}


####################
## archive as zip file
####################

filepattern <- "fig_model1_simulation_(toxic|lethal)_\\d+\\.pdf$"
zipfile <- "FigS6.zip"

if (file.exists(zipfile)) {
    file.remove(zipfile)
}


setwd("result")
filelist <- list.files(".", pattern = filepattern)
zip(zipfile = paste0("../", zipfile), files = filelist)
file.remove(filelist)
setwd("..")

