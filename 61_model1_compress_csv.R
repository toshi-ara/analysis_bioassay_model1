#!/usr/bin/env Rscript

filelist <- c(
    "model1_parameters_boot_params.csv",
    "model1_parameters_distribution.csv",
    "model1_simulation_raw.csv",
    "model1_simulation_summary.csv",
    "model1_parameters_compareRMSE.csv",
    "model1_parameters_compareD50.csv",
    "model1_parameters_robust_boot_params.csv"
)

zipfile <- "SupplementalData.zip"

if (file.exists(zipfile)) {
    file.remove(zipfile)
}


setwd("csv")
zip(zipfile = paste0("../", zipfile), files = filelist)
setwd("..")

