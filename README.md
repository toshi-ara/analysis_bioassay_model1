# files and variables
## 11_process_data.R
- rda/animal_data.RData
    - dat_raw

- rda/common_variables.RData
    - dose, log_dose: list of vector (unique dose)
    - x_seq: list of vector for probit curve
    - x_seq_df: list of data.frame for probit curve
    - n_mc: CPU core number for parallel::mclapply

## 21_model1_animal_probit.R
- rda/model1_animal_probit.RData
    - dat_response: tibble including response at each judgment time
    - res_d50: tibble including various data
    - res_probit_params: parameters by probit model

## 22_model1_animal_probit_table.R
- Table/Table1.tex                         ## frequency
- Table/Table2.tex                         ## TD50, LD50 (95%CI)
- result/parameter.json                    ## Mean, Sigma
- csv/model1_parameters_distribution.csv   ## all data of parameters

## 23_model1_animal_probit_plot.R
- Fig2.pdf
- FigS1.zip  ## dose-response curve at each judgment time (toxic/lethal)
    - fig_model1_animal_fitcurve_toxic.pdf
    - fig_model1_animal_fitcurve_lethal.pdf

## 31_model1_animal_boot.R
- rda/model1_animal_probit_boot.RData")
    - res_boot: list of boot function

## 32_model1_animal_boot_table.R
- rda/model1_animal_probit_boot_params.RData
    - res_boot_plot
- Table/Table3.tex                        ## estimated parameters (95%CI)
- csv/model1_parameters_boot_params.csv   ## estimated parameters (95%CI)

## 33_model1_animal_boot_plot.R
- Fig3.pdf    ## histogram of parameters (bootstrap sample)
- FigS2.zip   ## histogram of parameters (bootstrap sample)
    - fig_model1_animal_parameter_boot_toxic.pdf
    - fig_model1_animal_parameter_boot_lethal.pdf

## 41_model1_animal_cv.R
- rda/animal_data_split_cv.RData
    - dat_split_cv
    - k                  ## k-folds

## 42_model1_animal_cv_table.R
- rda/model1_animal_probit_cv.RData
    - res_score_sv
- Table/Table4.tex                        ## D50, Brier score, Chrsq-test

## 43_model1_animal_cv_plot.R
- Fig4.pdf    ## probit curve by training data / scatter plot by test data
- FigS3.zip   ## probit curve by training data / scatter plot by test data
    - fig_model1_animal_fitcurve_cv_toxic.pdf
    - fig_model1_animal_fitcurve_cv_lethal.pdf

## 51_model1_simulation.R
- rda/model1_simulation.RData
    - jt, n, n_group, N, seed
    - dat_sim_response
    - res_d50, d50, d50_summary

## 52_model1_simulation_table.R
- Table/Table5.tex
- csv/model1_simulation_summary.csv

## 53_model1_simulation_plot.R
- Fig5.pdf    ## vilolin plot by Monte Carlo simulation
- FigS4.zip   ## dose-response curve
    - fig_model1_animal_simulation_toxic_##.pdf
    - fig_model1_animal_simulation_lethal_##.pdf

## 61_model1_compress_csv.R
- SupplementalData.zip
    - above 4 csv files

