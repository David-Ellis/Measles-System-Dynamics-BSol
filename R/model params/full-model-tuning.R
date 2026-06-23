# Tune full model

library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(latex2exp)

source("R/functions/tuning.R")

sim_cases <- load_sim_data(
  "data/stella outputs/basic-model-testing/full-isolation-params.xlsx",
  "run-data",
  "run-params"
  ) 

obs_cases <- read_excel(
  "data/model params/BSol-outbreak-cases-oct23toapril24.xlsx",
  sheet = "Data"
) %>%
  mutate(
    ObsVal = `Birmingham Confirmed Cases` + `Solihull Estimate`,
    Week = `Week Number`
  ) %>%
  select(
    Week, ObsVal
  )

for (lag in 1:60) {
  print(calc_error(sim_cases, obs_cases, 50, lag))
}