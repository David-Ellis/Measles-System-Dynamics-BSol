library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(latex2exp)

source("R/functions/tuning.R")

sim_cases <- read_excel(
  "data/stella outputs/basic-model-testing/SIR-isolation-params-both.xlsx",
  sheet = "run-data2"
) %>%
  pivot_longer(
    cols = contains("Run"),
    names_to = "Run",
    values_to = "Infections"
  ) %>%
  left_join(
    read_excel(
      "data/stella outputs/basic-model-testing/SIR-isolation-params-both.xlsx",
      sheet = "run-params2"
    ),
    by = join_by("Run")
  ) %>%
  rename(
    SimVal = Infections
  )

obs_cases <- read_excel(
  "data/model params/BSol-outbreak-cases-oct23toapril24.xlsx",
  sheet = "cases"
) %>%
  mutate(
    ObsVal = `Birmingham Confirmed Cases` + `Solihull Estimate`,
    Week = `Week Number`
  ) %>%
  select(
    Week, ObsVal
  )

incidence_errors <- calc_all_errors(
  sim_cases,
  obs_cases,
  max_lag = 7
)

mins <- incidence_errors %>%
  filter(
    Error == min(Error)
  ) %>%
  mutate(
    min_error_fraction = 0
  )
print(mins)


errors10perc <- incidence_errors %>%
  filter(
    Error < 1.1*min(Error)
  ) 
print(range(errors10perc$`isolation proportion`))
print(range(errors10perc$`isolation threshold`))