# Tune full model

library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(latex2exp)

source("R/functions/tuning.R")

################################################################################
#                                    Cases                                     #          
################################################################################


sim_cases <- load_sim_data(
  "data/stella outputs/basic-model-testing/full-isol-hosp-params.xlsx",
  "run-cases",
  "run-params"
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

################################################################################
#                                    Admissions                                     #          
################################################################################


sim_admissions <- load_sim_data(
  "data/stella outputs/basic-model-testing/full-isol-hosp-params.xlsx",
  "run-admissions",
  "run-params"
) 


obs_admissions <- read_excel(
  "data/model params/BSol-outbreak-cases-oct23toapril24.xlsx",
  sheet = "admissions"
) %>%
  mutate(
    Week = floor(Day/ 7) 
  ) %>%
  group_by(Week) %>%
  summarise(
    ObsVal = sum(Admissions)
  )

admission_errors <- calc_all_errors(
  sim_admissions,
  obs_admissions,
  max_lag = 7
)

################################################################################
#                                    Combined                                     #          
################################################################################


comb_errors <- incidence_errors %>%
  rename(
    case_error = Error,
    case_lag = Lag
    ) %>%
  left_join(
    admission_errors %>%
      select(
        -c("Isolation proportion","Isolation threshold","Hosp rate adult")
        )%>%
      rename(
        hosp_error = Error,
        hosp_lag = Lag
      ),
    by = join_by("Run")
  ) %>%
  mutate(
    case_error = case_error / sd(case_error),
    hosp_error = hosp_error / sd(hosp_error),
    Error =  case_error + 0.5 * hosp_error
  ) 

mins <- comb_errors %>%
  filter(
    Error == min(Error)
  ) %>%
  mutate(
    min_error_fraction = 0
  ) 

plot_tuned_params(
    comb_errors, 
    param_pairs = list(
      c("Isolation threshold", "Isolation proportion"),
      c("Hosp rate adult", "Isolation proportion"),
      c("Isolation threshold", "Hosp rate adult")
    )
) 

