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
  ) %>%
  # Add extra simulation data
  rbind(
    load_sim_data(
      "data/stella outputs/basic-model-testing/full-isol-hosp-params-extra.xlsx",
      "run-cases",
      "run-params"
    ) %>%
      mutate(
        RunNum = as.numeric(stringr::str_extract(Run, "\\d+")) + 1000,
        Run = paste("Run", RunNum)
      ) %>%
      select(-RunNum)
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
) %>%
  # Add extra simulation data
  rbind(
    load_sim_data(
      "data/stella outputs/basic-model-testing/full-isol-hosp-params-extra.xlsx",
      "run-admissions",
      "run-params"
    ) %>%
      mutate(
        RunNum = as.numeric(stringr::str_extract(Run, "\\d+")) + 1000,
        Run = paste("Run", RunNum)
      ) %>%
      select(-RunNum)
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
print(mins)

errors10perc <- comb_errors %>%
  filter(
    Error < 1.1*min(Error)
  ) 
print(range(errors10perc$`Isolation proportion`))
print(range(errors10perc$`Isolation threshold`))
print(range(errors10perc$`Hosp rate adult`))

param_pair <- c("Isolation proportion",
                "Hosp rate adult")

plot_data <- comb_errors %>%
  group_by(
    !!sym(param_pair[1]), 
    !!sym(param_pair[2])
  ) %>%
  summarise(
    min_error = min(Error),
    .groups = "drop"
  ) %>%
  ungroup() %>%
  mutate(
    min_error_fraction = 100 * (min_error - min(min_error)) / min(min_error)
  ) 


source("R/functions/tuning.R")

plt<-plot_tuned_params(
    comb_errors, 
    param_pairs = list(
      c("Isolation threshold", "Isolation proportion"),
      c("Hosp rate adult", "Isolation proportion"),
      c("Isolation threshold", "Hosp rate adult")
    ),
    c(10, 30)
)  
plt

ggsave("figures/model-params/three-param-beta-v2.pdf", plt, 
       bg = "white", width = 5, height = 4)