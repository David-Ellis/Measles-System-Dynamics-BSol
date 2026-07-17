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
  "data/stella outputs/basic-model-testing/full-isol-hosp-params-zoom.xlsx",
  "run-cases",
  "run-params"
  ) 
# %>%
#   # Add extra simulation data
#   rbind(
#     load_sim_data(
#       "data/stella outputs/basic-model-testing/full-isol-hosp-params-extra.xlsx",
#       "run-cases",
#       "run-params"
#     ) %>%
#       mutate(
#         RunNum = as.numeric(stringr::str_extract(Run, "\\d+")) + 1000,
#         Run = paste("Run", RunNum)
#       ) %>%
#       select(-RunNum)
#   )

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
  "data/stella outputs/basic-model-testing/full-isol-hosp-params-zoom.xlsx",
  "run-admissions",
  "run-params"
) 
# %>%
#   # Add extra simulation data
#   rbind(
#     load_sim_data(
#       "data/stella outputs/basic-model-testing/full-isol-hosp-params-extra.xlsx",
#       "run-admissions",
#       "run-params"
#     ) %>%
#       mutate(
#         RunNum = as.numeric(stringr::str_extract(Run, "\\d+")) + 1000,
#         Run = paste("Run", RunNum)
#       ) %>%
#       select(-RunNum)
#   )


obs_admissions <- read_excel(
  "data/model params/BSol-outbreak-cases-oct23toapril24.xlsx",
  sheet = "admissions"
) %>%
  mutate(
    Week = floor(Day/ 7) 
  ) %>%
  group_by(Week) %>%
  summarise(
    ObsVal = sum(N)
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
        -c("Isolation proportion","Isolation delay","Hosp rate adult")
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
  ) %>%
  rename(
    `Adult Admission Proportion` = `Hosp rate adult`
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
print(range(errors10perc$`Isolation delay`))
print(range(errors10perc$`Adult Admission Proportion`))

source("R/functions/tuning.R")
palette <- ggpubr::get_palette((c("#FFFFFF", uob_colors[3])), 20)
plt<-plot_tuned_params(
    comb_errors, 
    param_pairs = list(
      c("Isolation delay", "Isolation proportion"),
      c("Adult Admission Proportion", "Isolation proportion"),
      c("Isolation delay", "Adult Admission Proportion")
    ),
    c(10, 30, 100),
    palette = palette
)  
plt

ggsave("figures/model-params/three-param-contour-zoom.pdf", plt,
       bg = "white", width = 6, height = 5)