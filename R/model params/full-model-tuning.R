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

# Omega sensitivity test

isol_delays <- list()
isol_prop <- list()
admin_prop <- list()

omegas <- seq(0, 1, 0.1)
for (omega_i in omegas){
  
  comb_errors_i <- comb_errors %>%
    mutate(
      Error_i = case_error + omega_i * hosp_error
    ) %>%
    filter(
      Error_i == min(Error_i, na.rm = T)
    )
  
  isol_delays[[as.character(omega_i)]] <- comb_errors_i$`Isolation delay`
  isol_prop[[as.character(omega_i)]] <- comb_errors_i$`Isolation proportion`
  admin_prop[[as.character(omega_i)]] <- comb_errors_i$`Adult Admission Proportion`
}

omega_sens_data <-
  data.frame(
    Omega = omegas,
    `Isolation delay` = unlist(isol_delays),
    `Isolation proportion` = unlist(isol_prop),
    `Adult Admission\nProportion` = unlist(admin_prop),
    check.names = F
  ) %>%
  pivot_longer(
    cols = c("Isolation delay", 
             "Isolation proportion", 
             "Adult Admission\nProportion"),
    names_to = "Parameter",
    values_to = "Value")

ggplot(omega_sens_data, aes(x = Omega, y = Value)) +
  geom_line(color = uob_colors[3]) +
  geom_point() +
  theme_bw() +
  facet_grid(
    Parameter~.,
    scales = "free_y",
    #space = "free_y",
    switch = "y") +
  theme(
    #strip.background = element_rect(fill="white"),
    legend.position = "none",
    strip.placement = "outside",
    #strip.text.y.left = element_text(face = "bold"),
    strip.background = element_blank()
  ) +
  labs(
    y = "",
    x = TeX("Hospital Adission Error Weight, \\omega")
  )

ggsave("figures/results/omega-sensitivity.pdf",
       width = 6, height = 5)





