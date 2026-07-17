library(readxl)
library(ggplot2)
library(dplyr)
source("R/functions/tuning.R")

load_prep_data <- function(
    sheet_name,
    label
) {
  sim_cases_1year <- load_sim_data(
    "data/stella outputs/results/tuned-outbreak-prep.xlsx",
    sheet_name,
    "params"
  ) %>%
    janitor::clean_names() %>%
    group_by(
      isolation_proportion, 
      isolation_threshold, 
      hosp_rate_adult, 
      vaccination_scale_factor
    ) %>%
    summarise(
      sim_val = sum(sim_val)
    ) %>%
    ungroup()
  
  sim_cases_1year <- sim_cases_1year %>%
    filter(
      isolation_proportion == 0.1, 
      isolation_threshold == 46.1, 
      hosp_rate_adult == 0.038
    ) %>%
    select(vaccination_scale_factor, sim_val) %>%
    left_join(
      sim_cases_1year %>%
        group_by(
          vaccination_scale_factor
        ) %>%
        summarise(
          sim_val_upper = max(sim_val),
          sim_val_lower = min(sim_val),
          .groups = "drop"
        ),
      by = join_by("vaccination_scale_factor")
    ) %>%
    mutate(
      lead_in = label
    )
}

sim_cases <- rbind(
    load_prep_data("cases-1-year","1 year"),
    load_prep_data("cases-3-year","3 year")
  )
    
sim_cases <- load_sim_data(
  "data/stella outputs/results/tuned-outbreak-prep.xlsx",
  "cases-3-year",
  "params"
) %>%
  janitor::clean_names() 

sim_cases_total <- sim_cases%>%
  group_by(
    vaccination_scale_factor
  ) %>%
  summarise(
    sim_val = sum(sim_val)
  )

ggplot(sim_cases_total, 
       aes(
         x = vaccination_scale_factor - 1,
         y = sim_val)
       ) +
  # geom_ribbon(
  #   aes(ymax = sim_val_upper, ymin = sim_val_lower),
  #   fill = "red",
  #   alpha = 0.3
  #             ) +
  geom_line() +
  theme_bw() +
  labs(
    y = "Total Cases",
    x = "Change in Vaccination %"
  ) +
  scale_y_continuous(
    expand = c(0,0),
    limits = c(0, 700)
  ) +
  scale_x_continuous(
    labels = scales::percent
    #expand = c(0,0)
  )

ggplot(sim_cases,
       aes(x=day, y = sim_val, color = run)) +
  geom_line()