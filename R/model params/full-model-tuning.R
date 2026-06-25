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

incidence_errors <- calc_all_errors(
    sim_cases,
    obs_cases,
    max_lag = 40
)

err_range <- 1.1 * min(incidence_errors$Error)

min_error <- incidence_errors %>%
  filter(
    Error == min(Error)
  )

ggplot(incidence_errors,
       aes(x = `Isolation threshold`, 
           y = `Isolation proportion`,
           fill= log10(Error))) + 
  geom_raster(interpolate = TRUE) +
  geom_contour(aes(z = Error),
               breaks = c(err_range), 
               color = "white",
               lwd = 0.8,
               linetype = "dashed") + 
  geom_point(
    data = min_error,
    aes(x = `Isolation threshold`, 
        y = `Isolation proportion`),
    size = 2,
    color = "white"
  ) +
  scale_fill_continuous(
    palette = viridis::magma(30)
  )+
  theme_bw() +
  scale_x_continuous(
    expand = c(0,0)
  ) +
  scale_y_continuous(
    expand = c(0,0)
  ) 