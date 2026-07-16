library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(latex2exp)
library(geomtextpath)
source("R/functions/tuning.R")

sim_cases <- load_sim_data(
  "data/stella outputs/results/outbreak-response-variation.xlsx",
  "cases",
  "run-params"
) %>%
  janitor::clean_names() %>%
  group_by(
    isolation_proportion, isolation_threshold
  ) %>%
  summarise(
    sim_val = sum(sim_val),
    .groups = "drop"
    ) %>%
  mutate(
    outcome = "Total Cases",
    base_sim = 373
  )

sim_admissions <- load_sim_data(
  "data/stella outputs/results/outbreak-response-variation.xlsx",
  "admissions",
  "run-params"
) %>%
  janitor::clean_names() %>%
  group_by(
    isolation_proportion, isolation_threshold
  ) %>%
  summarise(
    sim_val = sum(sim_val),
    .groups = "drop"
  ) %>%
  mutate(
    outcome = "Total Admissions",
    base_sim = 124.6 
  )

sim_data <- rbind(sim_cases, sim_admissions) %>%
  mutate(
    frac_diff = 100 * sim_val / base_sim
  ) %>%
  filter(
    isolation_proportion > 0.08
  )

plts <- list()
palletes <- list(
  "Total Cases" = viridis::magma(40),
  "Total Admissions" = viridis::mako(40)
)
ylab <- list(
  "Total Cases" = "Isolation Threshold",
  "Total Admissions" = ""
)

base_sim <- data.frame(
  isolation_proportion = c(0.10),
  isolation_threshold = c(46.1),
  frac_diff = NA,
  sim_val = NA
)

for (outcome_i in c("Total Cases", "Total Admissions")) {
  plts[[outcome_i]] <- sim_data %>% 
    filter(
      outcome == outcome_i
    ) %>%
    ggplot(
      aes(
        x = isolation_proportion,
        y = isolation_threshold,
        fill = frac_diff,
        z = sim_val
      )
    ) +
      geom_raster(
        interpolate = T
      ) +
      geom_textcontour(
        #aes(label = paste0(after_stat(level), "%")),
        linetype = "dashed",
        color = "white"
      ) +
      # Plot base sim value
      # geom_point(
      #   data = base_sim,
      # ) +
      facet_wrap(~outcome) +
      theme_bw() +
      scale_x_continuous(expand = c(0,0))  +
      scale_y_continuous(expand = c(0,0)) +
      scale_fill_continuous(palette = palletes[[outcome_i]]) +
      labs(
        y = ylab[[outcome_i]],
        x = "Isolation Proportion",
        fill = outcome_i
      ) +
      theme(
        legend.position = "none",
        strip.background = element_rect(
          fill="white"
          ),
        plot.background = element_rect(fill='transparent', color = NA)
      ) 
    
}

out_plot <- cowplot::plot_grid(
  plts[[1]], NULL, plts[[2]], 
  rel_widths = c(1, -0.1, 1),
  nrow = 1
  ) 

ggsave(
  "figures/results/outbreak-response.pdf",
  width = 6, height = 3
)