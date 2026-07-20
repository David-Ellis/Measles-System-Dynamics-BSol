library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
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
      run,
      vaccination_scale_factor,
      isolation_delay,
      isolation_proportion,
      hosp_rate_adult
    ) %>%
    summarise(
      sim_val = sum(sim_val)
    ) %>%
    ungroup()

  sim_cases_1year <- sim_cases_1year %>%
    filter(
      isolation_delay == 55.2,
      isolation_proportion == 0.094,
      hosp_rate_adult == 0.039
    ) %>%
    select(run, vaccination_scale_factor, sim_val) %>%
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
    #load_prep_data("cases-3-year","3 year"),
    load_prep_data("cases-5-year","5 year")
  ) %>%
  mutate(
    outcome = "Total Cases"
  )

sim_admissions <- rbind(
  load_prep_data("admissions-1-year","1 year"),
  #load_prep_data("cases-3-year","3 year"),
  load_prep_data("admissions-5-year","5 year")
) %>%
  mutate(
    outcome = "Total Hospital Admissions"
  )

plot_data <- rbind(sim_cases, sim_admissions)

plts <- list()

for (i in 1:2) {
  outcome_i <- unique(plot_data$outcome)[i]
  plt_i <- plot_data %>% 
    filter(outcome == outcome_i) %>%
    ggplot( 
         aes(
           x = vaccination_scale_factor - 1,
           y = sim_val,
           color = lead_in,
           linetype = lead_in,
           fill = lead_in)
  ) +
    geom_ribbon(
      aes(ymax = sim_val_upper, ymin = sim_val_lower),
      alpha = 0.3,
      lwd = 0,
      color = NA
    ) +
    geom_line(lwd = 1.3) +
    theme_bw() +
    labs(
      y = "",
      x = c("","Change in Number of Yearly MMR(V) Vaccinations")[i],
      color = "Outbreak after",
      fill = "Outbreak after",
      linetype = "Outbreak after"
    ) +
    scale_x_continuous(
      labels = scales::percent,
    ) +
    scale_color_manual(
      values = uob_colors
    ) + 
    scale_fill_manual(
      values = uob_colors
    ) + 
    theme(
      legend.position = c("inside","none")[i],
      legend.position.inside = c(0.88, 0.73),
      legend.background = element_rect(
        fill = NA
      ),
      #legend.title = element_blank(),
      legend.margin = margin(0, 0, 0, 0),
      strip.background = element_rect(fill="white"),
    ) + 
    facet_wrap(~outcome) +
    coord_cartesian(
      expand = F,
      ylim = c(0, c(300, 80)[i]))
  plts[[i]] <- plt_i 
}

out_plot <- cowplot::plot_grid(
  plts[[1]], NULL, plts[[2]], 
  rel_heights = c(1, -0.1, 1),
  ncol = 1
) 
out_plot

ggsave(
  "figures/results/outbreak-prep.pdf",
  width = 6, height = 4.5
)


# 
# 
# sim_cases %>% 
#   rename(cases=sim_val) %>% 
#   select(run, lead_in, cases) %>%
#   left_join(
#     sim_admissions %>% 
#       rename(admissions=sim_val) %>% 
#       select(run, lead_in, admissions),
#     by = join_by("run", "lead_in")
#   ) %>%
#   mutate(
#     check = cases / admissions
#   )