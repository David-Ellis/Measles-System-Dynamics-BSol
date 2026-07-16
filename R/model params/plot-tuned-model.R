# Plot tuned model

library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(latex2exp)

source("R/functions/styles.R")
################################################################################
#                           Load Observed Data                                 #
################################################################################

obs_cases <- read_excel(
  "data/model params/BSol-outbreak-cases-oct23toapril24.xlsx",
  sheet = "cases"
) %>%
  mutate(
    Infections = `Birmingham Confirmed Cases` + `Solihull Estimate`,
    Week = `Week Number`
  ) %>%
  select(
    Week, Infections
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
    Admissions = sum(N)
  )

obs_outcomes_weekly <- obs_cases %>%
  left_join(
    obs_admissions,
    by = join_by("Week")
  ) %>%
  pivot_longer(
    cols = c("Infections", "Admissions"),
    names_to = "Outcome",
    values_to = "Value"
  ) %>%
  mutate(
    # Fill NAs with 0
    Value = ifelse(is.na(Value),0, Value),
    Outcome = factor(Outcome, levels = c("Infections", "Admissions"))
  )

################################################################################
#                          Load Simulation Data                                #
################################################################################

cases <- read_excel(
  "data/stella outputs/results/tuned-full-output.xlsx",
  sheet = "infections"
  ) %>%
  mutate(
    Infections = `Baby infections` + `Child infections` + 
      `Adult infections`
  )

admissions <- read_excel(
  "data/stella outputs/results/tuned-full-output.xlsx",
  sheet = "hospitalisation"
) %>%
  rename(
    `Baby Admissions` = `Baby hospitalisation total`,
    `Child Admissions` = `Child hospitalisation total`,
    `Adult Admissions` = `Adult hospitalisation total`
  ) %>%
  mutate(
    Admissions = `Baby Admissions` + `Child Admissions` + `Adult Admissions`
  )

################################################################################
#                         Print outcomes by age                                #
################################################################################

cases %>% 
  summarise(across(contains("fections"), sum))

admissions %>% 
  summarise(across(contains("Admissions"), sum))

################################################################################
#                       Calculate weekly outcomes                              #
################################################################################

sir_outcomes_weekly <- read_excel(
  "data/stella outputs/results/tuned-SIR-output.xlsx"
) %>% 
  pivot_longer(
    cols = c(SIR, `SIR + isolation`),
    names_to = "Model",
    values_to = "Value"
  ) %>%
  mutate(
    Week = floor(Day/7) - 2
  ) %>%
  group_by(Model, Week) %>%
  summarise(
    Value = sum(Value),
    .groups = "drop"
  ) %>%
  mutate(
    Outcome = "Infections"
  )
  

sim_outcomes_weekly <- cases %>%
  left_join(
    admissions,
    by = join_by("Day")
    ) %>%
  select(Day, Infections, Admissions) %>%
  pivot_longer(
    cols = c(Infections, Admissions),
    names_to = "Outcome",
    values_to = "Value"
  ) %>%
  mutate(
    Week = floor(Day/7) + 3
  ) %>%
  group_by(Outcome, Week) %>%
  summarise(
    Value = sum(Value),
    .groups = "drop"
  ) %>% 
  mutate(
    Outcome = factor(Outcome, levels = c("Infections", "Admissions")),
    Model = "Full model"
  ) %>%
  rbind(
    sir_outcomes_weekly
  ) %>%
  filter(
    Week < 30,
    Value < 100
  ) 

################################################################################
#                    Plot simulated and observed outcomes                      #
################################################################################

plt <- ggplot(obs_outcomes_weekly, aes(x = Week, y = Value)) +
  geom_col(
    aes()
  ) +
  geom_line(
    data = sim_outcomes_weekly,
    lwd = 1.3,
    aes(color = Model, linetype = Model)
  ) +
  theme_bw() +
  facet_wrap(~Outcome, ncol = 1) +
  scale_y_continuous(
    expand = c(0,0)
  ) +
  scale_color_manual(
    values = c(uob_colors[1],  uob_colors[3], uob_colors[5])
  ) +
  scale_fill_manual(
    values = uob_colors[8]
  ) +
  theme(
    strip.background = element_rect(fill="white"),
    legend.position = "inside",
    legend.position.inside = c(0.85, 0.87),
    legend.background = element_rect(
      fill = NA
    ),
    legend.title = element_blank(),
    legend.margin = margin(0, 0, 0, 0)
  ) +
  labs(
    y = "",
    color = "Model",
    linetype = "Model",
    fill = ""
  )
plt
ggsave("figures/results/full-tuned.pdf", plt, width = 5, height = 4)
