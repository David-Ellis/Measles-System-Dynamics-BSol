library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(latex2exp)

################################################################################
#                Varying Initial Proportion Susceptible S(t=0)/N               #
################################################################################

data <- read_excel(
  "data/stella outputs/basic-model-testing/SIR-initial-susceptible.xlsx"
  ) %>%
  pivot_longer(
    cols = !contains("Day"),
    names_to = "S(t=0)/N",
    values_to = "I(t)"
  ) %>%
  mutate(
    `S(t=0)/N` = as.numeric(`S(t=0)/N`),
    group = factor(`S(t=0)/N`)
  ) %>%
  arrange(`S(t=0)/N`)%>%
  filter(
    !is.na(`I(t)`)
  )
  


# Plot all runs
data %>%
  filter(`S(t=0)/N` %in% seq(0.1,1,0.1)) %>%
  ggplot(aes(x = Day, y = `I(t)`, color = `S(t=0)/N`, group = group)) +
  geom_line(lwd = 1.05, alpha = 0.8) +
  theme_bw() +
  labs(y = TeX("Number of People Infected, $I(t)$"),
       color = TeX("$\\frac{S(t = 0)}{N}$"))

ggsave("figures/model-params/SIR-basic-trajectories.pdf", 
       width = 6, height = 3)

# Plot peak infection values

Imax = data %>%
  filter(
    `S(t=0)/N` > 0.075,
    `S(t=0)/N` <= 0.5
  ) %>%
  group_by(`S(t=0)/N`) %>%
  slice_max(`I(t)`, n = 1, with_ties = FALSE) %>%
  mutate(
    `log(Imax)` = log10(`I(t)`)
  ) %>%
  select(`S(t=0)/N`, Peak_Day = Day, `log(Imax)`) %>%
  pivot_longer(
    cols = c(Peak_Day, `log(Imax)`),
    names_to = "Outcome",
    values_to = "Value"
  )


Annotations <- data.frame(
  Outcome = c("log(Imax)", "Peak_Day", "Peak_Day", "log(Imax)"),
  x= c(0.09, 0.09, 0.149, 0.149), 
  y= c(-0.6, 60, 220, -1.7),
  label = c(
    "$\\frac{1}{R_0}$",
    "$\\frac{1}{R_0}$",
    "Probable BSol Range",
    "Probable BSol Range"
  ),
  angle = c(0,0, 90, 90),
  color = c("black", "black", "white", "white")
)

brum_susceptible_ribbon <- expand.grid(
  x = seq(0.1, 0.2, 0.001),
  y = seq(-400, 450, length.out = 10)
) %>%
  mutate(
    dist = abs(x - 0.15),
    alpha = exp(-(dist / 0.03)^2)
  )

ggplot(Imax, aes(x = `S(t=0)/N`, y = Value)) + 
  geom_raster(
    data = brum_susceptible_ribbon,
    aes(x = x, y = y, fill = alpha)
  ) +
  scale_fill_gradient(low = NA, high = "maroon", guide = "none") +
  geom_text(
    data = Annotations,
    aes(x, y, label = TeX(label), angle = angle, color = color),
    inherit.aes = FALSE, hjust = 0.5
  ) +
  scale_color_manual(
    values = c("black" = "black", "white" = "white")
    ) +
  geom_line(lwd = 1.05, alpha = 0.5) +
  geom_point(alpha = 0.8) +
  theme_bw() + 
  facet_wrap(
    ~Outcome, ncol = 1, scale = "free_y",
    labeller = as_labeller(c(
      "log(Imax)" = "Peak Infected Proportion log(I_max/N)",
      "Peak_Day" = "Days to Peak Infection"
      ))
    ) +
  geom_vline(xintercept = 1/13.5, 
             linetype = "dotted", lwd = 1.2) +
  ggh4x::facetted_pos_scales(
    y = list(
      scale_y_continuous(limits = c(-3.2, 0), expand = c(0,0)),
      scale_y_continuous(limits = c(0, 440), expand = c(0,0),
                         breaks = seq(0, 400, 100))
    )
  ) +
  labs(
    x = TeX("Proportion of the Population Initially Susceptible $\\frac{S(t = 0)}{N}$"),
    y = ""
  ) +
  scale_x_continuous(labels =  scales::label_percent()) +
  theme(
    strip.background = element_rect(fill="white"),
    legend.position = "none"
  )
    
ggsave("figures/model-params/SIR-max-infected.pdf", 
           width = 6, height = 5)

################################################################################
#            Varying Effective Reproduction Number Reff = alpha * R0           #
################################################################################

alphas <- c("1.00", "0.95", "0.90", "0.85", "0.80", "0.75")
t_max <- c()
outbreak_length <- c()
infected_proportion <- c()

data_path <- "data/stella outputs/basic-model-testing/SIR-Reff.xlsx"

for (alpha_i in alphas) {
  data_i <- read_excel(
    data_path,
    sheet = alpha_i
  )
  
  Imax_i <- max(data_i$infected)
  tmax_i <- data_i$Days[data_i$infected  == Imax_i]
  outbreak_length_i = max(data_i$Days[data_i$infected >= 0.05*Imax_i]) -
    min(data_i$Days[data_i$infected >= 0.05*Imax_i])
  Itotal_i <- data_i$susceptible[1] - data_i$susceptible[498]
  
  t_max <- c(t_max, tmax_i)
  outbreak_length <- c(outbreak_length, outbreak_length_i)
  infected_proportion <- c(infected_proportion, Itotal_i)
}

Reff_data <- data.frame(
  alpha = as.numeric(alphas),
  t_max = t_max,
  outbreak_length = outbreak_length,
  infected_proportion = infected_proportion
) %>%
  mutate(
    `Total Number Infected` = infected_proportion * 1300000,
    `Outbreak Length (Years)` = outbreak_length / 365
  ) %>%
  select(c(alpha, `Total Number Infected`, `Outbreak Length (Years)`)) %>%
  pivot_longer(
    cols = c("Total Number Infected", "Outbreak Length (Years)")
  ) 

ggplot(Reff_data, 
       aes(x = alpha, y = value)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  facet_wrap(~name, ncol = 1, scale = "free_y") +
  labs(
    y = "",
    x = TeX("Reproduction Number Scaling Factor, $\\alpha$")
  )+
  scale_x_continuous(labels =  scales::label_percent()) +
  theme(
    strip.background = element_rect(fill="white"),
    legend.position = "none"
  )

ggsave("figures/model-params/SIR-Reff.pdf", 
       width = 6, height = 5)

################################################################################
#                     Varying isolation proportion                             #
################################################################################

isol_props <- read_excel(
  "data/stella outputs/basic-model-testing/SIR-isolation-params-single.xlsx",
  sheet = "isol-prop-runs"
  ) %>%
  pivot_longer(
    cols = contains("Run"),
    names_to = "Run",
    values_to = "Infected"
  ) %>%
  left_join(
    read_excel(
      "data/stella outputs/basic-model-testing/SIR-isolation-params-single.xlsx",
      sheet = "isol-prop-params"
    ),
    by = join_by("Run")
  ) %>%
  filter(
    Run != "Run 1"
  ) %>%
  mutate(
    group = factor(`isolation proportion`)
  )

ggplot(isol_props, aes(x = Day, y = Infected, color = `isolation proportion`,
                       group = group)) +
  geom_line() +
  theme_bw() +
  scale_x_continuous(
    limits = c(0, 150),
    expand = c(0,0)
  ) +
  scale_y_continuous(
    limits = c(0, 30),
    expand = c(0,0)
  ) +
  labs(
    x = "Days Since Outbreak Start",
    y = TeX("Number of People Infected, $I(t)$"),
    color = "Isolation Proportion"
  ) +
  theme(
    #legend.direction = "horizontal",
    legend.position = "top",
    legend.margin = margin_auto(0.5),
    margins = margin_auto(10)
  )

ggsave("figures/model-params/SIR-isol-prop.pdf", 
       width = 6, height = 3)



################################################################################
#                     Varying isolation threshold                             #
################################################################################

isol_thresh <- read_excel(
  "data/stella outputs/basic-model-testing/SIR-isolation-params-single.xlsx",
  sheet = "isol-thresh-runs"
) %>%
  pivot_longer(
    cols = contains("Run"),
    names_to = "Run",
    values_to = "Infected"
  ) %>%
  left_join(
    read_excel(
      "data/stella outputs/basic-model-testing/SIR-isolation-params-single.xlsx",
      sheet = "isol-thresh-params"
    ),
    by = join_by("Run")
  ) %>%
  filter(
    Run != "Run 1"
  ) %>%
  mutate(
    group = factor(`isolation threshold`)
  )

ggplot(isol_thresh, aes(x = Day, y = Infected, color = `isolation threshold`,
                       group = group)) +
  geom_line() +
  theme_bw() +
  scale_x_continuous(
    limits = c(0, 150),
    expand = c(0,0)
  ) +
  scale_y_continuous(
    limits = c(0, 30),
    expand = c(0,0)
  ) +
  labs(
    x = "Days Since Outbreak Start",
    y = TeX("Number of People Infected, $I(t)$"),
    color = "Isolation Threshold"
  ) +
  theme(
    #legend.direction = "horizontal",
    legend.position = "top",
    legend.margin = margin_auto(0.5),
    margins = margin_auto(10)
  )

ggsave("figures/model-params/SIR-isol-thresh.pdf", 
       width = 6, height = 3)