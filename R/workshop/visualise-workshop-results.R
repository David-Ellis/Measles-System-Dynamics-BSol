library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(janitor)

source("R/functions/styles.R")

# Visualise most important flow factors

flow_factors <- read_excel(
  "data/workshop/flow-factors.xlsx"
) %>%
  clean_names()

flow_plt <- ggplot(
  flow_factors,
  aes(
    y = factor,
    x = votes,
    fill = theme,
    label = votes
  )) +
  geom_col() +
  geom_text(nudge_x = 0.3) +
  scale_x_continuous(
    expand = c(0,0),
    limits = c(0, 8)
  ) +
  theme_bw() +
  facet_grid(
    theme~.,
    #ncol=1,
    scales = "free_y",
    space = "free_y",
    switch = "y"
  )+
  labs(
    x = "Number of Votes",
    y = ""
  ) + 
  scale_fill_manual(
    values = uob_colors
  )+
  theme(
    #strip.background = element_rect(fill="white"),
    legend.position = "none",
    strip.placement = "outside",
    strip.text.y.left = element_text(face = "bold"),
    strip.background = element_blank()
  ) 

flow_plt

ggsave(
  "figures/workshop/flow-factors.pdf", 
  plot = flow_plt,
  width = 6, height = 4
  )

# Intervention ranking

int_data <- read_excel(
  "data/workshop/intervention-ranking.xlsx",
  sheet = "data"
) %>%
  clean_names() %>%
  pivot_longer(
    cols = contains("rank"),
    names_to = "rank",
    values_to = "votes"
      ) %>%
  mutate(
    rank = as.numeric(gsub("rank_", "", rank))
  ) %>%
  left_join(
    read_excel(
      "data/workshop/intervention-ranking.xlsx",
      sheet = "rank-points"
    ) %>%
      clean_names(),
    by = join_by("rank")
  ) %>%
  group_by(intervention) %>%
  summarise(
    total_points = sum(points * votes)
  ) %>%
  mutate(
    intervention = stringr::str_wrap(intervention, width = 55)
  ) %>%
  arrange(total_points)

int_data$intervention <- factor(
  int_data$intervention,
  levels = int_data$intervention
)

int_plot <- ggplot(
  int_data,
  aes(
    x = total_points, 
    y = intervention
  )
) +
  geom_col(
    fill = uob_colors[2]
  ) +
  theme_bw() +
  labs(
    y = "",
    x = "Total Points"
  ) +
  scale_x_continuous(
    expand = c(0,0),
    limits = c(0, 60)
  ) 

ggsave(
  "figures/workshop/intervention-voting.pdf", 
  plot = int_plot,
  width = 7, height = 3
)

