library(readxl)
library(dplyr)
library(ggplot2)
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
