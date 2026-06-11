library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(latex2exp)

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
#                     Varying isolation threshold                              #
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


################################################################################
#                              Varying both                                    #
################################################################################


isol_params_both <- read_excel(
  "data/stella outputs/basic-model-testing/SIR-isolation-params-both.xlsx",
  sheet = "run-data"
) %>%
  pivot_longer(
    cols = contains("Run"),
    names_to = "Run",
    values_to = "Infected"
  ) %>%
  left_join(
    read_excel(
      "data/stella outputs/basic-model-testing/SIR-isolation-params-both.xlsx",
      sheet = "run-params"
    ),
    by = join_by("Run")
  ) 

summary_stats <- isol_params_both %>%
  group_by(`isolation threshold`, `isolation proportion`) %>%
  filter(
    Infected >= 1,
    `isolation threshold` > 5,
    `isolation proportion` < 0.15) %>%
  summarise(
    `Total Cases` = sum(Infected) / 8.75,
    `Outbreak Length` = max(Day),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c("Total Cases", "Outbreak Length"),
    names_to = "Indicator",
    values_to = "Value"
  ) 

cases_plt <- summary_stats %>%
  filter(Indicator == "Total Cases") %>%
  ggplot(aes(x = `isolation threshold`, 
             y = `isolation proportion`,
             fill= Value)) + 
  geom_raster(interpolate = TRUE) +
  geom_contour(aes(z = Value),
               breaks = 422, 
               color = "white",
               lwd = 1.2) +
  theme_bw() +
  scale_x_continuous(
    expand = c(0,0)
  ) +
  scale_y_continuous(
    expand = c(0,0)
  ) +
  scale_fill_continuous(
    palette = viridis::viridis(30)
  )+
  facet_wrap(
    ~ Indicator
  ) +
  labs(
    fill = "Total Cases\n",
    y = "Isolation Proportion",
    x = "Isolation Threshold"
  ) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    strip.background = element_rect(fill="white")
  ) +
  annotate(
    "text", x = 37, y = 0.068, label = "422 cases",
    color = "white",
    angle = 33,
    size = 4.5
    )




length_plt <- summary_stats %>%
  filter(Indicator == "Outbreak Length") %>%
  ggplot(aes(x = `isolation threshold`, 
             y = `isolation proportion`,
             fill= Value)) + 
  geom_raster(interpolate = TRUE) +
  geom_contour(aes(z = Value),
               breaks = c(175), 
               color = "white",
               lwd = 1.2) +
  theme_bw() +
  scale_x_continuous(
    expand = c(0,0)
  ) +
  scale_y_continuous(
    expand = c(0,0)
  ) + 
  scale_fill_continuous(
    palette = viridis::magma(30),
    breaks = seq(150, 350, 100)
  ) +
  facet_wrap(
    ~ Indicator
  ) +
  labs(
    fill = "Outbreak Length\n        (Days)",
    x = "Isolation Threshold",
    y = ""
  ) +
  theme(
    legend.position = "bottom",
    strip.background = element_rect(fill="white")
  ) +
  annotate(
    "text", x = 45, y = 0.1, label = "175 days",
    color = "white",
    angle = 31,
    size = 4.5
  )



map_plts <- cowplot::plot_grid(cases_plt, length_plt)
map_plts



# extract contour values

get_contour <- function(
    data,
    indicator,
    value
) {
  data_i <- data %>%
    filter(
      Indicator == indicator
    )
  
  interp_out <- akima::interp(
    x = data_i$`isolation threshold`,
    y = data_i$`isolation proportion`,
    z = data_i$Value
  )
  
  cl <- contourLines(
    interp_out$x, 
    interp_out$y, 
    interp_out$z,
    levels = value)
  
  contour_df <- do.call(rbind, lapply(seq_along(cl), function(i) {
    data.frame(
      x = cl[[i]]$x,
      y = cl[[i]]$y,
      level = cl[[i]]$level,
      group = i
    )
  })) %>%
    mutate(
      Indicator = indicator
    )
  
  return(contour_df)
}
c1 <- get_contour(summary_stats, "Total Cases", 422)
c2 <- get_contour(summary_stats, "Outbreak Length", 175)
contours <- rbind(c1, c2)

# Find root
f1 <- approxfun(c1$x, c1$y)
f2 <- approxfun(c2$x, c2$y)

h <- function(x) f1(x) - f2(x)

xroot <- uniroot(h, interval = range(c1$x))$root
yroot <- f1(xroot)

cont_plt <- ggplot(contours, aes(x=x, y=y, color = Indicator)) +
  geom_line(lwd = 1.2) +
  geom_point(x = xroot, y = yroot, 
             color = "black", size = 3) +
  annotate("text", x = 78, y = 0.105, 
           label = "(71.8, 0.114)",
           size = 4.5) +
  theme_bw() +
  scale_color_manual(
    breaks = c("Total Cases", "Outbreak Length"),
    values = c(
      viridis::viridis(1, begin = 0.5),
      viridis::magma(1, begin = 0.5)
    )
  ) +
  labs(
    y = "Isolation Proportion",
    x = "Isolation Threshold",
    color = ""
  ) +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.15, 0.85),
    legend.background = element_blank(),
    legend.text = element_text(size = 12)
  )

cmb_plts <- cowplot::plot_grid(
  map_plts, cont_plt,
  ncol = 1,
  rel_heights = c(0.55, 0.45)
  )
cmb_plts

ggsave(
  "figures/model-params/SIR-isol-params.pdf",
  plot = cmb_plts,
  width = 7.5, height = 7
)