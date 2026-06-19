# Tune full model

library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(latex2exp)

isol_params_both <- read_excel(
  "data/stella outputs/basic-model-testing/full-isolation-params.xlsx",
  sheet = "run-data"
) %>%
  pivot_longer(
    cols = contains("Run"),
    names_to = "Run",
    values_to = "Infections"
  ) %>%
  left_join(
    read_excel(
      "data/stella outputs/basic-model-testing/full-isolation-params.xlsx",
      sheet = "run-params"
    ),
    by = join_by("Run")
  ) 

summary_stats <- isol_params_both %>%
  group_by(`Isolation threshold`, `Isolation proportion`) %>%
  filter(
    Infections >= 0.5,
    `Isolation threshold` > 5,
    `Isolation proportion` > 0.07,
    `Isolation proportion` < 0.15) %>%
  summarise(
    `Total Cases` = sum(Infections)/1000,
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
  ggplot(aes(x = `Isolation threshold`, 
             y = `Isolation proportion`,
             fill= Value)) + 
  geom_raster(interpolate = TRUE) +
  geom_contour(aes(z = Value),
               breaks = 0.422, 
               color = "white",
               lwd = 1.2) +
  theme_bw() +
  scale_x_continuous(
    expand = c(0,0)
  ) +
  scale_y_continuous(
    expand = c(0,0),

  ) +
  scale_fill_continuous(
    palette = viridis::viridis(30),
  ) +
  facet_wrap(
    ~ Indicator
  ) +
  labs(
    fill = "Total Cases\n(Thousands)",
    y = "Isolation Proportion",
    x = "Isolation Threshold"
  ) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    strip.background = element_rect(fill="white")
  ) +
  annotate(
    "text", x = 37, y = 0.121, label = "422 cases",
    color = "white",
    angle = 40,
    size = 4.5
  )


length_plt <- summary_stats %>%
  filter(Indicator == "Outbreak Length") %>%
  ggplot(aes(x = `Isolation threshold`, 
             y = `Isolation proportion`,
             fill= Value)) + 
  geom_raster(interpolate = TRUE) +
  scale_fill_continuous(
    palette = viridis::magma(30),
    limits = c(56, 522),
    breaks = c(100,300,500)
  ) +
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
    "text", x = 45, y = 0.12, label = "175 days",
    color = "white",
    angle = 12,
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
    x = data_i$`Isolation threshold`,
    y = data_i$`Isolation proportion`,
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
c1 <- get_contour(summary_stats, "Total Cases", 0.422)
c2 <- get_contour(summary_stats, "Outbreak Length", 175)
contours <- rbind(c1, c2)


# Find root
f1 <- approxfun(c1$x, c1$y)
f2 <- approxfun(c2$x, c2$y)

h <- function(x) f1(x) - f2(x)

xroot <- uniroot(h, interval = c(
  max(min(c1$x), min(c2$x)), 
  min(max(c1$x), max(c2$x))
  )
  )$root
yroot <- f1(xroot)

cont_plt <- ggplot(contours, aes(x=x, y=y, color = Indicator)) +
  geom_line(lwd = 1.2) +
  geom_point(x = xroot, y = yroot, 
             color = "black", size = 3) +
  annotate("text", x = xroot-4.5, y = yroot+0.004, 
           label = paste0("(", round(xroot, 1), ", ", round(yroot, 2),")"),
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
  "figures/model-params/full-model-isol-params.pdf",
  plot = cmb_plts,
  width = 7.5, height = 7
)

################################################################################
#                               Tuned Model                                    #
################################################################################

tuned_data_full <- read_excel(
  "data/stella outputs/basic-model-testing/tuned-full-output.xlsx"
) %>%
  mutate(
    `Total Infections` = `Baby infections`+`Child infections`+`Adult infections`
  ) %>%
  select(Day, `Total Infections`) %>%
  mutate(
    week_number = floor(Day / 7)+5,
    testing_curve = `Total Infections`
  ) %>%
  pivot_longer(
    cols = c("testing_curve"),
    names_to = "indicator",
    values_to = "value"
  ) %>%
  group_by(week_number, indicator) %>%
  summarise(
    value = sum(value)
  ) %>%
  mutate(
    indicator = case_when(
      indicator == "daily_infections" ~ "Weekly Infections (Full)",
      indicator == "testing_curve" ~ "Full Model",
      TRUE ~ "Error"
    )
  )


tuned_data_SIR <- read_excel(
  "data/stella outputs/basic-model-testing/tuned-SIR-output.xlsx"
)  %>%
  mutate(
    week_number = floor(Day / 7),
    testing_curve = Infections
  ) %>%
  pivot_longer(
    cols = c("testing_curve"),
    names_to = "indicator",
    values_to = "value"
  ) %>%
  group_by(week_number, indicator) %>%
  summarise(
    value = sum(value)
  ) %>%
  mutate(
    indicator = case_when(
      indicator == "daily_infections" ~ "Weekly Infections (SIR)",
      indicator == "testing_curve" ~ "SIR + Isolation",
      TRUE ~ "Error"
    )
  )

# Combine simple and full model outputs
model_data <- rbind(
  tuned_data_full,
  tuned_data_SIR
)

cases <- read_excel(
  "data/model params/BSol-outbreak-cases-oct23toapril24.xlsx",
  sheet = "Data"
) %>%
  janitor::clean_names() %>%
  rename(
    Birmingham = `birmingham_confirmed_cases`,
    Solihull = `solihull_estimate`
  ) %>%
  pivot_longer(
    cols = c("Birmingham", "Solihull"),
    names_to = "local_authority",
    values_to = "value"
  ) %>%
  mutate(
    outcome = "Confirmed Measles Cases",
    moving_average = TTR::SMA(value, n = 8.75)
  ) %>%
  group_by(week_number, outcome) %>%
  summarise(
    value = sum(value)
  )

ggplot(cases, aes(x = week_number, y = value)) +
  geom_col() +
  geom_line(
    data = model_data %>% 
      filter(indicator %in% c("Full Model", "SIR + Isolation")), 
    aes(x = week_number, y = value, color = indicator),
    lwd = 1.05
  ) +
  theme_bw() +
  scale_x_continuous(
    limits = c(0, 27),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0, 80),
    expand = c(0, 0)
  ) +
  labs(
    y = "Lab Confirmed Measles Cases",
    x = "Week Number",
    color = ""
  ) +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.15, 0.90),
    legend.background = element_rect(fill=NA)
  )

ggsave("figures/model-params/full-tuned.pdf", 
       width = 6, height = 3)


read_excel(
  "data/stella outputs/basic-model-testing/tuned-full-output.xlsx",
  sheet = 1
) %>%
  summarise(
    `Baby infections` = sum(`Baby infections`),
    `Child infections` = sum(`Child infections`),
    `Adult infections` = sum(`Adult infections`)
  )

read_excel(
  "data/stella outputs/basic-model-testing/tuned-full-output.xlsx",
  sheet = 2
) %>%
  summarise(
    `Baby hospitalisation` = sum(`Baby hospitalisation`),
    `Child hospitalisation` = sum(`Child hospitalisation`),
    `Adult hospitalisation` = sum(`Adult hospitalisation`)
  ) %>%
  mutate(
    Total = `Baby hospitalisation` + `Child hospitalisation` + `Adult hospitalisation`
  )