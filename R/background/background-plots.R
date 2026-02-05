library(ggplot2)
library(dplyr)

################################################################################
#                  Plot cases and MMR 1-dose vaccine rate                      #
################################################################################

cases <- read.csv("data/background/EnglandMeaslesCases.csv") %>%
  rename(
    Value = cases,
    Year = year
  ) %>%
  mutate(
    data_type = "Lab-confirmed measles cases",
    Area = "England"
  )

mmr_all <- read.csv("data/background/indicators-CountiesUAsfromApr2023.data.csv")

mmr_eng <- mmr_all %>%
  filter(
    Area.Name == "England",
    Category.Type == "",
    Parent.Name == ""
    ) %>%
  select(
    Time.period.Sortable, Value
  ) %>%
  mutate(
    Area = "England"
  )

mmr_BSol <- mmr_all %>%
  filter(
    Area.Name %in% c("Birmingham","Solihull")
  ) %>%
  group_by(Time.period.Sortable) %>%
  summarize(
    Count = sum(Count),
    Denominator = sum(Denominator)
  ) %>%
  mutate(
    Value = Count / Denominator * 100,
    Area = "BSol"
  ) %>%
  select(
    Time.period.Sortable, Value, Area
  )

mmr_plot <- rbind(mmr_eng, mmr_BSol) %>%
  mutate(
    Year = Time.period.Sortable/10000 + 0.5
  ) %>%
  select(Year, Value, Area) %>%
  mutate(
    data_type = "MMR coverage for one dose (2 years old) (%)"
  )

plot_data <- rbind(cases, mmr_plot)

ggplot(plot_data, aes(x = Year, y = Value, color = Area)) +
  geom_line(lwd = 1) +
  theme_bw() +
  facet_wrap(~data_type, scale = "free_y", ncol = 1) +
  xlim(2012, 2025) +
  theme(
    legend.position = "top",
    strip.background = element_rect(fill="white")
  ) +
  labs(
    color = "",
    y = "", 
    x = ""
    ) +
  scale_color_manual(
    breaks = c("England", "BSol"),
    values = c("#BF352D", "#2B6298")
  )

ggsave("figures/background/measles-background.svg")