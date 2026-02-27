library(ggplot2)
library(stringr)
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
    data_type = "Lab-confirmed measles cases (England)",
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
    Area = "Birmingham & Solihull"
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
    strip.background = element_rect(fill="white"),
    legend.box.margin = margin(0,0,-10,0)
  ) +
  labs(
    color = "",
    y = "", 
    x = "Year"
    ) +
  scale_color_manual(
    breaks = c("England", "Birmingham & Solihull"),
    values = c("#BF352D", "#2B6298")
  )

ggsave("figures/background/measles-background.svg", width = 5, height = 4)
ggsave("figures/background/measles-background.pdf", width = 5, height = 4)

################################################################################
#                        Plot coverage by ethnicity                            #
################################################################################

mmr_by_eth <- read.csv("data/background/mmr-by-ethnicity-bsol.csv") %>%
  mutate(
    ethnicity = str_remove(ethnicity_description, "99: "),
    ethnicity = str_remove(ethnicity, "^.+-\\s"),
    ethnicity = case_when(
      ethnicity == "British" ~ "White British",
      ethnicity == "Irish" ~ "White Irish",
      TRUE ~ ethnicity
      ),
    group = str_trim(
      str_extract(
        ethnicity_description, 
        "(?<=: )[^-]+"
        )
      )
    ) %>%
  arrange(desc(group)) %>%
  mutate(
    p_hat = vaccinated_perc,
    Z = qnorm(0.975),
    LowerCI95 =  (p_hat + Z^2/(2*patients) - Z * sqrt((p_hat*(1-p_hat)/patients) + Z^2/(4*patients^2))) / (1 + Z^2/patients),
    UpperCI95 = (p_hat + Z^2/(2*patients) + Z * sqrt((p_hat*(1-p_hat)/patients) + Z^2/(4*patients^2))) / (1 + Z^2/patients)
  )

mmr_by_eth$ethnicity <- factor(
  mmr_by_eth$ethnicity,
  levels = unique(mmr_by_eth$ethnicity)
) 

bsol_average <- mmr_by_eth %>%
  summarise(
    av_perc = sum(dose1_count)/sum(patients)
  ) %>%
  pull(av_perc)

ggplot(
  mmr_by_eth,
  aes(y = ethnicity, x = vaccinated_perc)
) +
  geom_col(fill = "#2B6298") +
  geom_errorbar(aes(xmin = LowerCI95, xmax = UpperCI95),
                width = 0.4, lwd = 0.8) +
  geom_vline(
    aes(
      xintercept = bsol_average,
      color = "Birmingham & Solihull Average"),
    linetype = "dashed", lwd =1.2,
             ) + 
  theme_bw() +
  labs(
    y = "",
    x = "Children aged <6 years who received 1 MMR dose",
    fill = ""
  ) +
  theme(
    legend.position = "top",
    plot.margin = margin(, 0.5, , , "cm"),
    legend.box.margin = margin(0,0,-10,0)
  ) + 
  scale_x_continuous(
    labels = scales::percent,
    limits = c(0,1),
    expand = c(0,0)
    ) +
  scale_colour_manual(
    values = c("Birmingham & Solihull Average" = "#BF352D"),
    name = NULL
  )

ggsave("figures/background/mmr-background.svg", width = 6.5, height = 4)
ggsave("figures/background/mmr-background.pdf", width = 6.5, height = 4)