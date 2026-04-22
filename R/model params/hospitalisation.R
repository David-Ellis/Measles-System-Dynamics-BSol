library(readxl)
library(dplyr)
library(janitor)
library(ggplot2)
library(lubridate)

source("R/config.R")

admissions <- read_excel(
  file.path(
    data_path,
    "bsol-measles-admissions.xlsx"
  )
) %>%
  clean_names() %>%
  mutate(
    year = year(admission_date),
    ethnic_group = case_when(
      ethnicity %in% c(
        "Bangladeshi",
        "Pakistani", 
        "Indian",
        "Any other Asian background"
        ) ~ "Asian",
      ethnicity %in% c(
        "Black African",
        "Black Caribbean",
        "Any other Black background"
      ) ~ "Black",
      ethnicity %in% c(
        "White and Black African",
        "White and Black Caribbean",
        "White and Asian",
        "Any other mixed background"
      ) ~ "Mixed",
      ethnicity %in% c(
        "White British",
        "White Irish", 
        "Any other white background"
        ) ~ "White",
      ethnicity %in% c(
        "Any other ethnic group",
        "Chinese"
      ) ~ "Other",
      TRUE ~ ethnicity
    )
  ) %>%
  # Filter to ensure full years
  filter(
    year < 2026,
    year > 2018
  ) %>%
  mutate(
    age_group = case_when(
      age_on_admission == 0 ~ "Less than 1",
      age_on_admission <= 4 ~ "1 to 4",
      age_on_admission <= 17 ~ "4 to 17",
      age_on_admission <= 64 ~ "18 to 64",
      TRUE ~ "65 and over"
    ),
    age_group = factor(
      age_group,
      levels = c("Less than 1","1 to 4","4 to 17","18 to 64","65 and over")
    )
  )

ggplot(admissions, aes(x= year, fill = ethnic_group)) +
  geom_bar() +
  theme_bw() +
  labs(
    y = "Measles-related Hospial Admissions in BSol",
    x = "",
    fill = ""
  ) +
  theme(
    legend.position = "top",
    legend.box.margin = margin(0,0,-10,0)
  )  +
  scale_fill_brewer(palette = "Dark2")+
  scale_y_continuous(
    limits = c(0, 150), expand = c(0,0)
  )


ggplot(admissions, aes(x= year, fill = age_group)) +
  geom_bar() +
  theme_bw() +
  labs(
    y = "Measles-related Hospial Admissions in BSol",
    x = "",
    fill = ""
  ) +
  theme(
    legend.position = "top",
    legend.box.margin = margin(0,0,-10,0)
  ) +
  scale_fill_manual(values = brewer.pal(n = 9, name = "BuPu")[3:9]) +
  scale_y_continuous(
    limits = c(0, 150), expand = c(0,0)
  )

admissions %>% 
  count(age_group) %>%
  mutate(
    perc = 100 * n / sum(n)
  )

ggplot(admissions, aes(x = spell_duration)) +
  geom_histogram(
    binwidth = 1, color = "black", fill = "#2B6298"
    ) +
  theme_bw()+
  scale_y_continuous(
    limits = c(0, 50), expand = c(0,0)
  ) +
  labs(
    y = "Measles-Related Hospial Admissions in BSol\n(2019 - 2025)",
    x = "Hospital Spell Duration (Days)"
  ) +
  scale_x_continuous(
    breaks = c(0,2,4,6,8,10)
  )