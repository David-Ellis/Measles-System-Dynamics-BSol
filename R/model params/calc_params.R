library(readxl)
library(dplyr)
library(janitor)
library(ggplot2)
source("R/functions/styles.R")
# Initial population sizes

bsol_ages <- read_excel("data/model params/BSol-Ages.xlsx") %>%
  clean_names() %>%
  mutate(
    age_group = case_when(
      age_86_categories_code == 0 ~ "Age < 1",
      age_86_categories_code < 5 ~ "Age 1 to 4",
      TRUE ~ "Age 5+"
    )
  ) %>% 
  filter(
    !is.na(age_group)
  ) %>%
  group_by(age_group) %>%
  summarise(
    observation = sum(observation)
  )

# child and baby migrant populations

bsol_migrant <- read_excel("data/model params/BSol-Ages-and-Birth-Country.xlsx") %>%
  clean_names() %>%
  mutate(
    region = case_when(
      grepl("United Kingdom", country_of_birth_8_categories) ~ "United Kingdom",
      grepl("Europe", country_of_birth_8_categories) ~ "Europe (Not UK)",
      TRUE ~ country_of_birth_8_categories
    ),
    age_group = case_when(
      age_86_categories_code == 0 ~ "Age < 1",
      age_86_categories_code < 6 ~ "Age 1 to 4",
      TRUE ~ "Age 5+"
    )
  ) %>%
  filter(
    !is.na(age_group)
  ) %>%
  group_by(region, age_group) %>%
  summarise(
    observation = sum(observation)
  ) %>%
  mutate(
    observation = case_when(
      age_group == "Age < 1" ~ observation,
      age_group == "Age 1 to 4" ~ observation / 4,
      age_group == "Age 5+" ~ NA
    )
  )
  
total_migration <- bsol_migrant %>%
  filter(age_group != "Age 5+") %>%
  group_by(region=="United Kingdom", age_group) %>%
  summarise(
    observation = sum(observation)
  )


################################################################################
#                        Vaccinated vs non-vaccinated                          #
################################################################################

who_coverage <- read_excel("data/model params/WHO Region MMR Coverage.xlsx") %>%
  clean_names() %>%
  filter(
    year == 2024
  ) %>%
  select(c(name, coverage))


ons_to_who_region <- read_excel(
  "data/model params/ons-to-who-region-lookup.xlsx",
  sheet = "Lookup") %>%
  clean_names()

bsol_migrant_plus <- read_excel("data/model params/BSol-Ages-and-Birth-Country-Plus.xlsx") %>%
  clean_names() %>%
  mutate(
    age_group = case_when(
      age_86_categories_code == 0 ~ "Age < 1",
      age_86_categories_code < 5 ~ "Age 1 to 4",
      TRUE ~ NA
    )
  ) %>%
  filter(
    !is.na(age_group),
    country_of_birth_22_categories != "Does not apply",
    country_of_birth_22_categories != "Other"
  ) %>%
  left_join(
    ons_to_who_region,
    by = join_by("country_of_birth_22_categories" == "ons_region")
  ) %>%
  group_by(region, age_group) %>%
  summarise(
    observation = sum(observation),
    .groups = "drop"
  ) %>%
  filter(
    region != "England"
  ) %>%
  left_join(
    who_coverage,
    by = join_by("region" == "name")
  ) %>%
  mutate(
    observation = case_when(
      age_group == "Age < 1" ~ observation,
      age_group == "Age 1 to 4" ~ observation / 4
    ),
    # Assume zero coverage for those aged less than 1
    coverage = case_when(
      age_group == "Age < 1" ~ 0,
      age_group == "Age 1 to 4" ~ coverage
    ),
    # convert to decimal
    coverage = coverage / 100,
    Vaccinated = coverage * observation,
    Unvaccinated = (1 - coverage) * observation
  )


bsol_migrant_plus %>%
  tidyr::pivot_longer(
    cols = c("Vaccinated", "Unvaccinated"),
    names_to = "status",
    values_to = "count"
  ) %>%
  mutate(
    region = stringr::str_wrap(region, 12)
  ) %>%
  ggplot(
    aes(x = region, y =  count, fill = status)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 500)) +
  geom_col() +
  theme_bw() +
  scale_fill_manual(values = c(uob_colors[[1]], uob_colors[[3]])) +
  facet_wrap(~age_group, ncol = 1) +
  labs(
    y = "Estimated Yearly Migrants",
    fill = "",
    x = "WHO Region"
  ) +
  theme(
    legend.position = "top",
    strip.background = element_rect(fill="white"),
    legend.box.margin = margin(0,0,-10,0)
  )

ggsave("figures/model-params/migration-estimates.pdf", 
       width = 6, height = 4)
 
# Print overall vaccinated child migration percentage

bsol_migrant_plus %>%
  filter(age_group == "Age 1 to 4") %>%
  summarise(
    Vaccinated = sum(Vaccinated),
    Unvaccinated = sum(Unvaccinated)
  ) %>%
  mutate(
    overall_perc = Vaccinated / (Vaccinated + Unvaccinated)
  )


################################################################################
#                        Yearly Vaccination Averages                           #
################################################################################

vaccinations <- read_excel(
  "data/model params/bsol-vaccinations.xlsx",
  sheet = "daily_vaccinations"
  ) %>%
  clean_names() %>%
  mutate(
    year = lubridate::year(vaccination_date)
  ) %>%
  filter(
    year >= 2021,
    year <= 2025,
    !(age_bracket_activity %in% c("<1 year", "Unknown"))
  ) %>%
  group_by(year, age_bracket_activity) %>%
  summarise(
    valid_dose = sum(valid_dose),
    .groups = "drop"
  ) %>%
  mutate(
    age_group = case_when(
      age_bracket_activity == "1 years to <5 years" ~ "1 to 4 years",
      age_bracket_activity == "5 years to 25 years" ~ "5 to 25 years",
      age_bracket_activity == "Over 25" ~ "Over 25 years"
    ),
    age_group = factor(age_group, levels = c(
      "Over 25 years", "5 to 25 years","1 to 4 years"
    ))
  )

ggplot(vaccinations, 
       aes(x = year, y = valid_dose, fill = age_group)) +
  geom_col() +
  theme_bw() +
  labs(
    y = "Patients Recieving first Dose of MMR(V)",
    fill = "",
    x = "Year"
  ) +
  theme(
    legend.position = "top",
    legend.box.margin = margin(0,0,-10,0)
  ) +
  scale_fill_manual(values = ggpubr::get_palette((c(uob_colors[3], "#FFFFFF")), 5)[1:3]
                    ) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 22500))

ggsave("figures/model-params/yearly-vaccinations.pdf", 
       width = 6, height = 3.5)

yearly_vacc_average <- vaccinations %>%
  group_by(age_bracket_activity) %>%
  summarise(
    year_av_dose = sum(valid_dose) / n()
  )
yearly_vacc_average