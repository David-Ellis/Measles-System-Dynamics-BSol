library(readxl)
library(dplyr)
library(janitor)
library(ggplot2)

# Initial population sizes

bsol_ages <- read_excel("data/model params/BSol-Ages.xlsx") %>%
  clean_names() %>%
  mutate(
    age_group = case_when(
      age_86_categories_code == 0 ~ "Age < 1",
      age_86_categories_code < 6 ~ "Age 1 to 5",
      TRUE ~ NA
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
      age_86_categories_code < 6 ~ "Age 1 to 5",
      TRUE ~ NA
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
      age_group == "Age 1 to 5" ~ observation / 5
    )
  )
  
total_migration <- bsol_migrant %>%
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
      age_86_categories_code < 6 ~ "Age 1 to 5",
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
      age_group == "Age 1 to 5" ~ observation / 5
    ),
    # Assume zero coverage for those aged less than 1
    coverage = case_when(
      age_group == "Age < 1" ~ 0,
      age_group == "Age 1 to 5" ~ coverage
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
    region = stringr::str_wrap(region, 15)
  ) %>%
  ggplot(
    aes(x = region, y =  count, fill = status)) +
  geom_col() +
  theme_bw() +
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

ggsave("figures/model-params/migration-estimates.pdf", width = 6.5, height = 5)

# Print overall vaccinated child migration percentage

bsol_migrant_plus %>%
  filter(age_group == "Age 1 to 5") %>%
  summarise(
    Vaccinated = sum(Vaccinated),
    Unvaccinated = sum(Unvaccinated)
  ) %>%
  mutate(
    overall_perc = Vaccinated / (Vaccinated + Unvaccinated)
  )
