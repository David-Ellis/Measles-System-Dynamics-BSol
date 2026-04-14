library(readxl)
library(dplyr)
library(janitor)

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