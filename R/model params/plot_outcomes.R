library(ggplot2)
library(dplyr)
library(readxl)
library(tidyr)
library(lubridate)
library(janitor)
source("R/config.R")

birmingham_color = "#5E4F9C"
solihull_color = "#F07F3C"

cases <- read_excel(
  "data/model params/BSol-outbreak-cases-oct23toapril24.xlsx",
  sheet = "cases"
  ) %>%
  clean_names() %>%
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
    outcome = "Confirmed Measles Cases"
    ) %>%
  select(-week_number)

admissions <- read_excel(
  file.path(
    data_path,
    "bsol-measles-admissions.xlsx"
  )
) %>%
  clean_names() %>%
  mutate(
    admission_date = as.Date(admission_date)
    )%>%
  filter(
    admission_date >= "2023-10-13",
    admission_date < "2024-04-12",
    # Only include persons first measles admission
    measles_admission_num == 1,
    !is.na(admission_date)
  ) %>%
  mutate(
    date_start  = cut(
      admission_date,
      breaks=seq(as.Date("2023-10-13"), as.Date("2024-04-12"), 7)
      )
    ) %>%
  group_by(
    date_start, local_authority,
  ) %>%
  summarise(
    value = n(),
    .groups = "drop"
  ) %>%
  mutate(
    outcome = "Measles Hospital Admissions",
    date_start = as.Date(date_start)
  )

combined_data <- rbind(admissions, cases) %>%
  mutate(
    local_authority = factor(
      local_authority, 
      levels = c("Solihull", "Birmingham")
    )
  )

# print total numbers over outbreak
combined_data %>%
  group_by(outcome) %>%
  summarise(
    value = sum(value)
  )



annot_df <- data.frame(
  outcome = "Confirmed Measles Cases",  # <- replace with the actual value
  x = Inf,
  y = Inf,
  label = "*Estimated case dates used for Solihull"
)

ggplot(combined_data, aes(x = date_start, y = value, fill = local_authority)) +
  geom_col(lwd = 1) +
  theme_bw() +
  facet_wrap(~outcome, scale = "free_y", ncol = 1) +
  labs(
    y = "Weekly Count",
    x = "",
    fill = ""
  ) +
  theme(
    legend.position = "top",
    strip.background = element_rect(fill="white"),
    legend.box.margin = margin(0,0,-10,0)
  ) +
  scale_fill_manual(
    values = c("Birmingham" = birmingham_color,
               "Solihull" = solihull_color)
  ) +
  scale_x_continuous(
    breaks = c(as.Date("2023-10-01"), as.Date("2024-01-01"), as.Date("2024-04-01")),
    labels = c("Oct 2023", "Jan 2024", "Apr 2024")
  ) +
  geom_text(
    data = annot_df,
    aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    vjust = 1.8,
    hjust = 1.05,
    color = "grey30",
    size = 2.5
  ) +
  scale_y_continuous(
    expand = c(0,0),
    limits = c(0, 60)
  )

ggsave("figures/model-params/outbreak-outcomes.pdf", 
       width = 6, height = 4)