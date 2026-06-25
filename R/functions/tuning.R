library(dplyr)
library(readxl)

load_sim_data <- function(
    path,
    data_sheet,
    param_sheet
) {
  read_excel(
    path,
    sheet = data_sheet
  ) %>%
    pivot_longer(
      cols = contains("Run"),
      names_to = "Run",
      values_to = "SimVal"
    ) %>%
    left_join(
      read_excel(
        "data/stella outputs/basic-model-testing/full-isolation-params.xlsx",
        sheet = param_sheet
      ),
      by = join_by("Run")
    ) 
}

get_run_data <- function(data, run_number) {
  data %>%
  filter(
    Run == paste0("Run ", run_number)
  )
}

group_weekly <- function(
    data,
    time_lag
) {
    data %>%
    mutate(
      Week = floor((Day + time_lag)/ 7) 
    ) %>%
    group_by(Week) %>%
    summarise(
      SimVal = sum(SimVal)
    ) 
}

calc_error <- function(
    sim_data, 
    obs_data,
    run_number,
    time_lag
) {
  sim_data_i <- get_run_data(sim_data, run_number) %>%
    group_weekly(time_lag)
  
  joined_i <- obs_data %>%
    left_join(
      sim_data_i,
      by = join_by("Week")
      ) %>%
    mutate(
      SimVal = ifelse(is.na(SimVal), 0, SimVal)
    )%>%
    summarise(
      Error = sum((ObsVal - SimVal)**2)
    ) %>%
    pull(Error)
}

calc_time_lag <- function(
    sim_data, 
    obs_data,
    run_number,
    max_lag = 40
) {
  
  h <- function(time_lag) calc_error(sim_data, obs_data, run_number, time_lag)
  
  time_lag <- optimize(h, interval = c(-max_lag, max_lag))$minimum
  
  if (abs(time_lag) == max_lag) {
    warning("Optimised time lag equal to interval maximum.")
  }
  
  return(time_lag)
}

calc_all_errors <- function(
    sim_data,
    obs_data,
    max_lag = 40
) {
  
  output <- sim_data %>%
    filter(Day == 0) %>%
    mutate(
      Error = NA
    ) %>%
    select(-c(Day, SimVal))
  
  n_runs <- nrow(output)
  
  for (run_number in 1:n_runs) {
    lag_i <- calc_time_lag(
      sim_data,
      obs_data,
      run_number,
      max_lag = max_lag
    )
    
    error_i <- calc_error(
      sim_data,
      obs_data,
      run_number,
      lag_i
    )
    
    output$Error[run_number] <- error_i
    
  }
  
  return(output)
}