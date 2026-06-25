library(dplyr)
library(readxl)
library(foreach)
library(doParallel)

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
    time_lag
) {
  sim_data_i <- sim_data %>%
    group_weekly(time_lag)
  
  obs_vals <- obs_data$ObsVal
  obs_weeks <- obs_data$Week
  
  sim_weeks <- sim_data_i$Week
  sim_vals  <- sim_data_i$SimVal
  
  sim_aligned <- sim_vals[
    match(obs_weeks, sim_weeks)
  ]
  
  sim_aligned[is.na(sim_aligned)] <- 0
  
  sum((obs_vals - sim_aligned)^2)
}

calc_time_lag <- function(
    sim_data, 
    obs_data,
    max_lag = 40
) {
  
  h <- function(time_lag) calc_error(sim_data, obs_data, time_lag)
  
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
    select(-c(Day, SimVal)) %>%
    arrange(Run)
  
  n_runs <- nrow(output)
  sim_runs <- split(sim_data, sim_data$Run)
  
  # Register cluster
  n_cores <- detectCores()
  cluster <- makeCluster(n_cores - 1)
  registerDoParallel(cluster)
  
  errors <- list()
  
  results <- foreach(
    run_number = 1:n_runs,
    .packages = c("dplyr"),
    .export = c("calc_time_lag", "calc_error", "sim_runs", "obs_data", "group_weekly")
  ) %dopar% {
    
    lag_i <- calc_time_lag(
      sim_runs[[run_number]],
      obs_data,
      max_lag = max_lag
    )
    
    error_i <- calc_error(
      sim_runs[[run_number]],
      obs_data,
      lag_i
    )
    
    data.frame(
      Run = sim_runs[[run_number]]$Run[1],
      Error = error_i,
      Lag = lag_i
    )
  }
    
  stopCluster(cl = cluster)
  
  output <- do.call(rbind, results) %>%
    left_join(output, 
              by = join_by("Run"))
  
  return(output)
}