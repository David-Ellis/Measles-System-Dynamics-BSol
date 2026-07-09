library(dplyr)
library(readxl)
library(foreach)
library(doParallel)
library(geomtextpath)

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
        path,
        sheet = param_sheet
      ),
      by = join_by("Run")
    ) 
}

group_weekly <- function(
    data
) {
    data %>%
    mutate(
      Week = floor((Day)/ 7) 
    ) %>%
    group_by(Run, Week) %>%
    summarise(
      SimVal = sum(SimVal),
      .groups = "drop"
    ) 
}

calc_error <- function(
    sim_data, 
    obs_data,
    time_lag
) {

  obs_vals <- obs_data$ObsVal
  obs_weeks <- obs_data$Week
  
  sim_weeks <- sim_data$Week + time_lag
  sim_vals  <- sim_data$SimVal
  
  sim_aligned <- sim_vals[
    match(obs_weeks, sim_weeks)
  ]
  
  sim_aligned[is.na(sim_aligned)] <- 0

  
  sum((obs_vals - sim_aligned)^2)
}

calc_time_lag <- function(
    sim_data, 
    obs_data,
    max_lag = 7
) {
  
  lags <- -max_lag:max_lag
  
  errors <- sapply(
    lags,
    function(lag) calc_error(sim_data, obs_data, lag)
  )
  
  best_lag <- lags[which.min(errors)]
  
  print(best_lag)
  if (abs(best_lag) == max_lag) {
    warning("Optimised time lag equal to interval maximum.")
  }
  
  return(best_lag)
}

calc_all_errors <- function(
    sim_data,
    obs_data,
    max_lag = 7
) {

  # Get parameters for each run
  params <- sim_data %>%
    filter(Day == 0) %>%
    select(-c(Day, SimVal)) %>%
    arrange(Run)
  
  sim_data_weekly <- sim_data %>%
    group_weekly()
  
  n_runs <- nrow(params)
  if (n_runs < 1) {
    stop("Number of runs calculated to be less than one.")
  }
  
  sim_runs <- split(sim_data_weekly, sim_data_weekly$Run)
  
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
    left_join(params, 
              by = join_by("Run"))
  
  return(output)
}

get_min_params <- function(
    error_func
) {
  error_func %>%
    filter(
      Error == min(Error)
    ) %>%
    mutate(
      min_error_fraction = 0
    ) 
}

# interp_error_function <- function(
#     error_func,
#     param1,
#     param2
# ) {
# 
#   interp_out<- interp::interp(
#     error_func[[param1]], 
#     error_func[[param2]], 
#     error_func$min_error_fraction, nx = 20, ny = 20
#   )
#   
#   print("Got here")
#   interp_df <- expand.grid(
#     param1 = interp_out$x,
#     param2 = interp_out$y
#   ) |>
#     mutate(min_error_fraction = as.vector(interp_out$z))
#   
#   colnames(interp_df) <- c(param1, param2, "min_error_fraction")
#   
#   return(interp_df)
# }

plot_param_pair <- function(
  error_func, 
  param_pair,
  min_error_coord,
  contour_fracs = c(10, 50, 100),
  palette = "Purples"#,
  #interpolate = F
) {
  
  global_min <- min_error_coord$Error
  
  min_error_coord <- min_error_coord %>%
    mutate(
      min_error_fraction = 0
    )
  
  plot_data <- error_func %>%
    group_by(
      !!sym(param_pair[1]), 
      !!sym(param_pair[2])
    ) %>%
    summarise(
      min_error = min(Error),
      .groups = "drop"
    ) %>%
    ungroup() %>%
    mutate(
      min_error_fraction = 100 * (min_error - min(min_error)) / min(min_error)
    ) 

    ggplot(
      plot_data,
      aes(x = !!sym(param_pair[1]),
          y = !!sym(param_pair[2]),
          color = min_error_fraction)
      ) + 
      geom_point(alpha = 1) +
      # geom_textcontour(
      #   aes(label = paste0(after_stat(level), "%")),
      #   breaks = contour_fracs,
      #   linetype = "dashed"
      # ) +
      geom_point(
      data = min_error_coord,
      aes(
        x =  !!sym(param_pair[1]),
        y = !!sym(param_pair[2]),
        ),
      color = "orange"
      ) +
      theme_bw() +
      scale_x_continuous(
        expand = c(0,0)
      )+
      scale_y_continuous(
        expand = c(0,0)
      ) +
      scale_color_continuous(
        palette = palette,
        limits = c(0, max(contour_fracs)),
        trans = 'reverse',
        na.value = "white"
      ) +
      labs(
        x = stringr::str_to_title(param_pair[1]),
        y = stringr::str_to_title(param_pair[2]),
        #fill = "Minimum Error Function"
      ) +
      theme(
        legend.position = "none"
      )
}


plot_tuned_params <- function(
    error_func,
    param_pairs,
    contour_fracs = c(10, 50, 100),
    palette = "Purples"#,
    #interpolate = F
) {

  
  min_error_coord <- get_min_params(error_func)
    
  plots <- lapply(
    param_pairs, 
    function(param_pair) {
      plot_param_pair(
        error_func,
        param_pair,
        min_error_coord,
        contour_fracs = contour_fracs,
        palette = palette#,
        #interpolate = interpolate
      )
    }
    )
  
  
  cowplot::plot_grid(plotlist = plots) 
  
}