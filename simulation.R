# Simulation
# This file generates the short sprint simulations and save it in `simulation-results.RData` file
# Copyright(c) 2022 by Mladen Jovanovic
# email: coach.mladen.jovanovic@gmail.com
# 

require(tidyverse)
require(shorts)

# function to return NULL if models cannot be created
tryNULL <- function(expr) {
  result <- NULL
  tryCatch(result <- expr, error = function(e) e)
  result
}

# control function for the `minpack.lm::nlsLM()` function
control = minpack.lm::nls.lm.control(maxiter = 1000)

# Simulation explore values
sim_MSS <- seq(7, 11, by = 0.05)
sim_MAC <- seq(7, 11, by = 0.05)
sim_flying_distance <- seq(0, 50, by = 1) / 100
sim_split_distances <- c("5, 10, 20, 30, 40")
sim_rounding <- 2

# Simulation parameters 
sim_params_df <- expand_grid(
  MSS = sim_MSS,
  MAC = sim_MAC,
  flying_distance = sim_flying_distance,
  split_distances = sim_split_distances,
  rounding = sim_rounding
)

# Report number of simulations
message(paste0("Total number of simulations: ", nrow(sim_params_df), "\n"))

# Function that runs the simulations
get_estimates <- function(df) {
  # Get true parameter values
  MSS <- df$MSS
  MAC <- df$MAC
  TAU <- MSS / MAC
  PMAX <- MSS * MAC / 4
  flying_distance <- df$flying_distance
  split_distances_chr <- df$split_distances
  rounding <- df$rounding

  # Extract split distances
  split_distances <- as.numeric(str_split(split_distances_chr, ",", simplify = TRUE))

  # Generate split times using known parameters and flying distance
  split_times <- create_timing_gates_splits(MSS = MSS, MAC = MAC, gates =  split_distances, FD = flying_distance)
  split_times <- round(split_times, rounding)
  
  # make models
  model_no_correction <- tryNULL(
    model_timing_gates(
      distance = split_distances,
      time = split_times,
      control = control
    )
  )

  #model_TC_fixed_03 <- tryNULL(
  #  model_timing_gates(
  #    distance = split_distances,
  #    time = split_times + 0.3,
  #    control = control
  #  )
  #)

  #model_TC_fixed_05 <- tryNULL(
  #  model_timing_gates(
  #    distance = split_distances,
  #    time = split_times + 0.5,
  #    control = control
  #  )
  #)

  model_estimated_TC <- tryNULL(
    model_timing_gates_TC(
      distance = split_distances,
      time = split_times,
      control = control
    )
  )

  model_estimated_FD <- tryNULL(
    model_timing_gates_FD(
      distance = split_distances,
      time = split_times,
      control = control
    )
  )
  
  #model_estimated_FD_and_TC <- tryNULL(
  #  model_timing_gates_FD_TC(
  #    distance = split_distances,
  #    time = split_times,
  #    control = control
  #  )
  #)

  # Combine models into a list
  model_list <- list(
    "No correction" = model_no_correction,
    # "Fixed TC (+0.3 s)" = model_TC_fixed_03,
    # "Fixed TC (+0.5 s)" = model_TC_fixed_05,
    "Estimated TC" = model_estimated_TC,
    "Estimated FD" = model_estimated_FD
    #"Estimated FD and TC" = model_estimated_FD_and_TC
  )

  # extract estimated parameters
  parameter_values <- imap_dfr(model_list, function(x, name) {
    if (!is.null(x)) {
      data.frame(
        model = name,
        parameter = c("MSS", "TAU", "MAC", "PMAX"),
        estimate = unname(coef(x)[1:4])
      )
    } else {
      warning(paste0("Model `", name, "` for ",
                     paste0("MSS=", MSS, ", MAC=", MAC, ", FD=", flying_distance, ", splits=`", split_distances_chr, "`, and rounding=", rounding),
                     " cannot be estimated. Returnig NA."), call. = FALSE)
      data.frame(
        model = name,
        parameter = c("MSS", "TAU", "MAC", "PMAX"),
        estimate = rep(NA, 4)
      )
    }
  })

  data.frame(df, parameter_values)
}

# Run the simulations
sim_res_df <- sim_params_df %>%
  rowwise() %>%
  do(get_estimates(.)) %>%
  ungroup()

# Add true values
true_params <- sim_params_df %>%
  mutate(MSS_ = MSS, MAC_ = MAC, TAU_ = MSS / MAC, PMAX_ = MSS * MAC / 4) %>%
  pivot_longer(cols = c(MSS_, MAC_, TAU_, PMAX_), names_to = "parameter", values_to = "true"
  ) %>%
  mutate(parameter = recode_factor(parameter,
    "MSS_" = "MSS",
    "TAU_" = "TAU",
    "MAC_" = "MAC",
    "PMAX_" = "PMAX"
  ))

# Merge together
sim_res_df <- sim_res_df %>%
  left_join(true_params) %>%
  mutate(
    diff = estimate - true,
    diff_perc = 100 * (diff / true),
    diff_mean = (estimate + true) / 2) %>%
  mutate(
    parameter = factor(
      parameter,
      levels = c("MSS", "MAC", "TAU", "PMAX")),
    model = factor(
      model,
      levels = c("No correction", "Estimated TC", "Estimated FD")
    )
  )

ROPE_df <- sim_res_df %>%
  filter(model == "No correction", flying_distance == 0)

# save
save(sim_res_df, ROPE_df, file = "simulation-results.RData")
