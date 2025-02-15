#Calibrate the STICS model using CroptimizeR
#From https://sticsrpacks.github.io/CroptimizR/articles/Parameter_estimation_simple_case.html



runCroptimizR <- function(model_options = sim_options,
                          sit_name. = sit_name,
                          var_name. = var_name,
                          lowerbnds. = lowerbnds,
                          upperbnds. = upperbnds,
                          reps. = reps,
                          max. = max,
                          tol. = tol,
                          out_dir = results_dir){
  

  # Run the model on all situations found in stics_inputs_path before optimization
  sim_before_optim <<- stics_wrapper(model_options = model_options)
  
  set_obs(javastics_workspace_path, usms = sit_name.,variables = var_name.)
  
  # upper and lower bounds of parameters to consider
  param_info <- list(
    lb = lowerbnds.,
    ub = upperbnds.
  )
  
  #Set options for the parameter estimation method
  optim_options <- list()
  optim_options$nb_rep <- reps.  # Number of repetitions of the minimization
                                # (each time starting with different initial
                                # values for the estimated parameters)
  optim_options$maxeval <- max.  # Maximum number of evaluations of the
                                # minimized criteria
  optim_options$xtol_rel <- tol. # Tolerance criterion between two iterations
                                      # (threshold for the relative difference of
                                      # parameter values between the 2 previous
                                      # iterations)
  optim_options$out_dir <- out_dir # path where to store the results
  # (graph and Rdata)
  optim_options$ranseed <- random_seed # set random seed so that each execution give the
                                # same results
                                # If you want randomization, don't set it.
  
  #Run the optimization
  res <- estim_param(
    obs_list = obs_list,
    model_function = stics_wrapper,
    model_options = model_options,
    optim_options = optim_options,
    param_info = param_info
  )
  
  final_vals <- res$final_values
  
  #Run model after optimization
  sim_after_optim <- stics_wrapper(
    param_values = res$final_values,
    model_options = model_options
  )
  
  save_results()
  get_stats_summary(simulations = sim_after_optim$sim_list,
                    usms = sit_name.)
  plot_simvobs(sim_before_optim, sim_after_optim)
  
  return(sim_after_optim)
}

