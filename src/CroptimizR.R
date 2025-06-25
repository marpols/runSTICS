#Calibrate the STICS model using CroptimizeR
#From https://sticsrpacks.github.io/CroptimizR/articles/Parameter_estimation_simple_case.html



runCroptimizR <- function(model_options = sim_options,
                          sit_name. = sit_name,
                          obs_list = obs,
                          var_name. = var_name,
                          lowerbnds. = lowerbnds,
                          upperbnds. = upperbnds,
                          reps. = reps,
                          max. = max,
                          tol. = tolerance,
                          ver = version,
                          out_dir = results_dir,
                          usms = usm_cal,
                          cal_file){
  

  # Run the model on all situations found in stics_inputs_path before optimization
  message("Calibrating model for:\n", 
          paste0(capture.output(var_name.), collapse = "\n"))
  values <- get_param_xml(cal_file, param = names(lowerbnds.)
                         )
  message("\nRunning model before optimization - ", Sys.time(),
          "\noriginal value(s):\n",
          paste0(capture.output(values), collapse = "\n")
          )
  sim_before_optim <<- stics_wrapper(model_options = model_options,
                                     situation = usms)

  dir.create(file.path(out_dir,"before"))
  save_results2(stics_inputs_path, 
                outdir = file.path(out_dir,"before"),
                usms = sit_name.)
  get_stats_summary(version = ver,
                    output_dir = file.path(out_dir,"before"),
                    obs_list = obs_list,
                    simulations = sim_before_optim$sim_list)

  
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
  optim_options$ranseed <- random_seed # set random seed so that each execution gives the
                                # same results
                                # If you want randomization, don't set it.
  
  #Run the optimization
  message("Running Optimiztion at ", Sys.time())
  res <- estim_param(
    obs_list = obs_list,
    model_function = stics_wrapper,
    model_options = model_options,
    optim_options = optim_options,
    param_info = param_info
  )
  
  final_vals <<- res$final_values
  
  #Run model after optimization
  message("Running model with optimized parameter(s)")
  sim_after_optim <<- stics_wrapper(
    param_values = res$final_values,
    model_options = model_options,
    situation = usms
  )
  write.csv(final_vals,
            file.path(out_dir,sprintf("%s_final-values.csv",version)))
  save_results3(sim_after_optim$sim_list)
  get_stats_summary(simulations = sim_after_optim$sim_list,
                    usms = sit_name.,
                    obs_list = obs_list,
                    version = version,
                    fname = "-after")
  plot_simvobs(baseline = sim_before_optim$sim_list, 
               new_sims = sim_after_optim$sim_list,
               obs = obs_list)
  plot_simvobs(sim_after_optim$sim_list,
               obs = obs_list,
               subname = "after")
  
  message("Completed at", Sys.time())
  
  return(sim_after_optim)
}

