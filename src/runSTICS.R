#Run STICS in it's current state or with updated variables for manual calibration
#(not using STICS parameter optimization algorithm)


get_sims <- function(stics_path,wkspc){
  sims <- SticsRFiles::get_sim(javastics = stics_path,
                                    workspace = wkspc, 
                                    usms_file = file.path(wkspc, "usms.xml"))
  return(sims)
}

save_results <- function(files,directory){
  
}


calibrate_STICS <- function(stics_path,
                            workspace,
                            situations,
                            param_values,
                            out_dir){
  
  sim_options <- stics_wrapper_options(javastics = stics_path,
                                     workspace = wkspc, 
                                     verbose = TRUE, 
                                     parallel = FALSE)
  
  if (typeof(param_values) == "list"){
    #parse over dataframe of multiple variable combinations
    n <<- 1
    while (n <= nrow(param_values)){
      vals <- as.double(param_values[n,])
      names(vals) <- colnames(param_values)
      
      results <- stics_wrapper(model_options = sim_options,
                               param_values = vals,
                               situation = situations)
      
      sims_list <- get_sims(stics_path, wkspc)
      
      n <<- n + 1
    }

  } else{
    #run once for one set of variables
    results <- stics_wrapper(model_options = sim_options,
                             param_values = param_values,
                             situation = situations)
    
    sims_list <- get_sims(stics_path, wkspc)
  }

}

runSTICS <- function(path, workspace, usm_list){
  #run for all usms in workspace
  results <- run_javastics(stics_path, wkspc, usms)
  
  sims_list <- get_sims(stics_path, wkspc)
  
  return(results)
}


obs_list <- SticsRFiles::get_obs(javastics = stics_path,
                                 workspace =  wkspc, 
                                 usm = usms,
                                 usms_file = file.path(wkspc, "usms.xml"))



