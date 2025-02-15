#Run STICS in it's current state or with updated variables for manual calibration
#(not using STICS parameter optimization algorithm)

stics_inputs_path <<- NULL #path to STICS txtfiles
sim_options <<- NULL #stics_wrapper_options
javastics_workspace_path <<- NULL #path to STICS Xmlfiles (when using CroptimizR)
sim_before_optim <<- NULL #baseline simulation run
obs_list <<- NULL #list of observations used

calib_init <- function(stics_path. = stics_path,
                       subdir. = subdir,
                       variables = NULL,
                       croptimizR = F){
  #convert xml files to txt files for use with stics_wrapper
  #set javastics and workspace path in for stics_wrapper_options
  #stics_path. - (chr) path to directory containing javastics.exe
  #subdir. - (chr) path to directory containing stics xml files
  #croptimizR - (bool) set T if using croptimizR workspace set up and functions
  
  stics_inputs_path <<- file.path(stics_path.,subdir.,"TxtFiles")
  
  if(croptimizR){
    javastics_workspace_path <<- file.path(stics_path.,subdir., "XmlFiles")
  }
  
  if (!dir.exists(stics_inputs_path)){
    dir.create(stics_inputs_path, showWarnings = T)
    message("Creating directory at ", stics_inputs_path)
    
    if (croptomizR){
      wpath <- javastics_workspace_path

    } else{
      wpath <- file.path(stics_path., subdir.)
    }
    gen_usms_xml2txt(
      javastics = stics_path.,
      workspace = wpath,
      out_dir = stics_inputs_path,
      verbose = TRUE
    )
  }
  sim_options <<- stics_wrapper_options(javastics = stics_path.,
                                       workspace = stics_inputs_path, 
                                       verbose = TRUE, 
                                       parallel = FALSE)
  message("Simulation options set as:\n javatics....", stics_path.,
          "\nworkspace....", stics_inputs_path)
}

set_obs <- function(workspace,
            usms = NULL,
            variables = NULL){
  #set list of observations to use for calibration
  
  if (is.null(usms)){
    #use all usms
    usms <- SticsRFiles::get_usms_list(file = file.path(workspace, "usms.xml"))
  }
  obs_list <<- get_obs(workspace., usm = usms)
  if (!is.null(variables)){
    #filter obs_list to include only listed variables
    obs_list <<- filter_obs(obs_list, var = variables, include = TRUE)
  }
}

param_grid <- function(lwr = lowerbnds,
                       upr = upperbnds,
                       by){
  #create a grid of all combinations of parameters from lower to upperbounds
  #lwr - (double) vector of lowerbounds by variable
  #upr - (double) vector of upperbounds by variable
  #by - (double) vector of number to increase each variable sequence by
  
  vars <- names(lwr)
  var_list <- list()
  
  i <<- 1
  while (i <= length(lwr)){
    var_list[[vars[i]]] <- seq(lwr[[i]],upr[[i]],by[i])
  i <<- i + 1
  }
  
  return(expand.grid(var_list))
}

save_results <- function(workspace,
                         directory = results_dir){
  #save mod_b and mod_s files for each usm
  usm_dirs <- list.dirs(workspace, recursive = F)
  
  for (d in usm_dirs){
    #create directory for usm in output directory
    matches <- grep("[^/\\\\]+$", d, value = TRUE)
    usm_name <- last_name <- regmatches(matches, regexpr("[^/\\\\]+$", matches))
    
    new_dir <- file.path(directory,usm_name)
    dir.create(new_dir)
    
    #get mod_b and mod_s files
    files <- grep("mod_[bs]", list.files(d, full.names = T))
  
    #copy mod files from workspace into output directory
    file.copy(files, file.path(new_dir, usm_name))
  }
}


calibrate_STICS <- function(situations,
                            param_values,
                            out_dir = results_dir,
                            sim_options. = sim_options){
  
  write.table(param_values, 
              file.path(out_dir,"parameter_table.txt"), 
              row.names = T)
  
  #run stics before calibration runs
  sim_before_optim <<- stics_wrapper(model_options = model_options)
  
  message("Running STICS over the following parameters:\n",
          paste0(capture.output(param_values), collapse = "\n"))
  
  if (typeof(param_values) == "list"){
    #parse over dataframe of multiple variable combinations (gridsearch)
    
    n <<- 1
    while (n <= nrow(param_values)){
      vals <- as.double(param_values[n,])
      names(vals) <- colnames(param_values)
      
      message("Running with:\n", paste0(capture.output(vals), collapse = "\n"))
      
      results <- stics_wrapper(model_options = sim_options.,
                               param_values = vals,
                               situation = situations)

      subset <- file.path(out_dir,n)
      
      write.table(vals, file = file.path(subset,"params.txt"))
      
      save_results(directory = subset)
      get_stats_summary(simulations = results$sim_list,
                        output_dir = subset, 
                        version = n, 
                        usms = situations)
      
      message("Results saved in: ", subset)
      
      n <<- n + 1
    }

  } else{
    #run once for one set of variables
    results <- stics_wrapper(model_options = sim_options.,
                             param_values = param_values,
                             situation = situations)
    
    save_results()
    get_stats_summary(simulations = results$sim_list,
                      usms = situations)
    
    message("Results saved in: ", subset)
    
  }
}

runSTICS <- function(path = stics_path,
                     workspace = file.path(stics_path, subdir),
                     usms = NULL){
  #run javastics directly in subdir workspace
  
  if (is.null(usms)){
    usms <- SticsRFiles::get_usms_list(file = file.path(workspace, "usms.xml"))
  }
  results <- run_javastics(stics_path, workspace, usms)
  
  save_results(workspace = workspace)
  get_stats_summary(simulations = results$sim_list)
  
  return(results)
}



