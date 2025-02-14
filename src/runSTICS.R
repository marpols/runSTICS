#Run STICS in it's current state or with updated variables for manual calibration
#(not using STICS parameter optimization algorithm)

stics_inputs_path <<- NULL
sim_options <<- NULL
javastics_workspace_path <<- NULL



calib_init <- function(stics_path = stics_path,
                       subdir = subdir, 
                       croptomizR = F){
  
  stics_inputs_path <<- file.path(stics_path,subdir,"TxtFiles")
  javastics_workspace_path <<- file.path(stics_path,subdir, "XmlFiles")
  
  if (!dir.exists(stics_inputs_path)){
    dir.create(stics_inputs_path, showWarnings = T)
    
    if (croptomizeR){
      wpath <- javastics_workspace_path

    } else{
      wpath <- file.path(stics_path, subdir)
    }
    gen_usms_xml2txt(
      javastics = stics_path,
      workspace = wpath,
      out_dir = stics_inputs_path,
      verbose = TRUE
    )
  }
  sim_options <<- stics_wrapper_options(javastics = stics_path,
                                       workspace = stics_inputs_path, 
                                       verbose = TRUE, 
                                       parallel = FALSE)
}

save_results <- function(workspace = workspace,
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


calibrate_STICS <- function(workspace,
                            situations,
                            param_values,
                            out_dir = results_dir,
                            stics_path = stics_path,
                            sim_options = sim_options){
  
  write.table(param_values, file.path(out_dir,"parameter_table.txt"))
  
  if (typeof(param_values) == "list"){
    #parse over dataframe of multiple variable combinations (gridsearch)

    n <<- 1
    while (n <= nrow(param_values)){
      vals <- as.double(param_values[n,])
      names(vals) <- colnames(param_values)
      
      results <- stics_wrapper(model_options = sim_options,
                               param_values = vals,
                               situation = situations)
      iter <- n
      subset <- file.path(out_dir,iter)
      write.table(vals, file = file.path(subset,"params.txt"))
      
      save_results(directory = subset)
      get_stats_summary(simulations = results$sim_list,
                        output_dir = subset, 
                        version = iter, 
                        usms = situations)
      
      n <<- n + 1
    }

  } else{
    #run once for one set of variables
    results <- stics_wrapper(model_options = sim_options,
                             param_values = param_values,
                             situation = situations)
    
    save_results(directory = subset)
    get_stats_summary(simulations = results$sim_list,
                      usms = situations)
    
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
  
  return(results)
}


obs_list <- SticsRFiles::get_obs(javastics = stics_path,
                                 workspace =  wkspc, 
                                 usm = usms,
                                 usms_file = file.path(wkspc, "usms.xml"))



