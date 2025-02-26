#Run STICS in it's current state or with updated variables for manual calibration
#(not using STICS parameter optimization algorithm)
if (!require("devtools")){
  install.packages("devtools")
}
if (!require("SticsRPacks")){
  devtools::install_github("SticsRPacks/SticsRPacks@*release")
  devtools::install_github("SticsRPacks/CroPlotR@*release")
}
if (!require("Metrics")){
  install.packages("Metrics")
}
if (!require("ggpubr")){
  install.packages("ggpubr")
  install.packages("jpeg")
}
packages <- c("devtools","SticsOnR", "CroPlotR","SticsRFiles","Metrics",
              "ggplot2","ggpubr","jpeg","CroptimizR","tidyr","readxl",
              "gridExtra")
lapply(packages, library, character.only=TRUE)


init_ <- function(stics_path. = stics_path,
                  subdir. = subdir,
                  xml_dir. = ""){
  #initialize workspace path, path to folder containing xml files, usm list, observations
  if (exists("xml_dir")){
    xml_dir. <- xml_dir
  }
  workspace <<- file.path(stics_path., subdir.) #main javastics workspace
  xml_path <<- file.path(stics_path., subdir., xml_dir.) 
  usms <<- SticsRFiles::get_usms_list(file = file.path(stics_path.,
                                                       subdir.,
                                                       xml_dir.,
                                                       "usms.xml"))#all usms from javastics workspace
  set_obs(xml_path,usms)
} 

calib_init <- function(stics_path. = stics_path,
                       subdir. = subdir,
                       xml_path = ""){
  #convert xml files to txt files for use with stics_wrapper
  #set javastics and workspace path in for stics_wrapper_options
  #stics_path. - (chr) path to directory containing javastics.exe
  #subdir. - (chr) path to directory containing stics xml files
  
  stics_inputs_path <<- file.path(stics_path.,subdir.,"TxtFiles") #path to STICS txtfiles
  
  if (!dir.exists(stics_inputs_path) | 
      length(list.files(stics_inputs_path)) == 0){
    dir.create(stics_inputs_path, showWarnings = T)
    message("Creating directory at ", stics_inputs_path)
    
    gen_usms_xml2txt(
      javastics = stics_path.,
      workspace = file.path(stics_path.,subdir.,xml_path),
      out_dir = stics_inputs_path,
      verbose = TRUE
    )
  }
  #stics_wrapper_options
  sim_options <<- stics_wrapper_options(javastics = stics_path.,
                                       workspace = stics_inputs_path, 
                                       verbose = TRUE, 
                                       parallel = FALSE)
  message("Simulation options set as:\n javatics....", stics_path.,
          "\nworkspace....", stics_inputs_path)
}

set_obs <- function(workspace. = workspace,
            usms = NULL,
            variables = NULL){
  #set list of observations to use for calibration
  
  if (is.null(usms)){
    #use all usms
    usms <- SticsRFiles::get_usms_list(file = file.path(workspace., "usms.xml"))
  }
  obs <<- get_obs(workspace., usm = usms)
  if (!is.null(variables)){
    #filter obs_list to include only listed variables
    obs <<- filter_obs(obs, var = variables, include = TRUE)
  }
}

update_ops <- function(workspace,
                        variables,
                        append = F){
  #update var.mod file (file containing what variables to output) when
  #workspace split into individual usm folders with individual var.mod files
  usmdirs <- list.dirs(workspace, recursive = F)
  for (d in usmdirs){
    gen_varmod(file.path(d),variables,append=append)
  }
  message("✅  variables updated for all usms")
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
                         usms = NULL,
                         outdir = results_dir){
  #save results (mod files) from running STICS directly in a javastics workspace folder
  
  message("saving results to:\n", outdir)
  if (is.null(usms)){
    usms <- SticsRFiles::get_usms_list(file = file.path(workspace, "usms.xml"))
  }
  
  files <- list.files(workspace, ".sti", full.names = T)
  
  for (u in usms){
    message("saving ",u)
    dir.create(file.path(results_dir, u)) #create new directory for individiual usm
    file.copy(from = files[grep(u, files)], 
              to = file.path(results_dir, u)) #copy usm files to new directory
  }
  
  file.copy(from = files[grep("modhistory.sti", files)],
            to = results_dir)
  file.copy(from = files[grep("mod_profil_Chum.sti", files)],
            to = results_dir)
  file.copy(from = files[grep("mod_rapport.sti", files)],
            to = results_dir)
  
  message("✅ all results saved" )
}

save_results2 <- function(workspace,
                          outdir = results_dir){
  #save results from individual usm folders
  message("saving results to:\n", outdir)

  #save mod_b and mod_s files for each usm
  usm_dirs <- list.dirs(workspace, recursive = F)
  
  for (d in usm_dirs){
    #create directory for usm in output directory
    matches <- grep("[^/\\\\]+$", d, value = TRUE)
    usm_name <- last_name <- regmatches(matches, regexpr("[^/\\\\]+$", matches))
    
    new_dir <- file.path(outdir,usm_name)
    dir.create(new_dir)
    
    files <- list.files(d, full.names = T)
  
    #copy mod files from workspace into output directory for specific usm
    file.copy(files[grep("mod_[b]",files)], new_dir)
    file.copy(files[grep("mod_[s]",files)], new_dir)
    file.copy(files[grep("modhistory",files)], new_dir)
    message(usm_name)
  }
  message("✅ all results saved" )
}


calibrate_STICS <- function(situations,
                            param_values,
                            out_dir = results_dir,
                            sim_options. = sim_options){
  
  write.table(param_values, 
              file.path(out_dir,"parameter_table.txt"), 
              row.names = T)
  
  #run stics before calibration runs
  sim_before_optim <<- stics_wrapper(model_options = sim_options.)
  
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
    
    save_results2(stics_inputs_path)
    get_stats_summary(simulations = results$sim_list,
                      usms = situations)
    
    message("Results saved in: ", subset)
    
  }
}

runSTICS <- function(path = stics_path,
                     workspace. = workspace,
                     usms = NULL,
                     version = NULL){
  #run javastics directly in subdir workspace
  
  if (is.null(usms)){
    usms <- SticsRFiles::get_usms_list(file = file.path(workspace., "usms.xml"))
  }
  results <- run_javastics(stics_path, workspace., usms)
  
  save_results(workspace = workspace., usms = usms)
  get_stats_summary(usms = usms, version = version)
  
  return(results)
}



