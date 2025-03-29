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
if (!require("readxl")){
  install.packages("readxl")
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
  }
  
  gen_usms_xml2txt(
    javastics = stics_path.,
    workspace = file.path(stics_path.,subdir.,xml_path),
    out_dir = stics_inputs_path,
    verbose = TRUE
  )

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
  
  i <- 1
  while (i <= length(lwr)){
    var_list[[vars[i]]] <- seq(lwr[[i]],upr[[i]],by[i])
  i <- i + 1
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
                          outdir = results_dir,
                          usms = NULL){
  #save results from individual usm folders
  message("saving results to:\n", outdir)

  #save mod_b and mod_s files for each usm
  usm_dirs <- list.dirs(workspace, recursive = F)
  
  if (!is.null(usms)){
    all_usms <- sapply(
      usm_dirs,
      function(d) regmatches(grep("[^/\\\\]+$", d, value = TRUE), 
                             regexpr("[^/\\\\]+$", grep("[^/\\\\]+$", d, value = TRUE))) %in% usm_cal,
      USE.NAMES = T
      )
    usm_dirs <- names(all_usms)[all_usms]
  }
  
  for (d in usm_dirs){
    #create directory for usm in output directory
    matches <- grep("[^/\\\\]+$", d, value = TRUE)
    usm_name  <- regmatches(matches, regexpr("[^/\\\\]+$", matches))
    
    new_dir <- file.path(outdir,usm_name)
    if(!exists(new_dir)){
      dir.create(new_dir)
    }
    
    files <- list.files(d, full.names = T)
  
    #copy mod files from workspace into output directory for specific usm
    file.copy(files[grep("mod_[b]",files)], new_dir)
    file.copy(files[grep("mod_[s]",files)], new_dir)
    file.copy(files[grep("modhistory",files)], new_dir)
    message(usm_name)
  }
  message("✅ all results saved" )
}

save_results3 <- function(sims,
                          out_dir = results_dir,
                          usms = NULL){
  #save results from a cropr_simulation list
  n <- 1
  message("Saving simulations in\n:", out_dir)
  if (!is.null(usms)){
    sims <- sims[names(sims) %in% usms]
  }
  while (n <= length(sims)){
    message(names(sims[n]))
    dir.create(file.path(out_dir,
                         names(sims[n]))
               )
    write.table(sims[[n]],
                file = file.path(out_dir,
                                 names(sims[n]),
                                 sprintf("mod_s%s.sti",names(sims[n]))),
                sep = "\t"
                )
    n <- n + 1
  }
  message("✅ all results saved" )
}

find_min <- function(stats,vars,
                     out_dir = results_dir){
  #find the smallest rmse_s and rmse_u from grid search
  min_error <- data.frame()
  i <- 1
  for (v in vars){
    rmse <- stats[stats$variable == v, c(1,4,43,44)] #get rmse_s and rmse_u for variable v
    min_error <- rbind(min_error,c(v,
                                   rmse$run[which.min(rmse$RMSEs_sd)],
                                   min(rmse$RMSEs_sd),
                                   rmse$run[which.min(rmse$RMSEu_sd)],
                                   min(rmse$RMSEu_sd)))
    i <- i + 1
  }
  write.table(min_error,file = file.path(out_dir,"min_error.txt"))
  return(min_error)
}


calibrate_STICS <- function(sits,
                            param_values,
                            vars = NULL,
                            ver = version,
                            out_dir = results_dir,
                            sim_options. = sim_options,
                            verbose = T){
  if (!verbose){
    suppressMessages()
  }
  message("Starting at: ", Sys.time())
  write.table(param_values, 
              file.path(out_dir,"parameter_table.txt"), 
              row.names = T)
  
  #run stics before calibration runs
  message("running STICS with original values")
  sim_before_optim <<- stics_wrapper(model_options = sim_options.,
                                     situation = sits,
                                     var = vars)
  if (!exists(file.path(out_dir,"before"))){
    dir.create(file.path(out_dir,"before"))
  }
  save_results2(stics_inputs_path, 
                outdir = file.path(out_dir,"before"),
                usms = sits)
  
  get_stats_summary(version = ver,
                    output_dir = file.path(out_dir,"before"),
                    obs_list = obs,
                    simulations = sim_before_optim$sim_list)
  
  message("Running STICS over the following parameters:\n",
          paste0(capture.output(param_values), collapse = "\n"))
  
  if (typeof(param_values) == "list"){
    #parse over dataframe of multiple variable combinations (gridsearch)
    
    results_metrics <- data.frame() #store metrics for each run
    
    n <- 1
    while (n <= nrow(param_values)){
      vals <- as.double(param_values[n,])
      names(vals) <- colnames(param_values)
      
      message("Running with:\n", paste0(capture.output(vals), collapse = "\n"))
      
      results <- stics_wrapper(model_options = sim_options.,
                               param_values = vals,
                               situation = sits,
                               var = vars)

      subset <- file.path(out_dir,n)
      dir.create(subset)
      
      write.table(vals, file = file.path(subset,"params.txt"))
      
      stats <- get_stats_summary(simulations = results$sim_list,
                        output_dir = subset, 
                        version = n, 
                        usms = sits)
      
      results_metrics <- rbind(results_metrics,
                               data.frame(run = n, stats))
      
      save_results3(results$sim_list,subset)
      
      plot_simvobs(baseline = sim_before_optim$sim_list, 
                   new_sims = results$sim_list,
                   obs = obs,
                   output_dir = subset)
      plot_simvobs(results$sim_list,
                   obs = obs,
                   output_dir = subset,
                   subname = "after")
      
      message("Results saved in: ", subset)
      
      n <- n + 1
    }
    message("End at: ", Sys.time())
    #save summary of all statistics
    write.table(results_metrics[,c("run","variable", "n_obs", "sd_obs", "sd_sim",
                           "RMSE","nRMSE","rRMSE",
                           "RMSEs","RMSEu","RMSEs_sd","RMSEu_sd")],
                file.path(out_dir,"metrics_summary.csv"))
    return(results_metrics)
  
  } else{
    #run once for one set of variables
    
    results <- stics_wrapper(model_options = sim_options.,
                             param_values = param_values,
                             situation = sits,
                             var = vars)
    
    plot_simvobs(baseline = sim_before_optim$sim_list, 
                 new_sims = results$sim_list,
                 obs = obs)
    
    save_results3(results$sim_list)
    get_stats_summary(simulations = results$sim_list,
                      usms = sits)
    
    message("Results saved in: ", out_dir)
    message("End at: ", Sys.time())
    return(results)
    
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



