#Calibrate the STICS model using CroptimizeR
#From https://sticsrpacks.github.io/CroptimizR/articles/Parameter_estimation_simple_case.html

data_dir <- file.path(stics_path,subdir)

javastics_workspace_path <- file.path(stics_path,subdir, "XmlFiles")

stics_inputs_path <- file.path(stics_path,subdir,"TxtFiles")
dir.create(stics_inputs_path, showWarnings = FALSE)

gen_usms_xml2txt(
  javastics = stics_path,
  workspace = javastics_workspace_path,
  out_dir = stics_inputs_path,
  verbose = TRUE
)

# Set the model options (see '? stics_wrapper_options' for details)
model_options <- stics_wrapper_options(javastics = stics_path,
                                       workspace = stics_inputs_path,
                                       parallel = FALSE
                                       )



# Run the model on all situations found in stics_inputs_path before optimization
sim_before_optim <- stics_wrapper(model_options = model_options)

#get list of observations to use for calibration
obs_list <- get_obs(javastics_workspace_path, usm = sit_name)
obs_list <- filter_obs(obs_list, var = var_name, include = TRUE)

# upper and lower bounds of parameters to consider
param_info <- list(
  lb = lowerbnds,
  ub = upperbnds
)

#Set options for the parameter estimation method
optim_options <- list()
optim_options$nb_rep <- reps  # Number of repetitions of the minimization
                              # (each time starting with different initial
                              # values for the estimated parameters)
optim_options$maxeval <- max  # Maximum number of evaluations of the
                              # minimized criteria
optim_options$xtol_rel <- tolerance # Tolerance criterion between two iterations
                                    # (threshold for the relative difference of
                                    # parameter values between the 2 previous
                                    # iterations)
optim_options$out_dir <- results_dir # path where to store the results
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

#Plot comparisons of simulations and observations using CroPlotR

#before optimization
p <- plot(
  sim_before_optim$sim_list,
  obs = obs_list,
  select_dyn = c("common")
)

#after optimization
p <-
  plot(
    sim_after_optim$sim_list,
    obs = obs_list,
    select_dyn = c("common")
  )

n <<- 1
while (length(sim_before_optim$sim_list)){
  p1 <-
    p[[n]] +
    labs(title = paste(p[[n]]$data$Sit_Name[1],"Before Optimization",sep=" ")) +
    theme(plot.title = element_text(size = 9, hjust = 0.5))
  
  p2 <- p[[n]] +
    labs(title = paste(p[[n]]$data$Sit_Name[1], "After Optimization", sep=" ")) +
    ylim(
      NA,
      ggplot_build(p1)$layout$panel_params[[1]]$y.range[2]
    ) +
    theme(plot.title = element_text(size = 9, hjust = 0.5))
  
  p3 <- grid.arrange(
    grobs = list(p1, p2),
    nrow = 1,
    ncol = 2,
    widths = c(5, 5)
  )
  # Save the graph
  ggsave(
    file.path(
      optim_options$out_dir,
      paste0(paste(p[[n]]$data$Sit_Name[1],"sim_obs_plots", ".png"))
    ),
    plot = p3
  )
  n <<- n + 1
}

