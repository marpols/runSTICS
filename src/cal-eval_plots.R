
get_stats_summary <- function(workspace. = workspace, 
                              version = NULL, 
                              output_dir = results_dir,
                              usms = NULL,
                              obs_list = NULL,
                              simulations = NULL,
                              variables = NULL, 
                              plot=T){
  #calculate statics/metrics for crop model
  
  if (is.null(usms) & (is.null(obs_list)|is.null(simulations))){
    usms <- SticsRFiles::get_usms_list(file = file.path(workspace., "usms.xml"))
  }
  if (is.null(obs_list)){
    message("obtaining obersvation files")
    obs_list <- get_obs(workspace., usm = usms)
  }
  if (!is.null(variables)){
    #filter obs_list to include only listed variables
    obs_list <- filter_obs(obs_list, var = variables, include = TRUE)
  }
  if(is.null(simulations)){
    message("obtaining simulation files")
    simulations <- get_sim(workspace = workspace.,usm = usms)
  }
  
  stats <- summary(simulations, obs = obs_list, all_situations = T)
  stats$RMSEs_sd <- stats$RMSEs/stats$sd_obs
  stats$RMSEu_sd <- stats$RMSEu/stats$sd_obs
  
  write.csv(stats[,c("variable", "n_obs", "sd_obs", "sd_sim",
                     "RMSEs","RMSEu","RMSEs_sd","RMSEu_sd")], 
            file.path(output_dir, paste(version,"_metrics.csv",sep="")))
  
  if (plot){
    plotRMSE(stats,output_dir, version)
  }
  
  return(stats)
  
}

plotRMSE <- function(stats, output_dir, version){
  #create plot of RMSEs/std dev of the measurements and RMSEu/std dev of the measurements
  img <- readJPEG("RMSEcheck.jpg")
  
  if (is.null(version)){
    #if no version title, make title name of subdir
    version <- subdir
  }
  
  plot <- ggplot(stats, aes(x=RMSEu_sd, y = RMSEs_sd, 
                            color=factor(variable), size=1),shape=1) + background_image(img) + geom_point() + 
    geom_text(label=stats$variable, nudge_x = 0.05, nudge_y = 0.025, check_overlap = F) +
    geom_abline(intercept=0, slope=1) +
    labs(x = "RMSEu / std-meas",y="RMSEs / std-meas", title=version, color="Variable") +
    expand_limits(x = 0, y = 0) +
    scale_x_continuous(expand = c(0, 0)) + 
    scale_y_continuous(expand = c(0, 0)) + 
    coord_cartesian(ylim = c(0,0.8), xlim=c(0,0.8)) +
    theme(text = element_text(size = 18),legend.text=element_text(size=10)) +
    guides(size="none",colour = guide_legend(override.aes = list(size=5)))
  
  plot #show plot
  
  ggsave(file.path(output_dir, paste(version,"_rmse.png",sep="")), width=7, height=5)
  
  message("✅succesufully created RMSE plot")
}

plot_simvobs <- function(baseline, 
                        new_sims = NULL,
                        obs. = obs,
                        ver = version,
                        output_dir = results_dir){
  #Plot comparisons of simulations and observations using CroPlotR
  

  #current run or before optimization
  pbf <- plot(
    baseline,
    obs = obs.,
    select_dyn = c("common")
  )
  
  if (!is.null(new_sims)){
    #after optimization
    paft <-
      plot(
        new_sims,
        obs = obs.,
        select_dyn = c("common")
      )
    ver <- paste("Before Optimization")
  }
  
  n <<- 1
  while (n <= length(baseline)){
    p1 <-
      pbf[[n]] +
      labs(title = paste(names(obs.)[n],ver,sep=" ")) +
      theme(plot.title = element_text(size = 9, hjust = 0.5))
    
    if(!is.null(new_sims)){
      p2 <- paft[[n]] +
        labs(title = paste(names(obs.)[n], "After Optimization", sep=" ")) +
        ylim(
          NA,
          ggplot_build(p1)$layout$panel_params[[1]]$y.range[2]
        ) +
        theme(plot.title = element_text(size = 9, hjust = 0.5))
    }
    
    if (!is.null(new_sims)){
      p3 <- grid.arrange(
        grobs = list(p1, p2),
        nrow = 1,
        ncol = 2,
        widths = c(5, 5)
      )
    }else {
      p3 <- p1
    }
    
    # Save the graph
    fname <- file.path(output_dir,
                       names(obs.)[n],
                       paste0(paste(names(obs.)[n],"_simvobs.png",sep=""))
    )
    ggsave(
      fname,
      plot = p3,
      width = 20,
      height = 15,
      units = "cm"
    )
    message("Sim vs obs plot for ", 
            names(obs.)[n], 
            " saved to:\n",
            fname)
    n <<- n + 1
  }
  message("✅succesfully created all sims vs obs plots")
}

plot_sim <- function(sims,
                     vars = NULL,
                     ver = version,
                     output_dir = results_dir
                     ){
  #plot simulation output without observations
  
  if (is.null(vars)){
    pbf <- plot(sims)
  } else {
    pbf <- plot(sims, var = vars)
  }
  
  n <<- 1
  while (n <= length(sims)){
    sit <- names(sims)[n]
    
    p1 <-
      pbf[[n]] +
      labs(title = paste(sit,ver,sep=" ")) +
      theme(plot.title = element_text(size = 9, hjust = 0.5))
    
    # Save the graph
    fname <- file.path(output_dir,
                      sit,
                      paste0(paste(sit,"_sims.png",sep="")))
    ggsave(
      fname,
      plot = p1,
      width = 20,
      height = 15,
      units = "cm"
    )
    message("Simulation plot for ", 
            sit, 
            " saved to:\n",
            fname
            )
    n <<- n + 1
  }
}

get_devstgs <- function(sims,
                        output_dir = results_dir){
  #return simulated dates of development stages
  #model outputs should include: 
  #igers, ilevs, iamfs, iflos, idrps,idepdess, imats

  table <- data.frame()
  n <- 1
  while (n <= length(sims)){
    sit <- names(sims[n])
    df <- data.frame(germination = max(sims[[n]]$igers),
                     emergence = max(sims[[n]]$ilevs),
                     juvenile.end = max(sims[[n]]$iamfs),
                     flowering = max(sims[[n]]$iflos),
                     organ.filling = max(sims[[n]]$idrps),
                     organ.water.dynamics = max(sims[[n]]$idebdess),
                     maturity = max(sims[[n]]$imats))
    row.names(df) <- sit
    table <- rbind(table, df)
    n <- n + 1
  }
  write.table(table, file.path(output_dir,"sim_growthStages.csv"))
  return(table)
}

append_sims <- function(slist){
  attr(slist, "class") <- "cropr_simulation"
  return(slist)
}
