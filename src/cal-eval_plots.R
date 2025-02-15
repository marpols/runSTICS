
get_stats_summary <- function(workspace. = workspace, 
                              version = NULL, 
                              output_dir = results_dir,
                              javastics_path = stics_path,
                              usms = NULL,
                              simulations = NULL,
                              variables = NULL, 
                              plot=T){
  #calculate statics/metrics for crop model
  
  if (is.null(usms)){
    usms <- SticsRFiles::get_usms_list(file = file.path(workspace., "usms.xml"))
  }
  obs_list <- get_obs(workspace., usm = usms)
  if (!is.null(variables)){
    #filter obs_list to include only listed variables
    obs_list <- filter_obs(obs_list, var = variables, include = TRUE)
  }
  if(is.null(simulations)){
    simulations <- SticsRFiles::get_sim(javastics = javastics_path,
                                      workspace = workspace., 
                                      usm = usms)
  }
  
  stats <- summary(simulations, obs = obs_list, all_situations = T)
  stats$RMSEs_sd <- stats$RMSEs/stats$sd_obs
  stats$RMSEu_sd <- stats$RMSEu/stats$sd_obs
  
  write.csv(stats[,c("variable", "n_obs", "sd_obs", "sd_sim","RMSEs","RMSEu","RMSEs_sd","RMSEu_sd")], 
            file.path(output_dir, paste(version,"_metrics.csv",sep="")))
  
  if (plot){
    plotRMSE(stats,output_dir)
  }
  
  return(stats)
  
}

plotRMSE <- function(stats, output_dir){
  #create plot of RMSEs/std dev of the measurements and RMSEu/std dev of the measurements
  img <- readJPEG("RMSEcheck.jpg")
  
  if (!is.null(version)){
    #if no version title, make title name of subdir
    version = subdir
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
  
  ggsave(file.path(output_dir, paste(version,".png",sep="")), width=7, height=5)
}

plot_simvobs <- function(baseline = NULL, 
                        new_sim,
                        obs = obs_list){
  #Plot comparisons of simulations and observations using CroPlotR
  
  if (!is.null(baseline)){
    #before optimization
    pbf <- plot(
      baseline$sim_list,
      obs = obs_list,
      select_dyn = c("common")
    )
  }
  
  #after optimization
  paft <-
    plot(
      new_sim$sim_list,
      obs = obs_list,
      select_dyn = c("common")
    )
  
  n <<- 1
  while (length(new_sim$sim_list)){
    if (!is.null(baseline)){
      p1 <-
        pbf[[n]] +
        labs(title = paste(pbf[[n]]$data$Sit_Name[1],"Before Optimization",sep=" ")) +
        theme(plot.title = element_text(size = 9, hjust = 0.5))
    }
    
    p2 <- paft[[n]] +
      labs(title = paste(paft[[n]]$data$Sit_Name[1], "After Optimization", sep=" ")) +
      ylim(
        NA,
        ggplot_build(p1)$layout$panel_params[[1]]$y.range[2]
      ) +
      theme(plot.title = element_text(size = 9, hjust = 0.5))
    
    if (!is.null(baseline)){
      p3 <- grid.arrange(
        grobs = list(p1, p2),
        nrow = 1,
        ncol = 2,
        widths = c(5, 5)
      )
    }else {
      p3 <- p2
    }
    
    # Save the graph
    ggsave(
      file.path(
        optim_options$out_dir,
        paste0(paste(p[[n]]$data$Sit_Name[1],"simvobs_plot.png",sep=""))
      ),
      plot = p3
    )
    n <<- n + 1
  }
  
}
