
get_stats_summary <- function(workspace = workspace, 
                              version = version, 
                              output_dir = results_dir,
                              javastics_path = stics_path,
                              usms = NULL,
                              simulations = NULL,
                              variables = NULL, 
                              plot=T){
  #calculate statics/metrics for crop model
  if (is.null(usms)){
    usms <- SticsRFiles::get_usms_list(file = file.path(workspace, "usms.xml"))
  }
  obs_list <- get_obs(workspace, usm = usms)
  if (!is.null(variables)){
    #filter obs_list to include only listed variables
    obs_list <- filter_obs(obs_list, var = variables, include = TRUE)
  }
  if(is.null(simulations)){
    simulations <- SticsRFiles::get_sim(javastics = javastics_path,
                                      workspace = workspace, 
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
  
}

plotRMSE <- function(stats, output_dir){
  #create plot of RMSEs/std dev of the measurements and RMSEu/std dev of the measurements
  img <- readJPEG("RMSEcheck.jpg")
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
  plot
  ggsave(file.path(output_dir, paste(version,".png",sep="")), width=7, height=5))
}