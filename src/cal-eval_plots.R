
get_stats_summary <- function(workspace. = workspace, 
                              version = "", 
                              output_dir = results_dir,
                              usms = NULL,
                              obs_list = NULL,
                              simulations = NULL,
                              variables = NULL,
                              fname = "",
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
                     "RMSE","nRMSE","rRMSE","MAE","Bias","EF",
                     "RMSEs","RMSEu","RMSEs_sd","RMSEu_sd")], 
            file.path(output_dir, sprintf("%s_metrics%s.csv",version, fname))
            )
  
  if (plot){
    plotRMSE(stats,output_dir, version)
  }
  
  return(stats)
  
}

get_single_stats <- function(simulations, obs_list, output_dir) {
  #calculate statistics for individual usms
  stats <- summary(simulations, obs = obs_list, all_situations = F)
  stats$RMSEs_sd <- stats$RMSEs / stats$sd_obs
  stats$RMSEu_sd <- stats$RMSEu / stats$sd_obs
  
  for (n in names(simulations)) {
    write.csv(stats[stats$situation == n, c(
      "variable",
      "n_obs",
      "sd_obs",
      "sd_sim",
      "RMSE",
      "nRMSE",
      "rRMSE",
      "MAE",
      "Bias",
      "EF",
      "RMSEs",
      "RMSEu",
      "RMSEs_sd",
      "RMSEu_sd"
    )], file.path(output_dir,n,sprintf("%s_metrics.csv", n)))
  }
}

plotRMSE <- function(stats, output_dir, version, save = T,
                     update_labs = F, customize = F, 
                     colours = NULL, labels = NULL,
                     width = 7, height = 5){
  #create plot of RMSEs/std dev of the measurements and RMSEu/std dev of the measurements
  img <- readJPEG("RMSEcheck.jpg")
  
  if(update_labs){
    stats <- stats |>
      mutate(variable = case_when(
        variable == "masec_n" ~ "TB",
        variable == "mafrais" ~ "TFW",
        variable == "mafruit" ~ "TuB",
        variable == "pdsfruitfrais" ~ "Yield",
        variable == "msrac_n" ~ "RtB",
        variable == "QNplante" ~ "TN",
        variable == "QNgrain" ~ "TuN",
        variable == "lai_n" ~ "lai",
        variable == "Qem_N2O" ~ "cum N2O",
        variable == "QNrac" ~ "RtN",
        variable == "QCrac" ~ "RtC",
        TRUE ~ variable
      ))
  }
  
  if (is.null(version)){
    #if no version title, make title name of subdir
    version <- subdir
  }
  
  if(customize){
  plot <- ggplot(stats, aes(x=RMSEu_sd, y = RMSEs_sd, 
                            color=variable, size=1),shape=1) +
    background_image(img) + 
    geom_point() + 
    geom_text_repel(label=stats$variable, nudge_y = 0.03) +
    geom_abline(intercept=0, slope=1) +
    labs(x = "RMSEu / std-meas",y="RMSEs / std-meas", title=version, color="Variable") +
    expand_limits(x = 0, y = 0) +
    scale_x_continuous(expand = c(0, 0)) + 
    scale_y_continuous(expand = c(0, 0)) +
    scale_color_manual(values = colours,
                       labels = labels) +
    coord_cartesian(ylim = c(0,0.8), xlim=c(0,0.8)) +
    theme(text = element_text(size = 18),legend.text=element_text(size=10),
          legend.position = "bottom") +
    guides(size="none",colour = guide_legend(override.aes = list(size=5))) +
    theme_minimal()
  }else{
    plot <- ggplot(stats, aes(x=RMSEu_sd, y = RMSEs_sd, 
                              color=variable, size=1),shape=1) + background_image(img) + geom_point() + 
      geom_text_repel(label=stats$variable, nudge_y = 0.03) +
      geom_abline(intercept=0, slope=1) +
      labs(x = "RMSEu / std-meas",y="RMSEs / std-meas", title=version, color="Variable") +
      expand_limits(x = 0, y = 0) +
      scale_x_continuous(expand = c(0, 0)) + 
      scale_y_continuous(expand = c(0, 0)) +
      coord_cartesian(ylim = c(0,0.8), xlim=c(0,0.8)) +
      theme(text = element_text(size = 18),legend.text=element_text(size=10)) +
      guides(size="none",colour = guide_legend(override.aes = list(size=5))) +
      theme_minimal()
  }
  
  plot #show plot
  if(save == T){
    ggsave(file.path(output_dir,
                     paste(version,"_rmse.png",sep="")), 
           width=width, height=height)
  }
  
  message("✅succesufully created RMSE plot")
}

plot_simvobs <- function(baseline, 
                        new_sims = NULL,
                        obs. = obs,
                        ver = version,
                        subname = "",
                        width = 20,
                        height = 15,
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
  
  
  for (n in names(baseline)){
    p1 <-
      pbf[[n]] +
      labs(title = paste(n,ver,sep=" ")) +
      theme(plot.title = element_text(size = 9, hjust = 0.5))
    
    if(!is.null(new_sims)){
      p2 <- paft[[n]] +
        labs(title = paste(n, "After Optimization", sep=" ")) +
        theme(plot.title = element_text(size = 9, hjust = 0.5))
    }
 
    # ylim(
    #   NA,
    #   ggplot_build(p1)$layout$panel_params[[1]]$y.range[2]
    # ) +
      
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
                       n,
                       sprintf("%s_%s_simvobs.png",n,subname)
    )
    ggsave(
      fname,
      plot = p3,
      width = width,
      height = height,
      units = "cm"
    )
    message("Sim vs obs plot for ", 
            n, 
            " saved to:\n",
            fname)
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
  
  for (n in names(sims)){
    
    p1 <-
      pbf[[n]] +
      labs(title = paste(n,ver,sep=" ")) +
      theme(plot.title = element_text(size = 9, hjust = 0.5))
    
    # Save the graph
    fname <- file.path(output_dir,
                      n,
                      paste0(paste(n,"_sims.png",sep="")))
    ggsave(
      fname,
      plot = p1,
      width = 20,
      height = 15,
      units = "cm"
    )
    message("Simulation plot for ", 
            n, 
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
  #add attribute "cropr_simulation" to list in order for SticsRPacks
  #to recognize it as a list of simulations
  attr(slist, "class") <- "cropr_simulation"
  return(slist)
}

dailyGHG <- function(obs,sims){
  #use simulated cummulative GHG values to obtain daily simulated GHG value (CO2 + N2O)
  i <- 1
  ghgs <- list()
  for (n in names(sims)){
    x <- obs[[n]][!is.na(obs[[n]]$CO2sol),1]
    y <- sims[[n]][which(sims[[n]]$Date %in% x | as.Date(sims[[n]]$Date) %in% as.Date((x-1))),]
    ghgs[[i]] <- getGHG(y) 
    i <- i + 1
  }
  names(ghgs) <- names(sims)
  return(append_sims(ghgs))
}
  
getGHG <- function(x){
  
  ghgsim <- list()
  l <- 1
  n <- 1
  while(n < nrow(x)){
    if (x$Date[n+1] - x$Date[n] == 1){
      ghgsim$Date[l] <- x$Date[n+1]
      ghgsim$CO2sol[l] <- x$GHG[n+1] - x$GHG[n]
      l <- l + 1
    }
    n <- n+1
  }
  ghgsim$Date <- as.POSIXct(ghgsim$Date,tz="UTC", format = '%Y-%m-%d')
  return(as.data.frame(ghgsim))
}

get_gs_sims <- function(){
  #get all simulations from grid search directory
  dirs <- list.dirs(results_dir, recursive = F)
  all_sims <- list()
  j <- 1
  for (d in dirs[-c(grep("before", dirs))]){
    m <- grep("[^/\\\\]+$", d, value = TRUE)
    dnum <- regmatches(m, regexpr("[^/\\\\]+$", m))
    sds <- list.dirs(d, recursive = F)
    sim_list <- list()
    i <- 1
    for (s in sds){
      matches <- grep("[^/\\\\]+$", s, value = TRUE)
      usm_name  <- regmatches(matches, regexpr("[^/\\\\]+$", matches))
      sim <- read.csv(file.path(s, sprintf("mod_s%s.sti", usm_name)), sep="\t")
      sim_list[[i]] <- sim
      sim_list[[i]]$Date <- as.POSIXct(sim_list[[i]]$Date)
      names(sim_list)[i] <- usm_name
      i <- i + 1
    }
    sim_list <- append_sims(sim_list)
    all_sims[[j]] <- sim_list
    names(all_sims)[j] <- dnum
  }
  return(all_sims)
}


plot_error <- function(params,
                       results,
                       fname = "",
                       out_dir = results_dir){
  #generate plots of rmse_s vs rmse_u for 1 parameter
  
  
  data <- cbind(results, params)
  x_axis <- names(params)
  
  fig <- ggplot(data, aes(x = .data[[x_axis]])) +
    geom_line(aes(y = RMSEs_sd),color = "gray30") +
    geom_line(aes(y = RMSEu_sd), color = "orange2") +
    scale_y_continuous(name = "RMSEs",
                       sec.axis = sec_axis(~.*1,name="RMSEu")) +
    labs(title = sprintf("%s %s",x_axis, fname)) +
    theme_minimal() +
    theme(
      axis.title.y = element_text(color = "gray30", size=13),
      axis.title.y.right = element_text(color = "orange2", size=13)
    ) +
    scale_color_manual(values = c("gray30", "orange2"))
    
    
    reg1 <- lm(data[,43] ~ data[,45])
    reg2 <- lm(data[,44] ~ data[,45])
    cm <- rbind(coef(reg1),coef(reg2)) # Coefficient matrix
    intercepts <- c(-solve(cbind(cm[,2],-1)) %*% cm[,1])
    
    
    ggsave(
      file.path(out_dir,sprintf("%s_%s.png",x_axis,fname)),
      plot = fig,
      width = 20,
      height = 15,
      units = "cm")
    
  
  names(intercepts) <- c(x_axis, "RMSEs/u")
  write.table(intercepts, 
              file = file.path(out_dir, sprintf("%s_intercepts.txt", fname)),
              sep = "\t",
              row.names = F)
  message("✅succesfully created error plot")
}

plot_error2 <- function(params, results, 
                       p1,
                       p2,
                       fname = "",
                       out_dir = results_dir){
  
  #generate plots of rmse_s vs rmse_u for 2 parameters
  if (!exists(file.path(out_dir,"error_plots"))){
    dir.create(file.path(out_dir,"error_plots"))
  }
  
  new_dir <- file.path(out_dir, "error_plots",fname)
  dir.create(new_dir)
  message(sprintf("plots for %s and %s ", p1, p2), 
          "saved in:\n", new_dir)
  
  data <- cbind(results, params)
  p1vals <- unique(data[,which(names(data)==p1)])
  p2vals <- unique(data[,which(names(data)==p2)])
  
  if (length(p1vals) > length(p2vals)){
    x_axis <- p1
    static <- p2
    iter <- p2vals
  }else{
    x_axis <- p2
    static <- p1
    iter <- p1vals
  }
  
  plots <- list()
  intercepts <- data.frame()
  
  n <- 1
  for (i in iter){
    x_axisc <- grep(x_axis, names(data))
    sc <- grep(static, names(data))
    subset <- data[data[,sc]==i , c(43,44,x_axisc)]
    
    fig <- ggplot(subset, aes(x = .data[[x_axis]])) +
      geom_line(aes(y = RMSEs_sd),color = "gray30") +
      geom_line(aes(y = RMSEu_sd), color = "orange2") +
      scale_y_continuous(name = "RMSEs",
                         sec.axis = sec_axis(~.*1,name="RMSEu")) +
      labs(title = sprintf("%s,%s = %.2f",fname,static,i)) +
      theme_minimal() +
      theme(
        axis.title.y = element_text(color = "gray30", size=13),
        axis.title.y.right = element_text(color = "orange2", size=13)
      ) +
      scale_color_manual(values = c("gray30", "orange2"))

    plots[[n]] <- fig
    
    # reg1 <- lm(subset[,1] ~ subset[,3])
    # reg2 <- lm(subset[,2] ~ subset[,3])
    # cm <- rbind(coef(reg1),coef(reg2)) # Coefficient matrix
    # intrcpt <- c(-solve(cbind(cm[,2],-1)) %*% cm[,1])
    # intrcpt[3] <- i
    # 
    # intercepts <- rbind(intercepts,intrcpt)
    
    ggsave(
      file.path(new_dir,sprintf("%s_%s_%.2f.png",fname,static,i)),
      plot = fig,
      width = 20,
      height = 15,
      units = "cm")
    
    n <- n + 1
  }
  p3 <- grid.arrange(
    grobs = plots
  )
    
  ggsave(
    file.path(new_dir,"allplots.png"),
    plot = p3,
    width = 30,
    height = 25,
    units = "cm")
  
  # names(intercepts) <- c(x_axis, "RMSEs/u",static)
  # write.table(intercepts, 
  #             file = file.path(new_dir, "intercepts.txt"),
  #             sep = "\t",
  #             row.names = F)
  message("✅succesfully created all error plots")
}