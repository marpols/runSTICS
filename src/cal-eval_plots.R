

obs_list <- get_obs(javastics_workspace_path, usm = sit_name)
obs_list <- filter_obs(obs_list, var = var_name, include = TRUE)

sims_list <- SticsRFiles::get_sim(javastics = javastics_workspace_path,
                             workspace = data_dir, 
                             usm = sit_name)

summary(sims_list, obs = obs_list, all_situations = T)

organize <- function(usm_list, subset){
  #group usms that are in subset into new list
  new_list <- list()
  for (usm in usm_list) {
    new_list <- append(new_list, subset(usm, names(usm) %in% subset))
  }
  #attach class: cropr_simulation atrriubte to sim lists so it is recognized by SticsRPacks.
  attr(new_list, "class") <- "cropr_simulation"
  return(new_list)
}



obs_calib <- organize(obs_list, usm_cal)
obs_eval <- organize(obs_list, usm_eval)

sims_calib <- organize(sims_list, usm_calib)
sims_eval <- organize(sims_list, usm_eval)


cal_stats <- summary(sims_calib, obs = obs_calib, all_situations = T)
eval_stats <- summary(sims_calib, obs = obs_eval, all_situations = T)

write.csv(cal_stats[,c("variable", "n_obs", "sd_obs", "sd_sim","RMSEs","RMSEu")],paste("cal",ver,".csv",sep=""))
write.csv(eval_stats[,c("variable", "n_obs", "sd_obs", "sd_sim","RMSEs","RMSEu")],paste("eval",ver,".csv",sep=""))

#RMSEu vs. RMSEs Plot-----------------------------------------------------------

img <- readJPEG("RMSEcheck.jpg")

cal_stats$RMSEs_sd <- cal_stats$RMSEs/cal_stats$sd_obs
eval_stats$RMSEu_sd <- eval_stats$RMSEu/eval_stats$sd_obs

if (calibration){
  stats <- cal_stats
} else{
  stats <- eval_stats
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
plot
ggsave(paste(getwd(),"/RMSEuRMSEs_",version,".png",sep=""), width=7, height=5)