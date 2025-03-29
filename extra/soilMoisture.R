data <- "\\\\chaemci01-cifs.efs.agr.gc.ca\\common\\COMMON\\Workgroups\\MesbahM\\Students\\Maria\\Mountain Gem Data\\2024 Data\\SoilMoistureHF.xlsx"

sm_hf <- as.data.frame(read_excel(data,col_names=FALSE))
names(sm_hf) <- sm_hf[1,]
sm_hf <- sm_hf |> group_by(plot)
sm_hf$plotname <- sub("\\s(H|F)$", "", sm_hf$plot)
sm_hf$plotname <- sub(" ","", sm_hf$plotname)
dates <- sm_hf[1,-1:-3]
dates <- dates[,1:ncol(dates)-1]
dates <- as.character(dates)
dates[4] <- "06/06/2024"
dates <- as.POSIXct(dates, format = "%d/%m/%Y")
hills <- sm_hf[sm_hf$`H/F`== "H",]
furrows <- sm_hf[sm_hf$`H/F` == "F",]

thills <- as.data.frame(t(hills)[-1:-3,])
names(thills) <- thills[nrow(thills),]
thills <- thills[1:19,]
thills$Date <- dates

tfurrows <- as.data.frame(t(furrows)[-1:-3,])
names(tfurrows) <- tfurrows[nrow(tfurrows),]
tfurrows<- tfurrows[1:19,]
tfurrows$Date <- dates

plot_details$plotname <- sub(".*-", "", plot_details$Plot)

plot_sm <- list()
for(i in 1:length(thills)){
  a <- (as.double(tfurrows[,i])/1.4)
  b <- (as.double(thills[,i])/1.4)
  df <- as.data.frame(cbind(a,b))
  names(df) <- c("F","H")
  df$Date <- dates
  df$plotname <- names(thills)[i]
  plot_sm[[i]] <- df
}
names(plot_sm) <- names(thills)

plots <- unique(sm_hf$plotname)[-1]

plot_sm <- plot_sm[1:length(plot_sm)-1]

allsm <- plot_sm[[1]]
for(i in 2:length(plot_sm)){
  allsm <- rbind(allsm,plot_sm[[i]])
}

allsm <- left_join(allsm, plot_details, by = "plotname")

allsm_sum <- allsm |> group_by(Date) |>
  summarise(Hills = mean(H),
            Furrows = mean(F)) 

allsm_sum$ratio <- allsm_sum$Hills/allsm_sum$Furrows
ratiohf <- mean(allsm_sum$ratio)


allsims_before <- sims_beforeSM[[1]]
for(i in 2:length(sims_beforeSM)){
  allsims_before <- rbind(allsims_before,sims_beforeSM[[i]])
}
allsims_bfSum <- allsims_before |> group_by(Date) |>
  summarise(Sim.Hills = mean(HR_1)) 

allsims_after <- sims_afterSM[[1]]
for(i in 2:length(sims_afterSM)){
  allsims_after <- rbind(allsims_after,sims_afterSM[[i]])
}
allsims_after <- left_join(allsm, plot_details, by = "plotname")

allsims_aftSum <- allsims_after |> group_by(Date) |>
  summarise(Sim.Hills = mean(HR_1)) 

x <- allsm_sum
x$Date <- as.Date(x$Date)
x <- left_join(x, allsims_bfSum, by = "Date")
x <- left_join(x, allsims_aftSum, by = "Date")

x$Sim.ratio <- allsm_sum$Hills/x$Sim.Hills.y
ratiohsim <- mean(x$Sim.ratio)
x$Sim.ratio.aft <- allsm_sum$Hills/x$Sim.Hills
ratiohsim_aft <- mean(x$Sim.ratio.aft)

x$Sim.ratio.f <- allsm_sum$Furrows/x$Sim.Hills.y
ratiofsim <- mean(x$Sim.ratio.f)

sm_simobs <- x[,c(1,2,3,4,6,8)]
names(sm_simobs) <- c(names(sm_simobs)[1:4],"Sim.before", "Sim.after")
old <- c(18.5,7.5)
new <- c(13.7, 5.65)

smplotbf <- ggplot(sm_simobs, aes(x = Date)) +
  geom_line(data = allsims_bfSum,
            aes(y = Sim.Hills, color = "Simulated"),
            linewidth = 0.8,
            na.rm = T) +
  geom_point(aes(y = Hills, color = "Observed"),
             shape = 16, size = 3) +
  geom_abline(slope = 0, intercept = old[1]) +
  geom_abline(slope = 0, intercept = old[2]) +
  annotate("text", x = as.POSIXct("2024-11-20"), y = old[1], 
           label = "FC = 18.5%", hjust = 0.7, vjust = -0.5) +
  annotate("text", x = as.POSIXct("2024-11-20"), y = old[2], 
           label = "WP = 7.5%", hjust = 0.7, vjust = -0.5) +
  scale_y_continuous(name = "Soil Moisture, gravimetric (%)") +
  scale_color_manual(values = paletteer_d("tvthemes::WaterTribe")[1:2]) +
  coord_cartesian(ylim = c(0,20)) +
  labs(title = "Before Adjustment", color = "") +
  theme_minimal() +
  theme(legend.position = "bottom")

smplotbf

smplotaft <- ggplot(sm_simobs, aes(x = Date)) +
  geom_line(data = allsims_aftSum,
            aes(y = Sim.Hills, color = "Simulated"),
            linewidth = 0.8,
            na.rm = T) +
  geom_point(aes(y = Hills, color = "Observed"),
             shape = 16, size = 3) +
  geom_abline(slope = 0, intercept = new[1]) +
  geom_abline(slope = 0, intercept = new[2]) +
  scale_y_continuous(name = "Soil Moisture, gravimetric (%)") +
  annotate("text", x = as.POSIXct("2024-11-20"), y = new[1], 
           label = "FC = 13.7%", hjust = 0.7, vjust = -0.5) +
  annotate("text", x = as.POSIXct("2024-11-20"), y = new[2], 
           label = "WP = 5.65%", hjust = 0.7, vjust = -0.5) +
  scale_color_manual(values = paletteer_d("tvthemes::WaterTribe")[1:2]) +
  coord_cartesian(ylim = c(0,20)) +
  labs(title = "After Adjustment", color = "") +
  theme_minimal() +
  theme(legend.position = "bottom")

smplotaft

smplots <- grid.arrange(smplotbf, smplotaft, ncol = 2)

ggsave(
  file.path(dir,"before-after_sm.png"),
  plot = smplots,
  width = 30,
  height = 10,
  units = "cm")

ghg_obssum <- allobs |> group_by(Rotation, Fertiliser) |>
  summarise(cumN2O = max(Qem_N2O, na.rm = T)) 
ghg_obssum$Fertiliser <- sub("N", "", ghg_obssum$Fertiliser)
ghg_obssum$EF <- (ghg_obssum$cumN2O/as.integer(ghg_obssum$Fertiliser))*100
ghg_simsum <- allsims |> group_by(Rotation, Fertiliser) |>
  summarise(cumN2O = max(Qem_N2O, na.rm = T)) 
ghg_simsum$Fertiliser <- sub("N", "", ghg_simsum$Fertiliser)
ghg_simsum$EF <- (ghg_simsum$cumN2O/as.integer(ghg_simsum$Fertiliser))*100
