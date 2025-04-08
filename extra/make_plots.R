library(SticsRPacks)
library(grid)
library(gridExtra)
library(dplyr)
library(stringi)

obs <- get_obs(workspace, usm = usms)
sims <- get_sim(workspace, usm = usms)

obs <- obs[plot_details[,1]]
sims <- sims[plot_details[,1]]

dfobs <- mapply(function(name, df) {
  row <- plot_details[plot_details$Plot == name,]
  new <- row[rep(1, times = nrow(df)),]
  cbind(df, new)
  }, names(obs), obs, SIMPLIFY = FALSE)

dfsims <- mapply(function(name, df) {
  row <- plot_details[plot_details$Plot == name,]
  new <- row[rep(1, times = nrow(df)),]
  cbind(df, new)
}, names(sims), sims, SIMPLIFY = FALSE)

allobs <- dfobs[[1]]
for(i in 2:length(dfobs)){
  message(names(dfobs)[i])
  dfobs[[i]] <- dfobs[[i]][,(names(dfobs[[i]]) %in% names(allobs))]
  col_check <- !(names(allobs) %in% names(dfobs[[i]]))
  if(T %in% col_check){
    new_col <- which(!(names(allobs) %in% names(dfobs[[i]])))
    dfobs[[i]][,ncol(dfobs[[i]])+1] <- NA
    names(dfobs[[i]])[ncol(dfobs[[i]])] <- setdiff(names(allobs), 
                                                      names(dfobs[[i]]))
  }
  dfobs[[i]] <- dfobs[[i]][, names(allobs)]
  allobs <- rbind(allobs,dfobs[[i]])
}

allsims <- dfsims[[1]]
for(i in 2:length(dfsims)){
  allsims <- rbind(allsims, dfsims[[i]])
}

make_plot <- function(df, title, x_var, y_var, y_title, y_lim){

  ggplot(df,aes(x = .data[[x_var]])) +
    geom_line(data = df[!is.na(df[grep(y_var, names(df))]),],
              aes(y = .data[[y_var]]),
              color = "red2", linewidth = 0.8,
              na.rm = F) +
    scale_y_continuous(name = y_title) +
    coord_cartesian(ylim = y_lim) +
    labs(title = title) +
    theme_minimal()
}

simobs_plot <- function(df, simdf, title, y_var, y_title, 
                        y_lim, colours1, colours2, x_var = "Date"){
  
  v <- org_vars[grep(paste(y_var,"$",sep=""), vars)]
  subplots <- unique(df$plot_id)
  cal <- subplots[which(stri_sub(subplots, -1) == "A")]
  eval <- subplots[which(stri_sub(subplots, -1) == "B")]
  
  df <- df[!is.na(df[,names(df) == v]),]
  df1 <- df[df$Fertiliser == "N170",]
  df2 <- df[df$Fertiliser == "N120",]
  df1p <- unique(df1$plot_id)
  df2p <- unique(df2$plot_id)
  
  start <- min(df$Date)
  end <- max(df$Date)
  
  simdf <- simdf[simdf$Date >= start & simdf$Date <= end,]
  
  simdf1 <- simdf[simdf$Fertiliser == "N170",]
  simdf2 <- simdf[simdf$Fertiliser == "N120",]
  
  plot1 <- ggplot(df,aes(x = .data[[x_var]])) +
    geom_line(data = df1[df1$plot_id == df1p[1],],
              aes(y = .data[[v]]),
              color = colours1[1], linewidth = 0.6,linetype = 2,
              na.rm = T) +
    geom_point(data = df1[df1$plot_id == df1p[1],],
               aes(y = .data[[v]], color = "Observed1"),
               shape = 17, size=3) +
    geom_line(data = df1[df1$plot_id == df1p[2],],
              aes(y = .data[[v]]),
              color = colours1[2], linewidth = 0.6,linetype = 2,
              na.rm = T) +
    geom_point(data = df1[df1$plot_id == df1p[2],],
               aes(y = .data[[v]], color = "Observed2"),
               shape = 15, size=3) +
    geom_line(data = df1[df1$plot_id == df1p[3],],
              aes(y = .data[[v]]),
              color = colours1[3], linewidth = 0.6,linetype = 2,
              na.rm = T) +
    geom_point(data = df1[df1$plot_id == df1p[3],],
               aes(y = .data[[v]], color = "Observed3"),
               shape = 16, size=3) +
    geom_line(data = simdf1[simdf1$plot_id == df1p[1],],
              aes(y = .data[[v]], color = "Simulated"),
              linewidth = 0.8,
              na.rm = T) +
    scale_y_continuous(name = y_title) +
    scale_color_manual(labels = c(df1p[1],
                                  df1p[2],
                                  df1p[3],
                                  "Simulated"),
                       values = c(Observed1 = colours1[1],
                                  Observed2 = colours1[2],
                                  Observed3 = colours1[3],
                                  Simulated = "black")) +
    coord_cartesian(ylim = y_lim) +
    labs(title = sprintf("%s - N170",title), color = "") +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  plot2 <- ggplot(df,aes(x = .data[[x_var]])) +
    geom_line(data = df2[df2$plot_id == df2p[1],],
              aes(y = .data[[v]]),
              color = colours2[1], linewidth = 0.6,linetype = 2,
              na.rm = T) +
    geom_point(data = df2[df2$plot_id == df2p[1],],
               aes(y = .data[[v]], color = "Observed1"),
               shape = 17, size=3) +
    geom_line(data = df2[df2$plot_id == df2p[2],],
              aes(y = .data[[v]]),
              color = colours2[2], linewidth = 0.6,linetype = 2,
              na.rm = T) +
    geom_point(data = df2[df2$plot_id == df2p[2],],
               aes(y = .data[[v]], color = "Observed2"),
               shape = 15, size=3) +
    geom_line(data = df2[df2$plot_id == df2p[3],],
              aes(y = .data[[v]]),
              color = colours2[3], linewidth = 0.6,linetype = 2,
              na.rm = T) +
    geom_point(data = df2[df2$plot_id == df2p[3],],
               aes(y = .data[[v]], color = "Observed3"),
               shape = 16, size=3) +
    geom_line(data = simdf2[simdf2$plot_id == df2p[1],],
              aes(y = .data[[v]], color = "Simulated"),
              linewidth = 0.8,
              na.rm = T) +
    scale_y_continuous(name = y_title) +
    scale_color_manual(labels = c(df2p[1],
                                  df2p[2],
                                  df2p[3],
                                  "Simulated"),
                       values = c(Observed1 = colours2[1],
                                  Observed2 = colours2[2],
                                  Observed3 = colours2[3],
                                  Simulated = "black")) +
    coord_cartesian(ylim = y_lim) +
    labs(title = sprintf("%s - N120",title), color = "") +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  p <- grid.arrange(plot1, plot2, nrow = 2)
  
  if (title == "Barley/Clover/Potato") fn <- "trt1"
  if (title == "Corn/Grass/Potato") fn <- "trt2"
  if (title == "Soybean /Mustard / Potato") fn <- "trt3"
  
  ggsave(
    file.path(dir,sprintf("%s_%s-obswsim.png",
                          fn,
                          gsub("\\.", " ", y_var))),
    plot = p,
    width = 10,
    height = 30,
    units = "cm")
  
  p
}

minmax_plot <- function(df, title, y_var, y_title, 
                        y_lim, colour1, colour2,
                        x_var = "Date"){
  min <- paste(y_var,"min",sep = ".")
  avg <- paste(y_var,"avg",sep = ".")
  max <- paste(y_var,"max",sep = ".")
  
  df <- df[!is.na(df[,names(df)==avg]),]
  df1 <- df[df$Fertiliser == "N170",]
  df2 <- df[df$Fertiliser == "N120",]
  
  plot <- ggplot(df,aes(x = .data[[x_var]])) +
    geom_ribbon(data = df1,
                aes(ymin = .data[[min]], ymax = .data[[max]]),
                fill = colour1, alpha = 0.2,
                na.rm = T) +
    geom_line(data = df1,
              aes(y = .data[[avg]], color = "N170"),
              linewidth = 0.8,
              na.rm = T) +
    geom_point(data = df1,
               aes(y = .data[[avg]]),
               color = colour1, shape = 17, size=3) +
    geom_ribbon(data = df2,
                aes(ymin = .data[[min]], ymax = .data[[max]]),
                fill = colour2, alpha = 0.2,
                na.rm = T) +
    geom_line(data = df2,
              aes(y = .data[[avg]], color = "N120"),
              linewidth = 0.8,
              na.rm = T) +
    geom_point(data = df2,
               aes(y = .data[[avg]]),
               color = colour2, shape = 15, size=3) +
    scale_y_continuous(name = y_title) +
    scale_color_manual(values = c(N170 = colour1,
                                  N120 = colour2)) +
    coord_cartesian(ylim = y_lim) +
    labs(title = sprintf("%s - %s", title, y_title), color = "") +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  if (title == "Barley/Clover/Potato") fn <- "trt1"
  if (title == "Corn/Grass/Potato") fn <- "trt2"
  if (title == "Soybean /Mustard / Potato") fn <- "trt3"
  
  ggsave(
    file.path(dir,sprintf("%s_%s.png",
                          fn,
                          gsub("\\.", " ", y_var))),
    plot = plot,
    width = 15,
    height = 10,
    units = "cm")
  
  plot
}

simobs_minmax <- function(df, simdf, title, y_var, y_title, 
                          y_lim, colour1, colour2, x_var = "Date"){
  min <- paste(y_var,"min",sep = ".")
  avg <- paste(y_var,"avg",sep = ".")
  max <- paste(y_var,"max",sep = ".")
  
  df <- df[!is.na(df[,names(df)==avg]),]
  df1 <- df[df$Fertiliser == "N170",]
  df2 <- df[df$Fertiliser == "N120",]
  
  start <- min(df$Date)
  end <- max(df$Date)
  
  simdf <- simdf[simdf$Date >= start & simdf$Date <= end,]
  
  simdf1 <- simdf[simdf$Fertiliser == "N170",]
  simdf2 <- simdf[simdf$Fertiliser == "N120",]
  
  
  plot1 <- ggplot(df,aes(x = .data[[x_var]])) +
    geom_ribbon(data = df1,
                aes(ymin = .data[[min]], ymax = .data[[max]]),
                fill = colour1, alpha = 0.2,
                na.rm = T) +
    geom_line(data = df1,
              aes(y = .data[[avg]], color = "Average"),
              linewidth = 0.8,
              na.rm = T) +
    geom_point(data = df1,
               aes(y = .data[[avg]]),
               color = colour1, shape = 17, size=3) +
    geom_line(data = simdf1,
              aes(y = .data[[avg]], color = "Simulated"),
              linewidth = 0.8,
              linetype = 2,
              na.rm = T) +
    scale_y_continuous(name = y_title) +
    scale_color_manual(values = c(Average = colour1,
                                  Simulated = "black")) +
    coord_cartesian(ylim = y_lim) +
    labs(title = sprintf("%s - N170",title), color = "") +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  plot2 <- ggplot(df,aes(x = .data[[x_var]])) +
    geom_ribbon(data = df2,
                aes(ymin = .data[[min]], ymax = .data[[max]]),
                fill = colour2, alpha = 0.2,
                na.rm = T) +
    geom_line(data = df2,
              aes(y = .data[[avg]], color = "Average"),
              linewidth = 0.8,
              na.rm = T) +
    geom_point(data = df2,
               aes(y = .data[[avg]]),
               color = colour2, shape = 15, size=3) +
    geom_line(data = simdf2,
              aes(y = .data[[avg]], color = "Simulated"),
              linewidth = 0.8,
              linetype = 2,
              na.rm = T) +
    scale_y_continuous(name = y_title) +
    scale_color_manual(values = c(Average = colour2,
                                  Simulated = "black")) +
    coord_cartesian(ylim = y_lim) +
    labs(title = sprintf("%s - N120",title), color = "") +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  p <- grid.arrange(plot1, plot2, ncol = 2)
  
  if (title == "Barley/Clover/Potato") fn <- "trt1"
  if (title == "Corn/Grass/Potato") fn <- "trt2"
  if (title == "Soybean /Mustard / Potato") fn <- "trt3"
  
  ggsave(
    file.path(dir,sprintf("%s_%s-wsim.png",
                          fn,
                          gsub("\\.", " ", y_var))),
    plot = p,
    width = 30,
    height = 10,
    units = "cm")
  
  p
}

make_summary <- function(df_list){
  avg <- df_list |> group_by(Rotation, Fertiliser,Date) |>
    summarise(Total.Biomass = mean(masec_n, na.rm = T),
              Total.Biomass.Fresh = mean(mafrais, na.rm = T),
              Tuber.Biomass = mean(mafruit, na.rm = T),
              Tuber.Biomass.Fresh = mean(pdsfruitfrais, na.rm = T),
              Root.Biomass = mean(msrac_n, na.rm = T),
              LAI = mean(lai_n, na.rm = T),
              Total.N = mean(QNplante, na.rm = T),
              Tuber.N = mean(QNgrain, na.rm = T),
              Root.N = mean(QNrac, na.rm = T),
              Cumulative.N2O = mean(Qem_N2O, na.rm = T),
              Soil.Moisture = mean(HR_1, na.rm = T),
              )
  
  min <- df_list |> group_by(Rotation, Fertiliser,Date) |>
    summarise(Total.Biomass = min(masec_n, na.rm = T),
              Total.Biomass.Fresh = min(mafrais, na.rm = T),
              Tuber.Biomass = min(mafruit, na.rm = T),
              Tuber.Biomass.Fresh = min(pdsfruitfrais, na.rm = T),
              Root.Biomass = min(msrac_n, na.rm = T),
              LAI = min(lai_n, na.rm = T),
              Total.N = min(QNplante, na.rm = T),
              Tuber.N = min(QNgrain, na.rm = T),
              Root.N = min(QNrac, na.rm = T),
              Cumulative.N2O = min(Qem_N2O, na.rm = T),
              Soil.Moisture = min(HR_1, na.rm = T),
    )
  
  max <- df_list |> group_by(Rotation, Fertiliser,Date) |>
    summarise(Total.Biomass = max(masec_n, na.rm = T),
              Total.Biomass.Fresh = max(mafrais, na.rm = T),
              Tuber.Biomass = max(mafruit, na.rm = T),
              Tuber.Biomass.Fresh = max(pdsfruitfrais, na.rm = T),
              Root.Biomass = max(msrac_n, na.rm = T),
              LAI = max(lai_n, na.rm = T),
              Total.N = max(QNplante, na.rm = T),
              Tuber.N = max(QNgrain, na.rm = T),
              Root.N = max(QNrac, na.rm = T),
              Cumulative.N2O = max(Qem_N2O, na.rm = T),
              Soil.Moisture = max(HR_1, na.rm = T),
    )
  
  df1  <- left_join(min,avg, by = c("Rotation", "Fertiliser","Date"))
  df2  <- left_join(df1,max,  by = c("Rotation", "Fertiliser","Date"))
  names(df2) <- c("Rotation", "Fertiliser", "Date",
                  paste(names(min), "min", sep=".")[-1:-3],
                  paste(names(avg), "avg", sep=".")[-1:-3],
                  paste(names(max), "max", sep=".")[-1:-3])
  df2[] <- lapply(df2, function(x) {
    if (is.numeric(x)) {
      x[is.infinite(x) | is.nan(x)] <- NA
    }
    x
  })
  df2
}



gen_plots <- function(df_list, type, title, x_axis, y_axis, y_title, y_lim, dir){
  
  i <- 1
  plots <- list()
  df_list <- df_list[plot_details$Plot]
  n <- names(df_list)
  for(df in df_list){
    plots[[i]] <- make_plot(df, n[i], x_axis, y_axis, y_title, y_lim)
    i <- i + 1
  }
  names(plots) <- names(n)
  
  grid <- grid.arrange(
    grobs = plots[1:9],
    nrow = 9,
    top = textGrob(title,gp=gpar(fontsize=20,font=3))
  )
  grid2 <- grid.arrange(
    grobs = plots[10:18],
    nrow = 9,
    top = textGrob("",gp=gpar(fontsize=20,font=3))
  )
  plot <- cbind(grid,grid2, size = "first")
  
  ggsave(
    file.path(dir,sprintf("%s_allplots.png", y_axis)),
    plot = plot,
    width = 20,
    height = 50,
    units = "cm")
}
#mafrais - plant and tuber fresh biomass
#compare b/t all subplots
gen_plots(dfobs, "Total Fresh Biomass", "Date", "mafrais", "kg/ha", c(0,120), dir)

#masec - plant and tuber dry biomass
#compare b/t all subplots
gen_plots(dfobs, "Total Dry Biomass", "Date", "masec_n", "kg/ha", c(0,25), dir)

#pdsfruitfrais - tuber fresh biomass
gen_plots(dfobs, "Tuber Fresh Biomass", "Date", "pdsfruitfrais", "t/ha", c(0,50), dir)

#mafruit - tuber dry biomass
#compare b/t all subplots
gen_plots(dfobs, "Tuber Dry Biomass", "Date", "mafruit", "kg/ha", c(0,12), dir)

#lai
#compare b/t all subplots
gen_plots(dfobs, "LAI", "Date", "lai_n", "lai", c(0,7), dir)

#msrac - root dry biomass
gen_plots(dfobs, "Root Biomass", "Date", "msrac_n", "t/ha", c(0,0.75), dir)

#QNplante - plant and tuber N content
gen_plots(dfobs, "Total Nitrogen", "Date", "QNplante", "kg/ha", c(0,375), dir)

#QNgrain - tuber N content
gen_plots(dfobs, "Tuber Nitrogen", "Date", "QNgrain", "kg/ha", c(0,120), dir)

#QNrac - root N content
gen_plots(dfobs, "Root Nitrogen", "Date", "QNrac", "kg/ha", c(0,25), dir)

colours1 <- c("#ff8566","#ff3300","#b32400")
colours2 <- c("#80aaff","#004de6","#003399")

trt1_all <- allobs[allobs$Rotation == "Barley/Clover/Potato",]
trt1_all$plot_id <- paste("Plot", substr(trt1_all$Plot, 
                           nchar(trt1_all$Plot) - 1,
                           nchar(trt1_all$Plot)),
                          sep = " ")
trt2_all <- allobs[allobs$Rotation == "Corn/Grass/Potato",]
trt2_all$plot_id <- paste("Plot", substr(trt2_all$Plot, 
                                         nchar(trt2_all$Plot) - 1,
                                         nchar(trt2_all$Plot)),
                          sep = " ")
trt3_all <- allobs[allobs$Rotation == "Soybean /Mustard / Potato",]
trt3_all$plot_id <- paste("Plot", substr(trt3_all$Plot, 
                                         nchar(trt3_all$Plot) - 1,
                                         nchar(trt3_all$Plot)),
                          sep = " ")
alltrt_list <- list(trt1 = trt1_all, trt2 = trt2_all, trt3 = trt3_all)

sum_obs <- make_summary(allobs)
sum_sims <- make_summary(allsims)

trt1 <- sum_obs[sum_obs$Rotation == "Barley/Clover/Potato",]
trt2 <- sum_obs[sum_obs$Rotation == "Corn/Grass/Potato",]
trt3 <- sum_obs[sum_obs$Rotation == "Soybean /Mustard / Potato",]

trt1_sims <- sum_sims[sum_sims$Rotation == "Barley/Clover/Potato",]
trt2_sims <- sum_sims[sum_sims$Rotation == "Corn/Grass/Potato",]
trt3_sims <- sum_sims[sum_sims$Rotation == "Soybean /Mustard / Potato",]

vars <- unique(sub("\\.(max|min|avg)$", "", names(sum_obs)))[4:13]
org_vars <- c("masec_n", "mafrais", 
              "mafruit", "pdsfruitfrais", 
              "msrac_n", "lai_n",
              "QNplante", "QNgrain","QNrac",
              "Qem_N2O")
names <- c("Total Biomass (t/ha)",
           "Total Biomass - Fresh (t/ha)",
           "Tuber Biomass (t/ha)",
           "Tuber Biomass - Fresh (t/ha)",
           "Root Biomass (t/ha)",
           "LAI",
           "Total N (kg/ha)",
           "Tuber N (kg/ha)",
           "Root N (kg/ha)",
           "Cumulative N2O (kg/d)")


titles <- c("Barley/Clover/Potato", 
            "Corn/Grass/Potato", 
            "Soybean/Mustard/Potato")

sum_list <- list(trt1 = trt1, trt2 = trt2, trt3 =  trt3)
sim_list <- list(trt1 = trt1_sims, trt2 =  trt2_sims, trt3 = trt3_sims)

ylims <- list(c(0,20), c(0,120), c(0,12), c(0,50),c(0,0.75), c(0,7),
              c(0,375),c(0,120), c(0,25), c(0,2.5))

minmax_plots <- list()
minmax_simplots <- list()

for(j in 1:3){
  title <- titles[j]
  trt <- sum_list[[j]]
  sims <- sim_list[[j]]
  
  sublist1 <- list()
  sublist2 <- list()

  message(title)
  for(i in 1:length(vars)){
    message(sprintf("   %s", vars[i]))
    p1 <- minmax_plot(trt, 
                title,
                vars[i], names[i],
                ylims[[i]], "red3", "blue4")
    p2 <- simobs_minmax(trt, sims,
                  title,
                  vars[i], names[i],
                  ylims[[i]], "red3", "blue4")
    sublist1[[i]] <- p1
    sublist2[[i]] <- p2
  }
  names(sublist1) <- vars
  names(sublist2) <- vars
  minmax_plots[[j]] <- sublist1
  minmax_simplots[[j]] <- sublist2
  
  if(j == 3){
    names(minmax_plots) <- titles
    names(minmax_simplots) <- titles
  }
}

#combine minmax plots for each trt/variable together
for (i in 1:length(vars)){
  message(vars[i])
  plot <- grid.arrange(grobs = c(minmax_plots[[1]][i],
                         minmax_plots[[2]][i],
                         minmax_plots[[3]][i]), 
               ncol = 3)
  ggsave(
    file.path(dir,sprintf("%s_compare.png", vars[i])),
    plot = plot,
    width = 30,
    height = 10,
    units = "cm")
}
#combine minmax_simplots for each trt/variable together
for (i in 1:length(vars)){
  message(vars[i])
  plot <- grid.arrange(grobs = c(minmax_simplots[[1]][i],
                         minmax_simplots[[2]][i],
                         minmax_simplots[[3]][i]), 
               nrow = 3)
  ggsave(
    file.path(dir,sprintf("%s_compareSim.png", vars[i])),
    plot = plot,
    width = 20,
    height = 30,
    units = "cm")
}

allsims$plot_id <- paste("Plot", substr(allsims$Plot, 
                                        nchar(allsims$Plot) - 1,
                                        nchar(allsims$Plot)),
                         sep = " ")

#create sim V obs plots
simVobs_plots <- list()

for(j in 1:3){
  title <- titles[j]
  trt <- alltrt_list[[j]]
  
  sublist <- list()
  
  message(title)
  for(i in 1:length(vars)){
    message(sprintf("   %s", vars[i]))
    p <- simobs_plot(trt, allsims, 
                      title,
                      vars[i], names[i],
                      ylims[[i]], colours1, colours2)

    sublist[[i]] <- p
  }
  names(sublist) <- vars
  simVobs_plots[[j]] <- sublist
  
  if(j == 3){
    names(simVobs_plots) <- titles
  }
}
#combine simVobs_plots for each trt/variable together
for (i in 1:length(vars)){
  message(vars[i])
  plot <- grid.arrange(grobs = c(simVobs_plots[[1]][i],
                                 simVobs_plots[[2]][i],
                                 simVobs_plots[[3]][i]), 
                       ncol = 3)
  ggsave(
    file.path(dir,sprintf("%s_compare-obsVsim2.png", vars[i])),
    plot = plot,
    width = 30,
    height = 15,
    units = "cm")
}

