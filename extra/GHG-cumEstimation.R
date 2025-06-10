packages <- c("rstanarm","pracma","bayesplot","tidyverse","caTools","readxl","openxlsx",
              "lubridate","dplyr", "zoo", "tidyr", "mgcv")
invisible(sapply(packages, function(pkg){
  if(!require(pkg, character.only = TRUE)){
    message("Installing ", pkg, "...")
    install.packages(pkg)
    } else {
    message(pkg, " is already installed.")
    }
  library(pkg, character.only = TRUE)
  }))

#Get GHG, soil moisture, soil temp measurements---------------------------------

outdir <- rstudioapi::selectDirectory() #output directory
filepath <- rstudioapi::selectFile() #file containing GHG measurements

#or provide path
#outdir <- "Z:\\Students\\Maria\\Mountain Gem Data\\2024 Data"
#filepath <- "Z:\\Students\\Maria\\Mountain Gem Data\\2024 Data\\GHGs.xlsx"

sheets <- excel_sheets(filepath)

ghg_meas <- setNames(lapply(sheets,
                            function(X) as.data.frame(read_excel(filepath, X))),
                     sheets)

# Include STICS simulated data--------------------------------------------------
# #Add simulated STICS N2O values to extend for April, early May
# splitdate <- function(df){
#   df <- df |>
#     mutate(
#       ian = year(Date),
#       mo = month(Date),
#       jo = day(Date),
#       jul = yday(Date)
#     )
#   df <- df[,c(3,4,5,6,2)]
#   df
# }
# 
# 
# simN2O <- lapply(sims, function(df) {
#   df <- df[strftime(df$Date, format = "%j") %in% c(110, 114, 119, 129), c(1, 17)]
#   df <- splitdate(df)
#   df <- cbind(df, data.frame(matrix(NA, nrow = 4, ncol = 5)))
#   df
# })
# 
# # Keep only names from ghg_meas[3:20] and assign column names
# simN2O <- simN2O[names(ghg_meas[3:20])]
# simN2O <- Map(function(df, ref) {
#   names(df) <- names(ref)
#   df
# }, simN2O, ghg_meas[3:20])
# 
# ghg_meas[3:20] <- Map(rbind,simN2O,ghg_meas[3:20])
# 
# #replace -999.99 from STICS file with NA
# ghg_meas <- lapply(ghg_meas, function(df) {
#   df[] <- lapply(df, function(col) replace(col, col == -999.99, NA))
#   df
# })


#Get climate data---------------------------------------------------------------

#read STICS climate file
climfile <- rstudioapi::selectFile()

headers <- c("station","year","month","day","jul","MinTemp","MaxTemp",
             "SolarRad","PET","Precipitation","WindSpeed","VP","CO2")
climate <- setNames(read.table(climfile),headers)

#climate <- setNames(read.table("extra/climat.txt"))

climate$Date <-
  as.POSIXct(paste(climate$year, climate$month, climate$day,sep="-"),
             format = "%Y-%m-%d", tz = "UTC")

data <- list()
i <- 1
for (usm in ghg_meas[3:20]){
  usm$Date <- as.Date(usm$jul - 1, 
                      origin = paste0(2024, "-01-01"))
  sm <- left_join(climate,
                  usm,
                  join_by(Date))
  sm <- sm[c(110:277),c(5:8, 10:12, 14, 19:22)]
  names(sm)[c(1,10)] <- c("jul","CO2")
  sm <- sm |> relocate(jul, .before = everything())
  sm <- sm |> relocate(Date, .before = everything())
  data[[i]] <- sm
  i <- i + 1
}
names(data) <- names(ghg_meas[3:20])

dfN2O <- lapply(data, function(df) df[!is.na(df$N2O), ])
dfCO2 <- lapply(data, function(df) df[!is.na(df$CO2), ])

#Estimation Methods-------------------------------------------------------------

#linear Interpolation
linInter <- function(df,var) {
  out <- as.data.frame(approx(x = df$jul,
                              y = df[[var]],
                              xout = min(df$jul):max(df$jul)
                              )
                       )
  out[[paste0("lin", var, ".cum")]] <- cumsum(out$y)
  setNames(out, c("jul","linN2O.daily","linN2O.cum"))
}

linN2O <- lapply(dfN2O, linInter, var = "N2O")
linCO2 <- lapply(dfCO2, linInter, var = "CO2")



#Trapezoidal Integration
trapInt <- function(df,var){
  sapply(1:nrow(df), function(i) {
    if (i == 1) return(df[[var]][1])  # No area under a single point
    trapz(df$jul[1:i], df[[var]][1:i])})
}

trapN2O <- lapply(dfN2O, trapInt, var = "N2O")
trapCO2 <- lapply(dfCO2, trapInt, var = "CO2")                 




#Bayesian Lognormal ~ Precipitation + MaxTemp

#using rstanarm package
bayesLog <- function(df,
                     ghg = c("N2O", "CO2"),
                     df2,
                     scaling_params,
                     seed){
  if(ghg == "N2O"){
    df$N2O[df$N2O == 0] <- 1e-6
    df$log_N2O <- log(df$N2O)
  }else{
    df$CO2[df$CO2 == 0] <- 1e-6
    df$log_CO2 <- log(df$CO2) 
  }
  
  if(ghg=="N2O"){
    model <- stan_glm(
      log_N2O ~ Precipitation + MaxTemp,
      data = df,
      family = gaussian(link = "identity"),  
      prior = normal(0, 2.5),
      chains = 4,
      iter = 2000,
      seed = seed
    )
  }else{
    model <- stan_glm(
      log_CO2 ~ Precipitation + MaxTemp,
      data = df,
      family = gaussian(link = "identity"),  
      prior = normal(0, 2.5),
      chains = 4,
      iter = 2000,
      refresh = 0,
      seed = seed
    )
  }
  # Predict log-flux from posterior
  pred_log <- posterior_predict(model, newdata = df2)
  
  # Back-transform to original scale
  pred <- exp(pred_log)
  
  #daily predicted values (not cummulative)
  # pred_draws <- t(pred)
  # 
  # pred_summary <- apply(pred_draws, 2, function(x) {
  #   c(mean = mean(x),
  #     median = median(x),
  #     lower = quantile(x, 0.025),
  #     upper = quantile(x, 0.975))
  # })

  # pred_summary <- t(pred_summary) |> 
  #   as.data.frame(col.names = pred_summary[1,]) |> 
  #   rownames_to_column(var = "x")
  # names(pred_summary)[1] <- "jul"
  # pred_summary$jul <- as.integer(pred_summary$jul)
  # 
  # df2 <- left_join(df2,pre_summary,join_by(jul))
  # 
  # df2 <- df2 |>
  #   mutate(across(all_of(cols), 
  #                 ~ .x * daily_scaling[[cur_column()]]$scale + 
  #                   daily_scaling[[cur_column()]]$center))
  
  #cummulative values
  cumulative_draws <- t(apply(pred, 1, cumsum))
  
  cumulative_summary <- apply(cumulative_draws, 2, function(x) {
    c(mean = mean(x),
      median = median(x),
      lower = quantile(x, 0.025),
      upper = quantile(x, 0.975))
  })
  
  cumulative_summary <- t(cumulative_summary) |> 
    as.data.frame(col.names = cumulative_summary[1,]) |> 
    rownames_to_column(var = "x")
  names(cumulative_summary)[1] <- "jul"
  cumulative_summary$jul <- as.integer(cumulative_summary$jul)
  
  df <- left_join(df,cumulative_summary,join_by(jul))
  
  #undo scaling
  df <- df |>
    mutate(across(all_of(cols), 
                  ~ .x * scaling_params[[cur_column()]]$scale + 
                    scaling_params[[cur_column()]]$center))

}

cols <- c("Precipitation", "MinTemp", "MaxTemp", 
          "SolarRad", "WindSpeed", "VP")

bayesN2O <- lapply(seq_along(dfN2O), function(i) {
  df <- dfN2O[[i]]
  df_params <- lapply(df[cols], function(x) {
    list(center = mean(x, na.rm = TRUE), scale = sd(x, na.rm = TRUE))
  })
  daily <- data[[i]] 
  # daily_params <- lapply(daily[cols], function(x) {
  #   list(center = mean(x, na.rm = TRUE), scale = sd(x, na.rm = TRUE))
  # })
  daily <- daily |> mutate(across(all_of(cols), ~ as.numeric(scale(.x))))
  
  df |>
    mutate(across(all_of(cols), ~ as.numeric(scale(.x)))) |>
    bayesLog(ghg = "N2O", daily,df_params, 1234)
})

bayesCO2 <- lapply(seq_along(dfCO2), function(i) {
  df <- dfCO2[[i]]
  df_params <- lapply(df[cols], function(x) {
    list(center = mean(x, na.rm = TRUE), scale = sd(x, na.rm = TRUE))
  })
  daily <- data[[i]] 
  # daily_params <- lapply(daily[cols], function(x) {
  #   list(center = mean(x, na.rm = TRUE), scale = sd(x, na.rm = TRUE))
  # })
  daily <- daily |> mutate(across(all_of(cols), ~ as.numeric(scale(.x))))
  
  df |>
    mutate(across(all_of(cols), ~ as.numeric(scale(.x)))) |>
    bayesLog(ghg = "CO2", daily,df_params, 1234)
})


#Using brms package - NEEDS WORK
# fit <- brms::brm(
#   N2O ~ Precipitation + MaxTemp,
#   data = test,
#   family = lognormal(),   # lognormal distribution
#   chains = 4,
#   iter = 4000,
#   cores = 4,
#   seed = seed
# )
# 
# post_preds <- posterior_predict(fit, newdata = data[[1]])
# summary_preds <- as.data.frame(fitted(fit, newdata = data[[1]], probs = c(0.025, 0.975)))
# cumN2O <- cumsum(summary_preds$Estimate)
# plot(cumN2O)


#create N2O summary of cummulative N2O estimations
summaryN2O  <- Map(function(list1, list2, list3, list4) {
  df <- cbind(list2[,c(2,14:17)], list3)
  names(df)[2:6] <- c("bayesN2O.mean.cum","bayesN2O.median.cum","bayesN2O.l2.5",
                      "bayesN2O.u97.5", "trapN2O.cum")
  df2 <- left_join(list1, df, by = "jul")
  left_join(df2,list4, by = "jul") |> relocate(Date, .before = everything())
}, linN2O, bayesN2O, trapN2O, data)

write.xlsx(summaryN2O, file.path(outdir,"cumEmissions_estimated.xlsx"))

#create figures to compare interpolation methods for specific field
p <- summaryN2O[[1]]

#plot bayesian lognormal results
bayesln <- ggplot(p,
                    aes(x = jul)) +
  geom_col(aes(y = Precipitation/2.5),
           position = "identity",
           fill = "blue",
           alpha = 0.5,) +
  geom_ribbon(data = p[!is.na(p$N2O),],
              aes(ymin = bayesN2O.l2.5, ymax = bayesN2O.u97.5), 
              fill = "skyblue",
              alpha = 0.4) +
  geom_line(data = p[!is.na(p$N2O),],
            aes(y = bayesN2O.median.cum),
            color = "black", linewidth = 0.8,
            na.rm = F,
            linetype = "dashed") +
  geom_line(data = p[!is.na(p$N2O),],
            aes(y = bayesN2O.mean.cum),
            color = "purple3", linewidth = 0.8,
            na.rm = F,
            linetype = "dashed") +
  scale_y_continuous(name = "N2O (kg/ha)",
                     sec.axis = sec_axis(~.*20,name="Precipitation (mm)")) +
  theme_minimal()

bayesln

#compare estimation methods
emissions <- ggplot(p,
                    aes(x = jul)) +
  geom_col(aes(y = Precipitation/20),
           position = "identity",
           fill = "blue",
           alpha = 0.5,) +
  geom_line(data = p[!is.na(p$N2O),],
            aes(y = linN2O.cum),
            color = "red2", linewidth = 0.8,
            na.rm = F,
            linetype = "dashed") +
  geom_line(data = p[!is.na(p$N2O),],
            aes(y = trapN2O.cum),
            color = "orange3", linewidth = 0.8,
            na.rm = F,
            linetype = "dashed") +
  geom_line(data = p[!is.na(p$N2O),],
            aes(y = bayesN2O.median.cum),
            color = "black", linewidth = 0.8,
            na.rm = F,
            linetype = "dashed") +
  geom_line(data = p[!is.na(p$N2O),],
            aes(y = bayesN2O.mean.cum),
            color = "purple3", linewidth = 0.8,
            na.rm = F,
            linetype = "dashed") +
  scale_y_continuous(name = "N2O (kg/ha)",
                     sec.axis = sec_axis(~.*20,name="Precipitation (mm)")) +
  theme_minimal()

emissions

#For STICS - create file formatted for STICS .obs-------------------------------
#days with other observations
# jul_days <- c(111, 115, 120, 130,138,144,150,158,164,171,184,186,192,198,199,
#               201,206,212,213,220,225,228,238,239,247,256,263,270,277)
# summary4obs <- lapply(summaryN2O, function(df) df[df$jul %in% jul_days,])
# summary4obs <- lapply(summary4obs, function(df) {
#   df[] <- lapply(df, function(col) replace(col, is.na(col), -999.99))
#   df
# })
# 
# write.xlsx(summary4obs, file.path(outdir,"cumEmissions_est4obs2.xlsx"))
# 
# #organise plots by rotation/treatment
# x <- rbind(as.list(names(ghg_meas[[1]])), ghg_meas[[1]])
# calibplots <- x[,c(1,3,4)]
# names(calibplots) <- c("Plot","Rotation", "Treatment")
# evalplots <- x[,c(2,3,4)]
# names(evalplots) <- c("Plot","Rotation", "Treatment")
# x <- rbind(calibplots,evalplots)
# ghg_meas[[1]] <- x
# remove(x, calibplots, evalplots)
# 
# gen_plots <- function(df, name){
#   ggplot(df,aes(x = jul)) +
#   geom_line(data = df[!is.na(df$N2O),],
#             aes(y = .data[[y_var]]),
#             color = "red2", linewidth = 0.8,
#             na.rm = F) +
#     scale_y_continuous(name = "N2O (kg/ha)") +
#     coord_cartesian(ylim = c(0,4.5)) +
#     labs(title = name) +
#   theme_minimal()
# }
# plots <- lapply(summaryN2O, gen_plots, name = 1:18)
# 
# i <- 1
# plots <- list()
# summaryN2O <- summaryN2O[ghg_meas[[1]][,1]]
# n <- names(summaryN2O)
# for(df in summaryN2O){
#   plots[[i]] <- gen_plots(df,n[i])
#   i <- i + 1
# }
# names(plots) <- names(summaryN2O)
# 
# grid <- grid.arrange(
#   grobs = plots[1:9],
#   nrow = 9
# )
# grid2 <- grid.arrange(
#   grobs = plots[10:18],
#   nrow = 9
# )
# plot <- cbind(grid,grid2, size = "first")
# 
# ggsave(
#   file.path(dir,"allplots.png"),
#   plot = plot,
#   width = 20,
#   height = 50,
#   units = "cm")

#final Plot
#subtract cumulative value from jul 129 from all cumulative values to get May to October
