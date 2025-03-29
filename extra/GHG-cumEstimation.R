install.packages(c("rstanarm", "bayesplot", "tidyverse"), dependencies = TRUE)
library(rstanarm)
library(bayesplot)
library(tidyverse)
library(caTools)
library(readxl)
library(openxlsx)
library(bayesplot)
library(lubridate)
library("dplyr")

dir <- "\\\\chaemci01-cifs.efs.agr.gc.ca\\common\\COMMON\\Workgroups\\MesbahM\\Students\\Maria\\Mountain Gem Data\\2024 Data"

#Get GHG, soil moisture, soil temp measurements---------------------------------
datadir <- "Z:\\Students\\Maria\\Mountain Gem Data\\2024 Data"
mfilepath <- "Z:\\Students\\Maria\\Mountain Gem Data\\2024 Data\\GHGs.xlsx"
sheets <- excel_sheets(mfilepath)

ghg_meas <- lapply(sheets, function(X) read_excel(mfilepath, X))
ghg_meas <- lapply(ghg_meas, as.data.frame)
names(ghg_meas) <- sheets

#Add simulated N2O values for April, early May
splitdate <- function(df){
  df <- df |>
    mutate(
      ian = year(Date),
      mo = month(Date),
      jo = day(Date),
      jul = yday(Date)
    )
  df <- df[,c(3,4,5,6,2)]
  df
}
simN2O <- lapply(sims, 
                 function(df) df[strftime(df$Date, format = "%j") %in% c(110, 114, 119, 129), 
                                 c(1, 17)])
simN2O <- lapply(simN2O, splitdate)
simN2O <- simN2O[names(ghg_meas[3:20])]
simN2O <- lapply(simN2O, function(df){
  df <- cbind(df,data.frame(matrix(NA, nrow = 4, ncol = 5)))
  names(df) <- names(ghg_meas[[3]])
  df
  }
  )

ghg_meas[3:20] <- Map(rbind,simN2O,ghg_meas[3:20])

#replace -999.99 with NA
ghg_meas <- lapply(ghg_meas, function(df) {
  df[] <- lapply(df, function(col) replace(col, col == -999.99, NA))
  df
})




#Get climate data---------------------------------------------------------------

climate <- read.table("extra/climat.txt")

climate$Date <-
  as.POSIXct(paste(climate$V2, climate$V3, climate$V4,sep="-"),
             format = "%Y-%m-%d", tz = "UTC")

data <- list()
i <- 1
for (usm in ghg_meas[3:20]){
  usm$Date <- as.Date(usm$jul - 1, 
                      origin = paste0(2024, "-01-01"))
  sm <- left_join(climate,
                  usm,
                  join_by(Date))
  sm <- sm[c(110:277),c(10,14,18,19,20,21,22)]
  sm$jul <- strftime(sm$Date, format = "%j")
  sm$jul <- as.double(sm$jul) + 1
  names(sm) <- c("Precipitation",
                 "Date", "jul", "N2O","CO2", 
                 "SoilMoisture", "SoilTemp")
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
linN2O <- lapply(dfN2O, function(df)
  approx(x = df$jul, 
         y = df$N2O, 
         xout = min(df$jul):max(df$jul))
  )
linN2O <- lapply(linN2O, as.data.frame)
linN2O <- lapply(linN2O, function(df){
  df$linN2O.cum <- cumsum(df$y)
  df
  })

linCO2 <- lapply(dfN2O, function(df)
  approx(x = df$jul, 
         y = df$CO2, 
         xout = min(df$jul):max(df$jul))
)
linCO2 <- lapply(linCO2, as.data.frame)
linCO2 <- lapply(linCO2, function(df){
  df$linCO2.cum <- cumsum(df$y)
  df
})


#Trapezoidal Integration
trapN2O <- lapply(dfN2O, function(df){
                 sapply(1:nrow(df), function(i) {
  if (i == 1) return(df$N2O[1])  # No area under a single point
  trapz(df$jul[1:i], df$N2O[1:i])})
  })

trapCO2 <- lapply(dfCO2, function(df){
  sapply(1:nrow(df), function(i) {
    if (i == 1) return(df$CO2[1])  # No area under a single point
    trapz(df$jul[1:i], df$CO2[1:i])})
})

#Bayesian Lognormal

bayesLog <- function(df,
                     ghg = c("N2O", "CO2")){
  if(ghg == "N2O"){
    df$N2O[df$N2O == 0] <- 1e-6
    df$log_N2O <- log(df$N2O)
  }else{
    df$CO2[df$CO2 == 0] <- 1e-6
    df$log_CO2 <- log(df$CO2) 
  }
  
  df$time <- df$jul - min(df$jul)
  
  set.seed(1234)
  if(ghg=="N2O"){
    model <- stan_glm(
      log_N2O ~ time,
      data = df,
      family = gaussian(),  
      prior = normal(0, 2.5),
      chains = 4,
      iter = 2000
    )
  }else{
    model <- stan_glm(
      log_CO2 ~ time,
      data = df,
      family = gaussian(),  
      prior = normal(0, 2.5),
      chains = 4,
      iter = 2000,
      refresh = 0
    )
  }
  # Predict log-flux from posterior
  pred_log <- posterior_predict(model)
  
  # Back-transform to original flux scale
  pred <- exp(pred_log)
  
  # Estimate average predicted daily flux per week
  mean_pred <- colMeans(pred)
  
  # Estimate cumulative emissions
  interval_days <- diff(df$time)
  if(ghg=="N2O"){
    df$cumN2O <- c(mean_pred[1],
                     cumsum(mean_pred[-1] * interval_days))
  }else{
    df$cumCO2 <- c(mean_pred[1],
                     cumsum(mean_pred[-1] * interval_days))
  }

}

bayesN2O <- lapply(dfN2O, bayesLog, ghg = "N2O")
bayesN2O <- lapply(bayesN2O, as.data.frame)
bayesN2O <- lapply(bayesN2O, rownames_to_column, var = "x")
bayesCO2 <- lapply(dfCO2, bayesLog, ghg = "CO2")
bayesCO2 <- lapply(bayesCO2, as.data.frame)

#create summary
summaryN2O  <- Map(function(list1, list2, list3, list4) {
  df <- cbind(list2, list3)
  names(df) <- c("x","bayesN2O.cum", "trapN2O.cum")
  df$x <- as.integer(df$x)
  df2 <- left_join(list1, df, by = "x")
  names(df2)[names(df2)== "x"] <- "jul"
  names(df2)[names(df2)== "y"] <- "linN2O"
  left_join(df2,list4, by = "jul") |> relocate(Date, .before = everything())
}, linN2O, bayesN2O, trapN2O, data)

write.xlsx(summaryN2O, file.path(datadir,"cumEmissions_estimated2.xlsx"))

p <- summaryN2O$`H24N170-201A`

emissions <- ggplot(p,
                    aes(x = jul)) +
  geom_col(aes(y = Precipitation/20),
           position = "identity",
           fill = "blue",
           alpha = 0.5) +
  geom_line(data = p[!is.na(p$N2O),],
            aes(y = linN2O.cum),
            color = "red2", linewidth = 0.8,
            na.rm = F) +
  geom_line(data = p[!is.na(p$N2O),],
            aes(y = trapN2O.cum),
            color = "purple3", linewidth = 0.8,
            na.rm = F) +
  geom_line(data = p[!is.na(p$N2O),],
            aes(y = bayesN2O.cum),
            color = "black", linewidth = 0.8,
            na.rm = F) +
  scale_y_continuous(name = "N2O (kg/ha)",
                     sec.axis = sec_axis(~.*20,name="Precipitation (mm)")) +
  theme_minimal()

emissions


#create file formatted for .obs
#days with other observations
jul_days <- c(111, 115, 120, 130,138,144,150,158,164,171,184,186,192,198,199,
              201,206,212,213,220,225,228,238,239,247,256,263,270,277)
summary4obs <- lapply(summaryN2O, function(df) df[df$jul %in% jul_days,])
summary4obs <- lapply(summary4obs, function(df) {
  df[] <- lapply(df, function(col) replace(col, is.na(col), -999.99))
  df
})

write.xlsx(summary4obs, file.path(datadir,"cumEmissions_est4obs2.xlsx"))

#organise plots by rotation/treatment
x <- rbind(as.list(names(ghg_meas[[1]])), ghg_meas[[1]])
calibplots <- x[,c(1,3,4)]
names(calibplots) <- c("Plot","Rotation", "Treatment")
evalplots <- x[,c(2,3,4)]
names(evalplots) <- c("Plot","Rotation", "Treatment")
x <- rbind(calibplots,evalplots)
ghg_meas[[1]] <- x
remove(x, calibplots, evalplots)



gen_plots <- function(df, name){
  ggplot(df,aes(x = jul)) +
  geom_line(data = df[!is.na(df$N2O),],
            aes(y = .data[[y_var]]),
            color = "red2", linewidth = 0.8,
            na.rm = F) +
    scale_y_continuous(name = "N2O (kg/ha)") +
    coord_cartesian(ylim = c(0,4.5)) +
    labs(title = name) +
  theme_minimal()
}
plots <- lapply(summaryN2O, gen_plots, name = 1:18)

i <- 1
plots <- list()
summaryN2O <- summaryN2O[ghg_meas[[1]][,1]]
n <- names(summaryN2O)
for(df in summaryN2O){
  plots[[i]] <- gen_plots(df,n[i])
  i <- i + 1
}
names(plots) <- names(summaryN2O)

grid <- grid.arrange(
  grobs = plots[1:9],
  nrow = 9
)
grid2 <- grid.arrange(
  grobs = plots[10:18],
  nrow = 9
)
plot <- cbind(grid,grid2, size = "first")

ggsave(
  file.path(dir,"allplots.png"),
  plot = plot,
  width = 20,
  height = 50,
  units = "cm")

#final Plot
#subtract cumulative value from jul 129 from all cumulative values to get May to October
