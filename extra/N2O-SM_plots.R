#code to generate plots for N2O vs Precipitation, SM vs Precipitation, N2O vs SM
#using STICS climate and observed data files

library("dplyr")

#get ratios between simulated and observed HR_1
# ratios <- c()
# for (usm in names(sims)){
#   sm <- inner_join(sims[[grep(usm,names(sims))]], 
#                    obs[[grep(usm,names(obs))]],
#                    join_by(Date))
#   ratios <- c(ratios,mean(sm$HR_1.y/sm$HR_1.x, na.rm=T))
# } 
# mean(ratios)

#Make precip vs N2O or soil moisture plots (%)
climate <- read.table(file.path(workspace, "TxtFiles", "H24N120-202A","climat.txt"))
climate$Date <-
  as.POSIXct(paste(climate$V2, climate$V3, climate$V4,sep="-"),
                      format = "%Y-%m-%d", tz = "UTC")
df <- data.frame(matrix(nrow=38, ncol=0))
for (usm in obs){
  sm <- inner_join(climate,
                   usm,
                   join_by(Date))
  sm <- sm[,c(10,14,27,29)]
  names(sm) <- c("Precipitation",
                 "Date", "N2O", "SoilMoisture")
  sm <- sm |> relocate(Date, .before = everything())
  sm <- sm |> filter(!is.na(N2O) | !is.na(SoilMoisture))
  df <- rbind(df, sm)
}

avgN2O <- df |> group_by(Date) |> summarise(avg = mean(N2O, na.rm = T))
minN2O <- df |> group_by(Date) |> summarise(min = min(N2O, na.rm = T))
maxN2O <- df |> group_by(Date) |> summarise(max = max(N2O, na.rm = T))

dfN2O <- cbind(avgN2O,minN2O[,2],maxN2O[,2])

avgSM <- df |> group_by(Date) |> summarise(avg = mean(SoilMoisture, na.rm = T))
minSM <- df |> group_by(Date) |> summarise(min = min(SoilMoisture, na.rm = T))
maxSM <- df |> group_by(Date) |> summarise(max = max(SoilMoisture, na.rm = T))

dfSM <- cbind(avgSM,minSM[,2],maxSM[,2])

dfN2Oprecip <- right_join(dfN2O, climate[,c(14,10)], join_by(Date))
dfSMprecip <- right_join(dfSM, climate[,c(14,10)], join_by(Date))

dfN2Oprecip <- dfN2Oprecip[!duplicated(dfN2Oprecip),] |> arrange(Date)
dfSMprecip <- dfSMprecip[!duplicated(dfSMprecip),] |> arrange(Date)

colnames(dfN2Oprecip)[5] <- "Precipitation"
colnames(dfSMprecip)[5] <- "Precipitation"

start <- which(dfN2Oprecip$Date == as.POSIXct("2024-05-17", tz = "UTC"))
end <- which(dfN2Oprecip$Date == as.POSIXct("2024-10-04", tz = "UTC"))

emissions <- ggplot(dfN2Oprecip[start:end,],
                    aes(x = Date)) +
  geom_col(aes(y = Precipitation/150),
           position = "identity",
           fill = "blue",
           alpha = 0.5) +
  geom_ribbon(aes(ymin = min, ymax = max), fill = "red", alpha = 0.2,
              na.rm = T) +
  geom_line(data = dfN2Oprecip[!is.na(dfN2Oprecip$avg),],
            aes(y = avg),
            color = "red", linewidth = 0.8,
            na.rm = F) +
  scale_y_continuous(name = "N2O (kg/ha/day)",
                     sec.axis = sec_axis(~.*150,name="Precipitation (mm)")) +
  theme_minimal()

emissions

soilM <- ggplot(dfSMprecip[start:end,],
                aes(x = Date)) +
  geom_col(aes(y = Precipitation*1.33, color = "Precip."),
           fill = "blue",
           position = "identity",
           alpha = 0.35) +
  geom_ribbon(aes(ymin = min, ymax = max), fill = "darkgreen", alpha = 0.2,
              na.rm = T) +
  geom_line(data = dfSMprecip[!is.na(dfSMprecip$avg),],
            aes(y = avg, color = "Soil Moisture"),
            size = 0.8,
            na.rm = F) +
  scale_y_continuous(name = "Soil Moisture (%)",
                     sec.axis = sec_axis(~./1.33,name="Precipitation (mm)")) +
  theme_minimal() +
  theme(
    axis.title.y = element_text(color = "darkgreen", size=13),
    axis.title.y.right = element_text(color = "blue", size=13),
    legend.position = "bottom"
  ) +
  geom_hline(aes(yintercept=18.5, color = "FC"), 
             linewidth = 0.6, linetype = 5, alpha = 0.5) +
  geom_hline(aes(yintercept=7.5, color = "WP"), 
             linewidth = 0.6, linetype = 5, alpha = 0.5) +
  scale_colour_manual("", 
                      breaks = c("Soil Moisture", "Precip.", "FC", "WP"),
                      values = c("Soil Moisture" = "darkgreen", 
                                 "Precip." = "#A6A6FF", 
                                 "FC" = "blue3", 
                                 "WP" = "black")) +
  labs(title  =  "Soil Moisture - Hills")
soilM

#Soil Moisture Vs. N2O
dfN2OsoilM <- left_join(dfN2Oprecip, dfSM, join_by(Date))

N2OsoilM <- ggplot(dfN2OsoilM[start:end,],
                   aes(x = Date)) +
  geom_ribbon(aes(ymin = min.y, ymax = max.y), fill = "darkgreen", alpha = 0.2,
              na.rm = T) +
  geom_line(data = dfN2OsoilM[!is.na(dfN2OsoilM$avg.y),],
            aes(y = avg.y, color = "Soil Moisture"),
            linewidth = 0.8,
            na.rm = F) +
  geom_ribbon(aes(ymin = min.x*200, ymax = max.x*200), fill = "red", alpha = 0.2,
              na.rm = T) +
  geom_line(data = dfN2OsoilM[!is.na(dfN2OsoilM$avg.x),],
            aes(y = avg.x*200,  color = "N2O"),
            linewidth = 0.8,
            na.rm = F) +
  scale_y_continuous(name = "Soil Moisture (%)",
                     sec.axis = sec_axis(~./200,name="N2O (kg/ha/day)")) +
  theme_minimal() +
  theme(
    axis.title.y = element_text(color = "darkgreen", size=13),
    axis.title.y.right = element_text(color = "red", size=13),
    legend.position = "bottom"
  ) +
  geom_hline(aes(yintercept=13.7, color = "FC"), 
             linewidth = 0.6, linetype = 5, alpha = 0.5) +
  geom_hline(aes(yintercept=5.6, color = "WP"), 
             linewidth = 0.6, linetype = 5, alpha = 0.5) +
  scale_colour_manual("", 
                      breaks = c("Soil Moisture", "N2O", "FC", "WP"),
                      values = c("Soil Moisture" = "darkgreen", 
                                 "N2O" = "red", 
                                 "FC" = "blue3", 
                                 "WP" = "black")) +
  labs(title  =  "Soil Moisture and N2O - Hills",
       subtitle = "adjusted FC and WP")

  
N2OsoilM


#simulated cummulative N2O vs estimated cummulative N2O vs. Precip.
