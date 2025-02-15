#R code to run STICS and for calibration and evaluation of model

if (!require("devtools")){
  install.packages("devtools")
}
if (!require("SticsRPacks")){
  devtools::install_github("SticsRPacks/SticsRPacks@*release")
  devtools::install_github("SticsRPacks/CroPlotR@*release")
}
if (!require("Metrics")){
  install.packages("Metrics")
}
if (!require("ggpubr")){
  install.packages("ggpubr")
  install.packages("jpeg")
}
packages <- c("devtools","SticsOnR", "CroPlotR","SticsRFiles","Metrics",
              "ggplot2","ggpubr","jpeg","CroptimizR","tidyr","readxl")
lapply(packages, library, character.only=TRUE)

source("scr/runSTICS.R")
source("src/CroptimizR.R")
source("src/cal-eval_plots.R")


stics_path <- "Z:\\Students\\Maria\\STICS\\MtnGem\\JavaSTICS-1.5.2-STICS-10.2.0"
subdir <-"MtnGem-Harrington" #directory containg xml files
#subdir <- "CroptimizR-MtnGem-Harrington"


#Select output variables
#outvars <- c("pdsfruitfrais","mafrais","mafruit","zrac","iflos","iamfs",
             # "imats","idrps","laimax","lai(n)","cet","cep","et","ep",
             # "QNplante","QNgrain","spfruit","swfac","swfac2moy","turfac","turfac2moy",
             # "N_mineralisation","leaching_from_plt","Ndenit","HR_vol_1_30",
             # "HR_vol_31_60","HR_vol_61_90","epsib")
outvars <- c("masec(n)","mafrais","mafruit","QNplante", "QNgrain", "QN", "lai(n)")

#apply new output variables
gen_varmod(wspc,outvars,append=F)


#STICS calibration--------------------------------------------------------------

version <- "default-"

#path where to store the results (graph and Rdata)
results_dir <- file.path("Z:\\Students\\Maria\\STICS\\MtnGem\\Calibration-2024",version)
dir.create(results_dir, showWarnings = FALSE)

#if completing calibration/evaluation
#specify list of usms used for calibration and list of usms used for evaluation
usm_list <- "usms_cal-eval.xlsx"
usm_list <- as.data.frame(read_excel(file.path(stics_path,usm_list),
                                     col_names=FALSE))
usm_cal <- usm_list[,1] #calibration
usm_eval <- usm_list[,2] #evaluation

#single situation/usm or vector of usms names to consider for calibration
sit_name <- usm_cal

#obs var names to consider in calibration. Single instance or vector
var_name <- "lai_n"

#select lower and upper bound of parameters to consider in calibration
lowerbnds <- c(dlaimax = 0.0005, 
               durvieF = 50)
upperbnds <- c(dlaimax = 0.0025, 
               durvieF = 400)

#step to increase each variable by (if using a grid search approach)
by <- c(0.0005, 50)

  #USING CROPTIMIZR-----------------------
calib_init(croptimizR = T)

random_seed <<- 1234 #set random seed

reps <- 7 #advised at least 5, 10 is reasonable maximum
max <- 500 #max number of evaluations
tolerance <- 1e-03  # Tolerance criterion between two iterations
                    # (threshold for the relative difference of
                    # parameter values between the 2 previous iterations)



results <- runCroptimizR()

  #MANUALLY-----------------------------
calib_init()

#For calibration
#change parameters for a single run
param_values <- c(0.002, 50)
names(param_values) <- c("dlaimax", "durvieF")

#or create a dataframe to parse through multiple possible parameter values/combinations
#(i.e. grid search)
param_values <- param_grid(by=by)

results <- calibrate_STICS()


#Run STICS----------------------------------------------------------------------
#within javastics workspace directory (i.e. not seperated by usms)

results <- runSTICS()

