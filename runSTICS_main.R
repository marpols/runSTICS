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
              "ggplot2","ggpubr","jpeg","CroptimizR","tidy","readxl")
lapply(packages, library, character.only=TRUE)

source("scr/runSTICS.R")


stics_path <- "Z:\\Students\\Maria\\STICS\\MtnGem\\JavaSTICS-1.5.2-STICS-10.2.0"
subdir <-"MtnGem-Harrington" #directory containg xml files
#subdir <- "CroptimizR-MtnGem-Harrington"


#Select output variables
outvars <- c("pdsfruitfrais","mafrais","mafruit","zrac","iflos","iamfs",
             "imats","idrps","laimax","lai(n)","cet","cep","et","ep",
             "QNplante","QNgrain","spfruit","swfac","swfac2moy","turfac","turfac2moy",
             "N_mineralisation","leaching_from_plt","Ndenit","HR_vol_1_30",
             "HR_vol_31_60","HR_vol_61_90","epsib")
#apply new output variables
gen_varmod(wspc,outvars,append=F)


#STICS calibration--------------------------------------------------------------
version <- ""

#if completing calibration/evaluation
#specify list of usms used for calibration and list of usms used for evaluation
usm_list <- as.data.frame(read_excel(file.path(stics_path,"usms_cal-eval.xlsx"),
                                     col_names=FALSE))
usm_cal <- usm_list[,1]
usm_eval <- usm_list[,2]

#path where to store the results (graph and Rdata)
results_dir <- file.path(stics_path,subdir,version)
dir.create(results_dir, showWarnings = FALSE)

  #USING CROPTIMIZR-----------------------
random_seed <- 1234 #set random seed

#single situation/usm or vector of usms names to consider for calibration
sit_name <- usm_cal

#obs var names to consider in calibration. Single instance or vector
var_name <- "lai_n"

#select lower and upper bound of parameters to consider in calibration
lowerbnds <- c(dlaimax = 0.0005, 
               durvieF = 50,)
upperbnds <- c(dlaimax = 0.0025, 
               durvieF = 400)

reps <- 7 #advised at least 5, 10 is reasonable maximum
max <- 500 #max number of evaluations
tolerance <- 1e-03  # Tolerance criterion between two iterations
                    # (threshold for the relative difference of
                    # parameter values between the 2 previous iterations)

source("src/CroptimizR.R")


  #MANUALLY-----------------------------

#For calibration
#change parameters for a single run
param_values <- c(0.002, 50)
names(param_values) <- c("dlaimax", "durvieF")

#or create a dataframe to parse through multiple possible parameter values/combinations
#(i.e. grid search)
param_values <- as.data.frame(tidyr::crossing(dlaimax = c(0.001),
                                              durvieF = c(50, 60, 70)))

results <- calibrate_STICS


#Run STICS----------------------------------------------------------------------

wkspc <- file.path(stics_path,subdir) #STICS workspace

#list of usms from workspace
usms <- SticsRFiles::get_usms_list(file = file.path(wkspc, "usms.xml"))

results <- runSTICS(stics_path, wkspc, usms)

