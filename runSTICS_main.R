#R code to run STICS and for calibration and evaluation of model

#set path to folder containing javastics.exe and path to javastics working directory
stics_path <- "Z:\\Students\\Maria\\STICS\\JavaSTICS-1.5.2-STICS-10.2.0"
#subdir <-"MtnGem-Harrington" #main workspace
subdir <- "CroptimizR-MtnGem-Harrington"
#subdir <- "Morissette-et-al_2016"
#subdir <- "MattRamsayData"
xml_dir <- "XmlFiles" #directory containing xml files (if different from subdir)


source("src/runSTICS.R")
source("src/CroptimizR.R")
source("src/cal-eval_plots.R")

init_()


#Select output variables
#outvars <- c("pdsfruitfrais","mafrais","mafruit","zrac","iflos","iamfs",
             # "imats","idrps","laimax","lai(n)","cet","cep","et","ep",
             # "QNplante","QNgrain","spfruit","swfac","swfac2moy","turfac","turfac2moy",
             # "N_mineralisation","leaching_from_plt","Ndenit","HR_vol_1_30",
             # "HR_vol_31_60","HR_vol_61_90","epsib")
#outvars <- c("masec(n)","mafrais","mafruit","QNplante", "QNgrain", "lai(n)")
#variables to output
outvars <- c("masec(n)","mafrais","mafruit","pdsfruitfrais","msrac(n)",
             "QNplante","QNgrain","QNrac","CNplante","CNgrain","QCrac","lai(n)",
             "zrac","resmes","cet", "et")

#apply new output variables
gen_varmod(workspace,outvars,append=F)

version <- "mountainGem_msrac"

#path to where the results will be stored (graph and Rdata)
results_dir <- file.path("Z:\\Students\\Maria\\STICS\\MtnGem-Calibration-2024",
                         version)
dir.create(results_dir, showWarnings = T)
message("output directory set as:\n", results_dir)

#Run STICS----------------------------------------------------------------------
#within javastics workspace directory

set_obs(xml_path,usms = usm_cal)

results <- runSTICS(usms = usm_cal,version = version)

sims <<- SticsRFiles::get_sim(javastics = stics_path,
                              workspace = workspace, 
                              usm = usm_cal)

plot_simvobs(sims, obs.= obs, ver = version) #plot sim vs. obs for each individual usm


#STICS calibration--------------------------------------------------------------

#if completing calibration/evaluation
#specify list of usms used for calibration and list of usms used for evaluation
usm_list <- "mtngem_cal-eval.xlsx"
usm_list <- as.data.frame(read_excel(file.path(workspace,usm_list),
                                     col_names=FALSE))
usm_cal <- usm_list[,1] #calibration
usm_eval <- usm_list[,2] #evaluation
  
#single situation/usm or vector of usms names to consider for calibration
sit_name <- usm_cal

#obs var names to consider in calibration. Single instance or vector
# var_name <- c("masec(n)","mafrais","mafruit","pdsfruitfrais","msrac(n)",
#               "QNplante","QNgrain","QNrac","CNplante","CNgrain","QCrac","lai(n)")
var_name <- c("msrac_n")

#select lower and upper bound of parameters to consider in calibration
lowerbnds <- c(draclong = 1.0, 
               debsenrac = 950,
               lvfront = 0.02)
upperbnds <- c(draclong = 100, 
               debsenrac = 1900,
               lvfront = 1.00)

#step to increase each variable by (if using a grid search approach)
by <- c(0.0005, 50)

  #USING CROPTIMIZR-----------------------
calib_init(xml_path = xml_dir)

random_seed <<- 1234 #set random seed

reps <- 7 #advised at least 5, 10 is reasonable maximum
max <- 500 #max number of evaluations
tolerance <- 1e-03  # Tolerance criterion between two iterations
                    # (threshold for the relative difference of
                    # parameter values between the 2 previous iterations)

set_obs(xml_path, usms = sit_name,variables = var_name)
update_ops(stics_inputs_path,outvars)

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



