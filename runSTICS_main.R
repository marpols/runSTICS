################################################################################
# code for running STICS in work space directory
# and for calibration and evaluation of model
# Author: Mariaelisa Polsinelli for completion of PhD Thesis
# McGil University, AAFC
################################################################################

#set path to folder containing javastics.exe and path to javastics working directory
stics_path <- "C:\\Users\\marpo\\Documents\\Research\\STICS\\JavaSTICS-1.5.3-STICS-10.3.0"
#stics_path <- "\\\\chaemci01-cifs.efs.agr.gc.ca\\common\\COMMON\\Workgroups\\MesbahM\\Students\\Maria\\STICS\\JavaSTICS-1.5.2-STICS-10.2.0"
subdir <- "irrigation_assessment"
# subdir <-"MtnGem-Harrington" #main workspace
# subdir <- "CroptimizR-MtnGem-Harrington"
# subdir <- "Morissette-et-al_2016"
# subdir <- "MattRamsayData"
# xml_dir <- "XmlFiles" #directory containing xml files (if different from subdir)

sources <- c("src/runSTICS.R", "src/CroptimizR.R", 
             "src/cal-eval_plots.R","src/files.R")
invisible(lapply(sources, source))

init_()
init_files(xml_file_path, "xml")
# init_files(xml_file_path, "txt") #"xml" - javastics workspace, "txt" - txt files folder for calibration

#Select output variables
#outvars <- c("pdsfruitfrais","mafrais","mafruit","zrac","iflos","iamfs",
             # "imats","idrps","laimax","lai(n)","cet","cep","et","ep",
             # "QNplante","QNgrain","spfruit","swfac","swfac2moy","turfac","turfac2moy",
             # "N_mineralisation","leaching_from_plt","Ndenit","HR_vol_1_30",
             # "HR_vol_31_60","HR_vol_61_90","epsib")
#outvars <- c("masec(n)","mafrais","mafruit","QNplante", "QNgrain", "lai(n)")
#outvars <- c("masec(n)","mafrais","mafruit","pdsfruitfrais","msrac(n)",
#              "QNplante","QNgrain","QNrac","CNplante","CNgrain","QCrac","lai(n)",
#              "zrac","resmes","cet", "et", "em_N2O", "CO2sol","GHG", "Qem_N2O","HR(1)")
#outvars <- c("masec(n)","mafrais","mafruit","pdsfruitfrais","msrac(n)",
#              "QNplante","QNgrain","CNplante","CNgrain","lai(n)",
#              "zrac","resmes","cet", "et", "em_N2O","GHG", "Qem_N2O","HR(1)",
#              "inn", "QCrac", "QNrac", 
#              "ulai_n", "turfac", "swfac", "senfac","innlai","splai", 
#              "idebdess","fpft", "allocfruit", "sourcepuits")
outvars <- c("HR_1", "pdsfruitfrais","cet","et")

#apply new output variables
gen_varmod(workspace,outvars,append=F)


#Set Version name and results directory-----------------------------------------
version <- "Harrington_ARY_noWS"

#path to where the results will be stored (graph and Rdata)
#results_dir <- file.path("\\\\chaemci01-cifs.efs.agr.gc.ca\\common\\COMMON\\Workgroups\\MesbahM\\Students\\Maria\\STICS\\MtnGem-Calibration-2024",
                         # version)
results_dir <- file.path(file.path(workspace,"RESULTS"),
                         version)
if(!(dir.exists(file.path(workspace,"RESULTS")))){
  dir.create(file.path(stics_path, workspace,"RESULTS"))
}
dir.create(results_dir, showWarnings = T)
message("output directory set as:\n", results_dir)

################################################################################
#  Run STICS
#  directly in javastics workspace directory
################################################################################

set_obs(xml_file_path)

results <- runSTICS(version = version, usms = usms[2:22])

sims <<- SticsRFiles::get_sim(javastics = stics_path,
                              workspace = workspace)
obns <- get_param_xml(usm_file, "fobs")[[1]][[1]]
ob_names <- obns[!(obns == "NA")]

usm_obs <- setNames(lapply(ob_names, function(o){
  o <- sub(".obs","",o)
  obs[[{{o}}]]
  }), usms
)

get_single_stats(sims, usm_obs, results_dir)

plot_simvobs(sims, obs.= usm_obs, ver = version) #plot sim vs. obs for each individual usm


#update parameters--------------------------------------------------------------
file <- param_gen
  
params <- list("codeh2oact")
  
type <- "xml" #"xml" or "txt" (calibration)

values <- list(2)

get_param_info()

get_param_xml(file, param = params) #view current parameter value

set_param(file, type, params = params, values = values)


#Create new usms----------------------------------------------------------------
#for each combination of weather stations, years, soil profiles
#names should match sta.xml files, existing soil profile names

# stations <- c("Summerside", "New Glasgow", "Harrington CDA CS", "East Point (AUT)")
# years <- 2004:2024
# soil_profiles <- c("CTW", "ARY", "CLO")
# usm_names <- c("RG-19") #usms from which other attributes will be copied (e.g plant, ini files)

stations <- c("Harrington CDA CS")
years <- 2004:2024
soil_profiles <- c("ARY_hills")
usm_names <- c("PEI")

add_usms(stations, years, soil_profiles, usm_names)




################################################################################
#    STICS calibration
################################################################################

#if completing calibration/evaluation
#specify list of usms used for calibration and list of usms used for evaluation
usm_list <- "mtngem_cal-eval.xlsx"
usm_list <- as.data.frame(read_excel(file.path(workspace,usm_list),
                                     col_names=FALSE))
#usm_cal <- usm_list[,1] #calibration
# usm_cal <- usms
usm_cal <- usms[4:6]
usm_eval <- usm_list[,2] #evaluation
  
#single situation/usm or vector of usms names to consider for calibration
sit_name <- usm_cal

#obs var names to consider in calibration. Single instance or vector------------
# var_name <- c("masec(n)","mafrais","mafruit","pdsfruitfrais","msrac(n)",
#               "QNplante","QNgrain","QNrac","CNplante","CNgrain","QCrac","lai(n)")
var_name <- c("QNplante", "lai_n", "mafruit")
var_name <- c("CNgrain", "QNgrain")
var_name <- c("masec_n", "QNplante") #TB, TN
var_name <- c("masec_n", "mafruit", "msrac_n") #all biomass
var_name <- c("masec_n", "mafruit", "QNplante","QNgrain") #biomass and N - no roots
var_name <- c("masec_n", "mafruit", "msrac_n","QNplante","QNgrain", "QNrac") #all biomass and N
var_name <- c("masec_n", "mafruit", "msrac_n", "lai_n") 
var_name <- c("lai_n")
var_name <- c("masec_n") #TB
var_name <- c("QNplante") #TN
var_name <- c("mafruit")#TuB
var_name <- c("QNplante","QNgrain")
var_name <- c("QNplante","QNgrain", "QNrac")#all N
var_name <- c("QNgrain") #TuN
var_name <- c("msrac_n") #Root Biomass
var_name <- c("msrac_n", "lai_n")
var_name <- c("Qem_N2O")
var_name <- c("HR_1")

#select lower and upper bound of parameters to consider in calibration----------

#emergence and starting
lowerbnds <- c(belong = 0.005)
upperbnds <- c(belong = 0.022)
lowerbnds <- c(belong = 0.005,
               celong = 1,
               elmax = 5,
               tgmin = -1)
upperbnds <- c(belong = 0.022,
               celong = 5,
               elmax =  20,
               tgmin = 3)

#leaves
lowerbnds <- c(innturgmin = -2,
               laicomp = 0,
               pentlaimax = 3, 
               vlaimax = 1.5)
upperbnds <- c(innturgmin = 0.4,
               laicomp = 3,
               pentlaimax = 5.5, 
               vlaimax = 2.5)
lowerbnds <- c(udlaimax = 2.4)
upperbnds <- c(udlaimax = 3.0)

#shootbiomass growth
lowerbnds <- c(efcroijuv = 1,
               efcroirepro = 1,
               efcroiveg = 1
               )
upperbnds <- c(efcroijuv = 2.6,
               efcroirepro = 5.2,
               efcroiveg = 4.25)
lowerbnds <- c(teoptbis = 17,
               remobres = 0,
               abscission = 0
               )
upperbnds <- c(teoptbis = 32,
               remobres = 0.2,
               abscission = 1.0
               )
lowerbnds <- c(abscission = 0)
upperbnds <- c(abscission = 1.0)

#yield formation - indeterminate plant
lowerbnds <- c(nboite = 1,
               allocfrmax = 0.05,
               afpf = 0.1,
               bfpf = 0,
               spfrmin = 0,
               spfrmax = 0.7
               )
upperbnds <- c(nboite = 10,
               allocfrmax = 1,
               afpf = 1, 
               bfpf = 15,
               spfrmin = 1,
               spfrmax = 2
               )
lowerbnds <- c(afpf = 0.1,
               bfpf = 4)
upperbnds <- c(afpf = 1,
               bfpf = 20)
lowerbnds <- c(irazomax = 0)
upperbnds <- c(irazomax = 1)
lowerbnds <- c(spfrmin = 0,
               spfrmax = 0)
upperbnds <- c(spfrmin = 1,
               spfrmax = 2)

#yield formation - indeterminate plant (lai)
lowerbnds <- c(splaimin = 0,
               splaimax = 0.5
                 )
upperbnds <- c(splaimin = 1,
               splaimax = 2
                 )
#nitrogen (biomass and N)
lowerbnds <- c(Vmax2 = 0.017,
               INNimin = -0.5)
upperbnds <- c(Vmax2 = 0.12,
               INNimin = 0.0)

#nitrogen (N only)
lowerbnds <- c(inngrain1 = 0.03,
               inngrain2 = 0.3)
upperbnds <- c(inngrain1 = 2,
               inngrain2 = 2)

#cultivar - tuber phenological stages
lowerbnds <- c(stlevdrp = 0,
               stdrpdes = 650)
upperbnds <- c(stlevdrp = 500,
               stdrpdes = 700)

#cultivar yield formation (mafruit)
lowerbnds <- c(stdrpnou = 0,
               stlevdrp = 0,
               stdrpdes = 650,
               dureefruit = 1000)
upperbnds <- c(stdrpnou = 200,
               stlevdrp = 1000,
               stdrpdes = 700,
               dureefruit = 2000)

#cultivar - LAI
lowerbnds <- c(adens = -1.5,
               bdens = 1,
               dlaimaxbrut = 0.0005,
               durvieF = 150,
               innsen = -1,
               stamflax = 650,
               stlevamf = 150)
upperbnds <- c(adens = 0,
               bdens = 10,
               dlaimaxbrut = 0.02,
               durvieF = 300,
               innsen = 1,
               stamflax = 1000,
               stlevamf = 300)

#cultivar - shoot biomass growth 
lowerbnds <- c(temin = 0,
               teopt = 12,
               psisto = 1)
upperbnds <- c(temin = 8,
               teopt = 25,
               psisto = 10)

#cultivar - yield formation (On tuber N (QNgrain))
lowerbnds <- c(vitirazo = 0.0)
upperbnds <- c(vitirazo = 0.025)

#cultivar - roots
lowerbnds <- c(croirac = 0)
upperbnds <- c(croirac = 0.5)

#roots
lowerbnds <- c(sensrsec = 0,
               draclong = 100,
               lvfront = 0.05,
               debsenrac = 800,
               longsperac = 17000)
upperbnds <- c(sensrsec = 1,
               draclong = 400,
               lvfront = 0.5,
               debsenrac = 1500,
               longsperac = 19000)

#N2O
lowerbnds <- c(profdenit = 20, 
               vpotdenit = 1)
upperbnds <- c(profdenit = 60, 
               vpotdenit = 2)

lowerbnds <- c(afruitpot = 0.08)
upperbnds <- c(afruitpot = 1.0)

#soil moisture
lowerbnds <- c(q0 = 1,
               zesx = 20,
               cfes = 1)
upperbnds <- c(q0 = 10,
               zesx = 80,
               cfes = 10)

#step to increase each variable by (if using a grid search approach)
by <- c(0.01)

#USING CROPTIMIZR---------------------------------------------------------------
calib_init() #initialize paths, options needed for calibration, update files

random_seed <<- 1234 #set random seed

reps <- 7 #advised at least 5, 10 is reasonable maximum
max <- 500 #max number of evaluations
tolerance <- 1e-03  # Tolerance criterion between two iterations
                    # (threshold for the relative difference of
                    # parameter values between the 2 previous iterations)

#set observations
set_obs(xml_file_path, usms = sit_name, variables = var_name)
#change model outputs (if needed)
update_ops(stics_inputs_path,outvars)

#run the optimization
results <- runCroptimizR(usms = sit_name, cal_file = sols)

#update parameters
set_param(plt, "txt",
          params = list(names()),
          values = list(10.0)
          )


#Using Grid Search--------------------------------------------------------------
calib_init()

#For calibration
#create a dataframe to parse through multiple possible parameter values/combinations
#from lower to upperbounds (i.e. grid search)
param_values <- param_grid(by=by)

#or test parameter values in a single run
param_values <- c(INNimin = -0.054)

#set observations
# set_obs(xml_file_path, usms = sit_name) #all output variables
set_obs(xml_file_path, usms = sit_name,variables = var_name) #specific variables

#run grid search
results <- calibrate_STICS(usm_cal,param_values)

#plot RMSEs v RMSEu for one parameter and one output variable
plot_error(param_values,results)

#or for multiple output variables
ovs <- unique(results$variable)

for (v in ovs){
  #one parameter
  plot_error(param_values,results[results$variable == v,], fname = v)
  
  #two parameters
  # # plot_error2(param_values,
  #             results[results$variable == v,],
  #             names(param_values)[1],
  #             names(param_values)[2],
  #             fname = v)
  
  #three parameters
  # plot_error2(param_values[param_values$tgmin == 3,], results[results$variable == v,],
  #             fname = paste(v, "_tgmin3", sep = ""),
  #             "stamflax", "stlevamf")
}

plot_simvobs()


