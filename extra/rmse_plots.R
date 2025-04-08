cal <- get_stats_summary(file.path(stics_path, "MtnGem-Harrington"),
                  "calib-final2",
                  output_dir = file.path(stics_path, "MtnGem-Harrington"),
                  usms = usm_cal, plot = F)

eval <- get_stats_summary(file.path(stics_path, "MtnGem-Harrington"),
                  "eval-final2",
                  output_dir = file.path(stics_path, "MtnGem-Harrington"),
                  usms = usm_eval, plot = F)

cal_vars <- c("masec_n", "mafruit", "QNplante", "QNgrain", "msrac_n", "lai_n", "Qem_N2O")
labels <- c("cum N20 - Cumulative N2O emissions",
            "LAI", 
            "RtB - Root Biomass",
            "TN - Total Biomass",
            "TN - Total N",
            "TuB - Tuber Biomass",
            "TuN - Tuber N")
extra_vars <- c("mafrais", "pdsfruitfrais", "QNrac", "QCrac")
labels <- c("RtC - Root C",  "RtN - Root N","TFW - Fresh Weight", "Yield")

colours <- paletteer_d("MoMAColors::Panton")

dir <- "\\\\chaemci01-cifs.efs.agr.gc.ca\\common\\COMMON\\Workgroups\\MesbahM\\Students\\Maria\\Mountain Gem Data\\2024 Data\\Figures"

plotRMSE(eval[eval$variable %in% extra_vars,], dir, "Evaluation - Additional", update_labs = T, 
         customize = T, colours = colours, labels = labels, width = 8)

sims <<- SticsRFiles::get_sim(javastics = stics_path,
                              workspace = workspace, 
                              usm = usm_eval)
plot_simvobs(sims, obs.= obs, ver = version)
