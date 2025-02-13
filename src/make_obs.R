#create .obs files from .xlsx file

library(readxl)

#path to observations excel file
obs_xlsx <- "C:\\Users\\PolsinelliM\\OneDrive - AGR-AGR\\Documents\\MntGem STICS\\2024 Data\\obs\\usms_obs.xlsx"
#output directory for .obs files
output <- "C:\\Users\\PolsinelliM\\OneDrive - AGR-AGR\\Documents\\MntGem STICS\\2024 Data\\obs"

gen_obs <- function(xl_file, outdir){
  #generates .obs files for STICS where each sheet of an excel file is an individual usm/set of observations
  #xl_file (chr) - path of excel file containg observations
  #outdir (chr) - path of output directory
  for (s in excel_sheets(xl_file)){
    if (s != "usm list"){
      obs <- read_excel(xl_file, s)
      write.table(obs, file.path(outdir,paste(s,".obs",sep="")),sep=";", row.names = FALSE, quote = FALSE)
    }
  }
}
