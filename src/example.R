# DEFINE THE PATH TO YOUR LOCALLY INSTALLED VERSION OF JAVASTICS
javastics_path <- stics_path

# Download the example USMs and define the path to the JavaStics workspace
# (JavaStics XML input files):
data_dir <- file.path(
  SticsRFiles::download_data(
    example_dirs = "study_case_1",
    stics_version = "V9.0"
  )
)

javastics_workspace_path <- file.path(data_dir, "XmlFiles")

stics_inputs_path <- file.path(data_dir, "TxtFiles")
dir.create(stics_inputs_path, showWarnings = FALSE)

gen_usms_xml2txt(
  javastics = javastics_path,
  workspace = javastics_workspace_path,
  out_dir = stics_inputs_path,
  verbose = TRUE
)
