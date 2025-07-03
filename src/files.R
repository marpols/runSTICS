init_files <- function(workspace,
               file_type = c("xml","txt"),
               plant_file = "potato_mountainGem_plt.xml",
               station = "Harrington"){
  #initialize file paths
  files <- list.files(path = file.path(stics_path, subdir), pattern = ".xml$", full.names = TRUE)
  usm_file <<- file.path(workspace, "usms.xml")
  sta <<- file.path(workspace, sprintf("%s_sta.xml", station))
  sols <<- file.path(workspace, "sols.xml")
  plt <<- file.path(
    if(file_type =="xml"){stics_path} else{workspace},
    "plant", plant_file)
  param_gen <<- file.path(stics_path,"config","param_gen.xml")
}

new_file <- function(path,
                     old_file, 
                     new_file,
                     file = c("usms","tec","sols","ini","plt","sta"),
                     type = c("xml","txt"),
                     new_path = NULL){
  #create new file by copying old file
  if (is.null(new_path)){new_path <- path}
  if (file %in% c("tec", "ini","plt","sta")){
    ext <- sprintf("_%s.%s",file,type)
  } else {
    ext <- sprintf("%s.%s",file,type)
  }
  file.copy(from = file.path(path,paste0(old_file,ext)), 
            to = file.path(new_path, paste0(new_file,ext)),
            overwrite = TRUE)
  message("new file created by copy\n", 
          file.path(new_path, paste0(new_file,ext)))
}

param_table <- function(params){
  #makes a readable data.frame of parameters
  df <- data.frame()
  n <<- 1
  while(n <= length(params[[1]])){
    r <- c(names(params[[1]][n]),params[[1]][[n]])
    df <- rbind(df,r)
    n <<- n + 1
  }
  colnames(df) <- c("parameter", names(params[1]))
  return(df)
}

compare_files <- function(old_params, new_params){
  #compare parameters between two files
  #return table of parameters and list of changed parameters
  
  old <- param_table(old_params)
  new <- param_table(new_params)
  
  df <- cbind(old,new[2]) #combine data.tables
  
  df$Same <- df[2] == df[3] #flag changed values
  
  return(c(Parameter_Table = df, Changed_Values = df[df$Same == F,]))
}

update_txt <- function(){
  #regenerate text files to update after changes have been made to an xml file
  gen_usms_xml2txt(
    javastics = stics_path,
    workspace = xml_file_path,
    out_dir = stics_inputs_path,
    verbose = TRUE
  )
}

set_param <- function(file,
                      file_type = c("xml","txt"),
                      params,
                      values,
                      select = NULL,
                      select_value = NULL,
                      overwrite = T){
  #update values
  #params <- list of parameter names
  #values <- list of names for respective params
  
  before <- get_param_xml(file, param = params, 
                select = select, select_value = select_value)
  message("Original Parameter Values:\n",
          paste0(capture.output(before), collapse = "\n"))
  set_param_xml(file, param = params, 
                values = values,
                select = select, select_value = select_value,
                overwrite = overwrite)
  after <- get_param_xml(file, param = params,
                select = select, select_value = select_value)
  message("New Values:\n",
          paste0(capture.output(after), collapse = "\n"))
  
  
  if(file_type == "txt"){
    #update .txt files
    message("updating parameters in txt files")
    update_txt()
  }
}

add_usms <- function(stns, yrs, sols, base_names){
  #creates new usm combinations based on an existing usm
  #updates weather station, soil profile, climate years
  
  file <- read_xml(usm_file)
  usm_nodes <- xml_find_all(file, ".//usm")
  last_node <- tail(usm_nodes, 1)[[1]]
  usm_parent <- xml_parent(last_node)
  
  combos <- expand.grid(
    name = base_names,
    station = stns,
    soil = sols,
    year = yrs,
    stringsAsFactors = FALSE
  )  |> mutate(across(everything(), ~ gsub(" ", "_", as.character(.))))
  
  combos$stn_initial <- sapply(combos$station, function(x) {
    matches <- unlist(regmatches(x, gregexpr("(?<=^|_)[A-Z]", x, perl = TRUE)))
    paste(matches, collapse = "")
  })
  
for(row in 1:nrow(combos)){
  base_node <- xml_find_all(file,
                            sprintf(".//usm[@nom = '%s']",
                                    combos[row,1]))
  
  new_node <- read_xml(as.character(base_node))
  
  soil_node <- xml_find_first(new_node, ".//nomsol")
  xml_text(soil_node) <- combos[row,3]
  
  sta_node <- xml_find_first(new_node, ".//fstation")
  xml_text(sta_node) <- sprintf("%s_sta.xml", combos[row,2])
  
  clim1 <- xml_find_first(new_node, ".//fclim1")
  xml_text(clim1) <- sprintf("%s.%s", toupper(combos[row,2]), combos[row,4])
  
  clim2 <- xml_find_first(new_node, ".//fclim2")
  xml_text(clim2) <- sprintf("%s.%s", toupper(combos[row,2]), combos[row,4])
  
  #set new usm name as: "base-name_YYYY_station-initial_soil-profile"
  xml_set_attr(new_node, "nom", sprintf("%s_%s_%s_%s",
                                        combos[row,1],
                                        combos[row,4],
                                        combos[row,5],
                                        combos[row,3]))
  
  
  xml_add_child(usm_parent, new_node, .where = "after")
}
  write_xml(file, usm_file)
}


