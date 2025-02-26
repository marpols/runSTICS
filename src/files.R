init_files <- function(workspace,
               file_type = c("xml","txt")){
  files <- list.files(path = file.path(stics_path, subdir), pattern = ".xml$", full.names = TRUE)
  usm_file <<- file.path(workspace, sprintf("usms.%s", file_type))
  sols <<- file.path(workspace, sprintf("sols.%s", file_type))
  plt <<- file.path(stics_path,"plant","potato_mountainGem_plt.xml")
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




usm_details <- get_param_xml(usm_file)
plt_params <- get_param_xml(plt)
plt_porg <- get_param_xml(plt_org)


set_param <- function(file,
                      file_type = c("xml","txt"),
                      params,
                      values,
                      select = NULL,
                      select_value = NULL,
                      overwrite = T){
  #update values
  get_param_xml(file, param = params, 
                select = select, select_value = select_value)
  set_param_xml(file, param = params, 
                values = values,
                select = select, select_value = select_value,
                overwrite = overwrite)
  get_param_xml(file, param = params,
                select = select, select_value = select_value)
}
