directories <- list.dirs("data/full_run", recursive = TRUE)

out_dirs_env <- as.vector(unlist(sapply(directories, function(dir){
  path_parts <- unlist(str_split(dir, "/"))
  if(length(path_parts) == 5){
    return(dir)
  }
  return(NULL)
})))

out_dirs_env <- out_dirs_env[grep("extra", out_dirs_env, invert = TRUE)]

lapply(out_dirs_env, function(dir){
  path_parts <- unlist(str_split(dir, "/"))
  new_dir_path <- file.path("data", "full_run_processed",  path_parts[3], path_parts[4], path_parts[5])
  if(!dir.exists(new_dir_path)){
    dir.create(new_dir_path, recursive = TRUE)
  }
  if(!file.exists(paste(new_dir_path, "env_stress_processed.rds", sep="/"))){
    print(dir)
    env_det <- as.numeric(str_extract(unlist(str_split(path_parts[3], pattern = "_")), "[:digit:]*"))
  
    file_names <- list.files(path = dir, pattern = "parameters", recursive = TRUE, include.dirs = TRUE)
    file_names_full <- as.vector(sapply(file_names, function(x){
      return(file.path(dir, x))
    }))
  file_names_full <- file_names_full[1]
  files <- process_tree_data(file_names_full)
  
  param_data <- readRDS(file_names_full)
  env_stress <- param_data$parameters$control$stress_regime
  env_stress <- data.frame(time = 0:105,
                           stress = (env_stress - (0:105)),
                           env_stress = env_stress, 
                           temp = 1)
  
  env_stress$env_mean = env_det[2]
  env_stress$env_sd = env_det[3]
  env_stress$env_rep = unique(files$env_rep)
  
  
  saveRDS(env_stress, file = paste(new_dir_path, "env_stress_processed.rds", sep="/"))
  }
})
