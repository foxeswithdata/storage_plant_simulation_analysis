rm(list=ls())

library(plyr)
library(tidyverse)
library(stringr)
library(plotrix) 
library(DescTools)

source("code/foxes_pallettes.R")
source("code/function_processing.R")
source("code/plotting.R")

## We want to remove the superfluous "dead species"

data_directories <- list.dirs("data/full_run")

out_dirs_runs <- as.vector(unlist(sapply(data_directories, function(dir){
  path_parts <- unlist(str_split(dir, "/"))
  if(length(path_parts) == 5){
    return(dir)
  }
  return(NULL)
})))

out_dirs_runs <- out_dirs_runs[grep("extra", out_dirs_runs, invert=TRUE)]

lapply(out_dirs_runs, function(test_dir){
  path_parts <- unlist(str_split(test_dir, "/"))
  new_dir_path <- file.path("data", "full_run_processed",  path_parts[3], path_parts[4], path_parts[5])
  if(!dir.exists(new_dir_path)){
    dir.create(new_dir_path, recursive = TRUE)
  }
  if(!file.exists(paste(new_dir_path, "tree_data_processed.rds", sep="/"))){
    print(test_dir)
    env_det <- as.numeric(str_extract(unlist(str_split(path_parts[3], pattern = "_")), "[:digit:]*"))
    
    file_names <- list.files(path = test_dir, pattern = "tree_data", recursive = TRUE, include.dirs = TRUE)
    file_names_full <- as.vector(sapply(file_names, function(x){
      return(file.path(test_dir, x))
    }))
    
    files <- plyr::rbind.fill(lapply(file_names_full, process_tree_data))
    all_data <- plyr::rbind.fill(lapply(1:nrow(files), function(f_ind){
      test_file_data <- readRDS(files$file[f_ind])
      test_file_data <- subset(test_file_data, diameter_stem  >= 0.05)
      return(test_file_data)
    }))
    
    all_data_death <- all_data %>% 
      group_by(species_id, tree_id) %>%
      subset(is_alive == FALSE) %>%
      dplyr::summarise(time = min(time))
    
    all_data_death_det <- left_join(all_data_death, all_data, by = c("species_id", "tree_id", "time"))
    all_data_live <- subset(all_data, is_alive == TRUE)
    all_data_full <- rbind.fill(all_data_live, all_data_death_det)
    all_data_full$env_mean = env_det[2]
    all_data_full$env_sd = env_det[3]
    all_data_full$env_rep = unique(files$env_rep)
    all_data_full$run_rep = unique(files$run_rep)
    
    saveRDS(all_data_full, file = paste(new_dir_path, "tree_data_processed.rds", sep="/"))
  }
})



