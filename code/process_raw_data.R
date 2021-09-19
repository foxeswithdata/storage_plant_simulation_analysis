library(tidyverse)
library(plyr)
library(stringr)
library(plotrix) 
library(DescTools)

source("code/foxes_pallettes.R")
source("code/function_processing.R")
source("code/plotting.R")

## We want to remove the superfluous "dead species"

file_names <- list.files(path = "data/full_run/", pattern = "tree_data", recursive = TRUE, include.dirs = TRUE)
file_names_full <- as.vector(sapply(file_names, function(x){
  return(file.path("data", "full_run", x))
}))

file_data_full <- sapply(file_names_full, function(x){
  if(file.info(x)$ctime == file.info(x)$mtime){
    print(x)
    data_trees <- readRDS(file = x)
    data_trees <- get_trees_live(data_trees)
    
    saveRDS(data_trees, file=x)
    return(x)  
  }
})


data_directories <- list.dirs("data/full_run")

out_dirs_runs <- as.vector(unlist(sapply(data_directories, function(dir){
  path_parts <- unlist(str_split(dir, "/"))
  if(length(path_parts) < 5){
    return(NULL)
  }
  return(dir)
})))

out_dirs_env <- as.vector(unlist(sapply(data_directories, function(dir){
  path_parts <- unlist(str_split(dir, "/"))
  if(length(path_parts) == 4){
    return(dir)
  }
  return(NULL)
})))


out_dirs_env_test <- out_dirs_env[6] 
out_dirs_env_total <- as.vector(unlist(sapply(data_directories, function(dir){
  path_parts <- unlist(str_split(dir, "/"))
  if(length(path_parts) == 3){
    return(dir)
  }
  return(NULL)
})))

out_env_run <- lapply(out_dirs_env, function(dir){
  path_parts <- unlist(str_split(dir, "/"))
  new_dir_path <- file.path("out", "full_run_auto", "environment_run_agg", path_parts[3], path_parts[4])
  
  print(new_dir_path)
  
  dir.create(new_dir_path, recursive = TRUE)
  
  file_names <- list.files(path = dir, pattern = "tree_data", recursive = TRUE, include.dirs = TRUE)
  file_names_full <- as.vector(sapply(file_names, function(x){
    return(file.path(dir, x))
  }))
  
  print(file_names_full)
  
  files <- plyr::rbind.fill(lapply(file_names_full, process_tree_data))
  
  print(files)
  
  all_data <- plyr::rbind.fill(lapply(unique(files$from), function(i){
    indx <- which(files$from == i)
    all_tree_data <- plyr::rbind.fill(lapply(indx, function(j){
      file_name = files$file[j]
      data = readRDS(file_name)
      data = get_trees_live(data)
      data = subset(data, diameter_stem >= 0.05)
      if(nrow(data) > 0){
        data$run_rep = files$run_rep[j]
        data$env_rep = files$env_rep[j]
      }
      return(data)
    }))
    
    if(nrow(all_tree_data) > 0){
      ## TREE HEIGHT
      {
      tree_height_max <- all_tree_data %>% group_by(species_id, run_rep, time) %>%
        dplyr::summarise(max_height = max(height, na.rm = TRUE)) %>%
        group_by(species_id) %>%
        dplyr::summarise(min_max_height = min(max_height, na.rm = TRUE),
                         mean_max_height = mean(max_height, na.rm = TRUE),
                         median_max_height = median(max_height, na.rm = TRUE),
                         quantile_005_max_height = quantile(max_height, probs = 0.05, na.rm = TRUE),
                         quantile_025_max_height = quantile(max_height, probs = 0.25, na.rm = TRUE),
                         quantile_075_max_height = quantile(max_height, probs = 0.75, na.rm = TRUE),
                         quantile_095_max_height = quantile(max_height, probs = 0.95, na.rm = TRUE),
                         max_max_height = max(max_height, na.rm = TRUE),
                         sd_max_height = sd(max_height, na.rm=TRUE),
                         se_max_height = std.error(max_height, na.rm=TRUE),
                         ci_lwr_max_height = ci_lower(max_height),
                         ci_upr_max_height = ci_upper(max_height))
      
      tree_height_ave <- all_tree_data %>% 
        group_by(species_id) %>%
        dplyr::summarise(min_height = min(height, na.rm = TRUE),
                         mean_height = mean(height, na.rm = TRUE),
                         median_height = median(height, na.rm = TRUE),
                         quantile_005_height = quantile(height, probs = 0.05, na.rm = TRUE),
                         quantile_025_height = quantile(height, probs = 0.25, na.rm = TRUE),
                         quantile_075_height = quantile(height, probs = 0.75, na.rm = TRUE),
                         quantile_095_height = quantile(height, probs = 0.95, na.rm = TRUE),
                         max_height = max(height, na.rm = TRUE),
                         sd_height = sd(height, na.rm=TRUE),
                         se_height = std.error(height, na.rm=TRUE),
                         ci_lwr_height = ci_lower(height),
                         ci_upr_height = ci_upper(height))
    }
      ## BASAL AREA
      {
      tree_sum_basal <- all_tree_data %>% group_by(species_id, run_rep, time) %>%
        dplyr::summarise(sum_basal = max(area_stem, na.rm = TRUE)) %>%
        group_by(species_id) %>%
        dplyr::summarise(min_sum_basal = min(sum_basal, na.rm = TRUE),
                         mean_sum_basal = mean(sum_basal, na.rm = TRUE),
                         median_sum_basal = median(sum_basal, na.rm = TRUE),
                         quantile_005_sum_basal = quantile(sum_basal, probs = 0.05, na.rm = TRUE),
                         quantile_025_sum_basal = quantile(sum_basal, probs = 0.25, na.rm = TRUE),
                         quantile_075_sum_basal = quantile(sum_basal, probs = 0.75, na.rm = TRUE),
                         quantile_095_sum_basal = quantile(sum_basal, probs = 0.95, na.rm = TRUE),
                         max_sum_basal = max(sum_basal, na.rm = TRUE),
                         sd_sum_basal = sd(sum_basal, na.rm=TRUE),
                         se_sum_basal = std.error(sum_basal, na.rm=TRUE),
                         ci_lwr_sum_basal = ci_lower(sum_basal),
                         ci_upr_sum_basal = ci_upper(sum_basal))
      
      tree_sum_basal_ha <- all_tree_data %>% group_by(species_id, run_rep, time) %>%
        dplyr::summarise(sum_basal = max(area_stem*100, na.rm = TRUE)) %>%
        group_by(species_id) %>%
        dplyr::summarise(min_sum_basal_ha = min(sum_basal, na.rm = TRUE),
                         mean_sum_basal_ha = mean(sum_basal, na.rm = TRUE),
                         median_sum_basal_ha = median(sum_basal, na.rm = TRUE),
                         quantile_005_sum_basal_ha = quantile(sum_basal, probs = 0.05, na.rm = TRUE),
                         quantile_025_sum_basal_ha = quantile(sum_basal, probs = 0.25, na.rm = TRUE),
                         quantile_075_sum_basal_ha = quantile(sum_basal, probs = 0.75, na.rm = TRUE),
                         quantile_095_sum_basal_ha = quantile(sum_basal, probs = 0.95, na.rm = TRUE),
                         max_sum_basal_ha = max(sum_basal, na.rm = TRUE),
                         sd_sum_basal_ha = sd(sum_basal, na.rm=TRUE),
                         se_sum_basal_ha = std.error(sum_basal, na.rm=TRUE),
                         ci_lwr_sum_basal_ha = ci_lower(sum_basal),
                         ci_upr_sum_basal_ha = ci_upper(sum_basal))
      
      tree_basal_ave <- all_tree_data %>% 
        group_by(species_id) %>%
        dplyr::summarise(min_basal = min(area_stem, na.rm = TRUE),
                         mean_basal = mean(area_stem, na.rm = TRUE),
                         median_basal = median(area_stem, na.rm = TRUE),
                         quantile_005_basal = quantile(area_stem, probs = 0.05, na.rm = TRUE),
                         quantile_025_basal = quantile(area_stem, probs = 0.25, na.rm = TRUE),
                         quantile_075_basal = quantile(area_stem, probs = 0.75, na.rm = TRUE),
                         quantile_095_basal = quantile(area_stem, probs = 0.95, na.rm = TRUE),
                         max_basal = max(area_stem, na.rm = TRUE),
                         sd_basal = sd(area_stem, na.rm=TRUE),
                         se_basal = std.error(area_stem, na.rm=TRUE),
                         ci_lwr_basal = ci_lower(area_stem),
                         ci_upr_basal = ci_upper(area_stem))
    }
      ## LEAF AREA
      {
      tree_sum_leaf_area <- all_tree_data %>% group_by(species_id, run_rep, time) %>%
        dplyr::summarise(sum_leaf_area = sum(area_leaf, na.rm = TRUE)) %>%
        group_by(species_id) %>%
        dplyr::summarise(min_sum_leaf_area = min(sum_leaf_area, na.rm = TRUE),
                         mean_sum_leaf_area = mean(sum_leaf_area, na.rm = TRUE),
                         median_sum_leaf_area = median(sum_leaf_area, na.rm = TRUE),
                         quantile_005_sum_leaf_area = quantile(sum_leaf_area, probs = 0.05, na.rm = TRUE),
                         quantile_025_sum_leaf_area = quantile(sum_leaf_area, probs = 0.25, na.rm = TRUE),
                         quantile_075_sum_leaf_area = quantile(sum_leaf_area, probs = 0.75, na.rm = TRUE),
                         quantile_095_sum_leaf_area = quantile(sum_leaf_area, probs = 0.95, na.rm = TRUE),
                         max_sum_leaf_area = max(sum_leaf_area, na.rm = TRUE),
                         sd_sum_leaf_area = sd(sum_leaf_area, na.rm=TRUE),
                         se_sum_leaf_area = std.error(sum_leaf_area, na.rm=TRUE),
                         ci_lwr_sum_leaf_area = ci_lower(sum_leaf_area),
                         ci_upr_sum_leaf_area = ci_upper(sum_leaf_area))
      
      tree_leaf_area_ave <- all_tree_data %>% 
        group_by(species_id) %>%
        dplyr::summarise(min_leaf_area = min(area_leaf, na.rm = TRUE),
                         mean_leaf_area = mean(area_leaf, na.rm = TRUE),
                         median_leaf_area = median(area_leaf, na.rm = TRUE),
                         quantile_005_leaf_area = quantile(area_leaf, probs = 0.05, na.rm = TRUE),
                         quantile_025_leaf_area = quantile(area_leaf, probs = 0.25, na.rm = TRUE),
                         quantile_075_leaf_area = quantile(area_leaf, probs = 0.75, na.rm = TRUE),
                         quantile_095_leaf_area = quantile(area_leaf, probs = 0.95, na.rm = TRUE),
                         max_leaf_area = max(area_leaf, na.rm = TRUE),
                         sd_leaf_area = sd(area_leaf, na.rm=TRUE),
                         se_leaf_area = std.error(area_leaf, na.rm=TRUE),
                         ci_lwr_leaf_area = ci_lower(area_leaf),
                         ci_upr_leaf_area = ci_upper(area_leaf))
    }
      ### LEAF AREA INDEX
      {
      tree_sum_lai <- all_tree_data %>% group_by(species_id, run_rep, time) %>%
        dplyr::summarise(sum_leaf_area = sum(area_leaf/100, na.rm = TRUE)) %>%
        group_by(species_id) %>%
        dplyr::summarise(min_sum_lai = min(sum_leaf_area, na.rm = TRUE),
                         mean_sum_lai = mean(sum_leaf_area, na.rm = TRUE),
                         median_sum_lai = median(sum_leaf_area, na.rm = TRUE),
                         quantile_005_sum_lai = quantile(sum_leaf_area, probs = 0.05, na.rm = TRUE),
                         quantile_025_sum_lai = quantile(sum_leaf_area, probs = 0.25, na.rm = TRUE),
                         quantile_075_sum_lai = quantile(sum_leaf_area, probs = 0.75, na.rm = TRUE),
                         quantile_095_sum_lai = quantile(sum_leaf_area, probs = 0.95, na.rm = TRUE),
                         max_sum_lai = max(sum_leaf_area, na.rm = TRUE),
                         sd_sum_lai = sd(sum_leaf_area, na.rm=TRUE),
                         se_sum_lai = std.error(sum_leaf_area, na.rm=TRUE),
                         ci_lwr_sum_lai = ci_lower(sum_leaf_area),
                         ci_upr_sum_lai = ci_upper(sum_leaf_area))
        }
      ## MASS STORAGE
      {
      tree_sum_mass_storage <- all_tree_data %>% group_by(species_id, run_rep, time) %>%
        dplyr::summarise(sum_mass_storage = sum(mass_storage, na.rm = TRUE)) %>%
        group_by(species_id) %>%
        dplyr::summarise(min_sum_mass_storage = min(sum_mass_storage, na.rm = TRUE),
                         mean_sum_mass_storage = mean(sum_mass_storage, na.rm = TRUE),
                         median_sum_mass_storage = median(sum_mass_storage, na.rm = TRUE),
                         quantile_005_sum_mass_storage = quantile(sum_mass_storage, probs = 0.05, na.rm = TRUE),
                         quantile_025_sum_mass_storage = quantile(sum_mass_storage, probs = 0.25, na.rm = TRUE),
                         quantile_075_sum_mass_storage = quantile(sum_mass_storage, probs = 0.75, na.rm = TRUE),
                         quantile_095_sum_mass_storage = quantile(sum_mass_storage, probs = 0.95, na.rm = TRUE),
                         max_sum_mass_storage = max(sum_mass_storage, na.rm = TRUE),
                         sd_sum_mass_storage = sd(sum_mass_storage, na.rm=TRUE),
                         se_sum_mass_storage = std.error(sum_mass_storage, na.rm=TRUE),
                         ci_lwr_sum_mass_storage = ci_lower(sum_mass_storage),
                         ci_upr_sum_mass_storage = ci_upper(sum_mass_storage))
      
      tree_mass_storage_m2 <- all_tree_data %>% group_by(species_id, run_rep, time) %>%
        dplyr::summarise(sum_mass_storage = sum(mass_storage/100, na.rm = TRUE)) %>%
        group_by(species_id) %>%
        dplyr::summarise(min_sum_mass_storage_m2 = min(sum_mass_storage, na.rm = TRUE),
                         mean_sum_mass_storage_m2 = mean(sum_mass_storage, na.rm = TRUE),
                         median_sum_mass_storage_m2 = median(sum_mass_storage, na.rm = TRUE),
                         quantile_005_sum_mass_storage_m2 = quantile(sum_mass_storage, probs = 0.05, na.rm = TRUE),
                         quantile_025_sum_mass_storage_m2 = quantile(sum_mass_storage, probs = 0.25, na.rm = TRUE),
                         quantile_075_sum_mass_storage_m2 = quantile(sum_mass_storage, probs = 0.75, na.rm = TRUE),
                         quantile_095_sum_mass_storage_m2 = quantile(sum_mass_storage, probs = 0.95, na.rm = TRUE),
                         max_sum_mass_storage_m2 = max(sum_mass_storage, na.rm = TRUE),
                         sd_sum_mass_storage_m2 = sd(sum_mass_storage, na.rm=TRUE),
                         se_sum_mass_storage_m2 = std.error(sum_mass_storage, na.rm=TRUE),
                         ci_lwr_sum_mass_storage_m2 = ci_lower(sum_mass_storage),
                         ci_upr_sum_mass_storage_m2 = ci_upper(sum_mass_storage))
      
      tree_mass_storage_ave <- all_tree_data %>% 
        group_by(species_id) %>%
        dplyr::summarise(min_mass_storage = min(mass_storage, na.rm = TRUE),
                         mean_mass_storage = mean(mass_storage, na.rm = TRUE),
                         median_mass_storage = median(mass_storage, na.rm = TRUE),
                         quantile_005_mass_storage = quantile(mass_storage, probs = 0.05, na.rm = TRUE),
                         quantile_025_mass_storage = quantile(mass_storage, probs = 0.25, na.rm = TRUE),
                         quantile_075_mass_storage = quantile(mass_storage, probs = 0.75, na.rm = TRUE),
                         quantile_095_mass_storage = quantile(mass_storage, probs = 0.95, na.rm = TRUE),
                         max_mass_storage = max(mass_storage, na.rm = TRUE),
                         sd_mass_storage = sd(mass_storage, na.rm=TRUE),
                         se_mass_storage = std.error(mass_storage, na.rm=TRUE),
                         ci_lwr_mass_storage = ci_lower(mass_storage),
                         ci_upr_mass_storage = ci_upper(mass_storage))
    }
      ## MASS LEAF
      {
      tree_sum_mass_leaf <- all_tree_data %>% group_by(species_id, run_rep, time) %>%
        dplyr::summarise(sum_mass_leaf = sum(mass_leaf, na.rm = TRUE)) %>%
        group_by(species_id) %>%
        dplyr::summarise(min_sum_mass_leaf = min(sum_mass_leaf, na.rm = TRUE),
                         mean_sum_mass_leaf = mean(sum_mass_leaf, na.rm = TRUE),
                         median_sum_mass_leaf = median(sum_mass_leaf, na.rm = TRUE),
                         quantile_005_sum_mass_leaf = quantile(sum_mass_leaf, probs = 0.05, na.rm = TRUE),
                         quantile_025_sum_mass_leaf = quantile(sum_mass_leaf, probs = 0.25, na.rm = TRUE),
                         quantile_075_sum_mass_leaf = quantile(sum_mass_leaf, probs = 0.75, na.rm = TRUE),
                         quantile_095_sum_mass_leaf = quantile(sum_mass_leaf, probs = 0.95, na.rm = TRUE),
                         max_sum_mass_leaf = max(sum_mass_leaf, na.rm = TRUE),
                         sd_sum_mass_leaf = sd(sum_mass_leaf, na.rm=TRUE),
                         se_sum_mass_leaf = std.error(sum_mass_leaf, na.rm=TRUE),
                         ci_lwr_sum_mass_leaf = ci_lower(sum_mass_leaf),
                         ci_upr_sum_mass_leaf = ci_upper(sum_mass_leaf))
      
      tree_mass_leaf_m2 <- all_tree_data %>% group_by(species_id, run_rep, time) %>%
        dplyr::summarise(sum_mass_leaf = sum(mass_leaf/100, na.rm = TRUE)) %>%
        group_by(species_id) %>%
        dplyr::summarise(min_sum_mass_leaf_m2 = min(sum_mass_leaf, na.rm = TRUE),
                         mean_sum_mass_leaf_m2 = mean(sum_mass_leaf, na.rm = TRUE),
                         median_sum_mass_leaf_m2 = median(sum_mass_leaf, na.rm = TRUE),
                         quantile_005_sum_mass_leaf_m2 = quantile(sum_mass_leaf, probs = 0.05, na.rm = TRUE),
                         quantile_025_sum_mass_leaf_m2 = quantile(sum_mass_leaf, probs = 0.25, na.rm = TRUE),
                         quantile_075_sum_mass_leaf_m2 = quantile(sum_mass_leaf, probs = 0.75, na.rm = TRUE),
                         quantile_095_sum_mass_leaf_m2 = quantile(sum_mass_leaf, probs = 0.95, na.rm = TRUE),
                         max_sum_mass_leaf_m2 = max(sum_mass_leaf, na.rm = TRUE),
                         sd_sum_mass_leaf_m2 = sd(sum_mass_leaf, na.rm=TRUE),
                         se_sum_mass_leaf_m2 = std.error(sum_mass_leaf, na.rm=TRUE),
                         ci_lwr_sum_mass_leaf_m2 = ci_lower(sum_mass_leaf),
                         ci_upr_sum_mass_leaf_m2 = ci_upper(sum_mass_leaf))
      
      tree_mass_leaf_ave <- all_tree_data %>% 
        group_by(species_id) %>%
        dplyr::summarise(min_mass_leaf = min(mass_leaf, na.rm = TRUE),
                         mean_mass_leaf = mean(mass_leaf, na.rm = TRUE),
                         median_mass_leaf = median(mass_leaf, na.rm = TRUE),
                         quantile_005_mass_leaf = quantile(mass_leaf, probs = 0.05, na.rm = TRUE),
                         quantile_025_mass_leaf = quantile(mass_leaf, probs = 0.25, na.rm = TRUE),
                         quantile_075_mass_leaf = quantile(mass_leaf, probs = 0.75, na.rm = TRUE),
                         quantile_095_mass_leaf = quantile(mass_leaf, probs = 0.95, na.rm = TRUE),
                         max_mass_leaf = max(mass_leaf, na.rm = TRUE),
                         sd_mass_leaf = sd(mass_leaf, na.rm=TRUE),
                         se_mass_leaf = std.error(mass_leaf, na.rm=TRUE),
                         ci_lwr_mass_leaf = ci_lower(mass_leaf),
                         ci_upr_mass_leaf = ci_upper(mass_leaf))
    }
      ## MASS SAPWOOD
      {
      tree_sum_mass_sapwood <- all_tree_data %>% group_by(species_id, run_rep, time) %>%
        dplyr::summarise(sum_mass_sapwood = sum(mass_sapwood, na.rm = TRUE)) %>%
        group_by(species_id) %>%
        dplyr::summarise(min_sum_mass_sapwood = min(sum_mass_sapwood, na.rm = TRUE),
                         mean_sum_mass_sapwood = mean(sum_mass_sapwood, na.rm = TRUE),
                         median_sum_mass_sapwood = median(sum_mass_sapwood, na.rm = TRUE),
                         quantile_005_sum_mass_sapwood = quantile(sum_mass_sapwood, probs = 0.05, na.rm = TRUE),
                         quantile_025_sum_mass_sapwood = quantile(sum_mass_sapwood, probs = 0.25, na.rm = TRUE),
                         quantile_075_sum_mass_sapwood = quantile(sum_mass_sapwood, probs = 0.75, na.rm = TRUE),
                         quantile_095_sum_mass_sapwood = quantile(sum_mass_sapwood, probs = 0.95, na.rm = TRUE),
                         max_sum_mass_sapwood = max(sum_mass_sapwood, na.rm = TRUE),
                         sd_sum_mass_sapwood = sd(sum_mass_sapwood, na.rm=TRUE),
                         se_sum_mass_sapwood = std.error(sum_mass_sapwood, na.rm=TRUE),
                         ci_lwr_sum_mass_sapwood = ci_lower(sum_mass_sapwood),
                         ci_upr_sum_mass_sapwood = ci_upper(sum_mass_sapwood))
      
      tree_mass_sapwood_m2 <- all_tree_data %>% group_by(species_id, run_rep, time) %>%
        dplyr::summarise(sum_mass_sapwood = sum(mass_sapwood/100, na.rm = TRUE)) %>%
        group_by(species_id) %>%
        dplyr::summarise(min_sum_mass_sapwood_m2 = min(sum_mass_sapwood, na.rm = TRUE),
                         mean_sum_mass_sapwood_m2 = mean(sum_mass_sapwood, na.rm = TRUE),
                         median_sum_mass_sapwood_m2 = median(sum_mass_sapwood, na.rm = TRUE),
                         quantile_005_sum_mass_sapwood_m2 = quantile(sum_mass_sapwood, probs = 0.05, na.rm = TRUE),
                         quantile_025_sum_mass_sapwood_m2 = quantile(sum_mass_sapwood, probs = 0.25, na.rm = TRUE),
                         quantile_075_sum_mass_sapwood_m2 = quantile(sum_mass_sapwood, probs = 0.75, na.rm = TRUE),
                         quantile_095_sum_mass_sapwood_m2 = quantile(sum_mass_sapwood, probs = 0.95, na.rm = TRUE),
                         max_sum_mass_sapwood_m2 = max(sum_mass_sapwood, na.rm = TRUE),
                         sd_sum_mass_sapwood_m2 = sd(sum_mass_sapwood, na.rm=TRUE),
                         se_sum_mass_sapwood_m2 = std.error(sum_mass_sapwood, na.rm=TRUE),
                         ci_lwr_sum_mass_sapwood_m2 = ci_lower(sum_mass_sapwood),
                         ci_upr_sum_mass_sapwood_m2 = ci_upper(sum_mass_sapwood))
      
      tree_mass_sapwood_ave <- all_tree_data %>% 
        group_by(species_id) %>%
        dplyr::summarise(min_mass_sapwood = min(mass_sapwood, na.rm = TRUE),
                         mean_mass_sapwood = mean(mass_sapwood, na.rm = TRUE),
                         median_mass_sapwood = median(mass_sapwood, na.rm = TRUE),
                         quantile_005_mass_sapwood = quantile(mass_sapwood, probs = 0.05, na.rm = TRUE),
                         quantile_025_mass_sapwood = quantile(mass_sapwood, probs = 0.25, na.rm = TRUE),
                         quantile_075_mass_sapwood = quantile(mass_sapwood, probs = 0.75, na.rm = TRUE),
                         quantile_095_mass_sapwood = quantile(mass_sapwood, probs = 0.95, na.rm = TRUE),
                         max_mass_sapwood = max(mass_sapwood, na.rm = TRUE),
                         sd_mass_sapwood = sd(mass_sapwood, na.rm=TRUE),
                         se_mass_sapwood = std.error(mass_sapwood, na.rm=TRUE),
                         ci_lwr_mass_sapwood = ci_lower(mass_sapwood),
                         ci_upr_mass_sapwood = ci_upper(mass_sapwood))
    }
      ## MASS BARK
      {
      tree_sum_mass_bark <- all_tree_data %>% group_by(species_id, run_rep, time) %>%
        dplyr::summarise(sum_mass_bark = sum(mass_bark, na.rm = TRUE)) %>%
        group_by(species_id) %>%
        dplyr::summarise(min_sum_mass_bark = min(sum_mass_bark, na.rm = TRUE),
                         mean_sum_mass_bark = mean(sum_mass_bark, na.rm = TRUE),
                         median_sum_mass_bark = median(sum_mass_bark, na.rm = TRUE),
                         quantile_005_sum_mass_bark = quantile(sum_mass_bark, probs = 0.05, na.rm = TRUE),
                         quantile_025_sum_mass_bark = quantile(sum_mass_bark, probs = 0.25, na.rm = TRUE),
                         quantile_075_sum_mass_bark = quantile(sum_mass_bark, probs = 0.75, na.rm = TRUE),
                         quantile_095_sum_mass_bark = quantile(sum_mass_bark, probs = 0.95, na.rm = TRUE),
                         max_sum_mass_bark = max(sum_mass_bark, na.rm = TRUE),
                         sd_sum_mass_bark = sd(sum_mass_bark, na.rm=TRUE),
                         se_sum_mass_bark = std.error(sum_mass_bark, na.rm=TRUE),
                         ci_lwr_sum_mass_bark = ci_lower(sum_mass_bark),
                         ci_upr_sum_mass_bark = ci_upper(sum_mass_bark))
      
      tree_mass_bark_m2 <- all_tree_data %>% group_by(species_id, run_rep, time) %>%
        dplyr::summarise(sum_mass_bark = sum(mass_bark/100, na.rm = TRUE)) %>%
        group_by(species_id) %>%
        dplyr::summarise(min_sum_mass_bark_m2 = min(sum_mass_bark, na.rm = TRUE),
                         mean_sum_mass_bark_m2 = mean(sum_mass_bark, na.rm = TRUE),
                         median_sum_mass_bark_m2 = median(sum_mass_bark, na.rm = TRUE),
                         quantile_005_sum_mass_bark_m2 = quantile(sum_mass_bark, probs = 0.05, na.rm = TRUE),
                         quantile_025_sum_mass_bark_m2 = quantile(sum_mass_bark, probs = 0.25, na.rm = TRUE),
                         quantile_075_sum_mass_bark_m2 = quantile(sum_mass_bark, probs = 0.75, na.rm = TRUE),
                         quantile_095_sum_mass_bark_m2 = quantile(sum_mass_bark, probs = 0.95, na.rm = TRUE),
                         max_sum_mass_bark_m2 = max(sum_mass_bark, na.rm = TRUE),
                         sd_sum_mass_bark_m2 = sd(sum_mass_bark, na.rm=TRUE),
                         se_sum_mass_bark_m2 = std.error(sum_mass_bark, na.rm=TRUE),
                         ci_lwr_sum_mass_bark_m2 = ci_lower(sum_mass_bark),
                         ci_upr_sum_mass_bark_m2 = ci_upper(sum_mass_bark))
      
      tree_mass_bark_ave <- all_tree_data %>% 
        group_by(species_id) %>%
        dplyr::summarise(min_mass_bark = min(mass_bark, na.rm = TRUE),
                         mean_mass_bark = mean(mass_bark, na.rm = TRUE),
                         median_mass_bark = median(mass_bark, na.rm = TRUE),
                         quantile_005_mass_bark = quantile(mass_bark, probs = 0.05, na.rm = TRUE),
                         quantile_025_mass_bark = quantile(mass_bark, probs = 0.25, na.rm = TRUE),
                         quantile_075_mass_bark = quantile(mass_bark, probs = 0.75, na.rm = TRUE),
                         quantile_095_mass_bark = quantile(mass_bark, probs = 0.95, na.rm = TRUE),
                         max_mass_bark = max(mass_bark, na.rm = TRUE),
                         sd_mass_bark = sd(mass_bark, na.rm=TRUE),
                         se_mass_bark = std.error(mass_bark, na.rm=TRUE),
                         ci_lwr_mass_bark = ci_lower(mass_bark),
                         ci_upr_mass_bark = ci_upper(mass_bark))
    }
      ## MASS ROOT
      {
      tree_sum_mass_root <- all_tree_data %>% group_by(species_id, run_rep, time) %>%
        dplyr::summarise(sum_mass_root = sum(mass_root, na.rm = TRUE)) %>%
        group_by(species_id) %>%
        dplyr::summarise(min_sum_mass_root = min(sum_mass_root, na.rm = TRUE),
                         mean_sum_mass_root = mean(sum_mass_root, na.rm = TRUE),
                         median_sum_mass_root = median(sum_mass_root, na.rm = TRUE),
                         quantile_005_sum_mass_root = quantile(sum_mass_root, probs = 0.05, na.rm = TRUE),
                         quantile_025_sum_mass_root = quantile(sum_mass_root, probs = 0.25, na.rm = TRUE),
                         quantile_075_sum_mass_root = quantile(sum_mass_root, probs = 0.75, na.rm = TRUE),
                         quantile_095_sum_mass_root = quantile(sum_mass_root, probs = 0.95, na.rm = TRUE),
                         max_sum_mass_root = max(sum_mass_root, na.rm = TRUE),
                         sd_sum_mass_root = sd(sum_mass_root, na.rm=TRUE),
                         se_sum_mass_root = std.error(sum_mass_root, na.rm=TRUE),
                         ci_lwr_sum_mass_root = ci_lower(sum_mass_root),
                         ci_upr_sum_mass_root = ci_upper(sum_mass_root))
      
      tree_mass_root_m2 <- all_tree_data %>% group_by(species_id, run_rep, time) %>%
        dplyr::summarise(sum_mass_root = sum(mass_root/100, na.rm = TRUE)) %>%
        group_by(species_id) %>%
        dplyr::summarise(min_sum_mass_root_m2 = min(sum_mass_root, na.rm = TRUE),
                         mean_sum_mass_root_m2 = mean(sum_mass_root, na.rm = TRUE),
                         median_sum_mass_root_m2 = median(sum_mass_root, na.rm = TRUE),
                         quantile_005_sum_mass_root_m2 = quantile(sum_mass_root, probs = 0.05, na.rm = TRUE),
                         quantile_025_sum_mass_root_m2 = quantile(sum_mass_root, probs = 0.25, na.rm = TRUE),
                         quantile_075_sum_mass_root_m2 = quantile(sum_mass_root, probs = 0.75, na.rm = TRUE),
                         quantile_095_sum_mass_root_m2 = quantile(sum_mass_root, probs = 0.95, na.rm = TRUE),
                         max_sum_mass_root_m2 = max(sum_mass_root, na.rm = TRUE),
                         sd_sum_mass_root_m2 = sd(sum_mass_root, na.rm=TRUE),
                         se_sum_mass_root_m2 = std.error(sum_mass_root, na.rm=TRUE),
                         ci_lwr_sum_mass_root_m2 = ci_lower(sum_mass_root),
                         ci_upr_sum_mass_root_m2 = ci_upper(sum_mass_root))
      
      tree_mass_root_ave <- all_tree_data %>% 
        group_by(species_id) %>%
        dplyr::summarise(min_mass_root = min(mass_root, na.rm = TRUE),
                         mean_mass_root = mean(mass_root, na.rm = TRUE),
                         median_mass_root = median(mass_root, na.rm = TRUE),
                         quantile_005_mass_root = quantile(mass_root, probs = 0.05, na.rm = TRUE),
                         quantile_025_mass_root = quantile(mass_root, probs = 0.25, na.rm = TRUE),
                         quantile_075_mass_root = quantile(mass_root, probs = 0.75, na.rm = TRUE),
                         quantile_095_mass_root = quantile(mass_root, probs = 0.95, na.rm = TRUE),
                         max_mass_root = max(mass_root, na.rm = TRUE),
                         sd_mass_root = sd(mass_root, na.rm=TRUE),
                         se_mass_root = std.error(mass_root, na.rm=TRUE),
                         ci_lwr_mass_root = ci_lower(mass_root),
                         ci_upr_mass_root = ci_upper(mass_root))
    }
      ## MASS HEARTWOOD
      {
      tree_sum_mass_heartwood <- all_tree_data %>% group_by(species_id, run_rep, time) %>%
        dplyr::summarise(sum_mass_heartwood = sum(mass_heartwood, na.rm = TRUE)) %>%
        group_by(species_id) %>%
        dplyr::summarise(min_sum_mass_heartwood = min(sum_mass_heartwood, na.rm = TRUE),
                         mean_sum_mass_heartwood = mean(sum_mass_heartwood, na.rm = TRUE),
                         median_sum_mass_heartwood = median(sum_mass_heartwood, na.rm = TRUE),
                         quantile_005_sum_mass_heartwood = quantile(sum_mass_heartwood, probs = 0.05, na.rm = TRUE),
                         quantile_025_sum_mass_heartwood = quantile(sum_mass_heartwood, probs = 0.25, na.rm = TRUE),
                         quantile_075_sum_mass_heartwood = quantile(sum_mass_heartwood, probs = 0.75, na.rm = TRUE),
                         quantile_095_sum_mass_heartwood = quantile(sum_mass_heartwood, probs = 0.95, na.rm = TRUE),
                         max_sum_mass_heartwood = max(sum_mass_heartwood, na.rm = TRUE),
                         sd_sum_mass_heartwood = sd(sum_mass_heartwood, na.rm=TRUE),
                         se_sum_mass_heartwood = std.error(sum_mass_heartwood, na.rm=TRUE),
                         ci_lwr_sum_mass_heartwood = ci_lower(sum_mass_heartwood),
                         ci_upr_sum_mass_heartwood = ci_upper(sum_mass_heartwood))
      
      tree_mass_heartwood_m2 <- all_tree_data %>% group_by(species_id, run_rep, time) %>%
        dplyr::summarise(sum_mass_heartwood = sum(mass_heartwood/100, na.rm = TRUE)) %>%
        group_by(species_id) %>%
        dplyr::summarise(min_sum_mass_heartwood_m2 = min(sum_mass_heartwood, na.rm = TRUE),
                         mean_sum_mass_heartwood_m2 = mean(sum_mass_heartwood, na.rm = TRUE),
                         median_sum_mass_heartwood_m2 = median(sum_mass_heartwood, na.rm = TRUE),
                         quantile_005_sum_mass_heartwood_m2 = quantile(sum_mass_heartwood, probs = 0.05, na.rm = TRUE),
                         quantile_025_sum_mass_heartwood_m2 = quantile(sum_mass_heartwood, probs = 0.25, na.rm = TRUE),
                         quantile_075_sum_mass_heartwood_m2 = quantile(sum_mass_heartwood, probs = 0.75, na.rm = TRUE),
                         quantile_095_sum_mass_heartwood_m2 = quantile(sum_mass_heartwood, probs = 0.95, na.rm = TRUE),
                         max_sum_mass_heartwood_m2 = max(sum_mass_heartwood, na.rm = TRUE),
                         sd_sum_mass_heartwood_m2 = sd(sum_mass_heartwood, na.rm=TRUE),
                         se_sum_mass_heartwood_m2 = std.error(sum_mass_heartwood, na.rm=TRUE),
                         ci_lwr_sum_mass_heartwood_m2 = ci_lower(sum_mass_heartwood),
                         ci_upr_sum_mass_heartwood_m2 = ci_upper(sum_mass_heartwood))
      
      tree_mass_heartwood_ave <- all_tree_data %>% 
        group_by(species_id) %>%
        dplyr::summarise(min_mass_heartwood = min(mass_heartwood, na.rm = TRUE),
                         mean_mass_heartwood = mean(mass_heartwood, na.rm = TRUE),
                         median_mass_heartwood = median(mass_heartwood, na.rm = TRUE),
                         quantile_005_mass_heartwood = quantile(mass_heartwood, probs = 0.05, na.rm = TRUE),
                         quantile_025_mass_heartwood = quantile(mass_heartwood, probs = 0.25, na.rm = TRUE),
                         quantile_075_mass_heartwood = quantile(mass_heartwood, probs = 0.75, na.rm = TRUE),
                         quantile_095_mass_heartwood = quantile(mass_heartwood, probs = 0.95, na.rm = TRUE),
                         max_mass_heartwood = max(mass_heartwood, na.rm = TRUE),
                         sd_mass_heartwood = sd(mass_heartwood, na.rm=TRUE),
                         se_mass_heartwood = std.error(mass_heartwood, na.rm=TRUE),
                         ci_lwr_mass_heartwood = ci_lower(mass_heartwood),
                         ci_upr_mass_heartwood = ci_upper(mass_heartwood))
        }
      ## MASS LIVE
      {
      tree_sum_mass_live <- all_tree_data %>% group_by(species_id, run_rep, time) %>%
        dplyr::summarise(sum_mass_live = sum((mass_leaf + mass_bark + mass_root + mass_sapwood), na.rm = TRUE)) %>%
        group_by(species_id) %>%
        dplyr::summarise(min_sum_mass_live = min(sum_mass_live, na.rm = TRUE),
                         mean_sum_mass_live = mean(sum_mass_live, na.rm = TRUE),
                         median_sum_mass_live = median(sum_mass_live, na.rm = TRUE),
                         quantile_005_sum_mass_live = quantile(sum_mass_live, probs = 0.05, na.rm = TRUE),
                         quantile_025_sum_mass_live = quantile(sum_mass_live, probs = 0.25, na.rm = TRUE),
                         quantile_075_sum_mass_live = quantile(sum_mass_live, probs = 0.75, na.rm = TRUE),
                         quantile_095_sum_mass_live = quantile(sum_mass_live, probs = 0.95, na.rm = TRUE),
                         max_sum_mass_live = max(sum_mass_live, na.rm = TRUE),
                         sd_sum_mass_live = sd(sum_mass_live, na.rm=TRUE),
                         se_sum_mass_live = std.error(sum_mass_live, na.rm=TRUE),
                         ci_lwr_sum_mass_live = ci_lower(sum_mass_live),
                         ci_upr_sum_mass_live = ci_upper(sum_mass_live))
      
      tree_mass_live_m2 <- all_tree_data %>% group_by(species_id, run_rep, time) %>%
        dplyr::summarise(sum_mass_live = sum((mass_leaf + mass_bark + mass_root + mass_sapwood)/100, na.rm = TRUE)) %>%
        group_by(species_id) %>%
        dplyr::summarise(min_sum_mass_live_m2 = min(sum_mass_live, na.rm = TRUE),
                         mean_sum_mass_live_m2 = mean(sum_mass_live, na.rm = TRUE),
                         median_sum_mass_live_m2 = median(sum_mass_live, na.rm = TRUE),
                         quantile_005_sum_mass_live_m2 = quantile(sum_mass_live, probs = 0.05, na.rm = TRUE),
                         quantile_025_sum_mass_live_m2 = quantile(sum_mass_live, probs = 0.25, na.rm = TRUE),
                         quantile_075_sum_mass_live_m2 = quantile(sum_mass_live, probs = 0.75, na.rm = TRUE),
                         quantile_095_sum_mass_live_m2 = quantile(sum_mass_live, probs = 0.95, na.rm = TRUE),
                         max_sum_mass_live_m2 = max(sum_mass_live, na.rm = TRUE),
                         sd_sum_mass_live_m2 = sd(sum_mass_live, na.rm=TRUE),
                         se_sum_mass_live_m2 = std.error(sum_mass_live, na.rm=TRUE),
                         ci_lwr_sum_mass_live_m2 = ci_lower(sum_mass_live),
                         ci_upr_sum_mass_live_m2 = ci_upper(sum_mass_live))
      
      tree_mass_live_ave <- all_tree_data %>% 
        group_by(species_id) %>%
        dplyr::summarise(min_mass_live = min((mass_leaf + mass_bark + mass_root + mass_sapwood), na.rm = TRUE),
                         mean_mass_live = mean((mass_leaf + mass_bark + mass_root + mass_sapwood), na.rm = TRUE),
                         median_mass_live = median((mass_leaf + mass_bark + mass_root + mass_sapwood), na.rm = TRUE),
                         quantile_005_mass_live = quantile((mass_leaf + mass_bark + mass_root + mass_sapwood), probs = 0.05, na.rm = TRUE),
                         quantile_025_mass_live = quantile((mass_leaf + mass_bark + mass_root + mass_sapwood), probs = 0.25, na.rm = TRUE),
                         quantile_075_mass_live = quantile((mass_leaf + mass_bark + mass_root + mass_sapwood), probs = 0.75, na.rm = TRUE),
                         quantile_095_mass_live = quantile((mass_leaf + mass_bark + mass_root + mass_sapwood), probs = 0.95, na.rm = TRUE),
                         max_mass_live = max((mass_leaf + mass_bark + mass_root + mass_sapwood), na.rm = TRUE),
                         sd_mass_live = sd((mass_leaf + mass_bark + mass_root + mass_sapwood), na.rm=TRUE),
                         se_mass_live = std.error((mass_leaf + mass_bark + mass_root + mass_sapwood), na.rm=TRUE),
                         ci_lwr_mass_live = ci_lower((mass_leaf + mass_bark + mass_root + mass_sapwood)),
                         ci_upr_mass_live = ci_upper((mass_leaf + mass_bark + mass_root + mass_sapwood)))
        }
      ## MASS Aboveground
      {
      tree_sum_mass_aboveground <- all_tree_data %>% group_by(species_id, run_rep, time) %>%
        dplyr::summarise(sum_mass_aboveground = sum((mass_leaf + mass_bark + mass_heartwood + mass_sapwood), na.rm = TRUE)) %>%
        group_by(species_id) %>%
        dplyr::summarise(min_sum_mass_aboveground = min(sum_mass_aboveground, na.rm = TRUE),
                         mean_sum_mass_aboveground = mean(sum_mass_aboveground, na.rm = TRUE),
                         median_sum_mass_aboveground = median(sum_mass_aboveground, na.rm = TRUE),
                         quantile_005_sum_mass_aboveground = quantile(sum_mass_aboveground, probs = 0.05, na.rm = TRUE),
                         quantile_025_sum_mass_aboveground = quantile(sum_mass_aboveground, probs = 0.25, na.rm = TRUE),
                         quantile_075_sum_mass_aboveground = quantile(sum_mass_aboveground, probs = 0.75, na.rm = TRUE),
                         quantile_095_sum_mass_aboveground = quantile(sum_mass_aboveground, probs = 0.95, na.rm = TRUE),
                         max_sum_mass_aboveground = max(sum_mass_aboveground, na.rm = TRUE),
                         sd_sum_mass_aboveground = sd(sum_mass_aboveground, na.rm=TRUE),
                         se_sum_mass_aboveground = std.error(sum_mass_aboveground, na.rm=TRUE),
                         ci_lwr_sum_mass_aboveground = ci_lower(sum_mass_aboveground),
                         ci_upr_sum_mass_aboveground = ci_upper(sum_mass_aboveground))
      
      tree_mass_aboveground_m2 <- all_tree_data %>% group_by(species_id, run_rep, time) %>%
        dplyr::summarise(sum_mass_aboveground = sum((mass_leaf + mass_bark + mass_heartwood + mass_sapwood)/100, na.rm = TRUE)) %>%
        group_by(species_id) %>%
        dplyr::summarise(min_sum_mass_aboveground_m2 = min(sum_mass_aboveground, na.rm = TRUE),
                         mean_sum_mass_aboveground_m2 = mean(sum_mass_aboveground, na.rm = TRUE),
                         median_sum_mass_aboveground_m2 = median(sum_mass_aboveground, na.rm = TRUE),
                         quantile_005_sum_mass_aboveground_m2 = quantile(sum_mass_aboveground, probs = 0.05, na.rm = TRUE),
                         quantile_025_sum_mass_aboveground_m2 = quantile(sum_mass_aboveground, probs = 0.25, na.rm = TRUE),
                         quantile_075_sum_mass_aboveground_m2 = quantile(sum_mass_aboveground, probs = 0.75, na.rm = TRUE),
                         quantile_095_sum_mass_aboveground_m2 = quantile(sum_mass_aboveground, probs = 0.95, na.rm = TRUE),
                         max_sum_mass_aboveground_m2 = max(sum_mass_aboveground, na.rm = TRUE),
                         sd_sum_mass_aboveground_m2 = sd(sum_mass_aboveground, na.rm=TRUE),
                         se_sum_mass_aboveground_m2 = std.error(sum_mass_aboveground, na.rm=TRUE),
                         ci_lwr_sum_mass_aboveground_m2 = ci_lower(sum_mass_aboveground),
                         ci_upr_sum_mass_aboveground_m2 = ci_upper(sum_mass_aboveground))
      
      tree_mass_aboveground_ave <- all_tree_data %>% 
        group_by(species_id) %>%
        dplyr::summarise(min_mass_aboveground = min((mass_leaf + mass_bark + mass_heartwood + mass_sapwood), na.rm = TRUE),
                         mean_mass_aboveground = mean((mass_leaf + mass_bark + mass_heartwood + mass_sapwood), na.rm = TRUE),
                         median_mass_aboveground = median((mass_leaf + mass_bark + mass_heartwood + mass_sapwood), na.rm = TRUE),
                         quantile_005_mass_aboveground = quantile((mass_leaf + mass_bark + mass_heartwood + mass_sapwood), probs = 0.05, na.rm = TRUE),
                         quantile_025_mass_aboveground = quantile((mass_leaf + mass_bark + mass_heartwood + mass_sapwood), probs = 0.25, na.rm = TRUE),
                         quantile_075_mass_aboveground = quantile((mass_leaf + mass_bark + mass_heartwood + mass_sapwood), probs = 0.75, na.rm = TRUE),
                         quantile_095_mass_aboveground = quantile((mass_leaf + mass_bark + mass_heartwood + mass_sapwood), probs = 0.95, na.rm = TRUE),
                         max_mass_aboveground = max((mass_leaf + mass_bark + mass_heartwood + mass_sapwood), na.rm = TRUE),
                         sd_mass_aboveground = sd((mass_leaf + mass_bark + mass_heartwood + mass_sapwood), na.rm=TRUE),
                         se_mass_aboveground = std.error((mass_leaf + mass_bark + mass_heartwood + mass_sapwood), na.rm=TRUE),
                         ci_lwr_mass_aboveground = ci_lower((mass_leaf + mass_bark + mass_heartwood + mass_sapwood)),
                         ci_upr_mass_aboveground = ci_upper((mass_leaf + mass_bark + mass_heartwood + mass_sapwood)))
        }
      ## MASS STEM
      {
      tree_sum_mass_stem <- all_tree_data %>% group_by(species_id, run_rep, time) %>%
        dplyr::summarise(sum_mass_stem = sum((mass_bark + mass_heartwood + mass_sapwood), na.rm = TRUE)) %>%
        group_by(species_id) %>%
        dplyr::summarise(min_sum_mass_stem = min(sum_mass_stem, na.rm = TRUE),
                         mean_sum_mass_stem = mean(sum_mass_stem, na.rm = TRUE),
                         median_sum_mass_stem = median(sum_mass_stem, na.rm = TRUE),
                         quantile_005_sum_mass_stem = quantile(sum_mass_stem, probs = 0.05, na.rm = TRUE),
                         quantile_025_sum_mass_stem = quantile(sum_mass_stem, probs = 0.25, na.rm = TRUE),
                         quantile_075_sum_mass_stem = quantile(sum_mass_stem, probs = 0.75, na.rm = TRUE),
                         quantile_095_sum_mass_stem = quantile(sum_mass_stem, probs = 0.95, na.rm = TRUE),
                         max_sum_mass_stem = max(sum_mass_stem, na.rm = TRUE),
                         sd_sum_mass_stem = sd(sum_mass_stem, na.rm=TRUE),
                         se_sum_mass_stem = std.error(sum_mass_stem, na.rm=TRUE),
                         ci_lwr_sum_mass_stem = ci_lower(sum_mass_stem),
                         ci_upr_sum_mass_stem = ci_upper(sum_mass_stem))
      
      tree_mass_stem_m2 <- all_tree_data %>% group_by(species_id, run_rep, time) %>%
        dplyr::summarise(sum_mass_stem = sum((mass_bark + mass_heartwood + mass_sapwood)/100, na.rm = TRUE)) %>%
        group_by(species_id) %>%
        dplyr::summarise(min_sum_mass_stem_m2 = min(sum_mass_stem, na.rm = TRUE),
                         mean_sum_mass_stem_m2 = mean(sum_mass_stem, na.rm = TRUE),
                         median_sum_mass_stem_m2 = median(sum_mass_stem, na.rm = TRUE),
                         quantile_005_sum_mass_stem_m2 = quantile(sum_mass_stem, probs = 0.05, na.rm = TRUE),
                         quantile_025_sum_mass_stem_m2 = quantile(sum_mass_stem, probs = 0.25, na.rm = TRUE),
                         quantile_075_sum_mass_stem_m2 = quantile(sum_mass_stem, probs = 0.75, na.rm = TRUE),
                         quantile_095_sum_mass_stem_m2 = quantile(sum_mass_stem, probs = 0.95, na.rm = TRUE),
                         max_sum_mass_stem_m2 = max(sum_mass_stem, na.rm = TRUE),
                         sd_sum_mass_stem_m2 = sd(sum_mass_stem, na.rm=TRUE),
                         se_sum_mass_stem_m2 = std.error(sum_mass_stem, na.rm=TRUE),
                         ci_lwr_sum_mass_stem_m2 = ci_lower(sum_mass_stem),
                         ci_upr_sum_mass_stem_m2 = ci_upper(sum_mass_stem))
      
      tree_mass_stem_ave <- all_tree_data %>% 
        group_by(species_id) %>%
        dplyr::summarise(min_mass_stem = min((mass_bark + mass_heartwood + mass_sapwood), na.rm = TRUE),
                         mean_mass_stem = mean((mass_bark + mass_heartwood + mass_sapwood), na.rm = TRUE),
                         median_mass_stem = median((mass_bark + mass_heartwood + mass_sapwood), na.rm = TRUE),
                         quantile_005_mass_stem = quantile((mass_bark + mass_heartwood + mass_sapwood), probs = 0.05, na.rm = TRUE),
                         quantile_025_mass_stem = quantile((mass_bark + mass_heartwood + mass_sapwood), probs = 0.25, na.rm = TRUE),
                         quantile_075_mass_stem = quantile((mass_bark + mass_heartwood + mass_sapwood), probs = 0.75, na.rm = TRUE),
                         quantile_095_mass_stem = quantile((mass_bark + mass_heartwood + mass_sapwood), probs = 0.95, na.rm = TRUE),
                         max_mass_stem = max((mass_bark + mass_heartwood + mass_sapwood), na.rm = TRUE),
                         sd_mass_stem = sd((mass_bark + mass_heartwood + mass_sapwood), na.rm=TRUE),
                         se_mass_stem = std.error((mass_bark + mass_heartwood + mass_sapwood), na.rm=TRUE),
                         ci_lwr_mass_stem = ci_lower((mass_bark + mass_heartwood + mass_sapwood)),
                         ci_upr_mass_stem = ci_upper((mass_bark + mass_heartwood + mass_sapwood)))
    }
      ### MORTALITY RISK FULL
      {
      tree_mort_risk_ave <- all_tree_data %>% 
        group_by(species_id) %>%
        dplyr::summarise(min_mortality_risk = min(mortality_new, na.rm = TRUE),
                         mean_mortality_risk = mean(mortality_new, na.rm = TRUE),
                         median_mortality_risk = median(mortality_new, na.rm = TRUE),
                         quantile_005_mortality_risk = quantile(mortality_new, probs = 0.05, na.rm = TRUE),
                         quantile_025_mortality_risk = quantile(mortality_new, probs = 0.25, na.rm = TRUE),
                         quantile_075_mortality_risk = quantile(mortality_new, probs = 0.75, na.rm = TRUE),
                         quantile_095_mortality_risk = quantile(mortality_new, probs = 0.95, na.rm = TRUE),
                         max_mortality_risk = max(mortality_new, na.rm = TRUE),
                         sd_mortality_risk = sd(mortality_new, na.rm=TRUE),
                         se_mortality_risk = std.error(mortality_new, na.rm=TRUE),
                         ci_lwr_mortality_risk = ci_lower(mortality_new),
                         ci_upr_mortality_risk = ci_upper(mortality_new))
        }
      ### MORTALITY RISK STORAGE DEPENDENT
      {
      tree_mort_risk_storage_ave <- all_tree_data %>% 
        group_by(species_id) %>%
        dplyr::summarise(min_mortality_risk_storage = min(mortality_storage_dependent, na.rm = TRUE),
                         mean_mortality_risk_storage = mean(mortality_storage_dependent, na.rm = TRUE),
                         median_mortality_risk_storage = median(mortality_storage_dependent, na.rm = TRUE),
                         quantile_005_mortality_risk_storage = quantile(mortality_storage_dependent, probs = 0.05, na.rm = TRUE),
                         quantile_025_mortality_risk_storage = quantile(mortality_storage_dependent, probs = 0.25, na.rm = TRUE),
                         quantile_075_mortality_risk_storage = quantile(mortality_storage_dependent, probs = 0.75, na.rm = TRUE),
                         quantile_095_mortality_risk_storage = quantile(mortality_storage_dependent, probs = 0.95, na.rm = TRUE),
                         max_mortality_risk_storage = max(mortality_storage_dependent, na.rm = TRUE),
                         sd_mortality_risk_storage = sd(mortality_storage_dependent, na.rm=TRUE),
                         se_mortality_risk_storage = std.error(mortality_storage_dependent, na.rm=TRUE),
                         ci_lwr_mortality_risk_storage = ci_lower(mortality_storage_dependent),
                         ci_upr_mortality_risk_storage = ci_upper(mortality_storage_dependent))
        }
      ### MORTALITY RISK GROWTH DEPENDENT
      {
      tree_mort_risk_growth_ave <- all_tree_data %>% 
        group_by(species_id) %>%
        dplyr::summarise(min_mortality_risk_growth = min(mortality_growth_dependent, na.rm = TRUE),
                         mean_mortality_risk_growth = mean(mortality_growth_dependent, na.rm = TRUE),
                         median_mortality_risk_growth = median(mortality_growth_dependent, na.rm = TRUE),
                         quantile_005_mortality_risk_growth = quantile(mortality_growth_dependent, probs = 0.05, na.rm = TRUE),
                         quantile_025_mortality_risk_growth = quantile(mortality_growth_dependent, probs = 0.25, na.rm = TRUE),
                         quantile_075_mortality_risk_growth = quantile(mortality_growth_dependent, probs = 0.75, na.rm = TRUE),
                         quantile_095_mortality_risk_growth = quantile(mortality_growth_dependent, probs = 0.95, na.rm = TRUE),
                         max_mortality_risk_growth = max(mortality_growth_dependent, na.rm = TRUE),
                         sd_mortality_risk_growth = sd(mortality_growth_dependent, na.rm=TRUE),
                         se_mortality_risk_growth = std.error(mortality_growth_dependent, na.rm=TRUE),
                         ci_lwr_mortality_risk_growth = ci_lower(mortality_growth_dependent),
                         ci_upr_mortality_risk_growth = ci_upper(mortality_growth_dependent))
        }
      ### FECUNDITY 
      {
      tree_sum_fecundity <- all_tree_data %>% group_by(species_id, run_rep, time) %>%
        dplyr::summarise(fecundity_sum = sum(fecundity, na.rm = TRUE)) %>%
        group_by(species_id) %>%
        dplyr::summarise(min_fecundity_sum = min(fecundity_sum, na.rm = TRUE),
                         mean_fecundity_sum = mean(fecundity_sum, na.rm = TRUE),
                         median_fecundity_sum = median(fecundity_sum, na.rm = TRUE),
                         quantile_005_fecundity_sum = quantile(fecundity_sum, probs = 0.05, na.rm = TRUE),
                         quantile_025_fecundity_sum = quantile(fecundity_sum, probs = 0.25, na.rm = TRUE),
                         quantile_075_fecundity_sum = quantile(fecundity_sum, probs = 0.75, na.rm = TRUE),
                         quantile_095_fecundity_sum = quantile(fecundity_sum, probs = 0.95, na.rm = TRUE),
                         max_fecundity_sum = max(fecundity_sum, na.rm = TRUE),
                         sd_fecundity_sum = sd(fecundity_sum, na.rm=TRUE),
                         se_fecundity_sum = std.error(fecundity_sum, na.rm=TRUE),
                         ci_lwr_fecundity_sum = ci_lower(fecundity_sum),
                         ci_upr_fecundity_sum = ci_upper(fecundity_sum))
      
      tree_fecundity <- all_tree_data %>% 
        group_by(species_id) %>%
        dplyr::summarise(min_fecundity = min(fecundity, na.rm = TRUE),
                         mean_fecundity = mean(fecundity, na.rm = TRUE),
                         median_fecundity = median(fecundity, na.rm = TRUE),
                         quantile_005_fecundity = quantile(fecundity, probs = 0.05, na.rm = TRUE),
                         quantile_025_fecundity = quantile(fecundity, probs = 0.25, na.rm = TRUE),
                         quantile_075_fecundity = quantile(fecundity, probs = 0.75, na.rm = TRUE),
                         quantile_095_fecundity = quantile(fecundity, probs = 0.95, na.rm = TRUE),
                         max_fecundity = max(fecundity, na.rm = TRUE),
                         sd_fecundity = sd(fecundity, na.rm=TRUE),
                         se_fecundity = std.error(fecundity, na.rm=TRUE),
                         ci_lwr_fecundity = ci_lower(fecundity),
                         ci_upr_fecundity = ci_upper(fecundity))
    }
      ### FECUNDITY DT
      {
      tree_sum_fecundity_dt <- all_tree_data %>% group_by(species_id, run_rep, time) %>%
        dplyr::summarise(fecundity_sum = sum(fecundity_dt_abs, na.rm = TRUE)) %>%
        group_by(species_id) %>%
        dplyr::summarise(min_fecundity_dt_sum = min(fecundity_sum, na.rm = TRUE),
                         mean_fecundity_dt_sum = mean(fecundity_sum, na.rm = TRUE),
                         median_fecundity_dt_sum = median(fecundity_sum, na.rm = TRUE),
                         quantile_005_fecundity_dt_sum = quantile(fecundity_sum, probs = 0.05, na.rm = TRUE),
                         quantile_025_fecundity_dt_sum = quantile(fecundity_sum, probs = 0.25, na.rm = TRUE),
                         quantile_075_fecundity_dt_sum = quantile(fecundity_sum, probs = 0.75, na.rm = TRUE),
                         quantile_095_fecundity_dt_sum = quantile(fecundity_sum, probs = 0.95, na.rm = TRUE),
                         max_fecundity_dt_sum = max(fecundity_sum, na.rm = TRUE),
                         sd_fecundity_dt_sum = sd(fecundity_sum, na.rm=TRUE),
                         se_fecundity_dt_sum = std.error(fecundity_sum, na.rm=TRUE),
                         ci_lwr_fecundity_dt_sum = ci_lower(fecundity_sum),
                         ci_upr_fecundity_dt_sum = ci_upper(fecundity_sum))
      
      tree_fecundity_dt <- all_tree_data %>% 
        group_by(species_id) %>%
        dplyr::summarise(min_fecundity_dt = min(fecundity_dt_abs, na.rm = TRUE),
                         mean_fecundity_dt = mean(fecundity_dt_abs, na.rm = TRUE),
                         median_fecundity_dt = median(fecundity_dt_abs, na.rm = TRUE),
                         quantile_005_fecundity_dt = quantile(fecundity_dt_abs, probs = 0.05, na.rm = TRUE),
                         quantile_025_fecundity_dt = quantile(fecundity_dt_abs, probs = 0.25, na.rm = TRUE),
                         quantile_075_fecundity_dt = quantile(fecundity_dt_abs, probs = 0.75, na.rm = TRUE),
                         quantile_095_fecundity_dt = quantile(fecundity_dt_abs, probs = 0.95, na.rm = TRUE),
                         max_fecundity_dt = max(fecundity_dt_abs, na.rm = TRUE),
                         sd_fecundity_dt = sd(fecundity_dt_abs, na.rm=TRUE),
                         se_fecundity_dt = std.error(fecundity_dt_abs, na.rm=TRUE),
                         ci_lwr_fecundity_dt = ci_lower(fecundity_dt_abs),
                         ci_upr_fecundity_dt = ci_upper(fecundity_dt_abs))
        }
      ### NUMBER OF LIVE TREES
      {
      tree_num_tree <- all_tree_data %>% group_by(species_id, run_rep, time) %>%
        dplyr::summarise(num_tree = n()) %>%
        group_by(species_id) %>%
        dplyr::summarise(min_num_tree = min(num_tree, na.rm = TRUE),
                         mean_num_tree = mean(num_tree, na.rm = TRUE),
                         median_num_tree = median(num_tree, na.rm = TRUE),
                         quantile_005_num_tree = quantile(num_tree, probs = 0.05, na.rm = TRUE),
                         quantile_025_num_tree = quantile(num_tree, probs = 0.25, na.rm = TRUE),
                         quantile_075_num_tree = quantile(num_tree, probs = 0.75, na.rm = TRUE),
                         quantile_095_num_tree = quantile(num_tree, probs = 0.95, na.rm = TRUE),
                         max_num_tree = max(num_tree, na.rm = TRUE),
                         sd_num_tree = sd(num_tree, na.rm=TRUE),
                         se_num_tree = std.error(num_tree, na.rm=TRUE),
                         ci_lwr_num_tree = ci_lower(num_tree),
                         ci_upr_num_tree = ci_upper(num_tree))
        }
      
      tree_height_ave$time = i + 5
      tree_height_max$time = i + 5
      
      tree_basal_ave$time = i + 5
      tree_sum_basal_ha$time = i + 5
      tree_sum_basal$time = i + 5
      
      tree_sum_leaf_area$time = i + 5
      tree_leaf_area_ave$time = i + 5
      
      tree_sum_lai$time = i + 5
      
      tree_sum_mass_storage$time = i + 5
      tree_mass_storage_m2$time = i + 5
      tree_mass_storage_ave$time = i + 5
      
      tree_sum_mass_leaf$time = i + 5
      tree_mass_leaf_m2$time = i + 5
      tree_mass_leaf_ave$time = i + 5
      
      tree_sum_mass_root$time = i + 5
      tree_mass_root_m2$time = i + 5
      tree_mass_root_ave$time = i + 5
      
      tree_sum_mass_heartwood$time = i + 5
      tree_mass_heartwood_m2$time = i + 5
      tree_mass_heartwood_ave$time = i + 5
      
      tree_sum_mass_bark$time = i + 5
      tree_mass_bark_m2$time = i + 5
      tree_mass_bark_ave$time = i + 5
      
      tree_sum_mass_sapwood$time = i + 5
      tree_mass_sapwood_m2$time = i + 5
      tree_mass_sapwood_ave$time = i + 5
      
      tree_sum_mass_live$time = i + 5
      tree_mass_live_m2$time = i + 5
      tree_mass_live_ave$time = i + 5
      
      tree_sum_mass_aboveground$time = i + 5
      tree_mass_aboveground_m2$time = i + 5
      tree_mass_aboveground_ave$time = i + 5
      
      tree_sum_mass_stem$time = i + 5
      tree_mass_stem_m2$time = i + 5
      tree_mass_stem_ave$time = i + 5
      
      tree_mort_risk_ave$time = i + 5
      tree_mort_risk_storage_ave$time = i + 5
      tree_mort_risk_growth_ave$time = i + 5
      
      tree_sum_fecundity$time = i + 5
      tree_fecundity$time = i + 5
      
      tree_sum_fecundity_dt$time = i + 5
      tree_fecundity_dt$time = i + 5
      
      tree_num_tree$time = i + 5
      
      tree_data_height <- merge(tree_height_ave, 
                                tree_height_max, 
                                by=(c("time", "species_id")), all= TRUE)
      tree_data_basal <- merge(tree_basal_ave,
                               merge(tree_sum_basal_ha,
                                     tree_sum_basal,
                                     by=(c("time", "species_id")), all= TRUE),
                               by=(c("time", "species_id")), all= TRUE)
      tree_leaf_area <- merge(tree_sum_leaf_area, 
                              merge(tree_leaf_area_ave, 
                                    tree_sum_lai,
                                    by=(c("time", "species_id")), all= TRUE),
                              by=(c("time", "species_id")), all= TRUE)
                                                                     
      tree_mass_storage <- merge(tree_sum_mass_storage, 
                                 merge(tree_mass_storage_m2, 
                                       tree_mass_storage_ave,
                                       by=(c("time", "species_id")), all= TRUE),
                                 by=(c("time", "species_id")), all= TRUE)

      tree_mass_leaf <- merge(tree_sum_mass_leaf, 
                              merge(tree_mass_leaf_m2, 
                                    tree_mass_leaf_ave, 
                                    by=(c("time", "species_id")), all= TRUE),
                              by=(c("time", "species_id")), all= TRUE)
      
      tree_mass_root<- merge(tree_sum_mass_root, 
                              merge(tree_mass_root_m2, 
                                    tree_mass_root_ave, 
                                    by=(c("time", "species_id")), all= TRUE),
                              by=(c("time", "species_id")), all= TRUE)
      
      tree_mass_bark <- merge(tree_sum_mass_bark, 
                              merge(tree_mass_bark_m2, 
                                    tree_mass_bark_ave, 
                                    by=(c("time", "species_id")), all= TRUE),
                              by=(c("time", "species_id")), all= TRUE)
      
      tree_mass_sapwood <- merge(tree_sum_mass_sapwood, 
                              merge(tree_mass_sapwood_m2, 
                                    tree_mass_sapwood_ave, 
                                    by=(c("time", "species_id")), all= TRUE),
                              by=(c("time", "species_id")), all= TRUE)
      
      tree_mass_heartwood <- merge(tree_sum_mass_heartwood, 
                              merge(tree_mass_heartwood_m2, 
                                    tree_mass_heartwood_ave, 
                                    by=(c("time", "species_id")), all= TRUE),
                              by=(c("time", "species_id")), all= TRUE)
      
      tree_mass_live <- merge(tree_sum_mass_live, 
                              merge(tree_mass_live_m2, 
                                    tree_mass_live_ave, 
                                    by=(c("time", "species_id")), all= TRUE),
                              by=(c("time", "species_id")), all= TRUE)
      
      tree_mass_aboveground <- merge(tree_sum_mass_aboveground, 
                              merge(tree_mass_aboveground_m2, 
                                    tree_mass_aboveground_ave, 
                                    by=(c("time", "species_id")), all= TRUE),
                              by=(c("time", "species_id")), all= TRUE)
      
      tree_mass_stem <- merge(tree_sum_mass_stem, 
                              merge(tree_mass_stem_m2, 
                                    tree_mass_stem_ave, 
                                    by=(c("time", "species_id")), all= TRUE),
                              by=(c("time", "species_id")), all= TRUE)
      
      tree_mort_risk <- merge(tree_mort_risk_ave,
                              merge(tree_mort_risk_storage_ave,
                                    tree_mort_risk_growth_ave,
                                    by=(c("time", "species_id")), all= TRUE),
                              by=(c("time", "species_id")), all= TRUE)
      
      tree_fecundity <- merge(tree_sum_fecundity,
                              tree_fecundity,
                              by=(c("time", "species_id")), all= TRUE)
      
      tree_fecundity_dt <- merge(tree_sum_fecundity_dt,
                              tree_fecundity_dt,
                              by=(c("time", "species_id")), all= TRUE)
      
      tree_data <- merge(tree_num_tree, tree_data_height, by=(c("time", "species_id")), all= TRUE)
      tree_data <- merge(tree_data, tree_data_basal, by=(c("time", "species_id")), all= TRUE)
      tree_data <- merge(tree_data, tree_leaf_area, by=(c("time", "species_id")), all= TRUE)
      tree_data <- merge(tree_data, tree_mass_storage, by=(c("time", "species_id")), all= TRUE)
      tree_data <- merge(tree_data, tree_mass_leaf, by=(c("time", "species_id")), all= TRUE)
      tree_data <- merge(tree_data, tree_mass_root, by=(c("time", "species_id")), all= TRUE)
      tree_data <- merge(tree_data, tree_mass_bark, by=(c("time", "species_id")), all= TRUE)
      tree_data <- merge(tree_data, tree_mass_sapwood, by=(c("time", "species_id")), all= TRUE)
      tree_data <- merge(tree_data, tree_mass_heartwood, by=(c("time", "species_id")), all= TRUE)
      tree_data <- merge(tree_data, tree_mass_live, by=(c("time", "species_id")), all= TRUE)
      tree_data <- merge(tree_data, tree_mass_aboveground, by=(c("time", "species_id")), all= TRUE)
      tree_data <- merge(tree_data, tree_mass_stem, by=(c("time", "species_id")), all= TRUE)
      tree_data <- merge(tree_data, tree_mort_risk, by=(c("time", "species_id")), all= TRUE)
      tree_data <- merge(tree_data, tree_fecundity, by=(c("time", "species_id")), all= TRUE)
      tree_data <- merge(tree_data, tree_fecundity_dt, by=(c("time", "species_id")), all= TRUE)
      
      
      return(tree_data)
    }
    else{
      return(data.frame())
    }
  }))
  
  return(all_data)
})

out_env <- lapply(out_dirs_env_total, function(dir){
  path_parts <- unlist(str_split(dir, "/"))
  new_dir_path <- file.path("out", "full_run_auto", "environment_agg", path_parts[3])
  
  print(new_dir_path)
  
  dir.create(new_dir_path, recursive = TRUE)
  
  file_names <- list.files(path = dir, pattern = "tree_data", recursive = TRUE, include.dirs = TRUE)
  file_names_full <- as.vector(sapply(file_names, function(x){
    return(file.path(dir, x))
  }))
  
  print(file_names_full)
  
  files <- plyr::rbind.fill(lapply(file_names_full, process_tree_data))
  
  print(files)
  
  all_data <- plyr::rbind.fill(lapply(unique(files$from), function(i){
    indx <- which(files$from == i)
    all_tree_data <- plyr::rbind.fill(lapply(indx, function(j){
      file_name = files$file[j]
      data = readRDS(file_name)
      data = get_trees_live(data)
      data = subset(data, diameter_stem >= 0.05)
      if(nrow(data) > 0){
        data$run_rep = files$run_rep[j]
        data$env_rep = files$env_rep[j]
      }
      return(data)
    }))
    
    if(nrow(all_tree_data) > 0){
      ######## TREE HEIGHT #######
      {
        tree_height_max <- all_tree_data %>% group_by(species_id, run_rep, env_rep,  time) %>%
          dplyr::summarise(max_height = max(height, na.rm = TRUE)) %>%
          group_by(species_id) %>%
          dplyr::summarise(min_max_height = min(max_height, na.rm = TRUE),
                           mean_max_height = mean(max_height, na.rm = TRUE),
                           median_max_height = median(max_height, na.rm = TRUE),
                           quantile_005_max_height = quantile(max_height, probs = 0.05, na.rm = TRUE),
                           quantile_025_max_height = quantile(max_height, probs = 0.25, na.rm = TRUE),
                           quantile_075_max_height = quantile(max_height, probs = 0.75, na.rm = TRUE),
                           quantile_095_max_height = quantile(max_height, probs = 0.95, na.rm = TRUE),
                           max_max_height = max(max_height, na.rm = TRUE),
                           sd_max_height = sd(max_height, na.rm=TRUE),
                           se_max_height = std.error(max_height, na.rm=TRUE),
                           ci_lwr_max_height = ci_lower(max_height),
                           ci_upr_max_height = ci_upper(max_height))
        
        tree_height_ave <- all_tree_data %>% 
          group_by(species_id) %>%
          dplyr::summarise(min_height = min(height, na.rm = TRUE),
                           mean_height = mean(height, na.rm = TRUE),
                           median_height = median(height, na.rm = TRUE),
                           quantile_005_height = quantile(height, probs = 0.05, na.rm = TRUE),
                           quantile_025_height = quantile(height, probs = 0.25, na.rm = TRUE),
                           quantile_075_height = quantile(height, probs = 0.75, na.rm = TRUE),
                           quantile_095_height = quantile(height, probs = 0.95, na.rm = TRUE),
                           max_height = max(height, na.rm = TRUE),
                           sd_height = sd(height, na.rm=TRUE),
                           se_height = std.error(height, na.rm=TRUE),
                           ci_lwr_height = ci_lower(height),
                           ci_upr_height = ci_upper(height))
      }
      ######## BASAL AREA ######### 
      {
        tree_sum_basal <- all_tree_data %>% group_by(species_id, run_rep, env_rep,  time) %>%
          dplyr::summarise(sum_basal = max(area_stem, na.rm = TRUE)) %>%
          group_by(species_id) %>%
          dplyr::summarise(min_sum_basal = min(sum_basal, na.rm = TRUE),
                           mean_sum_basal = mean(sum_basal, na.rm = TRUE),
                           median_sum_basal = median(sum_basal, na.rm = TRUE),
                           quantile_005_sum_basal = quantile(sum_basal, probs = 0.05, na.rm = TRUE),
                           quantile_025_sum_basal = quantile(sum_basal, probs = 0.25, na.rm = TRUE),
                           quantile_075_sum_basal = quantile(sum_basal, probs = 0.75, na.rm = TRUE),
                           quantile_095_sum_basal = quantile(sum_basal, probs = 0.95, na.rm = TRUE),
                           max_sum_basal = max(sum_basal, na.rm = TRUE),
                           sd_sum_basal = sd(sum_basal, na.rm=TRUE),
                           se_sum_basal = std.error(sum_basal, na.rm=TRUE),
                           ci_lwr_sum_basal = ci_lower(sum_basal),
                           ci_upr_sum_basal = ci_upper(sum_basal))
        
        tree_sum_basal_ha <- all_tree_data %>% group_by(species_id, run_rep, env_rep,  time) %>%
          dplyr::summarise(sum_basal = max(area_stem*100, na.rm = TRUE)) %>%
          group_by(species_id) %>%
          dplyr::summarise(min_sum_basal_ha = min(sum_basal, na.rm = TRUE),
                           mean_sum_basal_ha = mean(sum_basal, na.rm = TRUE),
                           median_sum_basal_ha = median(sum_basal, na.rm = TRUE),
                           quantile_005_sum_basal_ha = quantile(sum_basal, probs = 0.05, na.rm = TRUE),
                           quantile_025_sum_basal_ha = quantile(sum_basal, probs = 0.25, na.rm = TRUE),
                           quantile_075_sum_basal_ha = quantile(sum_basal, probs = 0.75, na.rm = TRUE),
                           quantile_095_sum_basal_ha = quantile(sum_basal, probs = 0.95, na.rm = TRUE),
                           max_sum_basal_ha = max(sum_basal, na.rm = TRUE),
                           sd_sum_basal_ha = sd(sum_basal, na.rm=TRUE),
                           se_sum_basal_ha = std.error(sum_basal, na.rm=TRUE),
                           ci_lwr_sum_basal_ha = ci_lower(sum_basal),
                           ci_upr_sum_basal_ha = ci_upper(sum_basal))
        
        tree_basal_ave <- all_tree_data %>% 
          group_by(species_id) %>%
          dplyr::summarise(min_basal = min(area_stem, na.rm = TRUE),
                           mean_basal = mean(area_stem, na.rm = TRUE),
                           median_basal = median(area_stem, na.rm = TRUE),
                           quantile_005_basal = quantile(area_stem, probs = 0.05, na.rm = TRUE),
                           quantile_025_basal = quantile(area_stem, probs = 0.25, na.rm = TRUE),
                           quantile_075_basal = quantile(area_stem, probs = 0.75, na.rm = TRUE),
                           quantile_095_basal = quantile(area_stem, probs = 0.95, na.rm = TRUE),
                           max_basal = max(area_stem, na.rm = TRUE),
                           sd_basal = sd(area_stem, na.rm=TRUE),
                           se_basal = std.error(area_stem, na.rm=TRUE),
                           ci_lwr_basal = ci_lower(area_stem),
                           ci_upr_basal = ci_upper(area_stem))
      }
      ######## LEAF AREA #########
      {
        tree_sum_leaf_area <- all_tree_data %>% group_by(species_id, run_rep, env_rep,  time) %>%
          dplyr::summarise(sum_leaf_area = sum(area_leaf, na.rm = TRUE)) %>%
          group_by(species_id) %>%
          dplyr::summarise(min_sum_leaf_area = min(sum_leaf_area, na.rm = TRUE),
                           mean_sum_leaf_area = mean(sum_leaf_area, na.rm = TRUE),
                           median_sum_leaf_area = median(sum_leaf_area, na.rm = TRUE),
                           quantile_005_sum_leaf_area = quantile(sum_leaf_area, probs = 0.05, na.rm = TRUE),
                           quantile_025_sum_leaf_area = quantile(sum_leaf_area, probs = 0.25, na.rm = TRUE),
                           quantile_075_sum_leaf_area = quantile(sum_leaf_area, probs = 0.75, na.rm = TRUE),
                           quantile_095_sum_leaf_area = quantile(sum_leaf_area, probs = 0.95, na.rm = TRUE),
                           max_sum_leaf_area = max(sum_leaf_area, na.rm = TRUE),
                           sd_sum_leaf_area = sd(sum_leaf_area, na.rm=TRUE),
                           se_sum_leaf_area = std.error(sum_leaf_area, na.rm=TRUE),
                           ci_lwr_sum_leaf_area = ci_lower(sum_leaf_area),
                           ci_upr_sum_leaf_area = ci_upper(sum_leaf_area))
        
        tree_leaf_area_ave <- all_tree_data %>% 
          group_by(species_id) %>%
          dplyr::summarise(min_leaf_area = min(area_leaf, na.rm = TRUE),
                           mean_leaf_area = mean(area_leaf, na.rm = TRUE),
                           median_leaf_area = median(area_leaf, na.rm = TRUE),
                           quantile_005_leaf_area = quantile(area_leaf, probs = 0.05, na.rm = TRUE),
                           quantile_025_leaf_area = quantile(area_leaf, probs = 0.25, na.rm = TRUE),
                           quantile_075_leaf_area = quantile(area_leaf, probs = 0.75, na.rm = TRUE),
                           quantile_095_leaf_area = quantile(area_leaf, probs = 0.95, na.rm = TRUE),
                           max_leaf_area = max(area_leaf, na.rm = TRUE),
                           sd_leaf_area = sd(area_leaf, na.rm=TRUE),
                           se_leaf_area = std.error(area_leaf, na.rm=TRUE),
                           ci_lwr_leaf_area = ci_lower(area_leaf),
                           ci_upr_leaf_area = ci_upper(area_leaf))
      }
      ######## LEAF AREA INDEX #########
      {
        tree_sum_lai <- all_tree_data %>% group_by(species_id, run_rep, env_rep,  time) %>%
          dplyr::summarise(sum_leaf_area = sum(area_leaf/100, na.rm = TRUE)) %>%
          group_by(species_id) %>%
          dplyr::summarise(min_sum_lai = min(sum_leaf_area, na.rm = TRUE),
                           mean_sum_lai = mean(sum_leaf_area, na.rm = TRUE),
                           median_sum_lai = median(sum_leaf_area, na.rm = TRUE),
                           quantile_005_sum_lai = quantile(sum_leaf_area, probs = 0.05, na.rm = TRUE),
                           quantile_025_sum_lai = quantile(sum_leaf_area, probs = 0.25, na.rm = TRUE),
                           quantile_075_sum_lai = quantile(sum_leaf_area, probs = 0.75, na.rm = TRUE),
                           quantile_095_sum_lai = quantile(sum_leaf_area, probs = 0.95, na.rm = TRUE),
                           max_sum_lai = max(sum_leaf_area, na.rm = TRUE),
                           sd_sum_lai = sd(sum_leaf_area, na.rm=TRUE),
                           se_sum_lai = std.error(sum_leaf_area, na.rm=TRUE),
                           ci_lwr_sum_lai = ci_lower(sum_leaf_area),
                           ci_upr_sum_lai = ci_upper(sum_leaf_area))
      }
      ######## MASS STORAGE ########
      {
        tree_sum_mass_storage <- all_tree_data %>% group_by(species_id, run_rep, env_rep,  time) %>%
          dplyr::summarise(sum_mass_storage = sum(mass_storage, na.rm = TRUE)) %>%
          group_by(species_id) %>%
          dplyr::summarise(min_sum_mass_storage = min(sum_mass_storage, na.rm = TRUE),
                           mean_sum_mass_storage = mean(sum_mass_storage, na.rm = TRUE),
                           median_sum_mass_storage = median(sum_mass_storage, na.rm = TRUE),
                           quantile_005_sum_mass_storage = quantile(sum_mass_storage, probs = 0.05, na.rm = TRUE),
                           quantile_025_sum_mass_storage = quantile(sum_mass_storage, probs = 0.25, na.rm = TRUE),
                           quantile_075_sum_mass_storage = quantile(sum_mass_storage, probs = 0.75, na.rm = TRUE),
                           quantile_095_sum_mass_storage = quantile(sum_mass_storage, probs = 0.95, na.rm = TRUE),
                           max_sum_mass_storage = max(sum_mass_storage, na.rm = TRUE),
                           sd_sum_mass_storage = sd(sum_mass_storage, na.rm=TRUE),
                           se_sum_mass_storage = std.error(sum_mass_storage, na.rm=TRUE),
                           ci_lwr_sum_mass_storage = ci_lower(sum_mass_storage),
                           ci_upr_sum_mass_storage = ci_upper(sum_mass_storage))
        
        tree_mass_storage_m2 <- all_tree_data %>% group_by(species_id, run_rep, env_rep,  time) %>%
          dplyr::summarise(sum_mass_storage = sum(mass_storage/100, na.rm = TRUE)) %>%
          group_by(species_id) %>%
          dplyr::summarise(min_sum_mass_storage_m2 = min(sum_mass_storage, na.rm = TRUE),
                           mean_sum_mass_storage_m2 = mean(sum_mass_storage, na.rm = TRUE),
                           median_sum_mass_storage_m2 = median(sum_mass_storage, na.rm = TRUE),
                           quantile_005_sum_mass_storage_m2 = quantile(sum_mass_storage, probs = 0.05, na.rm = TRUE),
                           quantile_025_sum_mass_storage_m2 = quantile(sum_mass_storage, probs = 0.25, na.rm = TRUE),
                           quantile_075_sum_mass_storage_m2 = quantile(sum_mass_storage, probs = 0.75, na.rm = TRUE),
                           quantile_095_sum_mass_storage_m2 = quantile(sum_mass_storage, probs = 0.95, na.rm = TRUE),
                           max_sum_mass_storage_m2 = max(sum_mass_storage, na.rm = TRUE),
                           sd_sum_mass_storage_m2 = sd(sum_mass_storage, na.rm=TRUE),
                           se_sum_mass_storage_m2 = std.error(sum_mass_storage, na.rm=TRUE),
                           ci_lwr_sum_mass_storage_m2 = ci_lower(sum_mass_storage),
                           ci_upr_sum_mass_storage_m2 = ci_upper(sum_mass_storage))
        
        tree_mass_storage_ave <- all_tree_data %>% 
          group_by(species_id) %>%
          dplyr::summarise(min_mass_storage = min(mass_storage, na.rm = TRUE),
                           mean_mass_storage = mean(mass_storage, na.rm = TRUE),
                           median_mass_storage = median(mass_storage, na.rm = TRUE),
                           quantile_005_mass_storage = quantile(mass_storage, probs = 0.05, na.rm = TRUE),
                           quantile_025_mass_storage = quantile(mass_storage, probs = 0.25, na.rm = TRUE),
                           quantile_075_mass_storage = quantile(mass_storage, probs = 0.75, na.rm = TRUE),
                           quantile_095_mass_storage = quantile(mass_storage, probs = 0.95, na.rm = TRUE),
                           max_mass_storage = max(mass_storage, na.rm = TRUE),
                           sd_mass_storage = sd(mass_storage, na.rm=TRUE),
                           se_mass_storage = std.error(mass_storage, na.rm=TRUE),
                           ci_lwr_mass_storage = ci_lower(mass_storage),
                           ci_upr_mass_storage = ci_upper(mass_storage))
      }
      ######## MASS LEAF ##########
      {
        tree_sum_mass_leaf <- all_tree_data %>% group_by(species_id, run_rep, env_rep,  time) %>%
          dplyr::summarise(sum_mass_leaf = sum(mass_leaf, na.rm = TRUE)) %>%
          group_by(species_id) %>%
          dplyr::summarise(min_sum_mass_leaf = min(sum_mass_leaf, na.rm = TRUE),
                           mean_sum_mass_leaf = mean(sum_mass_leaf, na.rm = TRUE),
                           median_sum_mass_leaf = median(sum_mass_leaf, na.rm = TRUE),
                           quantile_005_sum_mass_leaf = quantile(sum_mass_leaf, probs = 0.05, na.rm = TRUE),
                           quantile_025_sum_mass_leaf = quantile(sum_mass_leaf, probs = 0.25, na.rm = TRUE),
                           quantile_075_sum_mass_leaf = quantile(sum_mass_leaf, probs = 0.75, na.rm = TRUE),
                           quantile_095_sum_mass_leaf = quantile(sum_mass_leaf, probs = 0.95, na.rm = TRUE),
                           max_sum_mass_leaf = max(sum_mass_leaf, na.rm = TRUE),
                           sd_sum_mass_leaf = sd(sum_mass_leaf, na.rm=TRUE),
                           se_sum_mass_leaf = std.error(sum_mass_leaf, na.rm=TRUE),
                           ci_lwr_sum_mass_leaf = ci_lower(sum_mass_leaf),
                           ci_upr_sum_mass_leaf = ci_upper(sum_mass_leaf))
        
        tree_mass_leaf_m2 <- all_tree_data %>% group_by(species_id, run_rep, env_rep,  time) %>%
          dplyr::summarise(sum_mass_leaf = sum(mass_leaf/100, na.rm = TRUE)) %>%
          group_by(species_id) %>%
          dplyr::summarise(min_sum_mass_leaf_m2 = min(sum_mass_leaf, na.rm = TRUE),
                           mean_sum_mass_leaf_m2 = mean(sum_mass_leaf, na.rm = TRUE),
                           median_sum_mass_leaf_m2 = median(sum_mass_leaf, na.rm = TRUE),
                           quantile_005_sum_mass_leaf_m2 = quantile(sum_mass_leaf, probs = 0.05, na.rm = TRUE),
                           quantile_025_sum_mass_leaf_m2 = quantile(sum_mass_leaf, probs = 0.25, na.rm = TRUE),
                           quantile_075_sum_mass_leaf_m2 = quantile(sum_mass_leaf, probs = 0.75, na.rm = TRUE),
                           quantile_095_sum_mass_leaf_m2 = quantile(sum_mass_leaf, probs = 0.95, na.rm = TRUE),
                           max_sum_mass_leaf_m2 = max(sum_mass_leaf, na.rm = TRUE),
                           sd_sum_mass_leaf_m2 = sd(sum_mass_leaf, na.rm=TRUE),
                           se_sum_mass_leaf_m2 = std.error(sum_mass_leaf, na.rm=TRUE),
                           ci_lwr_sum_mass_leaf_m2 = ci_lower(sum_mass_leaf),
                           ci_upr_sum_mass_leaf_m2 = ci_upper(sum_mass_leaf))
        
        tree_mass_leaf_ave <- all_tree_data %>% 
          group_by(species_id) %>%
          dplyr::summarise(min_mass_leaf = min(mass_leaf, na.rm = TRUE),
                           mean_mass_leaf = mean(mass_leaf, na.rm = TRUE),
                           median_mass_leaf = median(mass_leaf, na.rm = TRUE),
                           quantile_005_mass_leaf = quantile(mass_leaf, probs = 0.05, na.rm = TRUE),
                           quantile_025_mass_leaf = quantile(mass_leaf, probs = 0.25, na.rm = TRUE),
                           quantile_075_mass_leaf = quantile(mass_leaf, probs = 0.75, na.rm = TRUE),
                           quantile_095_mass_leaf = quantile(mass_leaf, probs = 0.95, na.rm = TRUE),
                           max_mass_leaf = max(mass_leaf, na.rm = TRUE),
                           sd_mass_leaf = sd(mass_leaf, na.rm=TRUE),
                           se_mass_leaf = std.error(mass_leaf, na.rm=TRUE),
                           ci_lwr_mass_leaf = ci_lower(mass_leaf),
                           ci_upr_mass_leaf = ci_upper(mass_leaf))
      }
      ######## MASS SAPWOOD ########
      {
        tree_sum_mass_sapwood <- all_tree_data %>% group_by(species_id, run_rep, env_rep,  time) %>%
          dplyr::summarise(sum_mass_sapwood = sum(mass_sapwood, na.rm = TRUE)) %>%
          group_by(species_id) %>%
          dplyr::summarise(min_sum_mass_sapwood = min(sum_mass_sapwood, na.rm = TRUE),
                           mean_sum_mass_sapwood = mean(sum_mass_sapwood, na.rm = TRUE),
                           median_sum_mass_sapwood = median(sum_mass_sapwood, na.rm = TRUE),
                           quantile_005_sum_mass_sapwood = quantile(sum_mass_sapwood, probs = 0.05, na.rm = TRUE),
                           quantile_025_sum_mass_sapwood = quantile(sum_mass_sapwood, probs = 0.25, na.rm = TRUE),
                           quantile_075_sum_mass_sapwood = quantile(sum_mass_sapwood, probs = 0.75, na.rm = TRUE),
                           quantile_095_sum_mass_sapwood = quantile(sum_mass_sapwood, probs = 0.95, na.rm = TRUE),
                           max_sum_mass_sapwood = max(sum_mass_sapwood, na.rm = TRUE),
                           sd_sum_mass_sapwood = sd(sum_mass_sapwood, na.rm=TRUE),
                           se_sum_mass_sapwood = std.error(sum_mass_sapwood, na.rm=TRUE),
                           ci_lwr_sum_mass_sapwood = ci_lower(sum_mass_sapwood),
                           ci_upr_sum_mass_sapwood = ci_upper(sum_mass_sapwood))
        
        tree_mass_sapwood_m2 <- all_tree_data %>% group_by(species_id, run_rep,  env_rep, time) %>%
          dplyr::summarise(sum_mass_sapwood = sum(mass_sapwood/100, na.rm = TRUE)) %>%
          group_by(species_id) %>%
          dplyr::summarise(min_sum_mass_sapwood_m2 = min(sum_mass_sapwood, na.rm = TRUE),
                           mean_sum_mass_sapwood_m2 = mean(sum_mass_sapwood, na.rm = TRUE),
                           median_sum_mass_sapwood_m2 = median(sum_mass_sapwood, na.rm = TRUE),
                           quantile_005_sum_mass_sapwood_m2 = quantile(sum_mass_sapwood, probs = 0.05, na.rm = TRUE),
                           quantile_025_sum_mass_sapwood_m2 = quantile(sum_mass_sapwood, probs = 0.25, na.rm = TRUE),
                           quantile_075_sum_mass_sapwood_m2 = quantile(sum_mass_sapwood, probs = 0.75, na.rm = TRUE),
                           quantile_095_sum_mass_sapwood_m2 = quantile(sum_mass_sapwood, probs = 0.95, na.rm = TRUE),
                           max_sum_mass_sapwood_m2 = max(sum_mass_sapwood, na.rm = TRUE),
                           sd_sum_mass_sapwood_m2 = sd(sum_mass_sapwood, na.rm=TRUE),
                           se_sum_mass_sapwood_m2 = std.error(sum_mass_sapwood, na.rm=TRUE),
                           ci_lwr_sum_mass_sapwood_m2 = ci_lower(sum_mass_sapwood),
                           ci_upr_sum_mass_sapwood_m2 = ci_upper(sum_mass_sapwood))
        
        tree_mass_sapwood_ave <- all_tree_data %>% 
          group_by(species_id) %>%
          dplyr::summarise(min_mass_sapwood = min(mass_sapwood, na.rm = TRUE),
                           mean_mass_sapwood = mean(mass_sapwood, na.rm = TRUE),
                           median_mass_sapwood = median(mass_sapwood, na.rm = TRUE),
                           quantile_005_mass_sapwood = quantile(mass_sapwood, probs = 0.05, na.rm = TRUE),
                           quantile_025_mass_sapwood = quantile(mass_sapwood, probs = 0.25, na.rm = TRUE),
                           quantile_075_mass_sapwood = quantile(mass_sapwood, probs = 0.75, na.rm = TRUE),
                           quantile_095_mass_sapwood = quantile(mass_sapwood, probs = 0.95, na.rm = TRUE),
                           max_mass_sapwood = max(mass_sapwood, na.rm = TRUE),
                           sd_mass_sapwood = sd(mass_sapwood, na.rm=TRUE),
                           se_mass_sapwood = std.error(mass_sapwood, na.rm=TRUE),
                           ci_lwr_mass_sapwood = ci_lower(mass_sapwood),
                           ci_upr_mass_sapwood = ci_upper(mass_sapwood))
      }
      ######## MASS BARK ########
      {
        tree_sum_mass_bark <- all_tree_data %>% group_by(species_id, run_rep, env_rep,  time) %>%
          dplyr::summarise(sum_mass_bark = sum(mass_bark, na.rm = TRUE)) %>%
          group_by(species_id) %>%
          dplyr::summarise(min_sum_mass_bark = min(sum_mass_bark, na.rm = TRUE),
                           mean_sum_mass_bark = mean(sum_mass_bark, na.rm = TRUE),
                           median_sum_mass_bark = median(sum_mass_bark, na.rm = TRUE),
                           quantile_005_sum_mass_bark = quantile(sum_mass_bark, probs = 0.05, na.rm = TRUE),
                           quantile_025_sum_mass_bark = quantile(sum_mass_bark, probs = 0.25, na.rm = TRUE),
                           quantile_075_sum_mass_bark = quantile(sum_mass_bark, probs = 0.75, na.rm = TRUE),
                           quantile_095_sum_mass_bark = quantile(sum_mass_bark, probs = 0.95, na.rm = TRUE),
                           max_sum_mass_bark = max(sum_mass_bark, na.rm = TRUE),
                           sd_sum_mass_bark = sd(sum_mass_bark, na.rm=TRUE),
                           se_sum_mass_bark = std.error(sum_mass_bark, na.rm=TRUE),
                           ci_lwr_sum_mass_bark = ci_lower(sum_mass_bark),
                           ci_upr_sum_mass_bark = ci_upper(sum_mass_bark))
        
        tree_mass_bark_m2 <- all_tree_data %>% group_by(species_id, run_rep, env_rep,  time) %>%
          dplyr::summarise(sum_mass_bark = sum(mass_bark/100, na.rm = TRUE)) %>%
          group_by(species_id) %>%
          dplyr::summarise(min_sum_mass_bark_m2 = min(sum_mass_bark, na.rm = TRUE),
                           mean_sum_mass_bark_m2 = mean(sum_mass_bark, na.rm = TRUE),
                           median_sum_mass_bark_m2 = median(sum_mass_bark, na.rm = TRUE),
                           quantile_005_sum_mass_bark_m2 = quantile(sum_mass_bark, probs = 0.05, na.rm = TRUE),
                           quantile_025_sum_mass_bark_m2 = quantile(sum_mass_bark, probs = 0.25, na.rm = TRUE),
                           quantile_075_sum_mass_bark_m2 = quantile(sum_mass_bark, probs = 0.75, na.rm = TRUE),
                           quantile_095_sum_mass_bark_m2 = quantile(sum_mass_bark, probs = 0.95, na.rm = TRUE),
                           max_sum_mass_bark_m2 = max(sum_mass_bark, na.rm = TRUE),
                           sd_sum_mass_bark_m2 = sd(sum_mass_bark, na.rm=TRUE),
                           se_sum_mass_bark_m2 = std.error(sum_mass_bark, na.rm=TRUE),
                           ci_lwr_sum_mass_bark_m2 = ci_lower(sum_mass_bark),
                           ci_upr_sum_mass_bark_m2 = ci_upper(sum_mass_bark))
        
        tree_mass_bark_ave <- all_tree_data %>% 
          group_by(species_id) %>%
          dplyr::summarise(min_mass_bark = min(mass_bark, na.rm = TRUE),
                           mean_mass_bark = mean(mass_bark, na.rm = TRUE),
                           median_mass_bark = median(mass_bark, na.rm = TRUE),
                           quantile_005_mass_bark = quantile(mass_bark, probs = 0.05, na.rm = TRUE),
                           quantile_025_mass_bark = quantile(mass_bark, probs = 0.25, na.rm = TRUE),
                           quantile_075_mass_bark = quantile(mass_bark, probs = 0.75, na.rm = TRUE),
                           quantile_095_mass_bark = quantile(mass_bark, probs = 0.95, na.rm = TRUE),
                           max_mass_bark = max(mass_bark, na.rm = TRUE),
                           sd_mass_bark = sd(mass_bark, na.rm=TRUE),
                           se_mass_bark = std.error(mass_bark, na.rm=TRUE),
                           ci_lwr_mass_bark = ci_lower(mass_bark),
                           ci_upr_mass_bark = ci_upper(mass_bark))
      }
      ######## MASS ROOT ########
      { 
        tree_sum_mass_root <- all_tree_data %>% group_by(species_id, run_rep, env_rep,  time) %>%
          dplyr::summarise(sum_mass_root = sum(mass_root, na.rm = TRUE)) %>%
          group_by(species_id) %>%
          dplyr::summarise(min_sum_mass_root = min(sum_mass_root, na.rm = TRUE),
                           mean_sum_mass_root = mean(sum_mass_root, na.rm = TRUE),
                           median_sum_mass_root = median(sum_mass_root, na.rm = TRUE),
                           quantile_005_sum_mass_root = quantile(sum_mass_root, probs = 0.05, na.rm = TRUE),
                           quantile_025_sum_mass_root = quantile(sum_mass_root, probs = 0.25, na.rm = TRUE),
                           quantile_075_sum_mass_root = quantile(sum_mass_root, probs = 0.75, na.rm = TRUE),
                           quantile_095_sum_mass_root = quantile(sum_mass_root, probs = 0.95, na.rm = TRUE),
                           max_sum_mass_root = max(sum_mass_root, na.rm = TRUE),
                           sd_sum_mass_root = sd(sum_mass_root, na.rm=TRUE),
                           se_sum_mass_root = std.error(sum_mass_root, na.rm=TRUE),
                           ci_lwr_sum_mass_root = ci_lower(sum_mass_root),
                           ci_upr_sum_mass_root = ci_upper(sum_mass_root))
        
        tree_mass_root_m2 <- all_tree_data %>% group_by(species_id, run_rep, env_rep,  time) %>%
          dplyr::summarise(sum_mass_root = sum(mass_root/100, na.rm = TRUE)) %>%
          group_by(species_id) %>%
          dplyr::summarise(min_sum_mass_root_m2 = min(sum_mass_root, na.rm = TRUE),
                           mean_sum_mass_root_m2 = mean(sum_mass_root, na.rm = TRUE),
                           median_sum_mass_root_m2 = median(sum_mass_root, na.rm = TRUE),
                           quantile_005_sum_mass_root_m2 = quantile(sum_mass_root, probs = 0.05, na.rm = TRUE),
                           quantile_025_sum_mass_root_m2 = quantile(sum_mass_root, probs = 0.25, na.rm = TRUE),
                           quantile_075_sum_mass_root_m2 = quantile(sum_mass_root, probs = 0.75, na.rm = TRUE),
                           quantile_095_sum_mass_root_m2 = quantile(sum_mass_root, probs = 0.95, na.rm = TRUE),
                           max_sum_mass_root_m2 = max(sum_mass_root, na.rm = TRUE),
                           sd_sum_mass_root_m2 = sd(sum_mass_root, na.rm=TRUE),
                           se_sum_mass_root_m2 = std.error(sum_mass_root, na.rm=TRUE),
                           ci_lwr_sum_mass_root_m2 = ci_lower(sum_mass_root),
                           ci_upr_sum_mass_root_m2 = ci_upper(sum_mass_root))
        
        tree_mass_root_ave <- all_tree_data %>% 
          group_by(species_id) %>%
          dplyr::summarise(min_mass_root = min(mass_root, na.rm = TRUE),
                           mean_mass_root = mean(mass_root, na.rm = TRUE),
                           median_mass_root = median(mass_root, na.rm = TRUE),
                           quantile_005_mass_root = quantile(mass_root, probs = 0.05, na.rm = TRUE),
                           quantile_025_mass_root = quantile(mass_root, probs = 0.25, na.rm = TRUE),
                           quantile_075_mass_root = quantile(mass_root, probs = 0.75, na.rm = TRUE),
                           quantile_095_mass_root = quantile(mass_root, probs = 0.95, na.rm = TRUE),
                           max_mass_root = max(mass_root, na.rm = TRUE),
                           sd_mass_root = sd(mass_root, na.rm=TRUE),
                           se_mass_root = std.error(mass_root, na.rm=TRUE),
                           ci_lwr_mass_root = ci_lower(mass_root),
                           ci_upr_mass_root = ci_upper(mass_root))
      }
      ######## MASS HEARTWOOD #########
      {
        tree_sum_mass_heartwood <- all_tree_data %>% group_by(species_id, run_rep, env_rep,  time) %>%
          dplyr::summarise(sum_mass_heartwood = sum(mass_heartwood, na.rm = TRUE)) %>%
          group_by(species_id) %>%
          dplyr::summarise(min_sum_mass_heartwood = min(sum_mass_heartwood, na.rm = TRUE),
                           mean_sum_mass_heartwood = mean(sum_mass_heartwood, na.rm = TRUE),
                           median_sum_mass_heartwood = median(sum_mass_heartwood, na.rm = TRUE),
                           quantile_005_sum_mass_heartwood = quantile(sum_mass_heartwood, probs = 0.05, na.rm = TRUE),
                           quantile_025_sum_mass_heartwood = quantile(sum_mass_heartwood, probs = 0.25, na.rm = TRUE),
                           quantile_075_sum_mass_heartwood = quantile(sum_mass_heartwood, probs = 0.75, na.rm = TRUE),
                           quantile_095_sum_mass_heartwood = quantile(sum_mass_heartwood, probs = 0.95, na.rm = TRUE),
                           max_sum_mass_heartwood = max(sum_mass_heartwood, na.rm = TRUE),
                           sd_sum_mass_heartwood = sd(sum_mass_heartwood, na.rm=TRUE),
                           se_sum_mass_heartwood = std.error(sum_mass_heartwood, na.rm=TRUE),
                           ci_lwr_sum_mass_heartwood = ci_lower(sum_mass_heartwood),
                           ci_upr_sum_mass_heartwood = ci_upper(sum_mass_heartwood))
        
        tree_mass_heartwood_m2 <- all_tree_data %>% group_by(species_id, run_rep, env_rep,  time) %>%
          dplyr::summarise(sum_mass_heartwood = sum(mass_heartwood/100, na.rm = TRUE)) %>%
          group_by(species_id) %>%
          dplyr::summarise(min_sum_mass_heartwood_m2 = min(sum_mass_heartwood, na.rm = TRUE),
                           mean_sum_mass_heartwood_m2 = mean(sum_mass_heartwood, na.rm = TRUE),
                           median_sum_mass_heartwood_m2 = median(sum_mass_heartwood, na.rm = TRUE),
                           quantile_005_sum_mass_heartwood_m2 = quantile(sum_mass_heartwood, probs = 0.05, na.rm = TRUE),
                           quantile_025_sum_mass_heartwood_m2 = quantile(sum_mass_heartwood, probs = 0.25, na.rm = TRUE),
                           quantile_075_sum_mass_heartwood_m2 = quantile(sum_mass_heartwood, probs = 0.75, na.rm = TRUE),
                           quantile_095_sum_mass_heartwood_m2 = quantile(sum_mass_heartwood, probs = 0.95, na.rm = TRUE),
                           max_sum_mass_heartwood_m2 = max(sum_mass_heartwood, na.rm = TRUE),
                           sd_sum_mass_heartwood_m2 = sd(sum_mass_heartwood, na.rm=TRUE),
                           se_sum_mass_heartwood_m2 = std.error(sum_mass_heartwood, na.rm=TRUE),
                           ci_lwr_sum_mass_heartwood_m2 = ci_lower(sum_mass_heartwood),
                           ci_upr_sum_mass_heartwood_m2 = ci_upper(sum_mass_heartwood))
        
        tree_mass_heartwood_ave <- all_tree_data %>% 
          group_by(species_id) %>%
          dplyr::summarise(min_mass_heartwood = min(mass_heartwood, na.rm = TRUE),
                           mean_mass_heartwood = mean(mass_heartwood, na.rm = TRUE),
                           median_mass_heartwood = median(mass_heartwood, na.rm = TRUE),
                           quantile_005_mass_heartwood = quantile(mass_heartwood, probs = 0.05, na.rm = TRUE),
                           quantile_025_mass_heartwood = quantile(mass_heartwood, probs = 0.25, na.rm = TRUE),
                           quantile_075_mass_heartwood = quantile(mass_heartwood, probs = 0.75, na.rm = TRUE),
                           quantile_095_mass_heartwood = quantile(mass_heartwood, probs = 0.95, na.rm = TRUE),
                           max_mass_heartwood = max(mass_heartwood, na.rm = TRUE),
                           sd_mass_heartwood = sd(mass_heartwood, na.rm=TRUE),
                           se_mass_heartwood = std.error(mass_heartwood, na.rm=TRUE),
                           ci_lwr_mass_heartwood = ci_lower(mass_heartwood),
                           ci_upr_mass_heartwood = ci_upper(mass_heartwood))
      }
      ######## MASS LIVE ##########
      {
        tree_sum_mass_live <- all_tree_data %>% group_by(species_id, run_rep, env_rep,  time) %>%
          dplyr::summarise(sum_mass_live = sum((mass_leaf + mass_bark + mass_root + mass_sapwood), na.rm = TRUE)) %>%
          group_by(species_id) %>%
          dplyr::summarise(min_sum_mass_live = min(sum_mass_live, na.rm = TRUE),
                           mean_sum_mass_live = mean(sum_mass_live, na.rm = TRUE),
                           median_sum_mass_live = median(sum_mass_live, na.rm = TRUE),
                           quantile_005_sum_mass_live = quantile(sum_mass_live, probs = 0.05, na.rm = TRUE),
                           quantile_025_sum_mass_live = quantile(sum_mass_live, probs = 0.25, na.rm = TRUE),
                           quantile_075_sum_mass_live = quantile(sum_mass_live, probs = 0.75, na.rm = TRUE),
                           quantile_095_sum_mass_live = quantile(sum_mass_live, probs = 0.95, na.rm = TRUE),
                           max_sum_mass_live = max(sum_mass_live, na.rm = TRUE),
                           sd_sum_mass_live = sd(sum_mass_live, na.rm=TRUE),
                           se_sum_mass_live = std.error(sum_mass_live, na.rm=TRUE),
                           ci_lwr_sum_mass_live = ci_lower(sum_mass_live),
                           ci_upr_sum_mass_live = ci_upper(sum_mass_live))
        
        tree_mass_live_m2 <- all_tree_data %>% group_by(species_id, run_rep, env_rep,  time) %>%
          dplyr::summarise(sum_mass_live = sum((mass_leaf + mass_bark + mass_root + mass_sapwood)/100, na.rm = TRUE)) %>%
          group_by(species_id) %>%
          dplyr::summarise(min_sum_mass_live_m2 = min(sum_mass_live, na.rm = TRUE),
                           mean_sum_mass_live_m2 = mean(sum_mass_live, na.rm = TRUE),
                           median_sum_mass_live_m2 = median(sum_mass_live, na.rm = TRUE),
                           quantile_005_sum_mass_live_m2 = quantile(sum_mass_live, probs = 0.05, na.rm = TRUE),
                           quantile_025_sum_mass_live_m2 = quantile(sum_mass_live, probs = 0.25, na.rm = TRUE),
                           quantile_075_sum_mass_live_m2 = quantile(sum_mass_live, probs = 0.75, na.rm = TRUE),
                           quantile_095_sum_mass_live_m2 = quantile(sum_mass_live, probs = 0.95, na.rm = TRUE),
                           max_sum_mass_live_m2 = max(sum_mass_live, na.rm = TRUE),
                           sd_sum_mass_live_m2 = sd(sum_mass_live, na.rm=TRUE),
                           se_sum_mass_live_m2 = std.error(sum_mass_live, na.rm=TRUE),
                           ci_lwr_sum_mass_live_m2 = ci_lower(sum_mass_live),
                           ci_upr_sum_mass_live_m2 = ci_upper(sum_mass_live))
        
        tree_mass_live_ave <- all_tree_data %>% 
          group_by(species_id) %>%
          dplyr::summarise(min_mass_live = min((mass_leaf + mass_bark + mass_root + mass_sapwood), na.rm = TRUE),
                           mean_mass_live = mean((mass_leaf + mass_bark + mass_root + mass_sapwood), na.rm = TRUE),
                           median_mass_live = median((mass_leaf + mass_bark + mass_root + mass_sapwood), na.rm = TRUE),
                           quantile_005_mass_live = quantile((mass_leaf + mass_bark + mass_root + mass_sapwood), probs = 0.05, na.rm = TRUE),
                           quantile_025_mass_live = quantile((mass_leaf + mass_bark + mass_root + mass_sapwood), probs = 0.25, na.rm = TRUE),
                           quantile_075_mass_live = quantile((mass_leaf + mass_bark + mass_root + mass_sapwood), probs = 0.75, na.rm = TRUE),
                           quantile_095_mass_live = quantile((mass_leaf + mass_bark + mass_root + mass_sapwood), probs = 0.95, na.rm = TRUE),
                           max_mass_live = max((mass_leaf + mass_bark + mass_root + mass_sapwood), na.rm = TRUE),
                           sd_mass_live = sd((mass_leaf + mass_bark + mass_root + mass_sapwood), na.rm=TRUE),
                           se_mass_live = std.error((mass_leaf + mass_bark + mass_root + mass_sapwood), na.rm=TRUE),
                           ci_lwr_mass_live = ci_lower((mass_leaf + mass_bark + mass_root + mass_sapwood)),
                           ci_upr_mass_live = ci_upper((mass_leaf + mass_bark + mass_root + mass_sapwood)))
      }
      ######## MASS Aboveground ########
      {
        tree_sum_mass_aboveground <- all_tree_data %>% group_by(species_id, run_rep,  env_rep, time) %>%
          dplyr::summarise(sum_mass_aboveground = sum((mass_leaf + mass_bark + mass_heartwood + mass_sapwood), na.rm = TRUE)) %>%
          group_by(species_id) %>%
          dplyr::summarise(min_sum_mass_aboveground = min(sum_mass_aboveground, na.rm = TRUE),
                           mean_sum_mass_aboveground = mean(sum_mass_aboveground, na.rm = TRUE),
                           median_sum_mass_aboveground = median(sum_mass_aboveground, na.rm = TRUE),
                           quantile_005_sum_mass_aboveground = quantile(sum_mass_aboveground, probs = 0.05, na.rm = TRUE),
                           quantile_025_sum_mass_aboveground = quantile(sum_mass_aboveground, probs = 0.25, na.rm = TRUE),
                           quantile_075_sum_mass_aboveground = quantile(sum_mass_aboveground, probs = 0.75, na.rm = TRUE),
                           quantile_095_sum_mass_aboveground = quantile(sum_mass_aboveground, probs = 0.95, na.rm = TRUE),
                           max_sum_mass_aboveground = max(sum_mass_aboveground, na.rm = TRUE),
                           sd_sum_mass_aboveground = sd(sum_mass_aboveground, na.rm=TRUE),
                           se_sum_mass_aboveground = std.error(sum_mass_aboveground, na.rm=TRUE),
                           ci_lwr_sum_mass_aboveground = ci_lower(sum_mass_aboveground),
                           ci_upr_sum_mass_aboveground = ci_upper(sum_mass_aboveground))
        
        tree_mass_aboveground_m2 <- all_tree_data %>% group_by(species_id, run_rep, env_rep,  time) %>%
          dplyr::summarise(sum_mass_aboveground = sum((mass_leaf + mass_bark + mass_heartwood + mass_sapwood)/100, na.rm = TRUE)) %>%
          group_by(species_id) %>%
          dplyr::summarise(min_sum_mass_aboveground_m2 = min(sum_mass_aboveground, na.rm = TRUE),
                           mean_sum_mass_aboveground_m2 = mean(sum_mass_aboveground, na.rm = TRUE),
                           median_sum_mass_aboveground_m2 = median(sum_mass_aboveground, na.rm = TRUE),
                           quantile_005_sum_mass_aboveground_m2 = quantile(sum_mass_aboveground, probs = 0.05, na.rm = TRUE),
                           quantile_025_sum_mass_aboveground_m2 = quantile(sum_mass_aboveground, probs = 0.25, na.rm = TRUE),
                           quantile_075_sum_mass_aboveground_m2 = quantile(sum_mass_aboveground, probs = 0.75, na.rm = TRUE),
                           quantile_095_sum_mass_aboveground_m2 = quantile(sum_mass_aboveground, probs = 0.95, na.rm = TRUE),
                           max_sum_mass_aboveground_m2 = max(sum_mass_aboveground, na.rm = TRUE),
                           sd_sum_mass_aboveground_m2 = sd(sum_mass_aboveground, na.rm=TRUE),
                           se_sum_mass_aboveground_m2 = std.error(sum_mass_aboveground, na.rm=TRUE),
                           ci_lwr_sum_mass_aboveground_m2 = ci_lower(sum_mass_aboveground),
                           ci_upr_sum_mass_aboveground_m2 = ci_upper(sum_mass_aboveground))
        
        tree_mass_aboveground_ave <- all_tree_data %>% 
          group_by(species_id) %>%
          dplyr::summarise(min_mass_aboveground = min((mass_leaf + mass_bark + mass_heartwood + mass_sapwood), na.rm = TRUE),
                           mean_mass_aboveground = mean((mass_leaf + mass_bark + mass_heartwood + mass_sapwood), na.rm = TRUE),
                           median_mass_aboveground = median((mass_leaf + mass_bark + mass_heartwood + mass_sapwood), na.rm = TRUE),
                           quantile_005_mass_aboveground = quantile((mass_leaf + mass_bark + mass_heartwood + mass_sapwood), probs = 0.05, na.rm = TRUE),
                           quantile_025_mass_aboveground = quantile((mass_leaf + mass_bark + mass_heartwood + mass_sapwood), probs = 0.25, na.rm = TRUE),
                           quantile_075_mass_aboveground = quantile((mass_leaf + mass_bark + mass_heartwood + mass_sapwood), probs = 0.75, na.rm = TRUE),
                           quantile_095_mass_aboveground = quantile((mass_leaf + mass_bark + mass_heartwood + mass_sapwood), probs = 0.95, na.rm = TRUE),
                           max_mass_aboveground = max((mass_leaf + mass_bark + mass_heartwood + mass_sapwood), na.rm = TRUE),
                           sd_mass_aboveground = sd((mass_leaf + mass_bark + mass_heartwood + mass_sapwood), na.rm=TRUE),
                           se_mass_aboveground = std.error((mass_leaf + mass_bark + mass_heartwood + mass_sapwood), na.rm=TRUE),
                           ci_lwr_mass_aboveground = ci_lower((mass_leaf + mass_bark + mass_heartwood + mass_sapwood)),
                           ci_upr_mass_aboveground = ci_upper((mass_leaf + mass_bark + mass_heartwood + mass_sapwood)))
      }
      ######## MASS STEM #########
      {
        tree_sum_mass_stem <- all_tree_data %>% group_by(species_id, run_rep, env_rep,  time) %>%
          dplyr::summarise(sum_mass_stem = sum((mass_bark + mass_heartwood + mass_sapwood), na.rm = TRUE)) %>%
          group_by(species_id) %>%
          dplyr::summarise(min_sum_mass_stem = min(sum_mass_stem, na.rm = TRUE),
                           mean_sum_mass_stem = mean(sum_mass_stem, na.rm = TRUE),
                           median_sum_mass_stem = median(sum_mass_stem, na.rm = TRUE),
                           quantile_005_sum_mass_stem = quantile(sum_mass_stem, probs = 0.05, na.rm = TRUE),
                           quantile_025_sum_mass_stem = quantile(sum_mass_stem, probs = 0.25, na.rm = TRUE),
                           quantile_075_sum_mass_stem = quantile(sum_mass_stem, probs = 0.75, na.rm = TRUE),
                           quantile_095_sum_mass_stem = quantile(sum_mass_stem, probs = 0.95, na.rm = TRUE),
                           max_sum_mass_stem = max(sum_mass_stem, na.rm = TRUE),
                           sd_sum_mass_stem = sd(sum_mass_stem, na.rm=TRUE),
                           se_sum_mass_stem = std.error(sum_mass_stem, na.rm=TRUE),
                           ci_lwr_sum_mass_stem = ci_lower(sum_mass_stem),
                           ci_upr_sum_mass_stem = ci_upper(sum_mass_stem))
        
        tree_mass_stem_m2 <- all_tree_data %>% group_by(species_id, run_rep, env_rep,  time) %>%
          dplyr::summarise(sum_mass_stem = sum((mass_bark + mass_heartwood + mass_sapwood)/100, na.rm = TRUE)) %>%
          group_by(species_id) %>%
          dplyr::summarise(min_sum_mass_stem_m2 = min(sum_mass_stem, na.rm = TRUE),
                           mean_sum_mass_stem_m2 = mean(sum_mass_stem, na.rm = TRUE),
                           median_sum_mass_stem_m2 = median(sum_mass_stem, na.rm = TRUE),
                           quantile_005_sum_mass_stem_m2 = quantile(sum_mass_stem, probs = 0.05, na.rm = TRUE),
                           quantile_025_sum_mass_stem_m2 = quantile(sum_mass_stem, probs = 0.25, na.rm = TRUE),
                           quantile_075_sum_mass_stem_m2 = quantile(sum_mass_stem, probs = 0.75, na.rm = TRUE),
                           quantile_095_sum_mass_stem_m2 = quantile(sum_mass_stem, probs = 0.95, na.rm = TRUE),
                           max_sum_mass_stem_m2 = max(sum_mass_stem, na.rm = TRUE),
                           sd_sum_mass_stem_m2 = sd(sum_mass_stem, na.rm=TRUE),
                           se_sum_mass_stem_m2 = std.error(sum_mass_stem, na.rm=TRUE),
                           ci_lwr_sum_mass_stem_m2 = ci_lower(sum_mass_stem),
                           ci_upr_sum_mass_stem_m2 = ci_upper(sum_mass_stem))
        
        tree_mass_stem_ave <- all_tree_data %>% 
          group_by(species_id) %>%
          dplyr::summarise(min_mass_stem = min((mass_bark + mass_heartwood + mass_sapwood), na.rm = TRUE),
                           mean_mass_stem = mean((mass_bark + mass_heartwood + mass_sapwood), na.rm = TRUE),
                           median_mass_stem = median((mass_bark + mass_heartwood + mass_sapwood), na.rm = TRUE),
                           quantile_005_mass_stem = quantile((mass_bark + mass_heartwood + mass_sapwood), probs = 0.05, na.rm = TRUE),
                           quantile_025_mass_stem = quantile((mass_bark + mass_heartwood + mass_sapwood), probs = 0.25, na.rm = TRUE),
                           quantile_075_mass_stem = quantile((mass_bark + mass_heartwood + mass_sapwood), probs = 0.75, na.rm = TRUE),
                           quantile_095_mass_stem = quantile((mass_bark + mass_heartwood + mass_sapwood), probs = 0.95, na.rm = TRUE),
                           max_mass_stem = max((mass_bark + mass_heartwood + mass_sapwood), na.rm = TRUE),
                           sd_mass_stem = sd((mass_bark + mass_heartwood + mass_sapwood), na.rm=TRUE),
                           se_mass_stem = std.error((mass_bark + mass_heartwood + mass_sapwood), na.rm=TRUE),
                           ci_lwr_mass_stem = ci_lower((mass_bark + mass_heartwood + mass_sapwood)),
                           ci_upr_mass_stem = ci_upper((mass_bark + mass_heartwood + mass_sapwood)))
      }
      ######## MORTALITY RISK FULL ########
      {
        tree_mort_risk_ave <- all_tree_data %>% 
          group_by(species_id) %>%
          dplyr::summarise(min_mortality_risk = min(mortality_new, na.rm = TRUE),
                           mean_mortality_risk = mean(mortality_new, na.rm = TRUE),
                           median_mortality_risk = median(mortality_new, na.rm = TRUE),
                           quantile_005_mortality_risk = quantile(mortality_new, probs = 0.05, na.rm = TRUE),
                           quantile_025_mortality_risk = quantile(mortality_new, probs = 0.25, na.rm = TRUE),
                           quantile_075_mortality_risk = quantile(mortality_new, probs = 0.75, na.rm = TRUE),
                           quantile_095_mortality_risk = quantile(mortality_new, probs = 0.95, na.rm = TRUE),
                           max_mortality_risk = max(mortality_new, na.rm = TRUE),
                           sd_mortality_risk = sd(mortality_new, na.rm=TRUE),
                           se_mortality_risk = std.error(mortality_new, na.rm=TRUE),
                           ci_lwr_mortality_risk = ci_lower(mortality_new),
                           ci_upr_mortality_risk = ci_upper(mortality_new))
      }
      ######## MORTALITY RISK STORAGE DEPENDENT #########
      {
        tree_mort_risk_storage_ave <- all_tree_data %>% 
          group_by(species_id) %>%
          dplyr::summarise(min_mortality_risk_storage = min(mortality_storage_dependent, na.rm = TRUE),
                           mean_mortality_risk_storage = mean(mortality_storage_dependent, na.rm = TRUE),
                           median_mortality_risk_storage = median(mortality_storage_dependent, na.rm = TRUE),
                           quantile_005_mortality_risk_storage = quantile(mortality_storage_dependent, probs = 0.05, na.rm = TRUE),
                           quantile_025_mortality_risk_storage = quantile(mortality_storage_dependent, probs = 0.25, na.rm = TRUE),
                           quantile_075_mortality_risk_storage = quantile(mortality_storage_dependent, probs = 0.75, na.rm = TRUE),
                           quantile_095_mortality_risk_storage = quantile(mortality_storage_dependent, probs = 0.95, na.rm = TRUE),
                           max_mortality_risk_storage = max(mortality_storage_dependent, na.rm = TRUE),
                           sd_mortality_risk_storage = sd(mortality_storage_dependent, na.rm=TRUE),
                           se_mortality_risk_storage = std.error(mortality_storage_dependent, na.rm=TRUE),
                           ci_lwr_mortality_risk_storage = ci_lower(mortality_storage_dependent),
                           ci_upr_mortality_risk_storage = ci_upper(mortality_storage_dependent))
      }
      ######## MORTALITY RISK GROWTH DEPENDENT #########
      {
        tree_mort_risk_growth_ave <- all_tree_data %>% 
          group_by(species_id) %>%
          dplyr::summarise(min_mortality_risk_growth = min(mortality_growth_dependent, na.rm = TRUE),
                           mean_mortality_risk_growth = mean(mortality_growth_dependent, na.rm = TRUE),
                           median_mortality_risk_growth = median(mortality_growth_dependent, na.rm = TRUE),
                           quantile_005_mortality_risk_growth = quantile(mortality_growth_dependent, probs = 0.05, na.rm = TRUE),
                           quantile_025_mortality_risk_growth = quantile(mortality_growth_dependent, probs = 0.25, na.rm = TRUE),
                           quantile_075_mortality_risk_growth = quantile(mortality_growth_dependent, probs = 0.75, na.rm = TRUE),
                           quantile_095_mortality_risk_growth = quantile(mortality_growth_dependent, probs = 0.95, na.rm = TRUE),
                           max_mortality_risk_growth = max(mortality_growth_dependent, na.rm = TRUE),
                           sd_mortality_risk_growth = sd(mortality_growth_dependent, na.rm=TRUE),
                           se_mortality_risk_growth = std.error(mortality_growth_dependent, na.rm=TRUE),
                           ci_lwr_mortality_risk_growth = ci_lower(mortality_growth_dependent),
                           ci_upr_mortality_risk_growth = ci_upper(mortality_growth_dependent))
      }
      ######## FECUNDITY #########
      {
        tree_sum_fecundity <- all_tree_data %>% group_by(species_id, run_rep,  env_rep, time) %>%
          dplyr::summarise(fecundity_sum = sum(fecundity, na.rm = TRUE)) %>%
          group_by(species_id) %>%
          dplyr::summarise(min_fecundity_sum = min(fecundity_sum, na.rm = TRUE),
                           mean_fecundity_sum = mean(fecundity_sum, na.rm = TRUE),
                           median_fecundity_sum = median(fecundity_sum, na.rm = TRUE),
                           quantile_005_fecundity_sum = quantile(fecundity_sum, probs = 0.05, na.rm = TRUE),
                           quantile_025_fecundity_sum = quantile(fecundity_sum, probs = 0.25, na.rm = TRUE),
                           quantile_075_fecundity_sum = quantile(fecundity_sum, probs = 0.75, na.rm = TRUE),
                           quantile_095_fecundity_sum = quantile(fecundity_sum, probs = 0.95, na.rm = TRUE),
                           max_fecundity_sum = max(fecundity_sum, na.rm = TRUE),
                           sd_fecundity_sum = sd(fecundity_sum, na.rm=TRUE),
                           se_fecundity_sum = std.error(fecundity_sum, na.rm=TRUE),
                           ci_lwr_fecundity_sum = ci_lower(fecundity_sum),
                           ci_upr_fecundity_sum = ci_upper(fecundity_sum))
        
        tree_fecundity <- all_tree_data %>% 
          group_by(species_id) %>%
          dplyr::summarise(min_fecundity = min(fecundity, na.rm = TRUE),
                           mean_fecundity = mean(fecundity, na.rm = TRUE),
                           median_fecundity = median(fecundity, na.rm = TRUE),
                           quantile_005_fecundity = quantile(fecundity, probs = 0.05, na.rm = TRUE),
                           quantile_025_fecundity = quantile(fecundity, probs = 0.25, na.rm = TRUE),
                           quantile_075_fecundity = quantile(fecundity, probs = 0.75, na.rm = TRUE),
                           quantile_095_fecundity = quantile(fecundity, probs = 0.95, na.rm = TRUE),
                           max_fecundity = max(fecundity, na.rm = TRUE),
                           sd_fecundity = sd(fecundity, na.rm=TRUE),
                           se_fecundity = std.error(fecundity, na.rm=TRUE),
                           ci_lwr_fecundity = ci_lower(fecundity),
                           ci_upr_fecundity = ci_upper(fecundity))
      }
      ######## FECUNDITY DT #########
      {
        tree_sum_fecundity_dt <- all_tree_data %>% group_by(species_id, run_rep, env_rep,  time) %>%
          dplyr::summarise(fecundity_sum = sum(fecundity_dt_abs, na.rm = TRUE)) %>%
          group_by(species_id) %>%
          dplyr::summarise(min_fecundity_dt_sum = min(fecundity_sum, na.rm = TRUE),
                           mean_fecundity_dt_sum = mean(fecundity_sum, na.rm = TRUE),
                           median_fecundity_dt_sum = median(fecundity_sum, na.rm = TRUE),
                           quantile_005_fecundity_dt_sum = quantile(fecundity_sum, probs = 0.05, na.rm = TRUE),
                           quantile_025_fecundity_dt_sum = quantile(fecundity_sum, probs = 0.25, na.rm = TRUE),
                           quantile_075_fecundity_dt_sum = quantile(fecundity_sum, probs = 0.75, na.rm = TRUE),
                           quantile_095_fecundity_dt_sum = quantile(fecundity_sum, probs = 0.95, na.rm = TRUE),
                           max_fecundity_dt_sum = max(fecundity_sum, na.rm = TRUE),
                           sd_fecundity_dt_sum = sd(fecundity_sum, na.rm=TRUE),
                           se_fecundity_dt_sum = std.error(fecundity_sum, na.rm=TRUE),
                           ci_lwr_fecundity_dt_sum = ci_lower(fecundity_sum),
                           ci_upr_fecundity_dt_sum = ci_upper(fecundity_sum))
        
        tree_fecundity_dt <- all_tree_data %>% 
          group_by(species_id) %>%
          dplyr::summarise(min_fecundity_dt = min(fecundity_dt_abs, na.rm = TRUE),
                           mean_fecundity_dt = mean(fecundity_dt_abs, na.rm = TRUE),
                           median_fecundity_dt = median(fecundity_dt_abs, na.rm = TRUE),
                           quantile_005_fecundity_dt = quantile(fecundity_dt_abs, probs = 0.05, na.rm = TRUE),
                           quantile_025_fecundity_dt = quantile(fecundity_dt_abs, probs = 0.25, na.rm = TRUE),
                           quantile_075_fecundity_dt = quantile(fecundity_dt_abs, probs = 0.75, na.rm = TRUE),
                           quantile_095_fecundity_dt = quantile(fecundity_dt_abs, probs = 0.95, na.rm = TRUE),
                           max_fecundity_dt = max(fecundity_dt_abs, na.rm = TRUE),
                           sd_fecundity_dt = sd(fecundity_dt_abs, na.rm=TRUE),
                           se_fecundity_dt = std.error(fecundity_dt_abs, na.rm=TRUE),
                           ci_lwr_fecundity_dt = ci_lower(fecundity_dt_abs),
                           ci_upr_fecundity_dt = ci_upper(fecundity_dt_abs))
      }
      ######## NUMBER OF LIVE TREES ########
      {
        tree_num_tree <- all_tree_data %>% group_by(species_id, run_rep, env_rep, time) %>%
          dplyr::summarise(num_tree = n()) %>%
          group_by(species_id) %>%
          dplyr::summarise(min_num_tree = min(num_tree, na.rm = TRUE),
                           mean_num_tree = mean(num_tree, na.rm = TRUE),
                           median_num_tree = median(num_tree, na.rm = TRUE),
                           quantile_005_num_tree = quantile(num_tree, probs = 0.05, na.rm = TRUE),
                           quantile_025_num_tree = quantile(num_tree, probs = 0.25, na.rm = TRUE),
                           quantile_075_num_tree = quantile(num_tree, probs = 0.75, na.rm = TRUE),
                           quantile_095_num_tree = quantile(num_tree, probs = 0.95, na.rm = TRUE),
                           max_num_tree = max(num_tree, na.rm = TRUE),
                           sd_num_tree = sd(num_tree, na.rm=TRUE),
                           se_num_tree = std.error(num_tree, na.rm=TRUE),
                           ci_lwr_num_tree = ci_lower(num_tree),
                           ci_upr_num_tree = ci_upper(num_tree))
      }
      
      ######## Add time #########
      tree_height_ave$time = i + 5
      tree_height_max$time = i + 5
      
      tree_basal_ave$time = i + 5
      tree_sum_basal_ha$time = i + 5
      tree_sum_basal$time = i + 5
      
      tree_sum_leaf_area$time = i + 5
      tree_leaf_area_ave$time = i + 5
      
      tree_sum_lai$time = i + 5
      
      tree_sum_mass_storage$time = i + 5
      tree_mass_storage_m2$time = i + 5
      tree_mass_storage_ave$time = i + 5
      
      tree_sum_mass_leaf$time = i + 5
      tree_mass_leaf_m2$time = i + 5
      tree_mass_leaf_ave$time = i + 5
      
      tree_sum_mass_root$time = i + 5
      tree_mass_root_m2$time = i + 5
      tree_mass_root_ave$time = i + 5
      
      tree_sum_mass_heartwood$time = i + 5
      tree_mass_heartwood_m2$time = i + 5
      tree_mass_heartwood_ave$time = i + 5
      
      tree_sum_mass_bark$time = i + 5
      tree_mass_bark_m2$time = i + 5
      tree_mass_bark_ave$time = i + 5
      
      tree_sum_mass_sapwood$time = i + 5
      tree_mass_sapwood_m2$time = i + 5
      tree_mass_sapwood_ave$time = i + 5
      
      tree_sum_mass_live$time = i + 5
      tree_mass_live_m2$time = i + 5
      tree_mass_live_ave$time = i + 5
      
      tree_sum_mass_aboveground$time = i + 5
      tree_mass_aboveground_m2$time = i + 5
      tree_mass_aboveground_ave$time = i + 5
      
      tree_sum_mass_stem$time = i + 5
      tree_mass_stem_m2$time = i + 5
      tree_mass_stem_ave$time = i + 5
      
      tree_mort_risk_ave$time = i + 5
      tree_mort_risk_storage_ave$time = i + 5
      tree_mort_risk_growth_ave$time = i + 5
      
      tree_sum_fecundity$time = i + 5
      tree_fecundity$time = i + 5
      
      tree_sum_fecundity_dt$time = i + 5
      tree_fecundity_dt$time = i + 5
      
      tree_num_tree$time = i + 5
      
      ######### MERGE DATA ##########
      
      tree_data_height <- merge(tree_height_ave, 
                                tree_height_max, 
                                by=(c("time", "species_id")), all= TRUE)
      tree_data_basal <- merge(tree_basal_ave,
                               merge(tree_sum_basal_ha,
                                     tree_sum_basal,
                                     by=(c("time", "species_id")), all= TRUE),
                               by=(c("time", "species_id")), all= TRUE)
      tree_leaf_area <- merge(tree_sum_leaf_area, 
                              merge(tree_leaf_area_ave, 
                                    tree_sum_lai,
                                    by=(c("time", "species_id")), all= TRUE),
                              by=(c("time", "species_id")), all= TRUE)
      
      tree_mass_storage <- merge(tree_sum_mass_storage, 
                                 merge(tree_mass_storage_m2, 
                                       tree_mass_storage_ave,
                                       by=(c("time", "species_id")), all= TRUE),
                                 by=(c("time", "species_id")), all= TRUE)
      
      tree_mass_leaf <- merge(tree_sum_mass_leaf, 
                              merge(tree_mass_leaf_m2, 
                                    tree_mass_leaf_ave, 
                                    by=(c("time", "species_id")), all= TRUE),
                              by=(c("time", "species_id")), all= TRUE)
      
      tree_mass_root<- merge(tree_sum_mass_root, 
                             merge(tree_mass_root_m2, 
                                   tree_mass_root_ave, 
                                   by=(c("time", "species_id")), all= TRUE),
                             by=(c("time", "species_id")), all= TRUE)
      
      tree_mass_bark <- merge(tree_sum_mass_bark, 
                              merge(tree_mass_bark_m2, 
                                    tree_mass_bark_ave, 
                                    by=(c("time", "species_id")), all= TRUE),
                              by=(c("time", "species_id")), all= TRUE)
      
      tree_mass_sapwood <- merge(tree_sum_mass_sapwood, 
                                 merge(tree_mass_sapwood_m2, 
                                       tree_mass_sapwood_ave, 
                                       by=(c("time", "species_id")), all= TRUE),
                                 by=(c("time", "species_id")), all= TRUE)
      
      tree_mass_heartwood <- merge(tree_sum_mass_heartwood, 
                                   merge(tree_mass_heartwood_m2, 
                                         tree_mass_heartwood_ave, 
                                         by=(c("time", "species_id")), all= TRUE),
                                   by=(c("time", "species_id")), all= TRUE)
      
      tree_mass_live <- merge(tree_sum_mass_live, 
                              merge(tree_mass_live_m2, 
                                    tree_mass_live_ave, 
                                    by=(c("time", "species_id")), all= TRUE),
                              by=(c("time", "species_id")), all= TRUE)
      
      tree_mass_aboveground <- merge(tree_sum_mass_aboveground, 
                                     merge(tree_mass_aboveground_m2, 
                                           tree_mass_aboveground_ave, 
                                           by=(c("time", "species_id")), all= TRUE),
                                     by=(c("time", "species_id")), all= TRUE)
      
      tree_mass_stem <- merge(tree_sum_mass_stem, 
                              merge(tree_mass_stem_m2, 
                                    tree_mass_stem_ave, 
                                    by=(c("time", "species_id")), all= TRUE),
                              by=(c("time", "species_id")), all= TRUE)
      
      tree_mort_risk <- merge(tree_mort_risk_ave,
                              merge(tree_mort_risk_storage_ave,
                                    tree_mort_risk_growth_ave,
                                    by=(c("time", "species_id")), all= TRUE),
                              by=(c("time", "species_id")), all= TRUE)
      
      tree_fecundity <- merge(tree_sum_fecundity,
                              tree_fecundity,
                              by=(c("time", "species_id")), all= TRUE)
      
      tree_fecundity_dt <- merge(tree_sum_fecundity_dt,
                                 tree_fecundity_dt,
                                 by=(c("time", "species_id")), all= TRUE)
      
      tree_data <- merge(tree_num_tree, tree_data_height, by=(c("time", "species_id")), all= TRUE)
      tree_data <- merge(tree_data, tree_data_basal, by=(c("time", "species_id")), all= TRUE)
      tree_data <- merge(tree_data, tree_leaf_area, by=(c("time", "species_id")), all= TRUE)
      tree_data <- merge(tree_data, tree_mass_storage, by=(c("time", "species_id")), all= TRUE)
      tree_data <- merge(tree_data, tree_mass_leaf, by=(c("time", "species_id")), all= TRUE)
      tree_data <- merge(tree_data, tree_mass_root, by=(c("time", "species_id")), all= TRUE)
      tree_data <- merge(tree_data, tree_mass_bark, by=(c("time", "species_id")), all= TRUE)
      tree_data <- merge(tree_data, tree_mass_sapwood, by=(c("time", "species_id")), all= TRUE)
      tree_data <- merge(tree_data, tree_mass_heartwood, by=(c("time", "species_id")), all= TRUE)
      tree_data <- merge(tree_data, tree_mass_live, by=(c("time", "species_id")), all= TRUE)
      tree_data <- merge(tree_data, tree_mass_aboveground, by=(c("time", "species_id")), all= TRUE)
      tree_data <- merge(tree_data, tree_mass_stem, by=(c("time", "species_id")), all= TRUE)
      tree_data <- merge(tree_data, tree_mort_risk, by=(c("time", "species_id")), all= TRUE)
      tree_data <- merge(tree_data, tree_fecundity, by=(c("time", "species_id")), all= TRUE)
      tree_data <- merge(tree_data, tree_fecundity_dt, by=(c("time", "species_id")), all= TRUE)
      
      
      ########## END ##########
      
      return(tree_data)
    }
    else{
      return(data.frame())
    }
  }))
  
  return(all_data)
})

save.image("full_data.RData")


lapply(14:length(out_env_run), function(ind){
  
  print(ind)
  dir = out_dirs_env[[ind]]
  print(dir)
  
  # tree_data = out_env[[ind]]
  # 
  # if(length(tree_data)  ==0){
  #   return()
  # }
  # 
  # dir = out_dirs_env_total[[ind]]
  # path_parts <- unlist(str_split(dir, "/"))
  # new_dir_path <- file.path("out", "full_run_auto", "environment_agg", path_parts[3])
  
  tree_data = out_env_run[[ind]]

  if(length(tree_data)  ==0){
    return()
  }

  # dir = out_dirs_env[[ind]]
  path_parts <- unlist(str_split(dir, "/"))
  print(path_parts)
  new_dir_path <- file.path("out", "full_run_auto", "environment_run_agg", path_parts[3], path_parts[4])

  
  print(new_dir_path)
  
  dir.create(new_dir_path, recursive = TRUE)
  
  scm_ribbon <- scale_color_manual("Allocation \nStrategy:",
                            values=foxes_palettes$main[c(2, 1, 4, 3)],
                            labels=c("Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe"),
                            guide = guide_legend(override.aes = list(fill=foxes_palettes$main[c(2, 1, 4, 3)],
                                                                     alpha = 0.5)))
  scm <- scale_color_manual("Allocation \nStrategy:",
                            values=foxes_palettes$main[c(2, 1, 4, 3)],
                            labels=c("Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe"))
  
  sfm <- scale_fill_manual(values = foxes_palettes$main[c(2, 1, 4, 3)]) 
  
  gr_ribbon <- geom_ribbon(alpha = 0.35, linetype = "blank")
  
  foxes_theme <-       theme_bw() +
    theme(panel.grid.major = element_blank(),
          legend.position = "right",
          plot.margin = margin(1,1,1,1, "cm"),
          text=element_text(size=12, color = "#083855", family="Lato Light"),
          axis.text=element_text(size=8, color = "#083855", family="Lato Light"),
          strip.background = element_rect(fill = "white")) 
  
  species_wrap <- facet_wrap(.~species_id, nrow = 2, ncol = 2, labeller = labeller(species_id = c("1" = "Slow-Risky", 
                                                                                                  "2" = "Slow-Safe",
                                                                                                  "3" = "Fast-Risky", 
                                                                                                  "4" = "Fast-Safe")))
  full_dir_path <- new_dir_path
  ######### HEIGHT ###########
  {
  new_dir_path <- file.path(full_dir_path, "height")
  dir.create(new_dir_path, recursive = TRUE)
  
  ### PLOT MAXIMUM HEIGHT (mean)
  {
    p <- ggplot(tree_data, aes(x = time, y = mean_max_height, 
                               ymin = ci_lwr_max_height, ymax = ci_upr_max_height,
                               color = as.factor(species_id), fill = as.factor(species_id))) + 
      geom_line() +
      geom_ribbon(alpha = 0.5, linetype = "blank") +
      scm_ribbon + sfm +
      scale_x_continuous("Time (yrs)") +
      scale_y_continuous("Average Maximum Height (m) in Run") +
      species_wrap + 
      guides(fill = FALSE) +
      foxes_theme
    p
    
    file_figure <- tempfile(paste("mean_max_height_ci", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
    ggsave(file_figure, plot = p, device = NULL, path = NULL,
           scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
           units =  "mm")
  }
  
  {
    p <- ggplot(tree_data, aes(x = time, y = mean_max_height, 
                               ymin = mean_max_height - sd_max_height, ymax= mean_max_height + sd_max_height,
                               color = as.factor(species_id), fill = as.factor(species_id))) + 
      geom_line() +
      gr_ribbon + scm_ribbon + sfm +
      scale_x_continuous("Time (yrs)") +
      scale_y_continuous("Average Maximum Height (m) in Run") +
      species_wrap +
      guides(fill = FALSE) +
      foxes_theme
    p
    
    file_figure <- tempfile(paste("mean_max_height_sd", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
    ggsave(file_figure, plot = p, device = NULL, path = NULL,
           scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
           units =  "mm")
  }
  
  ### PLOT MAXIMUM HEIGHT (mean) together
  {
    p <- ggplot(tree_data, aes(x = time, y = mean_max_height, 
                               ymin = ci_lwr_max_height, ymax = ci_upr_max_height,
                               color = as.factor(species_id), fill = as.factor(species_id))) + 
      geom_line() +
      gr_ribbon + scm_ribbon + sfm +
      scale_x_continuous("Time (yrs)") +
      scale_y_continuous("Average Maximum Height (m) in Run") +
      guides(fill = FALSE) +
      foxes_theme
    p
    
    file_figure <- tempfile(paste("mean_max_grouped_height_ci", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
    ggsave(file_figure, plot = p, device = NULL, path = NULL,
           scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
           units =  "mm")
  } 
  
  {
    p <- ggplot(tree_data, aes(x = time, y = mean_max_height, 
                               ymin = mean_max_height - sd_max_height, ymax = mean_max_height + sd_max_height,
                               color = as.factor(species_id), fill = as.factor(species_id))) + 
      geom_line() +
      gr_ribbon + scm_ribbon + sfm +
      scale_x_continuous("Time (yrs)") +
      scale_y_continuous("Average Maximum Height (m) in Run") +
      guides(fill = FALSE) +
      foxes_theme
    p
    
    file_figure <- tempfile(paste("mean_max_grouped_height", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
    ggsave(file_figure, plot = p, device = NULL, path = NULL,
           scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
           units =  "mm")
  } 
  
  ### PLOT MAXIMUM HEIGHT Ind Strategy
  {
    p <- ggplot(tree_data, aes(x = time, y = max_max_height, color = as.factor(species_id))) + 
      geom_line() +
      scale_color_manual("Allocation \nStrategy:",
                         values=foxes_palettes$main[c(2, 1, 4, 3)],
                         labels=c("Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe")) +
      scale_x_continuous("Time (yrs)") +
      scale_y_continuous("Maximum Height (m) in All Runs") +
      species_wrap +
      foxes_theme
    p
    
    file_figure <- tempfile(paste("max_height", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
    ggsave(file_figure, plot = p, device = NULL, path = NULL,
           scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
           units =  "mm")
  }
  
  ### PLOT MAXIMUM HEIGHT together
  {
    p <- ggplot(tree_data, aes(x = time, y = max_max_height, color = as.factor(species_id))) + 
      geom_point() +
      geom_line() +
      scale_color_manual("Allocation \nStrategy:",
                         values=foxes_palettes$main[c(2, 1, 4, 3)], 
                         labels=c("Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe")) +
      scale_x_continuous("Time (yrs)") +
      scale_y_continuous("Maximum Height (m) Per Sub-Environment Repetition") +
      foxes_theme
    p
    
    file_figure <- tempfile(paste("max_grouped_height", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
    ggsave(file_figure, plot = p, device = NULL, path = NULL,
           scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
           units =  "mm")
  }
  
  ### MEADIN PLANT HEIGHT 
  {
    p <- ggplot(tree_data, aes(x = time, y = median_height, 
                               colour = as.factor(species_id), fill = as.factor(species_id))) +
      geom_line() +
      geom_ribbon(aes(ymin = quantile_005_height, ymax = quantile_095_height), alpha = 0.15, linetype = "blank") + 
      geom_ribbon(aes(ymin = quantile_025_height, ymax = quantile_075_height), alpha = 0.35, linetype = "blank") + 
      scm_ribbon + sfm +
      scale_x_continuous("Time (yrs)") +
      scale_y_continuous("Median Plant Height (m) in all runs") +
      species_wrap +
      guides(fill = FALSE) +
      foxes_theme
    p
    
    file_figure <- tempfile(paste("height", "median_iqr", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
    ggsave(file_figure, plot = p, device = NULL, path = NULL,
           scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
           units =  "mm")
  }
  
  ### MEAN PLANT HEIGHT
  {
    p <- ggplot(tree_data, aes(x = time, y = mean_height, 
                                 ymin = ci_lwr_height, ymax = ci_upr_height,
                                 colour = as.factor(species_id), fill = as.factor(species_id))) +
      geom_line() +
      gr_ribbon + scm_ribbon + sfm +
      scale_x_continuous("Time (yrs)") +
      scale_y_continuous("Mean Plant Height (m) in Sub-Environment Repetition") +
      species_wrap +
      guides(fill = FALSE) +
      foxes_theme
    p
    
    file_figure <- tempfile(paste("height", "mean_ci", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
    ggsave(file_figure, plot = p, device = NULL, path = NULL,
           scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
           units =  "mm")
  }
  
  {
    p <- ggplot(tree_data, aes(x = time, y = mean_height, 
                               ymin = mean_height - sd_height, ymax = mean_height + sd_height,
                               colour = as.factor(species_id), fill = as.factor(species_id))) +
      geom_line() +
      gr_ribbon + scm_ribbon + sfm +
      scale_x_continuous("Time (yrs)") +
      scale_y_continuous("Mean Plant Height (m) in Sub-Environment Repetition") +
      species_wrap +
      guides(fill = FALSE) +
      foxes_theme
    p
    
    file_figure <- tempfile(paste("height", "mean_sd", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
    ggsave(file_figure, plot = p, device = NULL, path = NULL,
           scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
           units =  "mm")
  }
  
  ### MEAN PLANT HEIGHT (grouped)
  {
    p <- ggplot(tree_data, aes(x = time, y = mean_height, 
                               ymin = ci_lwr_height, ymax = ci_upr_height,
                               colour = as.factor(species_id), fill = as.factor(species_id))) +
      geom_line() +
      gr_ribbon + scm_ribbon + sfm +
      scale_x_continuous("Time (yrs)") +
      scale_y_continuous("Mean Plant Height (m) in Sub-Environment Repetition") +
      guides(fill = FALSE) +
      foxes_theme
    p
    
    file_figure <- tempfile(paste("height", "mean_ci", "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
    ggsave(file_figure, plot = p, device = NULL, path = NULL,
           scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
           units =  "mm")
  }
  
  {
    p <- ggplot(tree_data, aes(x = time, y = mean_height, 
                               ymin = mean_height - sd_height, ymax = mean_height + sd_height,
                               colour = as.factor(species_id), fill = as.factor(species_id))) +
      geom_line() +
      gr_ribbon + scm_ribbon + sfm +
      scale_x_continuous("Time (yrs)") +
      scale_y_continuous("Mean Plant Height (m) in Sub-Environment Repetition") +
      guides(fill = FALSE) +
      foxes_theme
    p
    
    file_figure <- tempfile(paste("height", "mean_sd", "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
    ggsave(file_figure, plot = p, device = NULL, path = NULL,
           scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
           units =  "mm")
  }
  }
  
  ######### BASAL AREA ###########
  {
  new_dir_path <- file.path(full_dir_path, "basal")
  dir.create(new_dir_path, recursive = TRUE)
  ### PLOT SUM BASAL (mean)
  {
    p <- ggplot(tree_data, aes(x = time, y = mean_sum_basal, 
                               ymin = ci_lwr_sum_basal, ymax = ci_upr_sum_basal,
                               color = as.factor(species_id), fill = as.factor(species_id))) + 
      geom_line() +
      gr_ribbon + scm_ribbon + sfm +
      scale_x_continuous("Time (yrs)") +
      scale_y_continuous("Average Basal Area Coverage (m2/100m2) in Run") +
      species_wrap +
      guides(fill = FALSE) +
      foxes_theme
    p
    
    file_figure <- tempfile(paste("mean_basal_sum_ci", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
    ggsave(file_figure, plot = p, device = NULL, path = NULL,
           scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
           units =  "mm")
  }
  
  {
    p <- ggplot(tree_data, aes(x = time, y = mean_sum_basal, 
                               ymin = mean_sum_basal - sd_sum_basal, ymax = mean_sum_basal + sd_sum_basal,
                               color = as.factor(species_id), fill = as.factor(species_id))) + 
      geom_line() +
      gr_ribbon + scm_ribbon + sfm +
      scale_x_continuous("Time (yrs)") +
      scale_y_continuous("Average Basal Area Coverage (m2/100m2) in Run") +
      species_wrap +
      guides(fill = FALSE) +
      foxes_theme
    p
    
    file_figure <- tempfile(paste("mean_basal_sum_sd", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
    ggsave(file_figure, plot = p, device = NULL, path = NULL,
           scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
           units =  "mm")
  }
  
  ### PLOT SUM BASAL (mean) together
  {
    p <- ggplot(tree_data, aes(x = time, y = mean_sum_basal, 
                               ymin = ci_lwr_sum_basal, ymax = ci_upr_sum_basal,
                               color = as.factor(species_id), fill = as.factor(species_id))) + 
      geom_line() +
      gr_ribbon + scm_ribbon + sfm +
      scale_x_continuous("Time (yrs)") +
      scale_y_continuous("Average Basal Area Coverage (m2/100m2) in Run") +
      guides(fill = FALSE) +
      foxes_theme
    p
    
    file_figure <- tempfile(paste("mean_basal_sum_ci", "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
    ggsave(file_figure, plot = p, device = NULL, path = NULL,
           scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
           units =  "mm")
  }
  
  {
    p <- ggplot(tree_data, aes(x = time, y = mean_sum_basal, 
                               ymin = mean_sum_basal - sd_sum_basal, ymax = mean_sum_basal + sd_sum_basal,
                               color = as.factor(species_id), fill = as.factor(species_id))) + 
      geom_line() +
      gr_ribbon + scm_ribbon + sfm +
      scale_x_continuous("Time (yrs)") +
      scale_y_continuous("Average Basal Area Coverage (m2/100m2) in Run") +
      guides(fill = FALSE) +
      foxes_theme
    p
    
    file_figure <- tempfile(paste("mean_basal_sum_sd", "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
    ggsave(file_figure, plot = p, device = NULL, path = NULL,
           scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
           units =  "mm")
  }
  
  ### PLOT SUM BASAL (mean) HA
  {
    p <- ggplot(tree_data, aes(x = time, y = mean_sum_basal_ha, 
                               ymin = ci_lwr_sum_basal_ha, ymax = ci_upr_sum_basal_ha,
                               color = as.factor(species_id), fill = as.factor(species_id))) + 
      geom_line() +
      gr_ribbon + scm_ribbon + sfm +
      scale_x_continuous("Time (yrs)") +
      scale_y_continuous("Average Basal Area Coverage per Hectar (m2/100m2) in Run") +
      species_wrap +
      guides(fill = FALSE) +
      foxes_theme
    p
    
    file_figure <- tempfile(paste("mean_basal_sum_ci", "ha", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
    ggsave(file_figure, plot = p, device = NULL, path = NULL,
           scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
           units =  "mm")
  }
  
  {
    p <- ggplot(tree_data, aes(x = time, y = mean_sum_basal_ha, 
                               ymin = mean_sum_basal_ha - sd_sum_basal_ha, ymax = mean_sum_basal_ha + sd_sum_basal_ha,
                               color = as.factor(species_id), fill = as.factor(species_id))) + 
      geom_line() +
      gr_ribbon + scm_ribbon + sfm +
      scale_x_continuous("Time (yrs)") +
      scale_y_continuous("Average Basal Area Coverage per Hectar (m2/ha) in Run") +
      species_wrap +
      guides(fill = FALSE) +
      foxes_theme
    p
    
    file_figure <- tempfile(paste("mean_basal_sum_sd", "ha", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
    ggsave(file_figure, plot = p, device = NULL, path = NULL,
           scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
           units =  "mm")
  }
  
  ### PLOT SUM BASAL (mean) HA together
  {
    p <- ggplot(tree_data, aes(x = time, y = mean_sum_basal_ha, 
                               ymin = ci_lwr_sum_basal_ha, ymax = ci_upr_sum_basal_ha,
                               color = as.factor(species_id), fill = as.factor(species_id))) + 
      geom_line() +
      gr_ribbon + scm_ribbon + sfm +
      scale_x_continuous("Time (yrs)") +
      scale_y_continuous("Average Basal Area Coverage (m2/ha) in Run") +
      guides(fill = FALSE) +
      foxes_theme
    p
    
    file_figure <- tempfile(paste("mean_basal_sum_ci", "ha", "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
    ggsave(file_figure, plot = p, device = NULL, path = NULL,
           scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
           units =  "mm")
  }
  
  {
    p <- ggplot(tree_data, aes(x = time, y = mean_sum_basal, 
                               ymin = mean_sum_basal - sd_sum_basal, ymax = mean_sum_basal + sd_sum_basal,
                               color = as.factor(species_id), fill = as.factor(species_id))) + 
      geom_line() +
      gr_ribbon + scm_ribbon + sfm +
      scale_x_continuous("Time (yrs)") +
      scale_y_continuous("Average Basal Area Coverage per Hectar (m2/ha) in Run") +
      guides(fill = FALSE) +
      foxes_theme
    p
    
    file_figure <- tempfile(paste("mean_basal_sum_sd", "ha", "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
    ggsave(file_figure, plot = p, device = NULL, path = NULL,
           scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
           units =  "mm")
  }
  
  ### PLOT MAXIMUM Basal Area Ind Strategy
  {
    p <- ggplot(tree_data, aes(x = time, y = max_basal, color = as.factor(species_id))) + 
      geom_line() + geom_point() +
      scm +
      scale_x_continuous("Time (yrs)") +
      scale_y_continuous("Maximum Basal Area per Plant (m2) In Run") +
      species_wrap +
      guides(fill = FALSE) +
      foxes_theme
    p
    
    file_figure <- tempfile(paste("max_basal", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
    ggsave(file_figure, plot = p, device = NULL, path = NULL,
           scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
           units =  "mm")
  }
  
  ### PLOT MAXIMUM Basal Area Ind together
  {
    p <- ggplot(tree_data, aes(x = time, y = max_basal, color = as.factor(species_id))) + 
      geom_line() +
      geom_point() +
      scm + 
      scale_x_continuous("Time (yrs)") +
      scale_y_continuous("Maximum Basal Area per Plant (m2) Per Sub-Environment Repetition") +
      guides(fill = FALSE) +
      foxes_theme
    p
    
    file_figure <- tempfile(paste("max_grouped_basal", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
    ggsave(file_figure, plot = p, device = NULL, path = NULL,
           scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
           units =  "mm")
  }
  
  ### MEADIN Basal Area Ind together
  {
    p <- ggplot(tree_data, aes(x = time, y = median_basal, color = as.factor(species_id), fill = as.factor(species_id))) + 
      geom_ribbon(aes(ymin = quantile_005_basal, ymax = quantile_095_basal), alpha = 0.15, linetype = "blank") + 
      geom_ribbon(aes(ymin = quantile_025_basal, ymax = quantile_075_basal), alpha = 0.35, linetype = "blank") + 
      geom_line() +
      scm_ribbon + sfm +
      scale_x_continuous("Time (yrs)") +
      scale_y_continuous("Median Basal Area per Plant (m2) Per Sub-Environment Repetition") +
      species_wrap +
      guides(fill = FALSE) +
      theme_bw() +
      foxes_theme
    p
    
    file_figure <- tempfile(paste("basal", "median_iqr", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
    ggsave(file_figure, plot = p, device = NULL, path = NULL,
           scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
           units =  "mm")
  }
  
  ### MEAN Basal Area Ind
  {
    p <- ggplot(tree_data, aes(x = time, y = mean_basal, 
                               ymin = ci_lwr_basal, ymax = ci_upr_basal,
                               colour = as.factor(species_id), fill = as.factor(species_id))) +
      geom_line() +
      gr_ribbon + scm_ribbon + sfm +
      scale_x_continuous("Time (yrs)") +
      scale_y_continuous("Mean Plant Basal Area (m2) in Run") +
      species_wrap +
      guides(fill = FALSE) +
      foxes_theme
    p
    
    file_figure <- tempfile(paste("basal", "mean_ci", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
    ggsave(file_figure, plot = p, device = NULL, path = NULL,
           scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
           units =  "mm")
  }
  
  {
    p <- ggplot(tree_data, aes(x = time, y = mean_basal, 
                               ymin = mean_basal - sd_basal, ymax = mean_basal + sd_basal,
                               colour = as.factor(species_id), fill = as.factor(species_id))) +
      geom_line() +
      gr_ribbon + scm_ribbon + sfm +
      scale_x_continuous("Time (yrs)") +
      scale_y_continuous("Mean Plant Basal Area (m2) in Run") +
      species_wrap +
      guides(fill = FALSE) +
      foxes_theme
    p
    
    file_figure <- tempfile(paste("basal", "mean_sd", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
    ggsave(file_figure, plot = p, device = NULL, path = NULL,
           scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
           units =  "mm")
  }
  
  ### MEAN Basal Area Ind (grouped)
  {
    p <- ggplot(tree_data, aes(x = time, y = mean_basal, 
                               ymin = ci_lwr_basal, ymax = ci_upr_basal,
                               colour = as.factor(species_id), fill = as.factor(species_id))) +
      geom_line() +
      gr_ribbon + scm_ribbon + sfm +
      scale_x_continuous("Time (yrs)") +
      scale_y_continuous("Mean Plant Basal Area (m2) in Run") +
      guides(fill = FALSE) +
      foxes_theme
    p
    
    file_figure <- tempfile(paste("basal", "mean_ci", "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
    ggsave(file_figure, plot = p, device = NULL, path = NULL,
           scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
           units =  "mm")
  }
  
  {
    p <- ggplot(tree_data, aes(x = time, y = mean_basal, 
                               ymin = mean_basal - sd_basal, ymax = mean_basal + sd_basal,
                               colour = as.factor(species_id), fill = as.factor(species_id))) +
      geom_line() +
      gr_ribbon + scm_ribbon + sfm +
      scale_x_continuous("Time (yrs)") +
      scale_y_continuous("Mean Plant Basal Area (m2) in Run") +
      guides(fill = FALSE) +
      foxes_theme
    p
    
    file_figure <- tempfile(paste("basal", "mean_sd", "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
    ggsave(file_figure, plot = p, device = NULL, path = NULL,
           scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
           units =  "mm")
  }
  }
  
  ######### LEAF AREA ###########
  {
  new_dir_path <- file.path(full_dir_path, "leaf_area")
  dir.create(new_dir_path, recursive = TRUE)
  ### PLOT SUM LEAF AREA (mean)
  {
    p <- ggplot(tree_data, aes(x = time, y = mean_sum_leaf_area, 
                               ymin = ci_lwr_sum_leaf_area, ymax = ci_upr_sum_leaf_area,
                               color = as.factor(species_id), fill = as.factor(species_id))) + 
      geom_line() +
      gr_ribbon + scm_ribbon + sfm +
      scale_x_continuous("Time (yrs)") +
      scale_y_continuous("Average Total Leaf Area Coverage (m2/100m2) in Run") +
      species_wrap +
      guides(fill = FALSE) +
      foxes_theme
    p
    
    file_figure <- tempfile(paste("mean", "leaf_area", "ci", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
    ggsave(file_figure, plot = p, device = NULL, path = NULL,
           scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
           units =  "mm")
  }
  
  {
     p <- ggplot(tree_data, aes(x = time, y = mean_sum_leaf_area, 
                                 ymin = mean_sum_leaf_area - sd_sum_leaf_area, ymax = mean_sum_leaf_area + sd_sum_leaf_area,
                                 color = as.factor(species_id), fill = as.factor(species_id))) + 
      geom_line() +
      gr_ribbon + scm_ribbon + sfm +
      scale_x_continuous("Time (yrs)") +
      scale_y_continuous("Average Total Leaf Area Coverage (m2/100m2) in Run") +
      species_wrap +
      guides(fill = FALSE) +
      foxes_theme
    p
    
    file_figure <- tempfile(paste("mean", "leaf_area", "sd", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
    ggsave(file_figure, plot = p, device = NULL, path = NULL,
           scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
           units =  "mm")
  }
  
  ### PLOT SUM LEAF AREA (mean) together
  {
    p <- ggplot(tree_data, aes(x = time, y = mean_sum_leaf_area, 
                               ymin = ci_lwr_sum_leaf_area, ymax = ci_upr_sum_leaf_area,
                               color = as.factor(species_id), fill = as.factor(species_id))) + 
      geom_line() +
      gr_ribbon + scm_ribbon + sfm +
      scale_x_continuous("Time (yrs)") +
      scale_y_continuous("Average Total Leaf Area Coverage (m2/100m2) in Run") +
      guides(fill = FALSE) +
      foxes_theme
    p
    
    file_figure <- tempfile(paste("mean", "leaf_area", "ci", "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
    ggsave(file_figure, plot = p, device = NULL, path = NULL,
           scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
           units =  "mm")
  }
  
  {
    p <- ggplot(tree_data, aes(x = time, y = mean_sum_leaf_area, 
                               ymin = mean_sum_leaf_area - sd_sum_leaf_area, ymax = mean_sum_leaf_area + sd_sum_leaf_area,
                               color = as.factor(species_id), fill = as.factor(species_id))) + 
      geom_line() +
      gr_ribbon + scm_ribbon + sfm +
      scale_x_continuous("Time (yrs)") +
      scale_y_continuous("Average Total Leaf Area Coverage (m2/100m2) in Run") +
      guides(fill = FALSE) +
      foxes_theme
    p
    
    file_figure <- tempfile(paste("mean", "leaf_area", "sd", "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
    ggsave(file_figure, plot = p, device = NULL, path = NULL,
           scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
           units =  "mm")
  }
  
  ### PLOT SUM LEAF AREA (mean) LAI
  {
    p <- ggplot(tree_data, aes(x = time, y = mean_sum_lai, 
                               ymin = ci_lwr_sum_lai, ymax = ci_upr_sum_lai,
                               color = as.factor(species_id), fill = as.factor(species_id))) + 
      geom_line() +
      gr_ribbon + scm_ribbon + sfm +
      scale_x_continuous("Time (yrs)") +
      scale_y_continuous("Average Leaf Area Index (m2/m2) in Run") +
      species_wrap +
      guides(fill = FALSE) +
      foxes_theme
    p
    
    file_figure <- tempfile(paste("mean", "lai", "ci", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
    ggsave(file_figure, plot = p, device = NULL, path = NULL,
           scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
           units =  "mm")
  }
  
  {
    p <- ggplot(tree_data, aes(x = time, y = mean_sum_lai, 
                               ymin = mean_sum_lai - sd_sum_lai, ymax = mean_sum_lai + sd_sum_lai,
                               color = as.factor(species_id), fill = as.factor(species_id))) +  
      geom_line() +
      gr_ribbon + scm_ribbon + sfm +
      scale_x_continuous("Time (yrs)") +
      scale_y_continuous("Average Leaf Area Index (m2/m2) in Run") +
      species_wrap +
      guides(fill = FALSE) +
      foxes_theme
    p
    
    file_figure <- tempfile(paste("mean", "lai", "sd", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
    ggsave(file_figure, plot = p, device = NULL, path = NULL,
           scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
           units =  "mm")
  }
  
  ### PLOT SUM LEAF AREA (mean) LAI together
  {
    p <- ggplot(tree_data, aes(x = time, y = mean_sum_lai, 
                               ymin = ci_lwr_sum_lai, ymax = ci_upr_sum_lai,
                               color = as.factor(species_id), fill = as.factor(species_id))) + 
      geom_line() +
      gr_ribbon + scm_ribbon + sfm +
      scale_x_continuous("Time (yrs)") +
      scale_y_continuous("Average Leaf Area Index (m2/m2) in Run") +
      guides(fill = FALSE) +
      foxes_theme
    p
    
    file_figure <- tempfile(paste("mean", "lai", "ci","group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
    ggsave(file_figure, plot = p, device = NULL, path = NULL,
           scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
           units =  "mm")
  }
  
  {
    p <- ggplot(tree_data, aes(x = time, y = mean_sum_lai, 
                               ymin = mean_sum_lai - sd_sum_lai, ymax = mean_sum_lai + sd_sum_lai,
                               color = as.factor(species_id), fill = as.factor(species_id))) +  
      geom_line() +
      gr_ribbon + scm_ribbon + sfm +
      scale_x_continuous("Time (yrs)") +
      scale_y_continuous("Average Leaf Area Index (m2/m2) in Run") +
      guides(fill = FALSE) +
      foxes_theme
    p
    
    file_figure <- tempfile(paste("mean", "lai", "sd", "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
    ggsave(file_figure, plot = p, device = NULL, path = NULL,
           scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
           units =  "mm")
  }
  
  ### PLOT MAXIMUM LEAF AREA Ind Strategy
  {
    p <- ggplot(tree_data, aes(x = time, y = max_leaf_area, color = as.factor(species_id))) + 
      geom_line() + geom_point() +
      scm +
      scale_x_continuous("Time (yrs)") +
      scale_y_continuous("Maximum Leaf Area per Plant (m2) In Run") +
      species_wrap +
      guides(fill = FALSE) +
      foxes_theme
    p
    
    file_figure <- tempfile(paste("max", "leaf_area", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
    ggsave(file_figure, plot = p, device = NULL, path = NULL,
           scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
           units =  "mm")
  }
  
  ###  PLOT MAXIMUM LEAF AREA Ind together
  {
    p <- ggplot(tree_data, aes(x = time, y = max_leaf_area, color = as.factor(species_id))) + 
      geom_line() +
      geom_point() +
      scm + 
      scale_x_continuous("Time (yrs)") +
      scale_y_continuous("Maximum Leaf Area per Plant (m2) In Run") +
      guides(fill = FALSE) +
      foxes_theme
    p
    
    file_figure <- tempfile(paste("max", "leaf_area", "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
    ggsave(file_figure, plot = p, device = NULL, path = NULL,
           scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
           units =  "mm")
  }
  
  ### MEADIN LEAF Area Ind together
  {
    p <- ggplot(tree_data, aes(x = time, y = median_leaf_area, color = as.factor(species_id), fill = as.factor(species_id))) + 
      geom_ribbon(aes(ymin = quantile_005_leaf_area, ymax = quantile_095_leaf_area), alpha = 0.15, linetype = "blank") + 
      geom_ribbon(aes(ymin = quantile_025_leaf_area, ymax = quantile_075_leaf_area), alpha = 0.35, linetype = "blank") + 
      geom_line() +
      scm_ribbon + sfm +
      scale_x_continuous("Time (yrs)") +
      scale_y_continuous("Median Leaf Area per Plant (m2) In Run") +
      species_wrap +
      guides(fill = FALSE) +
      theme_bw() +
      foxes_theme
    p
    
    file_figure <- tempfile(paste("leaf_area", "median_iqr", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
    ggsave(file_figure, plot = p, device = NULL, path = NULL,
           scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
           units =  "mm")
  }
  
  ### MEAN LEAF Area Ind
  {
    p <- ggplot(tree_data, aes(x = time, y = mean_leaf_area, 
                               ymin = ci_lwr_leaf_area, ymax = ci_upr_leaf_area,
                               colour = as.factor(species_id), fill = as.factor(species_id))) +
      geom_line() +
      gr_ribbon + scm_ribbon + sfm +
      scale_x_continuous("Time (yrs)") +
      scale_y_continuous("Mean Plant Leaf Area (m2) in Run") +
      species_wrap +
      guides(fill = FALSE) +
      foxes_theme
    p
    
    file_figure <- tempfile(paste("leaf_area", "mean_ci", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
    ggsave(file_figure, plot = p, device = NULL, path = NULL,
           scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
           units =  "mm")
  }
  
  {
    p <- ggplot(tree_data, aes(x = time, y = mean_leaf_area, 
                               ymin = mean_leaf_area - sd_leaf_area, ymax = mean_leaf_area + sd_leaf_area,
                               colour = as.factor(species_id), fill = as.factor(species_id))) +
      geom_line() +
      gr_ribbon + scm_ribbon + sfm +
      scale_x_continuous("Time (yrs)") +
      scale_y_continuous("Mean Plant Leaf Area (m2) in Run") +
      species_wrap +
      guides(fill = FALSE) +
      foxes_theme
    p
    
    file_figure <- tempfile(paste("leaf_area", "mean_sd", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
    ggsave(file_figure, plot = p, device = NULL, path = NULL,
           scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
           units =  "mm")
  }
  
  ### MEAN LEAF Area Ind (grouped)
  {
    p <- ggplot(tree_data, aes(x = time, y = mean_leaf_area, 
                               ymin = ci_lwr_leaf_area, ymax = ci_upr_leaf_area,
                               colour = as.factor(species_id), fill = as.factor(species_id))) +
      geom_line() +
      gr_ribbon + scm_ribbon + sfm +
      scale_x_continuous("Time (yrs)") +
      scale_y_continuous("Mean Plant Leaf Area (m2) in Run") +
      guides(fill = FALSE) +
      foxes_theme
    p
    
    file_figure <- tempfile(paste("leaf_area", "mean_ci", "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
    ggsave(file_figure, plot = p, device = NULL, path = NULL,
           scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
           units =  "mm")
  }
  
  {
    p <- ggplot(tree_data, aes(x = time, y = mean_leaf_area, 
                               ymin = mean_leaf_area - sd_leaf_area, ymax = mean_leaf_area + sd_leaf_area,
                               colour = as.factor(species_id), fill = as.factor(species_id))) +
      geom_line() +
      gr_ribbon + scm_ribbon + sfm +
      scale_x_continuous("Time (yrs)") +
      scale_y_continuous("Mean Plant Leaf Area (m2) in Run") +
      guides(fill = FALSE) +
      foxes_theme
    p
    
    file_figure <- tempfile(paste("leaf_area", "mean_sd", "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
    ggsave(file_figure, plot = p, device = NULL, path = NULL,
           scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
           units =  "mm")
  }
  }
  
  ######### LEAF MASS ###########
  {
  new_dir_path <- file.path(full_dir_path, "leaf_mass")
  dir.create(new_dir_path, recursive = TRUE)
  file_name_in <- "leaf_mass"
  ### PLOT SUM LEAF MASS (mean)
  {
    p <- ggplot(tree_data, aes(x = time, y = mean_sum_mass_leaf, 
                               ymin = ci_lwr_sum_mass_leaf, ymax = ci_upr_sum_mass_leaf,
                               color = as.factor(species_id), fill = as.factor(species_id))) + 
      geom_line() +
      gr_ribbon + scm_ribbon + sfm +
      scale_x_continuous("Time (yrs)") +
      scale_y_continuous("Average Total Leaf Mass (kg/100m2) in Run") +
      species_wrap +
      guides(fill = FALSE) +
      foxes_theme
    p
    
    file_figure <- tempfile(paste("mean", file_name_in, "ci", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
    ggsave(file_figure, plot = p, device = NULL, path = NULL,
           scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
           units =  "mm")
  }
  
  {
    p <- ggplot(tree_data, aes(x = time, y = mean_sum_mass_leaf, 
                               ymin = mean_sum_mass_leaf - sd_sum_mass_leaf, ymax = mean_sum_mass_leaf + sd_sum_mass_leaf,
                               color = as.factor(species_id), fill = as.factor(species_id))) + 
      geom_line() +
      gr_ribbon + scm_ribbon + sfm +
      scale_x_continuous("Time (yrs)") +
      scale_y_continuous("Average Total Leaf Mass (kg/100m2) in Run") +
      species_wrap +
      guides(fill = FALSE) +
      foxes_theme
    p
    
    file_figure <- tempfile(paste("mean", file_name_in, "sd", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
    ggsave(file_figure, plot = p, device = NULL, path = NULL,
           scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
           units =  "mm")
  }
  
  ### PLOT SUM LEAF AREA (mean) together
  {
    p <- ggplot(tree_data, aes(x = time, y = mean_sum_mass_leaf, 
                               ymin = ci_lwr_sum_mass_leaf, ymax = ci_upr_sum_mass_leaf,
                               color = as.factor(species_id), fill = as.factor(species_id))) + 
      geom_line() +
      gr_ribbon + scm_ribbon + sfm +
      scale_x_continuous("Time (yrs)") +
      scale_y_continuous("Average Total Leaf Mass (kg/100m2) in Run") +
      guides(fill = FALSE) +
      foxes_theme
    p
    
    file_figure <- tempfile(paste("mean", file_name_in, "ci", "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
    ggsave(file_figure, plot = p, device = NULL, path = NULL,
           scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
           units =  "mm")
  }
  
  {
    p <- ggplot(tree_data, aes(x = time, y = mean_sum_mass_leaf, 
                               ymin = mean_sum_mass_leaf - sd_sum_mass_leaf, ymax = mean_sum_mass_leaf + sd_sum_mass_leaf,
                               color = as.factor(species_id), fill = as.factor(species_id))) + 
      geom_line() +
      gr_ribbon + scm_ribbon + sfm +
      scale_x_continuous("Time (yrs)") +
      scale_y_continuous("Average Total Leaf Mass (kg/100m2) in Run") +
      guides(fill = FALSE) +
      foxes_theme
    p
    
    file_figure <- tempfile(paste("mean", file_name_in, "sd", "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
    ggsave(file_figure, plot = p, device = NULL, path = NULL,
           scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
           units =  "mm")
  }
  
  ### PLOT SUM LEAF AREA (mean) per m2
  {
    p <- ggplot(tree_data, aes(x = time, y = mean_sum_mass_leaf_m2, 
                               ymin = ci_lwr_sum_mass_leaf_m2, ymax = ci_upr_sum_mass_leaf_m2,
                               color = as.factor(species_id), fill = as.factor(species_id))) + 
      geom_line() +
      gr_ribbon + scm_ribbon + sfm +
      scale_x_continuous("Time (yrs)") +
      scale_y_continuous("Average Total Leaf Mass (kgC/m2) in Run") +
      species_wrap +
      guides(fill = FALSE) +
      foxes_theme
    p
    
    file_figure <- tempfile(paste("mean", file_name_in, "m2", "ci", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
    ggsave(file_figure, plot = p, device = NULL, path = NULL,
           scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
           units =  "mm")
  }
  
  {
    p <- ggplot(tree_data, aes(x = time, y = mean_sum_mass_leaf_m2, 
                               ymin = mean_sum_mass_leaf_m2 - sd_sum_mass_leaf_m2, ymax = mean_sum_mass_leaf_m2 + sd_sum_mass_leaf_m2, 
                               color = as.factor(species_id), fill = as.factor(species_id))) +  
      geom_line() +
      gr_ribbon + scm_ribbon + sfm +
      scale_x_continuous("Time (yrs)") +
      scale_y_continuous("Average Total Leaf Mass (kgC/m2) in Run") +
      species_wrap +
      guides(fill = FALSE) +
      foxes_theme
    p
    
    file_figure <- tempfile(paste("mean", file_name_in, "m2", "sd", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
    ggsave(file_figure, plot = p, device = NULL, path = NULL,
           scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
           units =  "mm")
  }
  
  ### PLOT SUM LEAF MASS (mean) per m2 together
  {
    p <- ggplot(tree_data, aes(x = time, y = mean_sum_mass_leaf_m2, 
                               ymin = ci_lwr_sum_mass_leaf_m2, ymax = ci_upr_sum_mass_leaf_m2,
                               color = as.factor(species_id), fill = as.factor(species_id))) + 
      geom_line() +
      gr_ribbon + scm_ribbon + sfm +
      scale_x_continuous("Time (yrs)") +
      scale_y_continuous("Average Total Leaf Mass (kgC/m2) in Run") +
      guides(fill = FALSE) +
      foxes_theme
    p
    
    file_figure <- tempfile(paste("mean", file_name_in, "m2", "ci", "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
    ggsave(file_figure, plot = p, device = NULL, path = NULL,
           scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
           units =  "mm")
  }
  
  {
    p <- ggplot(tree_data, aes(x = time, y = mean_sum_mass_leaf_m2, 
                               ymin = mean_sum_mass_leaf_m2 - sd_sum_mass_leaf_m2,ymax = mean_sum_mass_leaf_m2 + sd_sum_mass_leaf_m2, 
                               color = as.factor(species_id), fill = as.factor(species_id))) +  
      geom_line() +
      gr_ribbon + scm_ribbon + sfm +
      scale_x_continuous("Time (yrs)") +
      scale_y_continuous("Average Total Leaf Mass (kgC/m2) in Run") +
      guides(fill = FALSE) +
      foxes_theme
    p
    
    file_figure <- tempfile(paste("mean", file_name_in, "m2", "sd", "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
    ggsave(file_figure, plot = p, device = NULL, path = NULL,
           scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
           units =  "mm")
  }
  
  ### PLOT MAXIMUM LEAF MASS Ind Strategy
  {
    p <- ggplot(tree_data, aes(x = time, y = max_mass_leaf, color = as.factor(species_id))) + 
      geom_line() + geom_point() +
      scm +
      scale_x_continuous("Time (yrs)") +
      scale_y_continuous("Maximum Leaf Mass per Plant (kgC) In Run") +
      species_wrap +
      guides(fill = FALSE) +
      foxes_theme
    p
    
    file_figure <- tempfile(paste("max", file_name_in, sep = "_"), tmpdir = new_dir_path, fileext = ".png")
    ggsave(file_figure, plot = p, device = NULL, path = NULL,
           scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
           units =  "mm")
  }
  
  ###  PLOT MAXIMUM LEAF AREA Ind together
  {
    p <- ggplot(tree_data, aes(x = time, y = max_mass_leaf, color = as.factor(species_id))) + 
      geom_line() +
      geom_point() +
      scm + 
      scale_x_continuous("Time (yrs)") +
      scale_y_continuous("Maximum Mass Leaf per Plant (kgC) In Run") +
      guides(fill = FALSE) +
      foxes_theme
    p
    
    file_figure <- tempfile(paste("max", file_name_in, "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
    ggsave(file_figure, plot = p, device = NULL, path = NULL,
           scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
           units =  "mm")
  }
  
  ### MEADIN LEAF MASS Ind together
  {
    p <- ggplot(tree_data, aes(x = time, y = median_mass_leaf, color = as.factor(species_id), fill = as.factor(species_id))) + 
      geom_ribbon(aes(ymin = quantile_005_mass_leaf, ymax = quantile_095_mass_leaf), alpha = 0.15, linetype = "blank") + 
      geom_ribbon(aes(ymin = quantile_025_mass_leaf, ymax = quantile_075_mass_leaf), alpha = 0.35, linetype = "blank") + 
      geom_line() +
      scm_ribbon + sfm +
      scale_x_continuous("Time (yrs)") +
      scale_y_continuous("Median Leaf Mass per Plant (kgC) In Run") +
      species_wrap +
      guides(fill = FALSE) +
      theme_bw() +
      foxes_theme
    p
    
    file_figure <- tempfile(paste(file_name_in, "median_iqr", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
    ggsave(file_figure, plot = p, device = NULL, path = NULL,
           scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
           units =  "mm")
  }
  
  ### MEAN LEAF MASS Ind
  {
    p <- ggplot(tree_data, aes(x = time, y = mean_mass_leaf,
                               ymin = ci_lwr_mass_leaf, ymax = ci_upr_mass_leaf,
                               colour = as.factor(species_id), fill = as.factor(species_id))) +
      geom_line() +
      gr_ribbon + scm_ribbon + sfm +
      scale_x_continuous("Time (yrs)") +
      scale_y_continuous("Mean Plant Leaf Mass (kgC) in Run") +
      species_wrap +
      guides(fill = FALSE) +
      foxes_theme
    p
    
    file_figure <- tempfile(paste(file_name_in, "mean_ci", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
    ggsave(file_figure, plot = p, device = NULL, path = NULL,
           scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
           units =  "mm")
  }
  
  {
    p <- ggplot(tree_data, aes(x = time, y = mean_mass_leaf, 
                               ymin = mean_mass_leaf - sd_mass_leaf, ymax = mean_mass_leaf + sd_mass_leaf,
                               colour = as.factor(species_id), fill = as.factor(species_id))) +
      geom_line() +
      gr_ribbon + scm_ribbon + sfm +
      scale_x_continuous("Time (yrs)") +
      scale_y_continuous("Mean Plant Leaf Mass (kgC) in Run") +
      species_wrap +
      guides(fill = FALSE) +
      foxes_theme
    p
    
    file_figure <- tempfile(paste(file_name_in, "mean_sd", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
    ggsave(file_figure, plot = p, device = NULL, path = NULL,
           scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
           units =  "mm")
  }
  
  ### MEAN LEAF MASS Ind (grouped)
  {
    p <- ggplot(tree_data, aes(x = time, y = mean_mass_leaf, 
                               ymin = ci_lwr_mass_leaf, ymax = ci_upr_mass_leaf,
                               colour = as.factor(species_id), fill = as.factor(species_id))) +
      geom_line() +
      gr_ribbon + scm_ribbon + sfm +
      scale_x_continuous("Time (yrs)") +
      scale_y_continuous("Mean Plant Leaf Mass (kgC) in Run") +
      guides(fill = FALSE) +
      foxes_theme
    p
    
    file_figure <- tempfile(paste(file_name_in, "mean_ci", "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
    ggsave(file_figure, plot = p, device = NULL, path = NULL,
           scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
           units =  "mm")
  }
  
  {
    p <- ggplot(tree_data, aes(x = time, y = mean_mass_leaf, 
                               ymin = mean_mass_leaf - sd_mass_leaf, ymax = mean_mass_leaf + sd_mass_leaf,
                               colour = as.factor(species_id), fill = as.factor(species_id))) +
      geom_line() +
      gr_ribbon + scm_ribbon + sfm +
      scale_x_continuous("Time (yrs)") +
      scale_y_continuous("Mean Plant Leaf Mass (kgC) in Run") +
      guides(fill = FALSE) +
      foxes_theme
    p
    
    file_figure <- tempfile(paste(file_name_in, "mean_sd", "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
    ggsave(file_figure, plot = p, device = NULL, path = NULL,
           scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
           units =  "mm")
  }
  }
  
  ######### ROOT MASS ##########
  {
    new_dir_path <- file.path(full_dir_path, "root_mass")
    dir.create(new_dir_path, recursive = TRUE)
    file_name_in <- "root_mass"
    ### PLOT SUM root MASS (mean)
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_sum_mass_root, 
                                 ymin = ci_lwr_sum_mass_root, ymax = ci_upr_sum_mass_root,
                                 color = as.factor(species_id), fill = as.factor(species_id))) + 
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Total Root Mass (kg/100m2) in Run") +
        species_wrap +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "ci", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_sum_mass_root, 
                                 ymin = mean_sum_mass_root - sd_sum_mass_root, ymax = mean_sum_mass_root + sd_sum_mass_root,
                                 color = as.factor(species_id), fill = as.factor(species_id))) + 
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Total Root Mass (kg/100m2) in Run") +
        species_wrap +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "sd", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    ### PLOT SUM ROOT AREA (mean) together
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_sum_mass_root, 
                                 ymin = ci_lwr_sum_mass_root, ymax = ci_upr_sum_mass_root,
                                 color = as.factor(species_id), fill = as.factor(species_id))) + 
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Total Root Mass (kg/100m2) in Run") +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "ci", "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_sum_mass_root, 
                                 ymin = mean_sum_mass_root - sd_sum_mass_root, ymax = mean_sum_mass_root + sd_sum_mass_root,
                                 color = as.factor(species_id), fill = as.factor(species_id))) + 
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Total Root Mass (kg/100m2) in Run") +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "sd", "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    ### PLOT SUM ROOT AREA (mean) per m2
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_sum_mass_root_m2, 
                                 ymin = ci_lwr_sum_mass_root_m2, ymax = ci_upr_sum_mass_root_m2,
                                 color = as.factor(species_id), fill = as.factor(species_id))) + 
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Total Root Mass (kgC/m2) in Run") +
        species_wrap +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "m2", "ci", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_sum_mass_root_m2, 
                                 ymin = mean_sum_mass_root_m2 - sd_sum_mass_root_m2, ymax = mean_sum_mass_root_m2 + sd_sum_mass_root_m2, 
                                 color = as.factor(species_id), fill = as.factor(species_id))) +  
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Total Root Mass (kgC/m2) in Run") +
        species_wrap +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "m2", "sd", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    ### PLOT SUM ROOT MASS (mean) per m2 together
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_sum_mass_root_m2, 
                                 ymin = ci_lwr_sum_mass_root_m2, ymax = ci_upr_sum_mass_root_m2,
                                 color = as.factor(species_id), fill = as.factor(species_id))) + 
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Total Root Mass (kgC/m2) in Run") +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "m2", "ci", "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_sum_mass_root_m2, 
                                 ymin = mean_sum_mass_root_m2 - sd_sum_mass_root_m2,ymax = mean_sum_mass_root_m2 + sd_sum_mass_root_m2, 
                                 color = as.factor(species_id), fill = as.factor(species_id))) +  
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Total Root Mass (kgC/m2) in Run") +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "m2", "sd", "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    ### PLOT MAXIMUM ROOT MASS Ind Strategy
    {
      p <- ggplot(tree_data, aes(x = time, y = max_mass_root, color = as.factor(species_id))) + 
        geom_line() + geom_point() +
        scm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Maximum Root Mass per Plant (kgC) In Run") +
        species_wrap +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("max", file_name_in, sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    ###  PLOT MAXIMUM ROOT AREA Ind together
    {
      p <- ggplot(tree_data, aes(x = time, y = max_mass_root, color = as.factor(species_id))) + 
        geom_line() +
        geom_point() +
        scm + 
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Maximum Mass Root per Plant (kgC) In Run") +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("max", file_name_in, "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    ### MEADIN ROOT MASS Ind together
    {
      p <- ggplot(tree_data, aes(x = time, y = median_mass_root, color = as.factor(species_id), fill = as.factor(species_id))) + 
        geom_ribbon(aes(ymin = quantile_005_mass_root, ymax = quantile_095_mass_root), alpha = 0.15, linetype = "blank") + 
        geom_ribbon(aes(ymin = quantile_025_mass_root, ymax = quantile_075_mass_root), alpha = 0.35, linetype = "blank") + 
        geom_line() +
        scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Median Root Mass per Plant (kgC) In Run") +
        species_wrap +
        guides(fill = FALSE) +
        theme_bw() +
        foxes_theme
      p
      
      file_figure <- tempfile(paste(file_name_in, "median_iqr", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    ### MEAN ROOT MASS Ind
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_mass_root,
                                 ymin = ci_lwr_mass_root, ymax = ci_upr_mass_root,
                                 colour = as.factor(species_id), fill = as.factor(species_id))) +
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Mean Plant Root Mass (kgC) in Run") +
        species_wrap +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste(file_name_in, "mean_ci", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_mass_root, 
                                 ymin = mean_mass_root - sd_mass_root, ymax = mean_mass_root + sd_mass_root,
                                 colour = as.factor(species_id), fill = as.factor(species_id))) +
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Mean Plant Root Mass (kgC) in Run") +
        species_wrap +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste(file_name_in, "mean_sd", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    ### MEAN Basal Area Ind (grouped)
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_mass_root, 
                                 ymin = ci_lwr_mass_root, ymax = ci_upr_mass_root,
                                 colour = as.factor(species_id), fill = as.factor(species_id))) +
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Mean Plant Root Mass (kgC) in Run") +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste(file_name_in, "mean_ci", "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_mass_root, 
                                 ymin = mean_mass_root - sd_mass_root, ymax = mean_mass_root + sd_mass_root,
                                 colour = as.factor(species_id), fill = as.factor(species_id))) +
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Mean Plant Root Mass (kgC) in Run") +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste(file_name_in, "mean_sd", "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
  }
  
  
  ######### BARK MASS ###########
  {
    new_dir_path <- file.path(full_dir_path, "bark_mass")
    dir.create(new_dir_path, recursive = TRUE)
    file_name_in <- "bark_mass"
    
    ### PLOT SUM BARK AREA (mean) per m2
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_sum_mass_bark_m2, 
                                 ymin = ci_lwr_sum_mass_bark_m2, ymax = ci_upr_sum_mass_bark_m2,
                                 color = as.factor(species_id), fill = as.factor(species_id))) + 
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Total Bark Mass (kgC/m2) in Run") +
        species_wrap +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "m2", "ci", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_sum_mass_bark_m2, 
                                 ymin = mean_sum_mass_bark_m2 - sd_sum_mass_bark_m2, ymax = mean_sum_mass_bark_m2 + sd_sum_mass_bark_m2, 
                                 color = as.factor(species_id), fill = as.factor(species_id))) +  
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Total Bark Mass (kgC/m2) in Run") +
        species_wrap +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "m2", "sd", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    ### PLOT SUM BARK MASS (mean) per m2 together
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_sum_mass_bark_m2, 
                                 ymin = ci_lwr_sum_mass_bark_m2, ymax = ci_upr_sum_mass_bark_m2,
                                 color = as.factor(species_id), fill = as.factor(species_id))) + 
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Total Bark Mass (kgC/m2) in Run") +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "m2", "ci", "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_sum_mass_bark_m2, 
                                 ymin = mean_sum_mass_bark_m2 - sd_sum_mass_bark_m2,ymax = mean_sum_mass_bark_m2 + sd_sum_mass_bark_m2, 
                                 color = as.factor(species_id), fill = as.factor(species_id))) +  
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Total Bark Mass (kgC/m2) in Run") +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "m2", "sd", "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    ### PLOT MAXIMUM BARK MASS Ind Strategy
    {
      p <- ggplot(tree_data, aes(x = time, y = max_mass_bark, color = as.factor(species_id))) + 
        geom_line() + geom_point() +
        scm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Maximum Bark Mass per Plant (kgC) In Run") +
        species_wrap +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("max", file_name_in, sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    ###  PLOT MAXIMUM Bark AREA Ind together
    {
      p <- ggplot(tree_data, aes(x = time, y = max_mass_bark, color = as.factor(species_id))) + 
        geom_line() +
        geom_point() +
        scm + 
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Maximum Mass Bark per Plant (kgC) In Run") +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("max", file_name_in, "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    ### MEADIN BARK MASS Ind together
    {
      p <- ggplot(tree_data, aes(x = time, y = median_mass_bark, color = as.factor(species_id), fill = as.factor(species_id))) + 
        geom_ribbon(aes(ymin = quantile_005_mass_bark, ymax = quantile_095_mass_bark), alpha = 0.15, linetype = "blank") + 
        geom_ribbon(aes(ymin = quantile_025_mass_bark, ymax = quantile_075_mass_bark), alpha = 0.35, linetype = "blank") + 
        geom_line() +
        scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Median Bark Mass per Plant (kgC) In Run") +
        species_wrap +
        guides(fill = FALSE) +
        theme_bw() +
        foxes_theme
      p
      
      file_figure <- tempfile(paste(file_name_in, "median_iqr", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    ### MEAN BARK MASS Ind
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_mass_bark,
                                 ymin = ci_lwr_mass_bark, ymax = ci_upr_mass_bark,
                                 colour = as.factor(species_id), fill = as.factor(species_id))) +
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Mean Plant Bark Mass (kgC) in Run") +
        species_wrap +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste(file_name_in, "mean_ci", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_mass_bark, 
                                 ymin = mean_mass_bark - sd_mass_bark, ymax = mean_mass_bark + sd_mass_bark,
                                 colour = as.factor(species_id), fill = as.factor(species_id))) +
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Mean Plant Bark Mass (kgC) in Run") +
        species_wrap +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste(file_name_in, "mean_sd", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    ### MEAN MASS BARK Ind (grouped)
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_mass_bark, 
                                 ymin = ci_lwr_mass_bark, ymax = ci_upr_mass_bark,
                                 colour = as.factor(species_id), fill = as.factor(species_id))) +
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Mean Plant Bark Mass (kgC) in Run") +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste(file_name_in, "mean_ci", "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_mass_bark, 
                                 ymin = mean_mass_bark - sd_mass_bark, ymax = mean_mass_bark + sd_mass_bark,
                                 colour = as.factor(species_id), fill = as.factor(species_id))) +
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Mean Plant Bark Mass (kgC) in Run") +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste(file_name_in, "mean_sd", "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
  }
  
  ######### SAPWOOD MASS ###########
  {
    new_dir_path <- file.path(full_dir_path, "sapwood_mass")
    dir.create(new_dir_path, recursive = TRUE)
    file_name_in <- "sapwood_mass"
    ### PLOT SUM SAPWOOD MASS (mean)
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_sum_mass_sapwood, 
                                 ymin = ci_lwr_sum_mass_sapwood, ymax = ci_upr_sum_mass_sapwood,
                                 color = as.factor(species_id), fill = as.factor(species_id))) + 
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Total Sapwood Mass (kg/100m2) in Run") +
        species_wrap +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "ci", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_sum_mass_sapwood, 
                                 ymin = mean_sum_mass_sapwood - sd_sum_mass_sapwood, ymax = mean_sum_mass_sapwood + sd_sum_mass_sapwood,
                                 color = as.factor(species_id), fill = as.factor(species_id))) + 
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Total Sapwood Mass (kg/100m2) in Run") +
        species_wrap +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "sd", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    ### PLOT SUM SAPWOOD AREA (mean) together
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_sum_mass_sapwood, 
                                 ymin = ci_lwr_sum_mass_sapwood, ymax = ci_upr_sum_mass_sapwood,
                                 color = as.factor(species_id), fill = as.factor(species_id))) + 
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Total Sapwood Mass (kg/100m2) in Run") +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "ci", "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_sum_mass_sapwood, 
                                 ymin = mean_sum_mass_sapwood - sd_sum_mass_sapwood, ymax = mean_sum_mass_sapwood + sd_sum_mass_sapwood,
                                 color = as.factor(species_id), fill = as.factor(species_id))) + 
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Total Sapwood Mass (kg/100m2) in Run") +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "sd", "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    ### PLOT SUM SAPWOOD AREA (mean) per m2
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_sum_mass_sapwood_m2, 
                                 ymin = ci_lwr_sum_mass_sapwood_m2, ymax = ci_upr_sum_mass_sapwood_m2,
                                 color = as.factor(species_id), fill = as.factor(species_id))) + 
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Total sapwood Mass (kgC/m2) in Run") +
        species_wrap +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "m2", "ci", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_sum_mass_sapwood_m2, 
                                 ymin = mean_sum_mass_sapwood_m2 - sd_sum_mass_sapwood_m2, ymax = mean_sum_mass_sapwood_m2 + sd_sum_mass_sapwood_m2, 
                                 color = as.factor(species_id), fill = as.factor(species_id))) +  
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Total Sapwood Mass (kgC/m2) in Run") +
        species_wrap +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "m2", "sd", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    ### PLOT SUM SAPWOOD MASS (mean) per m2 together
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_sum_mass_sapwood_m2, 
                                 ymin = ci_lwr_sum_mass_sapwood_m2, ymax = ci_upr_sum_mass_sapwood_m2,
                                 color = as.factor(species_id), fill = as.factor(species_id))) + 
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Total Sapwood Mass (kgC/m2) in Run") +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "m2", "ci", "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_sum_mass_sapwood_m2, 
                                 ymin = mean_sum_mass_sapwood_m2 - sd_sum_mass_sapwood_m2,ymax = mean_sum_mass_sapwood_m2 + sd_sum_mass_sapwood_m2, 
                                 color = as.factor(species_id), fill = as.factor(species_id))) +  
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Total Sapwood Mass (kgC/m2) in Run") +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "m2", "sd", "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    ### PLOT MAXIMUM SAPWOOD MASS Ind Strategy
    {
      p <- ggplot(tree_data, aes(x = time, y = max_mass_sapwood, color = as.factor(species_id))) + 
        geom_line() + geom_point() +
        scm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Maximum Sapwood Mass per Plant (kgC) In Run") +
        species_wrap +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("max", file_name_in, sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    ###  PLOT MAXIMUM SAPWOOD AREA Ind together
    {
      p <- ggplot(tree_data, aes(x = time, y = max_mass_sapwood, color = as.factor(species_id))) + 
        geom_line() +
        geom_point() +
        scm + 
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Maximum Mass Sapwood per Plant (kgC) In Run") +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("max", file_name_in, "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    ### MEADIN SAPWOOD MASS Ind together
    {
      p <- ggplot(tree_data, aes(x = time, y = median_mass_sapwood, color = as.factor(species_id), fill = as.factor(species_id))) + 
        geom_ribbon(aes(ymin = quantile_005_mass_sapwood, ymax = quantile_095_mass_sapwood), alpha = 0.15, linetype = "blank") + 
        geom_ribbon(aes(ymin = quantile_025_mass_sapwood, ymax = quantile_075_mass_sapwood), alpha = 0.35, linetype = "blank") + 
        geom_line() +
        scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Median Sapwood Mass per Plant (kgC) In Run") +
        species_wrap +
        guides(fill = FALSE) +
        theme_bw() +
        foxes_theme
      p
      
      file_figure <- tempfile(paste(file_name_in, "median_iqr", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    ### MEAN SAPWOOD MASS Ind
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_mass_sapwood,
                                 # ymin = ci_lwr_mass_sapwood, ymax = ci_upr_mass_sapwood,
                                 ymin = ci_lwr_mass_sapwood.x, ymax = ci_upr_mass_sapwood.x,
                                 colour = as.factor(species_id), fill = as.factor(species_id))) +
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Mean Plant Sapwood Mass (kgC) in Run") +
        species_wrap +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste(file_name_in, "mean_ci", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_mass_sapwood, 
                                 ymin = mean_mass_sapwood - sd_mass_sapwood, ymax = mean_mass_sapwood + sd_mass_sapwood,
                                 colour = as.factor(species_id), fill = as.factor(species_id))) +
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Mean Plant Sapwood Mass (kgC) in Run") +
        species_wrap +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste(file_name_in, "mean_sd", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    ### MEAN MASS SAPWOOD Ind (grouped)
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_mass_sapwood, 
                                 # ymin = ci_lwr_mass_sapwood, ymax = ci_upr_mass_sapwood,
                                 ymin = ci_lwr_mass_sapwood.x, ymax = ci_upr_mass_sapwood.x,
                                 colour = as.factor(species_id), fill = as.factor(species_id))) +
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Mean Plant Sapwood Mass (kgC) in Run") +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste(file_name_in, "mean_ci", "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_mass_sapwood, 
                                 ymin = mean_mass_sapwood - sd_mass_sapwood, ymax = mean_mass_sapwood + sd_mass_sapwood,
                                 colour = as.factor(species_id), fill = as.factor(species_id))) +
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Mean Plant Sapwood Mass (kgC) in Run") +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste(file_name_in, "mean_sd", "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
  }
  
  ######### HEARTWOOD MASS ###########
  {
    new_dir_path <- file.path(full_dir_path, "heartwood_mass")
    dir.create(new_dir_path, recursive = TRUE)
    file_name_in <- "heartwood_mass"
    ### PLOT SUM HEARTWOOD MASS (mean)
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_sum_mass_heartwood, 
                                 ymin = ci_lwr_sum_mass_heartwood, ymax = ci_upr_sum_mass_heartwood,
                                 color = as.factor(species_id), fill = as.factor(species_id))) + 
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Total Heartwood Mass (kg/100m2) in Run") +
        species_wrap +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "ci", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_sum_mass_heartwood, 
                                 ymin = mean_sum_mass_heartwood - sd_sum_mass_heartwood, ymax = mean_sum_mass_heartwood + sd_sum_mass_heartwood,
                                 color = as.factor(species_id), fill = as.factor(species_id))) + 
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Total Heartwood Mass (kg/100m2) in Run") +
        species_wrap +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "sd", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    ### PLOT SUM HEARTWOOD AREA (mean) together
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_sum_mass_heartwood, 
                                 ymin = ci_lwr_sum_mass_heartwood, ymax = ci_upr_sum_mass_heartwood,
                                 color = as.factor(species_id), fill = as.factor(species_id))) + 
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Total Heartwood Mass (kg/100m2) in Run") +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "ci", "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_sum_mass_heartwood, 
                                 ymin = mean_sum_mass_heartwood - sd_sum_mass_heartwood, ymax = mean_sum_mass_heartwood + sd_sum_mass_heartwood,
                                 color = as.factor(species_id), fill = as.factor(species_id))) + 
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Total Heartwood Mass (kg/100m2) in Run") +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "sd", "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    ### PLOT SUM HEARTWOOD AREA (mean) per m2
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_sum_mass_heartwood_m2, 
                                 ymin = ci_lwr_sum_mass_heartwood_m2, ymax = ci_upr_sum_mass_heartwood_m2,
                                 color = as.factor(species_id), fill = as.factor(species_id))) + 
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Total Heartwood Mass (kgC/m2) in Run") +
        species_wrap +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "m2", "ci", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_sum_mass_heartwood_m2, 
                                 ymin = mean_sum_mass_heartwood_m2 - sd_sum_mass_heartwood_m2, ymax = mean_sum_mass_heartwood_m2 + sd_sum_mass_heartwood_m2, 
                                 color = as.factor(species_id), fill = as.factor(species_id))) +  
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Total Heartwood Mass (kgC/m2) in Run") +
        species_wrap +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "m2", "sd", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    ### PLOT SUM HEARTWOOD MASS (mean) per m2 together
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_sum_mass_heartwood_m2, 
                                 ymin = ci_lwr_sum_mass_heartwood_m2, ymax = ci_upr_sum_mass_heartwood_m2,
                                 color = as.factor(species_id), fill = as.factor(species_id))) + 
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Total Heartwood Mass (kgC/m2) in Run") +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "m2", "ci", "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_sum_mass_heartwood_m2, 
                                 ymin = mean_sum_mass_heartwood_m2 - sd_sum_mass_heartwood_m2,ymax = mean_sum_mass_heartwood_m2 + sd_sum_mass_heartwood_m2, 
                                 color = as.factor(species_id), fill = as.factor(species_id))) +  
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Total Heartwood Mass (kgC/m2) in Run") +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "m2", "sd", "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    ### PLOT MAXIMUM HEARTWOOD MASS Ind Strategy
    {
      p <- ggplot(tree_data, aes(x = time, y = max_mass_heartwood, color = as.factor(species_id))) + 
        geom_line() + geom_point() +
        scm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Maximum Heartwood Mass per Plant (kgC) In Run") +
        species_wrap +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("max", file_name_in, sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    ###  PLOT MAXIMUM HEARTWOOD AREA Ind together
    {
      p <- ggplot(tree_data, aes(x = time, y = max_mass_heartwood, color = as.factor(species_id))) + 
        geom_line() +
        geom_point() +
        scm + 
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Maximum Mass Heartwood per Plant (kgC) In Run") +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("max", file_name_in, "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    ### MEADIN HEARTWOOD MASS Ind together
    {
      p <- ggplot(tree_data, aes(x = time, y = median_mass_heartwood, color = as.factor(species_id), fill = as.factor(species_id))) + 
        geom_ribbon(aes(ymin = quantile_005_mass_heartwood, ymax = quantile_095_mass_heartwood), alpha = 0.15, linetype = "blank") + 
        geom_ribbon(aes(ymin = quantile_025_mass_heartwood, ymax = quantile_075_mass_heartwood), alpha = 0.35, linetype = "blank") + 
        geom_line() +
        scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Median Heartwood Mass per Plant (kgC) In Run") +
        species_wrap +
        guides(fill = FALSE) +
        theme_bw() +
        foxes_theme
      p
      
      file_figure <- tempfile(paste(file_name_in, "median_iqr", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    ### MEAN HEARTWOOD MASS Ind
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_mass_heartwood,
                                 # ymin = ci_lwr_mass_heartwood, ymax = ci_upr_mass_heartwood,
                                 ymin = ci_lwr_mass_sapwood.y, ymax = ci_upr_mass_sapwood.y,
                                 colour = as.factor(species_id), fill = as.factor(species_id))) +
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Mean Plant Heartwood Mass (kgC) in Run") +
        species_wrap +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste(file_name_in, "mean_ci", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_mass_heartwood, 
                                 ymin = mean_mass_heartwood - sd_mass_heartwood, ymax = mean_mass_heartwood + sd_mass_heartwood,
                                 colour = as.factor(species_id), fill = as.factor(species_id))) +
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Mean Plant Heartwood Mass (kgC) in Run") +
        species_wrap +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste(file_name_in, "mean_sd", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    ### MEAN MASS HEARTWOOD Ind (grouped)
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_mass_heartwood, 
                                 # ymin = ci_lwr_mass_heartwood, ymax = ci_upr_mass_heartwood,
                                 ymin = ci_lwr_mass_sapwood.y, ymax = ci_upr_mass_sapwood.y,
                                 colour = as.factor(species_id), fill = as.factor(species_id))) +
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Mean Plant Heartwood Mass (kgC) in Run") +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste(file_name_in, "mean_ci", "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_mass_heartwood, 
                                 ymin = mean_mass_heartwood - sd_mass_heartwood, ymax = mean_mass_heartwood + sd_mass_heartwood,
                                 colour = as.factor(species_id), fill = as.factor(species_id))) +
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Mean Plant Heartwood Mass (kgC) in Run") +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste(file_name_in, "mean_sd", "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
  }
  
  ######### STORAGE MASS ###########
  {
    new_dir_path <- file.path(full_dir_path, "storage_mass")
    dir.create(new_dir_path, recursive = TRUE)
    file_name_in <- "storage_mass"
    ### PLOT SUM STORAGE MASS (mean)
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_sum_mass_storage, 
                                 ymin = ci_lwr_sum_mass_storage, ymax = ci_upr_sum_mass_storage,
                                 color = as.factor(species_id), fill = as.factor(species_id))) + 
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Total Storage Mass (kg/100m2) in Run") +
        species_wrap +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "ci", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_sum_mass_storage, 
                                 ymin = mean_sum_mass_storage - sd_sum_mass_storage, ymax = mean_sum_mass_storage + sd_sum_mass_storage,
                                 color = as.factor(species_id), fill = as.factor(species_id))) + 
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Total Storage Mass (kg/100m2) in Run") +
        species_wrap +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "sd", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    ### PLOT SUM STORAGE AREA (mean) together
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_sum_mass_storage, 
                                 ymin = ci_lwr_sum_mass_storage, ymax = ci_upr_sum_mass_storage,
                                 color = as.factor(species_id), fill = as.factor(species_id))) + 
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Total Storage Mass (kg/100m2) in Run") +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "ci", "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_sum_mass_storage, 
                                 ymin = mean_sum_mass_storage - sd_sum_mass_storage, ymax = mean_sum_mass_storage + sd_sum_mass_storage,
                                 color = as.factor(species_id), fill = as.factor(species_id))) + 
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Total Storage Mass (kg/100m2) in Run") +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "sd", "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    ### PLOT SUM STORAGE AREA (mean) per m2
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_sum_mass_storage_m2, 
                                 ymin = ci_lwr_sum_mass_storage_m2, ymax = ci_upr_sum_mass_storage_m2,
                                 color = as.factor(species_id), fill = as.factor(species_id))) + 
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Total Storage Mass (kgC/m2) in Run") +
        species_wrap +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "m2", "ci", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_sum_mass_storage_m2, 
                                 ymin = mean_sum_mass_storage_m2 - sd_sum_mass_storage_m2, ymax = mean_sum_mass_storage_m2 + sd_sum_mass_storage_m2, 
                                 color = as.factor(species_id), fill = as.factor(species_id))) +  
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Total Storage Mass (kgC/m2) in Run") +
        species_wrap +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "m2", "sd", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    ### PLOT SUM STORAGE MASS (mean) per m2 together
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_sum_mass_storage_m2, 
                                 ymin = ci_lwr_sum_mass_storage_m2, ymax = ci_upr_sum_mass_storage_m2,
                                 color = as.factor(species_id), fill = as.factor(species_id))) + 
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Total Storage Mass (kgC/m2) in Run") +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "m2", "ci", "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_sum_mass_storage_m2, 
                                 ymin = mean_sum_mass_storage_m2 - sd_sum_mass_storage_m2,ymax = mean_sum_mass_storage_m2 + sd_sum_mass_storage_m2, 
                                 color = as.factor(species_id), fill = as.factor(species_id))) +  
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Total Storage Mass (kgC/m2) in Run") +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "m2", "sd", "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    ### PLOT MAXIMUM STORAGE MASS Ind Strategy
    {
      p <- ggplot(tree_data, aes(x = time, y = max_mass_storage, color = as.factor(species_id))) + 
        geom_line() + geom_point() +
        scm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Maximum Storage Mass per Plant (kgC) In Run") +
        species_wrap +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("max", file_name_in, sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    ### MAXIMUM STORAGE AREA Ind together
    {
      p <- ggplot(tree_data, aes(x = time, y = max_mass_storage, color = as.factor(species_id))) + 
        geom_line() +
        geom_point() +
        scm + 
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Maximum Mass Storage per Plant (kgC) In Run") +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("max", file_name_in, "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    ### MEADIN STORAGE MASS Ind together
    {
      p <- ggplot(tree_data, aes(x = time, y = median_mass_storage, color = as.factor(species_id), fill = as.factor(species_id))) + 
        geom_ribbon(aes(ymin = quantile_005_mass_storage, ymax = quantile_095_mass_storage), alpha = 0.15, linetype = "blank") + 
        geom_ribbon(aes(ymin = quantile_025_mass_storage, ymax = quantile_075_mass_storage), alpha = 0.35, linetype = "blank") + 
        geom_line() +
        scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Median Storage Mass per Plant (kgC) In Run") +
        species_wrap +
        guides(fill = FALSE) +
        theme_bw() +
        foxes_theme
      p
      
      file_figure <- tempfile(paste(file_name_in, "median_iqr", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    ### MEAN STORAGE MASS Ind
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_mass_storage,
                                 ymin = ci_lwr_mass_storage, ymax = ci_upr_mass_storage,
                                 colour = as.factor(species_id), fill = as.factor(species_id))) +
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Mean Plant Storage Mass (kgC) in Run") +
        species_wrap +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste(file_name_in, "mean_ci", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_mass_storage, 
                                 ymin = mean_mass_storage - sd_mass_storage, ymax = mean_mass_storage + sd_mass_storage,
                                 colour = as.factor(species_id), fill = as.factor(species_id))) +
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Mean Plant Storage Mass (kgC) in Run") +
        species_wrap +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste(file_name_in, "mean_sd", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    ### MEAN MASS STORAGE Ind (grouped)
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_mass_storage, 
                                 ymin = ci_lwr_mass_storage, ymax = ci_upr_mass_storage,
                                 colour = as.factor(species_id), fill = as.factor(species_id))) +
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Mean Plant Storage Mass (kgC) in Run") +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste(file_name_in, "mean_ci", "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_mass_storage, 
                                 ymin = mean_mass_storage - sd_mass_storage, ymax = mean_mass_storage + sd_mass_storage,
                                 colour = as.factor(species_id), fill = as.factor(species_id))) +
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Mean Plant Storage Mass (kgC) in Run") +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste(file_name_in, "mean_sd", "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
  }
  
  ######### LIVE MASS ###########
  {
    new_dir_path <- file.path(full_dir_path, "live_mass")
    dir.create(new_dir_path, recursive = TRUE)
    file_name_in <- "live_mass"
    
    live_mass_data <- select(tree_data, time, species_id, mean_sum_mass_leaf, mean_sum_mass_bark, mean_sum_mass_root, mean_sum_mass_sapwood)
    live_mass_data_m2 <- select(tree_data, time, species_id, mean_sum_mass_leaf_m2, mean_sum_mass_bark_m2, mean_sum_mass_root_m2, mean_sum_mass_sapwood_m2)
    
    live_mass_data <- live_mass_data %>% 
      pivot_longer(cols=c("mean_sum_mass_leaf", "mean_sum_mass_bark", "mean_sum_mass_root", "mean_sum_mass_sapwood"), 
                   values_to="mass_value", names_to="mass_type")
    
    live_mass_data$mass_type <- fct_recode(as.factor(live_mass_data$mass_type), 
                                           Leaf = "mean_sum_mass_leaf", 
                                           Bark = "mean_sum_mass_bark",
                                           Root = "mean_sum_mass_root",
                                           Sapwood = "mean_sum_mass_sapwood")
    
    live_mass_data_percent <- select(tree_data, time, species_id, mean_sum_mass_leaf, mean_sum_mass_bark, mean_sum_mass_root, mean_sum_mass_sapwood) %>%
      mutate(sum_mass = mean_sum_mass_leaf + mean_sum_mass_bark + mean_sum_mass_root + mean_sum_mass_sapwood) %>%
      pivot_longer(cols=c("mean_sum_mass_leaf", "mean_sum_mass_bark", "mean_sum_mass_root", "mean_sum_mass_sapwood"), 
                   values_to="mass_value", names_to="mass_type") %>%
      mutate(mass_value = 100*mass_value/sum_mass)
    
    live_mass_data_percent$mass_type <- fct_recode(as.factor(live_mass_data_percent$mass_type), 
                                           Leaf = "mean_sum_mass_leaf", 
                                           Bark = "mean_sum_mass_bark",
                                           Root = "mean_sum_mass_root",
                                           Sapwood = "mean_sum_mass_sapwood")
    
    live_mass_data_m2 <- live_mass_data_m2 %>% 
      pivot_longer(cols=c("mean_sum_mass_leaf_m2", "mean_sum_mass_bark_m2", "mean_sum_mass_root_m2", "mean_sum_mass_sapwood_m2"), 
                   values_to="mass_value", names_to="mass_type")

    live_mass_data_m2$mass_type <- fct_recode(as.factor(live_mass_data_m2$mass_type), 
                                                   Leaf = "mean_sum_mass_leaf_m2", 
                                                   Bark = "mean_sum_mass_bark_m2",
                                                   Root = "mean_sum_mass_root_m2",
                                                   Sapwood = "mean_sum_mass_sapwood_m2")    
    
    ### PLOT SUM LIVE MASS (mean)
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_sum_mass_live, 
                                 ymin = ci_lwr_sum_mass_live, ymax = ci_upr_sum_mass_live,
                                 color = as.factor(species_id), fill = as.factor(species_id))) + 
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Total Live Mass (kg/100m2) in Run") +
        species_wrap +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "ci", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_sum_mass_live, 
                                 ymin = mean_sum_mass_live - sd_sum_mass_live, ymax = mean_sum_mass_live + sd_sum_mass_live,
                                 color = as.factor(species_id), fill = as.factor(species_id))) + 
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Total Live Mass (kg/100m2) in Run") +
        species_wrap +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "sd", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    ### PLOT SUM LIVE AREA (mean) together
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_sum_mass_live, 
                                 ymin = ci_lwr_sum_mass_live, ymax = ci_upr_sum_mass_live,
                                 color = as.factor(species_id), fill = as.factor(species_id))) + 
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Total Live Mass (kg/100m2) in Run") +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "ci", "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_sum_mass_live, 
                                 ymin = mean_sum_mass_live - sd_sum_mass_live, ymax = mean_sum_mass_live + sd_sum_mass_live,
                                 color = as.factor(species_id), fill = as.factor(species_id))) + 
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Total Live Mass (kg/100m2) in Run") +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "sd", "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    ### PLOT STACKED LIVE AREA (mean)
    {
      p <- ggplot(live_mass_data, aes(x = time, y = mass_value, 
                                 fill = as.factor(mass_type))) + 
        geom_area() +
        scale_fill_manual("Mass Type", values = c(foxes_palettes$extra_light[3],
                                                  foxes_palettes$extra_light[1], 
                                                  foxes_palettes$light[4],
                                                  foxes_palettes$extra_light[2]
                                                  )) +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Live Mass (kg/100m2) in Run") +
        species_wrap +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "stacked", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    {
      p <- ggplot(live_mass_data_m2, aes(x = time, y = mass_value, 
                                      fill = as.factor(mass_type))) + 
        geom_area() +
        scale_fill_manual("Mass Type", values = c(foxes_palettes$extra_light[3],
                                                  foxes_palettes$extra_light[1], 
                                                  foxes_palettes$light[4],
                                                  foxes_palettes$extra_light[2]
        )) +
        scale_x_continuous("Time (yrs)") +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Live Mass (kg/m2) in Run") +
        species_wrap +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "m2", "stacked", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    {
      p <- ggplot(live_mass_data_percent, aes(x = time, y = mass_value, 
                                         fill = as.factor(mass_type))) + 
        geom_area() +
        scale_fill_manual("Mass Type", values = c(foxes_palettes$extra_light[3],
                                                  foxes_palettes$extra_light[1], 
                                                  foxes_palettes$light[4],
                                                  foxes_palettes$extra_light[2]
        )) +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Live Mass % in Run") +
        species_wrap +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "percent", "stacked", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    ### PLOT SUM LIVE AREA (mean) per m2
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_sum_mass_live_m2, 
                                 ymin = ci_lwr_sum_mass_live_m2, ymax = ci_upr_sum_mass_live_m2,
                                 color = as.factor(species_id), fill = as.factor(species_id))) + 
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Total Live Mass (kgC/m2) in Run") +
        species_wrap +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "m2", "ci", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_sum_mass_live_m2, 
                                 ymin = mean_sum_mass_live_m2 - sd_sum_mass_live_m2, ymax = mean_sum_mass_live_m2 + sd_sum_mass_live_m2, 
                                 color = as.factor(species_id), fill = as.factor(species_id))) +  
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Total Live Mass (kgC/m2) in Run") +
        species_wrap +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "m2", "sd", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    ### PLOT SUM LIVE MASS (mean) per m2 together
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_sum_mass_live_m2, 
                                 ymin = ci_lwr_sum_mass_live_m2, ymax = ci_upr_sum_mass_live_m2,
                                 color = as.factor(species_id), fill = as.factor(species_id))) + 
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Total Live Mass (kgC/m2) in Run") +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "m2", "ci", "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_sum_mass_live_m2, 
                                 ymin = mean_sum_mass_live_m2 - sd_sum_mass_live_m2,ymax = mean_sum_mass_live_m2 + sd_sum_mass_live_m2, 
                                 color = as.factor(species_id), fill = as.factor(species_id))) +  
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Total Live Mass (kgC/m2) in Run") +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "m2", "sd", "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    ### PLOT MAXIMUM LIVE MASS Ind Strategy
    {
      p <- ggplot(tree_data, aes(x = time, y = max_mass_live, color = as.factor(species_id))) + 
        geom_line() + geom_point() +
        scm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Maximum Live Mass per Plant (kgC) In Run") +
        species_wrap +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("max", file_name_in, sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    ### MAXIMUM LIVE AREA Ind together
    {
      p <- ggplot(tree_data, aes(x = time, y = max_mass_live, color = as.factor(species_id))) + 
        geom_line() +
        geom_point() +
        scm + 
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Maximum Mass Live per Plant (kgC) In Run") +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("max", file_name_in, "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    ### MEADIN LIVE MASS Ind together
    {
      p <- ggplot(tree_data, aes(x = time, y = median_mass_live, color = as.factor(species_id), fill = as.factor(species_id))) + 
        geom_ribbon(aes(ymin = quantile_005_mass_live, ymax = quantile_095_mass_live), alpha = 0.15, linetype = "blank") + 
        geom_ribbon(aes(ymin = quantile_025_mass_live, ymax = quantile_075_mass_live), alpha = 0.35, linetype = "blank") + 
        geom_line() +
        scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Median Live Mass per Plant (kgC) In Run") +
        species_wrap +
        guides(fill = FALSE) +
        theme_bw() +
        foxes_theme
      p
      
      file_figure <- tempfile(paste(file_name_in, "median_iqr", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    ### MEAN LIVE MASS Ind
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_mass_live,
                                 ymin = ci_lwr_mass_live, ymax = ci_upr_mass_live,
                                 colour = as.factor(species_id), fill = as.factor(species_id))) +
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Mean Plant Live Mass (kgC) in Run") +
        species_wrap +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste(file_name_in, "mean_ci", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_mass_live, 
                                 ymin = mean_mass_live - sd_mass_live, ymax = mean_mass_live + sd_mass_live,
                                 colour = as.factor(species_id), fill = as.factor(species_id))) +
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Mean Plant Live Mass (kgC) in Run") +
        species_wrap +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste(file_name_in, "mean_sd", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    ### MEAN MASS LIVE Ind (grouped)
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_mass_live, 
                                 ymin = ci_lwr_mass_live, ymax = ci_upr_mass_live,
                                 colour = as.factor(species_id), fill = as.factor(species_id))) +
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Mean Plant Live Mass (kgC) in Run") +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste(file_name_in, "mean_ci", "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_mass_live, 
                                 ymin = mean_mass_live - sd_mass_live, ymax = mean_mass_live + sd_mass_live,
                                 colour = as.factor(species_id), fill = as.factor(species_id))) +
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Mean Plant Live Mass (kgC) in Run") +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste(file_name_in, "mean_sd", "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
  }
  
  ######### ABOVEGROUND MASS ###########
  {
    new_dir_path <- file.path(full_dir_path, "aboveground_mass")
    dir.create(new_dir_path, recursive = TRUE)
    file_name_in <- "aboveground_mass"
    
    aboveground_mass_data <- select(tree_data, time, species_id, mean_sum_mass_leaf, mean_sum_mass_bark, mean_sum_mass_heartwood, mean_sum_mass_sapwood)
    aboveground_mass_data_m2 <- select(tree_data, time, species_id, mean_sum_mass_leaf_m2, mean_sum_mass_bark_m2, mean_sum_mass_heartwood_m2, mean_sum_mass_sapwood_m2)
    
    aboveground_mass_data <- aboveground_mass_data %>% 
      pivot_longer(cols=c("mean_sum_mass_leaf", "mean_sum_mass_bark", "mean_sum_mass_heartwood", "mean_sum_mass_sapwood"), 
                   values_to="mass_value", names_to="mass_type")
    
    aboveground_mass_data$mass_type <- fct_recode(as.factor(aboveground_mass_data$mass_type), 
                                           Leaf = "mean_sum_mass_leaf", 
                                           Bark = "mean_sum_mass_bark",
                                           Heartwood = "mean_sum_mass_heartwood",
                                           Sapwood = "mean_sum_mass_sapwood")
    
    aboveground_mass_data_percent <- select(tree_data, time, species_id, mean_sum_mass_leaf, mean_sum_mass_bark, mean_sum_mass_heartwood, mean_sum_mass_sapwood) %>%
      mutate(sum_mass = mean_sum_mass_leaf + mean_sum_mass_bark + mean_sum_mass_heartwood + mean_sum_mass_sapwood) %>%
      pivot_longer(cols=c("mean_sum_mass_leaf", "mean_sum_mass_bark", "mean_sum_mass_heartwood", "mean_sum_mass_sapwood"), 
                   values_to="mass_value", names_to="mass_type") %>%
      mutate(mass_value = 100*mass_value/sum_mass)
    
    aboveground_mass_data_percent$mass_type <- fct_recode(as.factor(aboveground_mass_data_percent$mass_type), 
                                                   Leaf = "mean_sum_mass_leaf", 
                                                   Bark = "mean_sum_mass_bark",
                                                   Heartwood = "mean_sum_mass_heartwood",
                                                   Sapwood = "mean_sum_mass_sapwood")
    
    aboveground_mass_data_m2 <- aboveground_mass_data_m2 %>% 
      pivot_longer(cols=c("mean_sum_mass_leaf_m2", "mean_sum_mass_bark_m2", "mean_sum_mass_heartwood_m2", "mean_sum_mass_sapwood_m2"), 
                   values_to="mass_value", names_to="mass_type")
    
    aboveground_mass_data_m2$mass_type <- fct_recode(as.factor(aboveground_mass_data_m2$mass_type), 
                                              Leaf = "mean_sum_mass_leaf_m2", 
                                              Bark = "mean_sum_mass_bark_m2",
                                              Heartwood = "mean_sum_mass_heartwood_m2",
                                              Sapwood = "mean_sum_mass_sapwood_m2") 
    ### PLOT SUM ABOVEGROUND MASS (mean)
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_sum_mass_aboveground, 
                                 ymin = ci_lwr_sum_mass_aboveground, ymax = ci_upr_sum_mass_aboveground,
                                 color = as.factor(species_id), fill = as.factor(species_id))) + 
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Total Aboveground Mass (kg/100m2) in Run") +
        species_wrap +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "ci", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_sum_mass_aboveground, 
                                 ymin = mean_sum_mass_aboveground - sd_sum_mass_aboveground, ymax = mean_sum_mass_aboveground + sd_sum_mass_aboveground,
                                 color = as.factor(species_id), fill = as.factor(species_id))) + 
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Total Aboveground Mass (kg/100m2) in Run") +
        species_wrap +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "sd", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    ### PLOT SUM ABOVEGROUND AREA (mean) together
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_sum_mass_aboveground, 
                                 ymin = ci_lwr_sum_mass_aboveground, ymax = ci_upr_sum_mass_aboveground,
                                 color = as.factor(species_id), fill = as.factor(species_id))) + 
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Total Aboveground Mass (kg/100m2) in Run") +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "ci", "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_sum_mass_aboveground, 
                                 ymin = mean_sum_mass_aboveground - sd_sum_mass_aboveground, ymax = mean_sum_mass_aboveground + sd_sum_mass_aboveground,
                                 color = as.factor(species_id), fill = as.factor(species_id))) + 
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Total Aboveground Mass (kg/100m2) in Run") +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "sd", "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    ### PLOT SUM ABOVEGROUND AREA (mean) per m2
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_sum_mass_aboveground_m2, 
                                 ymin = ci_lwr_sum_mass_aboveground_m2, ymax = ci_upr_sum_mass_aboveground_m2,
                                 color = as.factor(species_id), fill = as.factor(species_id))) + 
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Total Aboveground Mass (kgC/m2) in Run") +
        species_wrap +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "m2", "ci", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_sum_mass_aboveground_m2, 
                                 ymin = mean_sum_mass_aboveground_m2 - sd_sum_mass_aboveground_m2, ymax = mean_sum_mass_aboveground_m2 + sd_sum_mass_aboveground_m2, 
                                 color = as.factor(species_id), fill = as.factor(species_id))) +  
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Total Aboveground Mass (kgC/m2) in Run") +
        species_wrap +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "m2", "sd", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    ### PLOT SUM ABOVEGROUND MASS (mean) per m2 together
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_sum_mass_aboveground_m2, 
                                 ymin = ci_lwr_sum_mass_aboveground_m2, ymax = ci_upr_sum_mass_aboveground_m2,
                                 color = as.factor(species_id), fill = as.factor(species_id))) + 
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Total Aboveground Mass (kgC/m2) in Run") +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "m2", "ci", "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_sum_mass_aboveground_m2, 
                                 ymin = mean_sum_mass_aboveground_m2 - sd_sum_mass_aboveground_m2,ymax = mean_sum_mass_aboveground_m2 + sd_sum_mass_aboveground_m2, 
                                 color = as.factor(species_id), fill = as.factor(species_id))) +  
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Total Aboveground Mass (kgC/m2) in Run") +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "m2", "sd", "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    ### PLOT STACKED ABOVEgROUND AREA (mean)
    {
      p <- ggplot(aboveground_mass_data, aes(x = time, y = mass_value, 
                                      fill = as.factor(mass_type))) + 
        geom_area() +
        scale_fill_manual("Mass Type", values = c(foxes_palettes$extra_light[3],
                                                  foxes_palettes$light[5],
                                                  foxes_palettes$extra_light[1], 
                                                  foxes_palettes$extra_light[2])) +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Live Mass (kg/100m2) in Run") +
        species_wrap +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "stacked", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    {
      p <- ggplot(aboveground_mass_data_m2, aes(x = time, y = mass_value, 
                                         fill = as.factor(mass_type))) + 
        geom_area() +
        scale_fill_manual("Mass Type", values = c(foxes_palettes$extra_light[3],
                                                  foxes_palettes$light[5],
                                                  foxes_palettes$extra_light[1], 
                                                  foxes_palettes$extra_light[2])) +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Live Mass (kg/m2) in Run") +
        species_wrap +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "m2", "stacked", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    {
      p <- ggplot(aboveground_mass_data_percent, aes(x = time, y = mass_value, 
                                              fill = as.factor(mass_type))) + 
        geom_area() +
        scale_fill_manual("Mass Type", values = c(foxes_palettes$extra_light[3],
                                                  foxes_palettes$light[5],
                                                  foxes_palettes$extra_light[1], 
                                                  foxes_palettes$extra_light[2])) +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Live Mass % in Run") +
        species_wrap +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "percent", "stacked", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    ### PLOT MAXIMUM ABOVEGROUND MASS Ind Strategy
    {
      p <- ggplot(tree_data, aes(x = time, y = max_mass_aboveground, color = as.factor(species_id))) + 
        geom_line() + geom_point() +
        scm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Maximum Aboveground Mass per Plant (kgC) In Run") +
        species_wrap +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("max", file_name_in, sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    ### MAXIMUM ABOVEGROUND AREA Ind together
    {
      p <- ggplot(tree_data, aes(x = time, y = max_mass_aboveground, color = as.factor(species_id))) + 
        geom_line() +
        geom_point() +
        scm + 
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Maximum Mass Aboveground per Plant (kgC) In Run") +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("max", file_name_in, "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    ### MEADIN ABOVEGROUND MASS Ind together
    {
      p <- ggplot(tree_data, aes(x = time, y = median_mass_aboveground, color = as.factor(species_id), fill = as.factor(species_id))) + 
        geom_ribbon(aes(ymin = quantile_005_mass_aboveground, ymax = quantile_095_mass_aboveground), alpha = 0.15, linetype = "blank") + 
        geom_ribbon(aes(ymin = quantile_025_mass_aboveground, ymax = quantile_075_mass_aboveground), alpha = 0.35, linetype = "blank") + 
        geom_line() +
        scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Median Aboveground Mass per Plant (kgC) In Run") +
        species_wrap +
        guides(fill = FALSE) +
        theme_bw() +
        foxes_theme
      p
      
      file_figure <- tempfile(paste(file_name_in, "median_iqr", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    ### MEAN ABOVEGROUND MASS Ind
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_mass_aboveground,
                                 ymin = ci_lwr_mass_aboveground, ymax = ci_upr_mass_aboveground,
                                 colour = as.factor(species_id), fill = as.factor(species_id))) +
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Mean Plant Aboveground Mass (kgC) in Run") +
        species_wrap +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste(file_name_in, "mean_ci", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_mass_aboveground, 
                                 ymin = mean_mass_aboveground - sd_mass_aboveground, ymax = mean_mass_aboveground + sd_mass_aboveground,
                                 colour = as.factor(species_id), fill = as.factor(species_id))) +
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Mean Plant Aboveground Mass (kgC) in Run") +
        species_wrap +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste(file_name_in, "mean_sd", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    ### MEAN MASS ABOVEGROUND Ind (grouped)
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_mass_aboveground, 
                                 ymin = ci_lwr_mass_aboveground, ymax = ci_upr_mass_aboveground,
                                 colour = as.factor(species_id), fill = as.factor(species_id))) +
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Mean Plant Aboveground Mass (kgC) in Run") +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste(file_name_in, "mean_ci", "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_mass_aboveground, 
                                 ymin = mean_mass_aboveground - sd_mass_aboveground, ymax = mean_mass_aboveground + sd_mass_aboveground,
                                 colour = as.factor(species_id), fill = as.factor(species_id))) +
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Mean Plant Aboveground Mass (kgC) in Run") +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste(file_name_in, "mean_sd", "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
  }
  
  ######### STEM MASS ###########
  {
    new_dir_path <- file.path(full_dir_path, "stem_mass")
    dir.create(new_dir_path, recursive = TRUE)
    file_name_in <- "stem_mass"
    
    
    stem_mass_data <- select(tree_data, time, species_id, mean_sum_mass_bark, mean_sum_mass_heartwood, mean_sum_mass_sapwood)
    stem_mass_data_m2 <- select(tree_data, time, species_id, mean_sum_mass_bark_m2, mean_sum_mass_heartwood_m2, mean_sum_mass_sapwood_m2)
    
    stem_mass_data <- stem_mass_data %>% 
      pivot_longer(cols=c("mean_sum_mass_bark", "mean_sum_mass_heartwood", "mean_sum_mass_sapwood"), 
                   values_to="mass_value", names_to="mass_type")
    
    stem_mass_data$mass_type <- fct_recode(as.factor(stem_mass_data$mass_type), 
                                                  Bark = "mean_sum_mass_bark",
                                                  Heartwood = "mean_sum_mass_heartwood",
                                                  Sapwood = "mean_sum_mass_sapwood")
    
    stem_mass_data_percent <- select(tree_data, time, species_id, mean_sum_mass_bark, mean_sum_mass_heartwood, mean_sum_mass_sapwood) %>%
      mutate(sum_mass = mean_sum_mass_bark + mean_sum_mass_heartwood + mean_sum_mass_sapwood) %>%
      pivot_longer(cols=c("mean_sum_mass_bark", "mean_sum_mass_heartwood", "mean_sum_mass_sapwood"), 
                   values_to="mass_value", names_to="mass_type") %>%
      mutate(mass_value = 100*mass_value/sum_mass)
    
    stem_mass_data_percent$mass_type <- fct_recode(as.factor(stem_mass_data_percent$mass_type), 
                                                          Bark = "mean_sum_mass_bark",
                                                          Heartwood = "mean_sum_mass_heartwood",
                                                          Sapwood = "mean_sum_mass_sapwood")
    
    stem_mass_data_m2 <- stem_mass_data_m2 %>% 
      pivot_longer(cols=c("mean_sum_mass_bark_m2", "mean_sum_mass_heartwood_m2", "mean_sum_mass_sapwood_m2"), 
                   values_to="mass_value", names_to="mass_type")
    
    stem_mass_data_m2$mass_type <- fct_recode(as.factor(stem_mass_data_m2$mass_type), 
                                                     Bark = "mean_sum_mass_bark_m2",
                                                     Heartwood = "mean_sum_mass_heartwood_m2",
                                                     Sapwood = "mean_sum_mass_sapwood_m2") 
    
    ### PLOT SUM STEM MASS (mean)
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_sum_mass_stem, 
                                 ymin = ci_lwr_sum_mass_stem, ymax = ci_upr_sum_mass_stem,
                                 color = as.factor(species_id), fill = as.factor(species_id))) + 
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Total Stem Mass (kg/100m2) in Run") +
        species_wrap +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "ci", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_sum_mass_stem, 
                                 ymin = mean_sum_mass_stem - sd_sum_mass_stem, ymax = mean_sum_mass_stem + sd_sum_mass_stem,
                                 color = as.factor(species_id), fill = as.factor(species_id))) + 
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Total Stem Mass (kg/100m2) in Run") +
        species_wrap +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "sd", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    ### PLOT SUM STEM AREA (mean) together
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_sum_mass_stem, 
                                 ymin = ci_lwr_sum_mass_stem, ymax = ci_upr_sum_mass_stem,
                                 color = as.factor(species_id), fill = as.factor(species_id))) + 
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Total Stem Mass (kg/100m2) in Run") +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "ci", "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_sum_mass_stem, 
                                 ymin = mean_sum_mass_stem - sd_sum_mass_stem, ymax = mean_sum_mass_stem + sd_sum_mass_stem,
                                 color = as.factor(species_id), fill = as.factor(species_id))) + 
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Total Stem Mass (kg/100m2) in Run") +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "sd", "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    ### PLOT SUM STEM AREA (mean) per m2
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_sum_mass_stem_m2, 
                                 ymin = ci_lwr_sum_mass_stem_m2, ymax = ci_upr_sum_mass_stem_m2,
                                 color = as.factor(species_id), fill = as.factor(species_id))) + 
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Total Stem Mass (kgC/m2) in Run") +
        species_wrap +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "m2", "ci", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_sum_mass_stem_m2, 
                                 ymin = mean_sum_mass_stem_m2 - sd_sum_mass_stem_m2, ymax = mean_sum_mass_stem_m2 + sd_sum_mass_stem_m2, 
                                 color = as.factor(species_id), fill = as.factor(species_id))) +  
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Total Stem Mass (kgC/m2) in Run") +
        species_wrap +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "m2", "sd", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    ### PLOT SUM STEM MASS (mean) per m2 together
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_sum_mass_stem_m2, 
                                 ymin = ci_lwr_sum_mass_stem_m2, ymax = ci_upr_sum_mass_stem_m2,
                                 color = as.factor(species_id), fill = as.factor(species_id))) + 
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Total Stem Mass (kgC/m2) in Run") +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "m2", "ci", "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_sum_mass_stem_m2, 
                                 ymin = mean_sum_mass_stem_m2 - sd_sum_mass_stem_m2,ymax = mean_sum_mass_stem_m2 + sd_sum_mass_stem_m2, 
                                 color = as.factor(species_id), fill = as.factor(species_id))) +  
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Total Stem Mass (kgC/m2) in Run") +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "m2", "sd", "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    ### PLOT STACKED LIVE AREA (mean)
    {
      p <- ggplot(stem_mass_data, aes(x = time, y = mass_value, 
                                             fill = as.factor(mass_type))) + 
        geom_area() +
        scale_fill_manual("Mass Type", values = c(foxes_palettes$extra_light[3],
                                                  foxes_palettes$light[5],
                                                  foxes_palettes$extra_light[2])) +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Live Mass (kg/100m2) in Run") +
        species_wrap +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "stacked", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    {
      p <- ggplot(stem_mass_data_m2, aes(x = time, y = mass_value, 
                                                fill = as.factor(mass_type))) + 
        geom_area() +
        scale_fill_manual("Mass Type", values = c(foxes_palettes$extra_light[3],
                                                  foxes_palettes$light[5],
                                                  foxes_palettes$extra_light[2])) +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Live Mass (kg/m2) in Run") +
        species_wrap +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "m2", "stacked", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    {
      p <- ggplot(stem_mass_data_percent, aes(x = time, y = mass_value, 
                                                     fill = as.factor(mass_type))) + 
        geom_area() +
        scale_fill_manual("Mass Type", values = c(foxes_palettes$extra_light[3],
                                                  foxes_palettes$light[5],
                                                  foxes_palettes$extra_light[2])) +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Live Mass % in Run") +
        species_wrap +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "percent", "stacked", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    ### PLOT MAXIMUM STEM MASS Ind Strategy
    {
      p <- ggplot(tree_data, aes(x = time, y = max_mass_stem, color = as.factor(species_id))) + 
        geom_line() + geom_point() +
        scm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Maximum Stem Mass per Plant (kgC) In Run") +
        species_wrap +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("max", file_name_in, sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    ### MAXIMUM STEM AREA Ind together
    {
      p <- ggplot(tree_data, aes(x = time, y = max_mass_stem, color = as.factor(species_id))) + 
        geom_line() +
        geom_point() +
        scm + 
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Maximum Mass Stem per Plant (kgC) In Run") +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("max", file_name_in, "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    ### MEADIN STEM MASS Ind together
    {
      p <- ggplot(tree_data, aes(x = time, y = median_mass_stem, color = as.factor(species_id), fill = as.factor(species_id))) + 
        geom_ribbon(aes(ymin = quantile_005_mass_stem, ymax = quantile_095_mass_stem), alpha = 0.15, linetype = "blank") + 
        geom_ribbon(aes(ymin = quantile_025_mass_stem, ymax = quantile_075_mass_stem), alpha = 0.35, linetype = "blank") + 
        geom_line() +
        scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Median Stem Mass per Plant (kgC) In Run") +
        species_wrap +
        guides(fill = FALSE) +
        theme_bw() +
        foxes_theme
      p
      
      file_figure <- tempfile(paste(file_name_in, "median_iqr", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    ### MEAN STEM MASS Ind
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_mass_stem,
                                 ymin = ci_lwr_mass_stem, ymax = ci_upr_mass_stem,
                                 colour = as.factor(species_id), fill = as.factor(species_id))) +
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Mean Plant Stem Mass (kgC) in Run") +
        species_wrap +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste(file_name_in, "mean_ci", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_mass_stem, 
                                 ymin = mean_mass_stem - sd_mass_stem, ymax = mean_mass_stem + sd_mass_stem,
                                 colour = as.factor(species_id), fill = as.factor(species_id))) +
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Mean Plant Stem Mass (kgC) in Run") +
        species_wrap +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste(file_name_in, "mean_sd", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    ### MEAN MASS STEM Ind (grouped)
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_mass_stem, 
                                 ymin = ci_lwr_mass_stem, ymax = ci_upr_mass_stem,
                                 colour = as.factor(species_id), fill = as.factor(species_id))) +
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Mean Plant Stem Mass (kgC) in Run") +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste(file_name_in, "mean_ci", "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_mass_stem, 
                                 ymin = mean_mass_stem - sd_mass_stem, ymax = mean_mass_stem + sd_mass_stem,
                                 colour = as.factor(species_id), fill = as.factor(species_id))) +
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Mean Plant Stem Mass (kgC) in Run") +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste(file_name_in, "mean_sd", "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
  }
  
  ######### MORTALITY ###########
  {
    new_dir_path <- file.path(full_dir_path, "mortality_rate")
    dir.create(new_dir_path, recursive = TRUE)
    file_name_in <- "mortality_rate"
    
    mortality_risk_data <- select(tree_data, time, species_id, mean_mortality_risk, mean_mortality_risk_storage, mean_mortality_risk_growth)
    
    mortality_risk_data$mean_mortality_risk_base = mortality_risk_data$mean_mortality_risk - mortality_risk_data$mean_mortality_risk_storage - mortality_risk_data$mean_mortality_risk_growth
    
    mortality_risk_data <- mortality_risk_data %>% 
      pivot_longer(cols=c("mean_mortality_risk_base", "mean_mortality_risk_storage", "mean_mortality_risk_growth"), 
                   values_to="mortality_risk", names_to="mortality_type")
    
    mortality_risk_data$mortality_type <- fct_recode(as.factor(mortality_risk_data$mortality_type), 
                                           Base = "mean_mortality_risk_base",
                                           Growth = "mean_mortality_risk_growth",
                                           Storage = "mean_mortality_risk_storage")
    
    mortality_risk_data_percent <- mortality_risk_data %>%
      mutate(mortality_risk = 100*mortality_risk/mean_mortality_risk)
    
    
    ### PLOT MORTALITY RISK (mean)
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_mortality_risk, 
                                 ymin = ci_lwr_mortality_risk, ymax = ci_upr_mortality_risk,
                                 color = as.factor(species_id), fill = as.factor(species_id))) + 
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Mortality Risk per Individual") +
        facet_wrap(.~species_id, nrow = 2, ncol = 2, scales = "free_y", labeller = labeller(species_id = c("1" = "Slow-Risky", 
                                                                                                           "2" = "Slow-Safe",
                                                                                                           "3" = "Fast-Risky", 
                                                                                                           "4" = "Fast-Safe"))) +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "ci", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_mortality_risk, 
                                 ymin = mean_mortality_risk - sd_mortality_risk, ymax = mean_mortality_risk + sd_mortality_risk,
                                 color = as.factor(species_id), fill = as.factor(species_id))) + 
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Mortality Risk per Individual") +
        facet_wrap(.~species_id, nrow = 2, ncol = 2, scales = "free_y", labeller = labeller(species_id = c("1" = "Slow-Risky", 
                                                                                                           "2" = "Slow-Safe",
                                                                                                           "3" = "Fast-Risky", 
                                                                                                           "4" = "Fast-Safe"))) +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "sd", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    ### PLOT MORTALITY RISK STORAGE (mean)
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_mortality_risk_storage, 
                                 ymin = ci_lwr_mortality_risk_storage, ymax = ci_upr_mortality_risk_storage,
                                 color = as.factor(species_id), fill = as.factor(species_id))) + 
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Mortality Risk (Storage) per Individual") +
        facet_wrap(.~species_id, nrow = 2, ncol = 2, scales = "free_y", labeller = labeller(species_id = c("1" = "Slow-Risky", 
                                                                                                           "2" = "Slow-Safe",
                                                                                                           "3" = "Fast-Risky", 
                                                                                                           "4" = "Fast-Safe"))) +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "storage", "ci", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_mortality_risk_storage, 
                                 ymin = mean_mortality_risk_storage - sd_mortality_risk_storage, ymax = mean_mortality_risk_storage + sd_mortality_risk_storage,
                                 color = as.factor(species_id), fill = as.factor(species_id))) + 
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Mortality Risk (Storage) per Individual") +
        facet_wrap(.~species_id, nrow = 2, ncol = 2, scales = "free_y", labeller = labeller(species_id = c("1" = "Slow-Risky", 
                                                                                                           "2" = "Slow-Safe",
                                                                                                           "3" = "Fast-Risky", 
                                                                                                           "4" = "Fast-Safe"))) +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "sd", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    ### PLOT MORTALITY RISK GROWTH (mean)
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_mortality_risk_growth, 
                                 ymin = ci_lwr_mortality_risk_growth, ymax = ci_upr_mortality_risk_growth,
                                 color = as.factor(species_id), fill = as.factor(species_id))) + 
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Mortality Risk (Growth) per Individual") +
        facet_wrap(.~species_id, nrow = 2, ncol = 2, scales = "free_y", labeller = labeller(species_id = c("1" = "Slow-Risky", 
                                                                                                           "2" = "Slow-Safe",
                                                                                                           "3" = "Fast-Risky", 
                                                                                                           "4" = "Fast-Safe"))) +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "growth", "ci", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_mortality_risk_growth, 
                                 ymin = mean_mortality_risk_growth - sd_mortality_risk_growth, ymax = mean_mortality_risk_growth + sd_mortality_risk_growth,
                                 color = as.factor(species_id), fill = as.factor(species_id))) + 
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Mortality Risk (Growth) per Individual") +
        facet_wrap(.~species_id, nrow = 2, ncol = 2, scales = "free_y", labeller = labeller(species_id = c("1" = "Slow-Risky", 
                                                                                                           "2" = "Slow-Safe",
                                                                                                           "3" = "Fast-Risky", 
                                                                                                           "4" = "Fast-Safe"))) +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "growth", "sd", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    ### PLOT STACKED MORTALITY RISK (mean)
    {
      p <- ggplot(mortality_risk_data, aes(x = time, y = mortality_risk, 
                                      fill = as.factor(mortality_type))) + 
        geom_area() +
        scale_fill_manual("Mortality Type", values = c(foxes_palettes$extra_dark[3],
                                                  foxes_palettes$extra_dark[1],
                                                  foxes_palettes$extra_dark[2])) +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Mortality Risk ") +
        facet_wrap(.~species_id, nrow = 2, ncol = 2, scales = "free_y", labeller = labeller(species_id = c("1" = "Slow-Risky", 
                                                                                        "2" = "Slow-Safe",
                                                                                        "3" = "Fast-Risky", 
                                                                                        "4" = "Fast-Safe"))) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "stacked", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    {
      p <- ggplot(mortality_risk_data_percent, aes(x = time, y = mortality_risk, 
                                           fill = as.factor(mortality_type))) + 
        geom_area() +
        scale_fill_manual("Mortality Type", values = c(foxes_palettes$extra_light[3],
                                                       foxes_palettes$light[5],
                                                       foxes_palettes$extra_light[2])) +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Live Mass (kg/100m2) in Run") +
        species_wrap +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "percent", "stacked", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
 
    ### MEADIN Mortality Risk together
    {
      p <- ggplot(tree_data, aes(x = time, y = median_mortality_risk, color = as.factor(species_id), fill = as.factor(species_id))) + 
        geom_ribbon(aes(ymin = quantile_005_mortality_risk, ymax = quantile_095_mortality_risk), alpha = 0.15, linetype = "blank") + 
        geom_ribbon(aes(ymin = quantile_025_mortality_risk, ymax = quantile_075_mortality_risk), alpha = 0.35, linetype = "blank") + 
        geom_line() +
        scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Median Mortality Risk per Plant") +
        facet_wrap(.~species_id, nrow = 2, ncol = 2, scales = "free_y", labeller = labeller(species_id = c("1" = "Slow-Risky", 
                                                                                                           "2" = "Slow-Safe",
                                                                                                           "3" = "Fast-Risky", 
                                                                                                           "4" = "Fast-Safe"))) +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste(file_name_in, "median_iqr", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    {
      p <- ggplot(tree_data, aes(x = time, y = median_mortality_risk_storage, color = as.factor(species_id), fill = as.factor(species_id))) + 
        geom_ribbon(aes(ymin = quantile_005_mortality_risk_storage, ymax = quantile_095_mortality_risk_storage), alpha = 0.15, linetype = "blank") + 
        geom_ribbon(aes(ymin = quantile_025_mortality_risk_storage, ymax = quantile_075_mortality_risk_storage), alpha = 0.35, linetype = "blank") + 
        geom_line() +
        scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Median Mortality Risk (Storage) per Plant") +
        facet_wrap(.~species_id, nrow = 2, ncol = 2, scales = "free_y", labeller = labeller(species_id = c("1" = "Slow-Risky", 
                                                                                                           "2" = "Slow-Safe",
                                                                                                           "3" = "Fast-Risky", 
                                                                                                           "4" = "Fast-Safe"))) +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste(file_name_in, "storage", "median_iqr", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    {
      p <- ggplot(tree_data, aes(x = time, y = median_mortality_risk_growth, color = as.factor(species_id), fill = as.factor(species_id))) + 
        geom_ribbon(aes(ymin = quantile_005_mortality_risk_growth, ymax = quantile_095_mortality_risk_growth), alpha = 0.15, linetype = "blank") + 
        geom_ribbon(aes(ymin = quantile_025_mortality_risk_growth, ymax = quantile_075_mortality_risk_growth), alpha = 0.35, linetype = "blank") + 
        geom_line() +
        scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Median Mortality Risk (Growth) per Plant") +
        facet_wrap(.~species_id, nrow = 2, ncol = 2, scales = "free_y", labeller = labeller(species_id = c("1" = "Slow-Risky", 
                                                                                                           "2" = "Slow-Safe",
                                                                                                           "3" = "Fast-Risky", 
                                                                                                           "4" = "Fast-Safe"))) +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste(file_name_in, "growth", "median_iqr", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
  }
  ######### FECUNDITY ###########
  {
    new_dir_path <- file.path(full_dir_path, "fecundity")
    dir.create(new_dir_path, recursive = TRUE)
    file_name_in <- "fecundity"
    ### PLOT SUM FECUNDITY (mean)
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_fecundity_sum, 
                                 ymin = ci_lwr_fecundity_sum, ymax = ci_upr_fecundity_sum,
                                 color = as.factor(species_id), fill = as.factor(species_id))) + 
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Total Fecundity (#seeds/100m2) in Run") +
        species_wrap +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "ci", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_fecundity_sum, 
                                 ymin = mean_fecundity_sum - sd_fecundity_sum, ymax = mean_fecundity_sum + sd_fecundity_sum,
                                 color = as.factor(species_id), fill = as.factor(species_id))) + 
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Total Fecundity (#seeds/100m2) in Run") +
        species_wrap +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "sd", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    ### PLOT SUM FECUNDITY (mean) together
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_fecundity_sum, 
                                 ymin = ci_lwr_fecundity_sum, ymax = ci_upr_fecundity_sum,
                                 color = as.factor(species_id), fill = as.factor(species_id))) + 
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Total Fecundity (#seeds/100m2) in Run") +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "ci", "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_fecundity_sum, 
                                 ymin = mean_fecundity_sum - sd_fecundity_sum, ymax = mean_fecundity_sum + sd_fecundity_sum,
                                 color = as.factor(species_id), fill = as.factor(species_id))) + 
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Total Fecundity (#seeds/100m2) in Run") +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "sd", "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    ### PLOT SUM LEAF AREA (mean) per m2
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_fecundity_sum/100, 
                                 ymin = ci_lwr_fecundity_sum/100, ymax = ci_upr_fecundity_sum/100,
                                 color = as.factor(species_id), fill = as.factor(species_id))) + 
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Total Fecundity (#seeds/m2) in Run") +
        species_wrap +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "m2", "ci", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_fecundity_sum/100, 
                                 ymin = mean_fecundity_sum/100 - sd_fecundity_sum/100, ymax = mean_fecundity_sum/100 + sd_fecundity_sum/100,
                                 color = as.factor(species_id), fill = as.factor(species_id))) + 
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Total Fecundity (#seeds/m2) in Run") +
        species_wrap +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "m2", "sd", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    ### PLOT SUM LEAF MASS (mean) per m2 together
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_fecundity_sum/100, 
                                 ymin = ci_lwr_fecundity_sum/100, ymax = ci_upr_fecundity_sum/100,
                                 color = as.factor(species_id), fill = as.factor(species_id))) + 
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Total Fecundity (#seeds/m2) in Run") +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "m2", "ci", "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_fecundity_sum/100, 
                                 ymin = mean_fecundity_sum/100 - sd_fecundity_sum/100, ymax = mean_fecundity_sum/100 + sd_fecundity_sum/100,
                                 color = as.factor(species_id), fill = as.factor(species_id))) + 
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Total Fecundity (#seeds/m2) in Run") +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "m2", "sd", "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    ### PLOT MAXIMUM LEAF MASS Ind Strategy
    {
      p <- ggplot(tree_data, aes(x = time, y = max_fecundity, color = as.factor(species_id))) + 
        geom_line() + geom_point() +
        scm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Maximum Fecundity Per Plant (#Seeds) In Run") +
        species_wrap +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("max", file_name_in, sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    ###  PLOT MAXIMUM LEAF AREA Ind together
    {
      p <- ggplot(tree_data, aes(x = time, y = max_fecundity, color = as.factor(species_id))) + 
        geom_line() +
        geom_point() +
        scm + 
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Maximum Fecundity Per Plant (#Seeds) In Run") +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("max", file_name_in, "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    ### MEADIN LEAF MASS Ind together
    {
      p <- ggplot(tree_data, aes(x = time, y = median_fecundity, color = as.factor(species_id), fill = as.factor(species_id))) + 
        geom_ribbon(aes(ymin = quantile_005_fecundity, ymax = quantile_095_fecundity), alpha = 0.15, linetype = "blank") + 
        geom_ribbon(aes(ymin = quantile_025_fecundity, ymax = quantile_075_fecundity), alpha = 0.35, linetype = "blank") + 
        geom_line() +
        scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Median Fecundity per Plant (#seeds) In Run") +
        species_wrap +
        guides(fill = FALSE) +
        theme_bw() +
        foxes_theme
      p
      
      file_figure <- tempfile(paste(file_name_in, "median_iqr", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    ### MEAN LEAF MASS Ind
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_fecundity,
                                 ymin = ci_lwr_fecundity, ymax = ci_upr_fecundity,
                                 colour = as.factor(species_id), fill = as.factor(species_id))) +
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Mean Fecundity per Plant (#seeds) In Run") +
        species_wrap +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste(file_name_in, "mean_ci", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_fecundity, 
                                 ymin = mean_fecundity - sd_fecundity, ymax = mean_fecundity + sd_fecundity,
                                 colour = as.factor(species_id), fill = as.factor(species_id))) +
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Mean Fecundity per Plant (#seeds) In Run") +
        species_wrap +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste(file_name_in, "mean_sd", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    ### MEAN LEAF MASS Ind (grouped)
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_fecundity,
                                 ymin = ci_lwr_fecundity, ymax = ci_upr_fecundity,
                                 colour = as.factor(species_id), fill = as.factor(species_id))) +
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Mean Fecundity per Plant (#seeds) In Run") +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste(file_name_in, "mean_ci", "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_fecundity, 
                                 ymin = mean_fecundity - sd_fecundity, ymax = mean_fecundity + sd_fecundity,
                                 colour = as.factor(species_id), fill = as.factor(species_id))) +
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Mean Fecundity per Plant (#seeds) In Run") +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste(file_name_in, "mean_sd", "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
  }
  
  ######### FECUNDITY DT ###########
  {
    new_dir_path <- file.path(full_dir_path, "fecundity_dt")
    dir.create(new_dir_path, recursive = TRUE)
    file_name_in <- "fecundity_dt"
    ### PLOT SUM FECUNDITY (mean)
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_fecundity_dt_sum, 
                                 ymin = ci_lwr_fecundity_dt_sum, ymax = ci_upr_fecundity_dt_sum,
                                 color = as.factor(species_id), fill = as.factor(species_id))) + 
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Fecundity Rate (#seeds/100m2/yr) in Run") +
        species_wrap +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "ci", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_fecundity_dt_sum, 
                                 ymin = mean_fecundity_dt_sum - sd_fecundity_dt_sum, ymax = mean_fecundity_dt_sum + sd_fecundity_dt_sum,
                                 color = as.factor(species_id), fill = as.factor(species_id))) + 
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Fecundity Rate (#seeds/100m2/yr) in Run") +
        species_wrap +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "sd", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    ### PLOT SUM FECUNDITY (mean) together
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_fecundity_dt_sum, 
                                 ymin = ci_lwr_fecundity_dt_sum, ymax = ci_upr_fecundity_dt_sum,
                                 color = as.factor(species_id), fill = as.factor(species_id))) + 
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Fecundity Rate (#seeds/100m2/yr) in Run") +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "ci", "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_fecundity_dt_sum, 
                                 ymin = mean_fecundity_dt_sum - sd_fecundity_dt_sum, ymax = mean_fecundity_dt_sum + sd_fecundity_dt_sum,
                                 color = as.factor(species_id), fill = as.factor(species_id))) + 
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Fecundity Rate (#seeds/100m2/yr) in Run") +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "sd", "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    ### PLOT SUM LEAF AREA (mean) per m2
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_fecundity_dt_sum/100, 
                                 ymin = ci_lwr_fecundity_dt_sum/100, ymax = ci_upr_fecundity_dt_sum/100,
                                 color = as.factor(species_id), fill = as.factor(species_id))) + 
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Fecundity Rate (#seeds/m2/yr) in Run") +
        species_wrap +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "m2", "ci", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_fecundity_dt_sum/100, 
                                 ymin = mean_fecundity_dt_sum/100 - sd_fecundity_dt_sum/100, ymax = mean_fecundity_dt_sum/100 + sd_fecundity_dt_sum/100,
                                 color = as.factor(species_id), fill = as.factor(species_id))) + 
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Fecundity Rate (#seeds/m2/yr) in Run") +
        species_wrap +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "m2", "sd", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    ### PLOT SUM LEAF MASS (mean) per m2 together
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_fecundity_dt_sum/100, 
                                 ymin = ci_lwr_fecundity_dt_sum/100, ymax = ci_upr_fecundity_dt_sum/100,
                                 color = as.factor(species_id), fill = as.factor(species_id))) + 
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Fecundity Rate (#seeds/m2/yr) in Run") +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "m2", "ci", "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_fecundity_dt_sum/100, 
                                 ymin = mean_fecundity_dt_sum/100 - sd_fecundity_dt_sum/100, ymax = mean_fecundity_dt_sum/100 + sd_fecundity_dt_sum/100,
                                 color = as.factor(species_id), fill = as.factor(species_id))) + 
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Average Fecundity Rate (#seeds/m2/yr) in Run") +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("mean", file_name_in, "m2", "sd", "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    ### PLOT MAXIMUM LEAF MASS Ind Strategy
    {
      p <- ggplot(tree_data, aes(x = time, y = max_fecundity_dt, color = as.factor(species_id))) + 
        geom_line() + geom_point() +
        scm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Maximum Fecundity Rate per Plant (#seeds/yr) in Run") +
        species_wrap +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("max", file_name_in, sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    ###  PLOT MAXIMUM LEAF AREA Ind together
    {
      p <- ggplot(tree_data, aes(x = time, y = max_fecundity_dt, color = as.factor(species_id))) + 
        geom_line() +
        geom_point() +
        scm + 
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Maximum Fecundity Rate per Plant (#seeds/yr) in Run") +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("max", file_name_in, "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    ### MEADIN LEAF MASS Ind together
    {
      p <- ggplot(tree_data, aes(x = time, y = median_fecundity_dt, color = as.factor(species_id), fill = as.factor(species_id))) + 
        geom_ribbon(aes(ymin = quantile_005_fecundity_dt, ymax = quantile_095_fecundity_dt), alpha = 0.15, linetype = "blank") + 
        geom_ribbon(aes(ymin = quantile_025_fecundity_dt, ymax = quantile_075_fecundity_dt), alpha = 0.35, linetype = "blank") + 
        geom_line() +
        scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Median Fecundity Rate per Plant (#seeds/yr) in Run") +
        species_wrap +
        guides(fill = FALSE) +
        theme_bw() +
        foxes_theme
      p
      
      file_figure <- tempfile(paste(file_name_in, "median_iqr", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    ### MEAN LEAF MASS Ind
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_fecundity_dt,
                                 ymin = ci_lwr_fecundity_dt, ymax = ci_upr_fecundity_dt,
                                 colour = as.factor(species_id), fill = as.factor(species_id))) +
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Mean Fecundity Rate per Plant (#seeds/yr) in Run") +
        species_wrap +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste(file_name_in, "mean_ci", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_fecundity_dt, 
                                 ymin = mean_fecundity_dt - sd_fecundity_dt, ymax = mean_fecundity_dt + sd_fecundity_dt,
                                 colour = as.factor(species_id), fill = as.factor(species_id))) +
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Mean Fecundity Rate per Plant (#seeds/yr) in Run") +
        species_wrap +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste(file_name_in, "mean_sd", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    ### MEAN LEAF MASS Ind (grouped)
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_fecundity_dt,
                                 ymin = ci_lwr_fecundity_dt, ymax = ci_upr_fecundity_dt,
                                 colour = as.factor(species_id), fill = as.factor(species_id))) +
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Mean Fecundity Rate per Plant (#seeds/yr) in Run") +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste(file_name_in, "mean_ci", "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_fecundity_dt, 
                                 ymin = mean_fecundity_dt - sd_fecundity_dt, ymax = mean_fecundity_dt + sd_fecundity_dt,
                                 colour = as.factor(species_id), fill = as.factor(species_id))) +
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Mean Fecundity Rate per Plant (#seeds/yr) in Run") +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste(file_name_in, "mean_sd", "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
  }
  
  ######### # Live Individuals ###########
  {
    new_dir_path <- file.path(full_dir_path, "trees_live")
    dir.create(new_dir_path, recursive = TRUE)
    file_name_in <- "trees_live"

    ### PLOT MAXIMUM # Live Individuals Ind Strategy
    {
      p <- ggplot(tree_data, aes(x = time, y = max_num_tree, color = as.factor(species_id))) + 
        geom_line() + geom_point() +
        scm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Maximum Number of Trees In Run") +
        species_wrap +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("max", file_name_in, sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    ###  PLOT MAXIMUM LEAF AREA Ind together
    {
      p <- ggplot(tree_data, aes(x = time, y = max_num_tree, color = as.factor(species_id))) + 
        geom_line() +
        geom_point() +
        scm + 
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Maximum Number of Trees In Run") +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste("max", file_name_in, "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    ### MEADIN LEAF MASS Ind together
    {
      p <- ggplot(tree_data, aes(x = time, y = median_num_tree, color = as.factor(species_id), fill = as.factor(species_id))) + 
        geom_ribbon(aes(ymin = quantile_005_num_tree, ymax = quantile_095_num_tree), alpha = 0.15, linetype = "blank") + 
        geom_ribbon(aes(ymin = quantile_025_num_tree, ymax = quantile_075_num_tree), alpha = 0.35, linetype = "blank") + 
        geom_line() +
        scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Median Number of Trees In Run") +
        species_wrap +
        guides(fill = FALSE) +
        theme_bw() +
        foxes_theme
      p
      
      file_figure <- tempfile(paste(file_name_in, "median_iqr", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    {
      p <- ggplot(tree_data, aes(x = time, y = median_num_tree/100, color = as.factor(species_id), fill = as.factor(species_id))) + 
        geom_ribbon(aes(ymin = quantile_005_num_tree/100, ymax = quantile_095_num_tree/100), alpha = 0.15, linetype = "blank") + 
        geom_ribbon(aes(ymin = quantile_025_num_tree/100, ymax = quantile_075_num_tree/100), alpha = 0.35, linetype = "blank") + 
        geom_line() +
        scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Median Number of Trees per m2 In Run") +
        species_wrap +
        guides(fill = FALSE) +
        theme_bw() +
        foxes_theme
      p
      
      file_figure <- tempfile(paste(file_name_in, "median_iqr", "m2", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    ### MEAN LEAF MASS Ind
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_num_tree,
                                 ymin = ci_lwr_num_tree, ymax = ci_upr_num_tree,
                                 colour = as.factor(species_id), fill = as.factor(species_id))) +
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Mean Number of Trees per 100m2 In Run") +
        species_wrap +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste(file_name_in, "mean_ci", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_num_tree, 
                                 ymin = mean_num_tree - sd_num_tree, ymax = mean_num_tree + sd_num_tree,
                                 colour = as.factor(species_id), fill = as.factor(species_id))) +
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Mean Number of Trees per 100m2 In Run") +
        species_wrap +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste(file_name_in, "mean_sd", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_num_tree/100,
                                 ymin = ci_lwr_num_tree/100, ymax = ci_upr_num_tree/100,
                                 colour = as.factor(species_id), fill = as.factor(species_id))) +
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Mean Number of Trees per m2 In Run") +
        species_wrap +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste(file_name_in, "mean_ci", "m2", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_num_tree/100, 
                                 ymin = mean_num_tree/100 - sd_num_tree/100, ymax = mean_num_tree/100 + sd_num_tree/100,
                                 colour = as.factor(species_id), fill = as.factor(species_id))) +
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Mean Number of Trees per m2 In Run") +
        species_wrap +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste(file_name_in, "mean_sd", "m2", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    ### MEAN LEAF MASS Ind (grouped)
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_num_tree,
                                 ymin = ci_lwr_num_tree, ymax = ci_upr_num_tree,
                                 colour = as.factor(species_id), fill = as.factor(species_id))) +
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Mean Number of Trees per 100m2 In Run") +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste(file_name_in, "mean_ci", "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_num_tree, 
                                 ymin = mean_num_tree - sd_num_tree, ymax = mean_num_tree + sd_num_tree,
                                 colour = as.factor(species_id), fill = as.factor(species_id))) +
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Mean Number of Trees per 100m2 In Run") +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste(file_name_in, "mean_sd", "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_num_tree/100,
                                 ymin = ci_lwr_num_tree/100, ymax = ci_upr_num_tree/100,
                                 colour = as.factor(species_id), fill = as.factor(species_id))) +
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Mean Number of Trees per m2 In Run") +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste(file_name_in, "mean_ci", "group", "m2", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
    
    {
      p <- ggplot(tree_data, aes(x = time, y = mean_num_tree/100, 
                                 ymin = mean_num_tree/100 - sd_num_tree/100, ymax = mean_num_tree/100 + sd_num_tree/100,
                                 colour = as.factor(species_id), fill = as.factor(species_id))) +
        geom_line() +
        gr_ribbon + scm_ribbon + sfm +
        scale_x_continuous("Time (yrs)") +
        scale_y_continuous("Mean Number of Trees per m2 In Run") +
        guides(fill = FALSE) +
        foxes_theme
      p
      
      file_figure <- tempfile(paste(file_name_in, "mean_sd", "group", "m2", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
      ggsave(file_figure, plot = p, device = NULL, path = NULL,
             scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
             units =  "mm")
    }
  }
  
})


process_tree_data <- function(file){
  path_parts <- unlist(str_split(file, "/"))
  
  run_rep = unlist(str_split(path_parts[(length(path_parts)-1)], "_"))
  run_rep = run_rep[length(run_rep)]
  env_rep = unlist(str_split(path_parts[(length(path_parts)-2)], "_"))
  env_rep = env_rep[length(env_rep)]
  
  file_name <- path_parts[length(path_parts)]
  
  file_parts <- unlist(str_split(file_name, "_"))
  
  from = floor(as.numeric(file_parts[4]))
  
  to_parts = unlist(str_split(file_parts[6], "-"))
  to = floor(as.numeric(to_parts[1]))
  
  return(data.frame(file = file,
                    file_name = file_name,
                    from = from,
                    to = to,
                    run_rep = run_rep,
                    env_rep = env_rep,
                    stringsAsFactors = FALSE))
}




