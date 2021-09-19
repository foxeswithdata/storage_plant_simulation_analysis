rm(list=ls())

library(plyr)
library(tidyverse)
library(stringr)
library(plotrix) 
library(DescTools)
library(gridExtra)

source("code/foxes_pallettes.R")
source("code/function_processing.R")
source("code/plotting.R")

data_directories <- list.dirs("data/full_run_processed", recursive = FALSE)

h = 2
h2 = h/2

max_time = 99

data_rolling_ave_env <- plyr::rbind.fill(lapply(data_directories, function(dat){
  files <- list.files(path = dat, pattern = "env", recursive = TRUE)
  file_names_full <- as.vector(sapply(files, function(x){
    return(file.path(dat, x))
  }))
  
  file_names_full <- file_names_full[grep("extra", file_names_full, invert = TRUE)]
  print(file_names_full)
  
  all_data <- plyr::rbind.fill(lapply(file_names_full, function(file_name){
    data <- readRDS(file_name)
    return(data)
  }))
  
  seq_time = seq(from = h2, to = max_time, by = h2)
  
  env_summary <- plyr::rbind.fill(lapply(seq_time, function(t){
    summary_data <- all_data %>% 
      dplyr::filter((time > (t-h2)) & (time < (t+h2)))

    print(t)
    print(colnames(summary_data))
    print(nrow(summary_data))
    
    if(nrow(summary_data) > 0){
      height_min = min(summary_data$height)
      height_max = max(summary_data$height)
      
      seq_height = seq(from = height_min, to = height_max, length.out = 20)
      
      print(seq_height)
      
      canopy_openness <- plyr::rbind.fill(lapply(1:(length(seq_height)-1), function(h){
        summary_height_data <- summary_data %>% 
          dplyr::filter((height > (seq_height[h])) & (height <= (seq_height[h+1]))) %>%
          dplyr::group_by(env_sd, env_mean) %>%
          summarise(canopy_openness_mean = mean(canopy_openness))
        summary_height_data$height = (seq_height[h] + seq_height[h+1])/2
        return(summary_height_data)
      }))
      
      ##normalise for reference
      canopy_openness$canopy_openness_mean = canopy_openness$canopy_openness_mean/sum(canopy_openness$canopy_openness_mean)
      
      print("finished")
      
      canopy_openness$time = t
      return(canopy_openness)
    }
  }))
  
  return(env_summary)
}))

save(data_rolling_ave_env, "rolling_ave_ENVIRONMENT_apr.RData")