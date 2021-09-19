library(tidyverse)
library(plyr)
library(stringr)

source("code/foxes_pallettes.R")
source("code/function_processing.R")
source("code/plotting.R")

figure_out_dir <- file.path("out", "figures", "full_run", "env_075_60d", "envrep_3")
if(!dir.exists(figure_out_dir)){
  dir.create(figure_out_dir, recursive = TRUE)  
}

file_names <- list.files(path = "data/full_run/env_075_60d/envrep_3/", pattern = "tree_data", recursive = TRUE, include.dirs = TRUE)

file_names_full <- sapply(file_names, function(x){
  return(file.path("data", "full_run", "env_075_60d", "envrep_3", x))
})

file_names_full <- as.vector(file_names_full)
species_names <- c("Slow-Risky", "Slow-Safe", "Fast-Risky", "Fast-Safe")

tree_live_dead <- process_data(file_names_full, subset_live_dead, FALSE)

tree_deaths <- lapply(unique(tree_live_dead$env_rep), function(er){
    tree_live_dead_sub <- subset(tree_live_dead, env_rep == er)
    
    return(plyr::rbind.fill(lapply(unique(tree_live_dead_sub$run_rep), function(rr){
      tree_live_dead_sub_sub <- subset(tree_live_dead_sub, run_rep == rr)
      
      return(plyr::rbind.fill(lapply(unique(tree_live_dead_sub_sub$species_id), function(sp_id){
        tree_live_dead_sub_sub_sub <- subset(tree_live_dead_sub_sub, species_id == sp_id)
        
        return(plyr::rbind.fill(lapply(unique(tree_live_dead_sub_sub_sub$tree_id), function(tr_id){
          tree_live_dead_sub_sub_sub_sub <- subset(tree_live_dead_sub_sub_sub, tree_id == tr_id)
          tree_live_dead_sub_sub_sub_sub <- arrange(tree_live_dead_sub_sub_sub_sub, time)
          diff_alive <- c(0,diff(tree_live_dead_sub_sub_sub_sub$is_alive))
          born <- c(1, rep(0, times =( nrow(tree_live_dead_sub_sub_sub_sub)-1)))
          return(data.frame(time = tree_live_dead_sub_sub_sub_sub$time,
                            species_id = sp_id,
                            tree_id = tr_id,
                            died = -diff_alive,
                            born = born))
        })))
      })))
    })))
})


tree_deaths <- plyr::rbind.fill(tree_deaths)



