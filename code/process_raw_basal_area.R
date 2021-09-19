library(tidyverse)
library(plyr)
library(stringr)

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

out_dirs_env_total <- as.vector(unlist(sapply(data_directories, function(dir){
  path_parts <- unlist(str_split(dir, "/"))
  if(length(path_parts) == 3){
    return(dir)
  }
  return(NULL)
})))

out_dirs_env_test <- out_dirs_env[6] 


out_env_run_basal <- lapply(out_dirs_env, function(dir){
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
  
  all_basal <- plyr::rbind.fill(lapply(unique(files$from), function(i){
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
                         max_sum_basal = max(sum_basal, na.rm = TRUE))
      
      tree_basal_ave <- all_tree_data %>% 
        group_by(species_id) %>%
        dplyr::summarise(min_basal = min(area_stem, na.rm = TRUE),
                         mean_basal = mean(area_stem, na.rm = TRUE),
                         median_basal = median(area_stem, na.rm = TRUE),
                         quantile_005_basal = quantile(area_stem, probs = 0.05, na.rm = TRUE),
                         quantile_025_basal = quantile(area_stem, probs = 0.25, na.rm = TRUE),
                         quantile_075_basal = quantile(area_stem, probs = 0.75, na.rm = TRUE),
                         quantile_095_basal = quantile(area_stem, probs = 0.95, na.rm = TRUE),
                         max_basal = max(area_stem, na.rm = TRUE))
      
      
      
      tree_basal_ave$time = i + 5
      tree_sum_basal$time = i + 5
      
      tree_basal <- merge(tree_basal_ave, tree_sum_basal, by=(c("time", "species_id")), all= TRUE)
      
      return(tree_basal)
    }
    else{
      return(data.frame())
    }
  }))
  
  return(all_basal)
})

out_env_basal <- lapply(out_dirs_env_total, function(dir){
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
  
  all_basal <- plyr::rbind.fill(lapply(unique(files$from), function(i){
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
      tree_sum_basal <- all_tree_data %>% group_by(species_id, run_rep, env_rep, time) %>%
        dplyr::summarise(sum_basal = max(area_stem, na.rm = TRUE)) %>%
        group_by(species_id) %>%
        dplyr::summarise(min_sum_basal = min(sum_basal, na.rm = TRUE),
                         mean_sum_basal = mean(sum_basal, na.rm = TRUE),
                         median_sum_basal = median(sum_basal, na.rm = TRUE),
                         quantile_005_sum_basal = quantile(sum_basal, probs = 0.05, na.rm = TRUE),
                         quantile_025_sum_basal = quantile(sum_basal, probs = 0.25, na.rm = TRUE),
                         quantile_075_sum_basal = quantile(sum_basal, probs = 0.75, na.rm = TRUE),
                         quantile_095_sum_basal = quantile(sum_basal, probs = 0.95, na.rm = TRUE),
                         max_sum_basal = max(sum_basal, na.rm = TRUE))
      
      tree_basal_ave <- all_tree_data %>% 
        group_by(species_id) %>%
        dplyr::summarise(min_basal = min(area_stem, na.rm = TRUE),
                         mean_basal = mean(area_stem, na.rm = TRUE),
                         median_basal = median(area_stem, na.rm = TRUE),
                         quantile_005_basal = quantile(area_stem, probs = 0.05, na.rm = TRUE),
                         quantile_025_basal = quantile(area_stem, probs = 0.25, na.rm = TRUE),
                         quantile_075_basal = quantile(area_stem, probs = 0.75, na.rm = TRUE),
                         quantile_095_basal = quantile(area_stem, probs = 0.95, na.rm = TRUE),
                         max_basal = max(area_stem, na.rm = TRUE))
      
      
      
      tree_basal_ave$time = i + 5
      tree_sum_basal$time = i + 5
      
      tree_basal <- merge(tree_basal_ave, tree_sum_basal, by=(c("time", "species_id")), all= TRUE)
      
      return(tree_basal)
    }
    else{
      return(data.frame())
    }
  }))
  
  return(all_basal)
})

save.image("basal_data.RData")


lapply(2:length(out_env_run_basal), function(ind){
  
  basal_data = out_env_run_basal[[ind]]
  
  if(length(basal_data)  ==0){
    return()
  }
  
  dir = out_dirs_env[[ind]]
  path_parts <- unlist(str_split(dir, "/"))
  new_dir_path <- file.path("out", "full_run_auto", "environment_run_agg", path_parts[3], path_parts[4])
  
  print(new_dir_path)
  
  dir.create(new_dir_path, recursive = TRUE)
  
  ### PLOT SUM BASAL (mean)
  {
    p <- ggplot(basal_data, aes(x = time, y = mean_sum_basal, color = as.factor(species_id))) + 
      geom_line() +
      geom_point() +
      geom_linerange(aes(ymin = quantile_025_sum_basal, ymax = quantile_075_sum_basal), alpha = 0.75) +
      scale_color_manual("Allocation \nStrategy:",
                         values=foxes_palettes$main[c(2, 1, 4, 3)],
                         labels=c("Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe")) +
      scale_x_continuous("Time (yrs)") +
      scale_y_continuous("Average Basal Area Coverage per Hectar (m2/ha) Per Sub-Environment Repetition") +
      facet_wrap(.~species_id, nrow = 2, ncol = 2, labeller = labeller(species_id = c("1" = "Slow-Risky", 
                                                                                      "2" = "Slow-Safe",
                                                                                      "3" = "Fast-Risky", 
                                                                                      "4" = "Fast-Safe"))) +
      guides(fill = FALSE) +
      theme_bw() +
      theme(panel.grid.major = element_blank(),
            legend.position = "right",
            plot.margin = margin(1,1,1,1, "cm"),
            text=element_text(size=12, color = "#083855", family="Lato Light"),
            axis.text=element_text(size=8, color = "#083855", family="Lato Light"),
            strip.background = element_rect(fill = "white"))
    p
    
    file_figure <- tempfile(paste("mean_basal_sum", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
    ggsave(file_figure, plot = p, device = NULL, path = NULL,
           scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
           units =  "mm")
  }
  
  ### PLOT MAXIMUM HEIGHT (mean) together
  {
    p <- ggplot(basal_data, aes(x = time, y = mean_sum_basal, color = as.factor(species_id))) + 
      geom_line() +
      geom_point() +
      geom_linerange(aes(ymin = quantile_025_sum_basal, ymax = quantile_075_sum_basal), alpha = 0.75) +
      scale_color_manual("Allocation \nStrategy:",
                         values=foxes_palettes$main[c(2, 1, 4, 3)],
                         labels=c("Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe")) +
      scale_x_continuous("Time (yrs)") +
      scale_y_continuous("Average Basal Area Coverage per Hectar (m2/ha) Per Sub-Environment Repetition") +
      guides(fill = FALSE) +
      theme_bw() +
      theme(panel.grid.major = element_blank(),
            legend.position = "right",
            plot.margin = margin(1,1,1,1, "cm"),
            text=element_text(size=12, color = "#083855", family="Lato Light"),
            axis.text=element_text(size=8, color = "#083855", family="Lato Light"),
            strip.background = element_rect(fill = "white"))
    p
    
    file_figure <- tempfile(paste("mean_basal_sum_grouped", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
    ggsave(file_figure, plot = p, device = NULL, path = NULL,
           scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
           units =  "mm")
  } 
  
  ### PLOT MAXIMUM Basal Area Ind Strategy
  {
    p <- ggplot(basal_data, aes(x = time, y = max_basal, color = as.factor(species_id))) + 
      geom_line() +
      geom_point() +
      scale_color_manual("Allocation \nStrategy:",
                         values=foxes_palettes$main[c(2, 1, 4, 3)],
                         labels=c("Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe")) +
      scale_x_continuous("Time (yrs)") +
      scale_y_continuous("Maximum Basal Area per Plant (m2) Per Sub-Environment Repetition") +
      facet_wrap(.~species_id, nrow = 2, ncol = 2, labeller = labeller(species_id = c("1" = "Slow-Risky", 
                                                                                      "2" = "Slow-Safe",
                                                                                      "3" = "Fast-Risky", 
                                                                                      "4" = "Fast-Safe"))) +
      guides(fill = FALSE) +
      theme_bw() +
      theme(panel.grid.major = element_blank(),
            legend.position = "right",
            plot.margin = margin(1,1,1,1, "cm"),
            text=element_text(size=12, color = "#083855", family="Lato Light"),
            axis.text=element_text(size=8, color = "#083855", family="Lato Light"),
            strip.background = element_rect(fill = "white"))
    p
    
    file_figure <- tempfile(paste("max_basal", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
    ggsave(file_figure, plot = p, device = NULL, path = NULL,
           scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
           units =  "mm")
  }
  
  ### PLOT MAXIMUM HEIGHT together
  {
    p <- ggplot(basal_data, aes(x = time, y = max_basal, color = as.factor(species_id))) + 
      geom_line() +
      geom_point() +
      scale_color_manual("Allocation \nStrategy:",
                         values=foxes_palettes$main[c(2, 1, 4, 3)],
                         labels=c("Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe")) +
      scale_x_continuous("Time (yrs)") +
      scale_y_continuous("Maximum Basal Area per Plant (m2) Per Sub-Environment Repetition") +
      guides(fill = FALSE) +
      theme_bw() +
      theme(panel.grid.major = element_blank(),
            legend.position = "right",
            plot.margin = margin(1,1,1,1, "cm"),
            text=element_text(size=12, color = "#083855", family="Lato Light"),
            axis.text=element_text(size=8, color = "#083855", family="Lato Light"),
            strip.background = element_rect(fill = "white"))
    p
    
    file_figure <- tempfile(paste("max_grouped_basal", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
    ggsave(file_figure, plot = p, device = NULL, path = NULL,
           scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
           units =  "mm")
  }
  
  ### MEADIN PLANT HEIGHT 
  {
    p <- ggplot(basal_data, aes(x = time, y = median_basal, color = as.factor(species_id))) + 
      geom_line() +
      geom_point() +
      geom_linerange(aes(ymin = quantile_025_basal, ymax = quantile_075_basal), alpha = 0.75) +
      scale_color_manual("Allocation \nStrategy:",
                         values=foxes_palettes$main[c(2, 1, 4, 3)],
                         labels=c("Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe")) +
      scale_x_continuous("Time (yrs)") +
      scale_y_continuous("Median Basal Area per Plant (m2) Per Sub-Environment Repetition") +
      facet_wrap(.~species_id, nrow = 2, ncol = 2, labeller = labeller(species_id = c("1" = "Slow-Risky", 
                                                                                      "2" = "Slow-Safe",
                                                                                      "3" = "Fast-Risky", 
                                                                                      "4" = "Fast-Safe"))) +
      guides(fill = FALSE) +
      theme_bw() +
      theme(panel.grid.major = element_blank(),
            legend.position = "right",
            plot.margin = margin(1,1,1,1, "cm"),
            text=element_text(size=12, color = "#083855", family="Lato Light"),
            axis.text=element_text(size=8, color = "#083855", family="Lato Light"),
            strip.background = element_rect(fill = "white"))
    p
    
    file_figure <- tempfile(paste("basal", "median_iqr", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
    ggsave(file_figure, plot = p, device = NULL, path = NULL,
           scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
           units =  "mm")
  }
  
  ### MEDIAN PLANT HEIGHT (grouped)
  {
    p <- ggplot(basal_data, aes(x = time, y = median_basal, color = as.factor(species_id))) + 
      geom_line() +
      geom_point() +
      geom_linerange(aes(ymin = quantile_025_basal, ymax = quantile_075_basal), alpha = 0.75) +
      scale_color_manual("Allocation \nStrategy:",
                         values=foxes_palettes$main[c(2, 1, 4, 3)],
                         labels=c("Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe")) +
      scale_x_continuous("Time (yrs)") +
      scale_y_continuous("Median Basal Area per Plant (m2) Per Sub-Environment Repetition") +
      guides(fill = FALSE) +
      theme_bw() +
      theme(panel.grid.major = element_blank(),
            legend.position = "right",
            plot.margin = margin(1,1,1,1, "cm"),
            text=element_text(size=12, color = "#083855", family="Lato Light"),
            axis.text=element_text(size=8, color = "#083855", family="Lato Light"),
            strip.background = element_rect(fill = "white"))
    p
    
    file_figure <- tempfile(paste("basal", "median_iqr", "grouped", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
    ggsave(file_figure, plot = p, device = NULL, path = NULL,
           scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
           units =  "mm")
  }
  
  ### MEAN PLANT HEIGHT
  {
    p <- ggplot(basal_data, aes(x = time, y = mean_basal, color = as.factor(species_id))) + 
      geom_line() +
      geom_point() +
      geom_linerange(aes(ymin = quantile_025_basal, ymax = quantile_075_basal), alpha = 0.75) +
      scale_color_manual("Allocation \nStrategy:",
                         values=foxes_palettes$main[c(2, 1, 4, 3)],
                         labels=c("Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe")) +
      scale_x_continuous("Time (yrs)") +
      scale_y_continuous("Mean Basal Area per Plant (m2) Per Sub-Environment Repetition") +
      facet_wrap(.~species_id, nrow = 2, ncol = 2, labeller = labeller(species_id = c("1" = "Slow-Risky", 
                                                                                      "2" = "Slow-Safe",
                                                                                      "3" = "Fast-Risky", 
                                                                                      "4" = "Fast-Safe"))) +
      guides(fill = FALSE) +
      theme_bw() +
      theme(panel.grid.major = element_blank(),
            legend.position = "right",
            plot.margin = margin(1,1,1,1, "cm"),
            text=element_text(size=12, color = "#083855", family="Lato Light"),
            axis.text=element_text(size=8, color = "#083855", family="Lato Light"),
            strip.background = element_rect(fill = "white"))
    p
    
    file_figure <- tempfile(paste("basal", "mean_iqr", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
    ggsave(file_figure, plot = p, device = NULL, path = NULL,
           scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
           units =  "mm")
  }
  
  ### MEAN PLANT HEIGHT (grouped)
  {
    p <- ggplot(basal_data, aes(x = time, y = mean_basal, color = as.factor(species_id))) + 
      geom_line() +
      geom_point() +
      geom_linerange(aes(ymin = quantile_025_basal, ymax = quantile_075_basal), alpha = 0.75) +
      scale_color_manual("Allocation \nStrategy:",
                         values=foxes_palettes$main[c(2, 1, 4, 3)],
                         labels=c("Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe")) +
      scale_x_continuous("Time (yrs)") +
      scale_y_continuous("Mean Basal Area per Plant (m2) Per Sub-Environment Repetition") +
      guides(fill = FALSE) +
      theme_bw() +
      theme(panel.grid.major = element_blank(),
            legend.position = "right",
            plot.margin = margin(1,1,1,1, "cm"),
            text=element_text(size=12, color = "#083855", family="Lato Light"),
            axis.text=element_text(size=8, color = "#083855", family="Lato Light"),
            strip.background = element_rect(fill = "white"))
    p
    
    file_figure <- tempfile(paste("height", "mean_iqr", "grouped", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
    ggsave(file_figure, plot = p, device = NULL, path = NULL,
           scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
           units =  "mm")
  }
  
})

lapply(2:length(out_env_basal), function(ind){
  
  basal_data = out_env_basal[[ind]]
  
  if(length(basal_data)  ==0){
    return()
  }
  
  dir = out_dirs_env_total[[ind]]
  path_parts <- unlist(str_split(dir, "/"))
  new_dir_path <- file.path("out", "full_run_auto", "environment_agg", path_parts[3])
  
  print(new_dir_path)
  
  dir.create(new_dir_path, recursive = TRUE)
  
  ### PLOT SUM BASAL (mean)
  {
    p <- ggplot(basal_data, aes(x = time, y = mean_sum_basal, color = as.factor(species_id))) + 
      geom_line() +
      geom_point() +
      geom_linerange(aes(ymin = quantile_025_sum_basal, ymax = quantile_075_sum_basal), alpha = 0.75) +
      scale_color_manual("Allocation \nStrategy:",
                         values=foxes_palettes$main[c(2, 1, 4, 3)],
                         labels=c("Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe")) +
      scale_x_continuous("Time (yrs)") +
      scale_y_continuous("Average Basal Area Coverage per Hectar (m2/ha) Per Sub-Environment Repetition") +
      facet_wrap(.~species_id, nrow = 2, ncol = 2, labeller = labeller(species_id = c("1" = "Slow-Risky", 
                                                                                      "2" = "Slow-Safe",
                                                                                      "3" = "Fast-Risky", 
                                                                                      "4" = "Fast-Safe"))) +
      guides(fill = FALSE) +
      theme_bw() +
      theme(panel.grid.major = element_blank(),
            legend.position = "right",
            plot.margin = margin(1,1,1,1, "cm"),
            text=element_text(size=12, color = "#083855", family="Lato Light"),
            axis.text=element_text(size=8, color = "#083855", family="Lato Light"),
            strip.background = element_rect(fill = "white"))
    p
    
    file_figure <- tempfile(paste("mean_basal_sum", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
    ggsave(file_figure, plot = p, device = NULL, path = NULL,
           scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
           units =  "mm")
  }
  
  ### PLOT MAXIMUM HEIGHT (mean) together
  {
    p <- ggplot(basal_data, aes(x = time, y = mean_sum_basal, color = as.factor(species_id))) + 
      geom_line() +
      geom_point() +
      geom_linerange(aes(ymin = quantile_025_sum_basal, ymax = quantile_075_sum_basal), alpha = 0.75) +
      scale_color_manual("Allocation \nStrategy:",
                         values=foxes_palettes$main[c(2, 1, 4, 3)],
                         labels=c("Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe")) +
      scale_x_continuous("Time (yrs)") +
      scale_y_continuous("Average Basal Area Coverage per Hectar (m2/ha) Per Sub-Environment Repetition") +
      guides(fill = FALSE) +
      theme_bw() +
      theme(panel.grid.major = element_blank(),
            legend.position = "right",
            plot.margin = margin(1,1,1,1, "cm"),
            text=element_text(size=12, color = "#083855", family="Lato Light"),
            axis.text=element_text(size=8, color = "#083855", family="Lato Light"),
            strip.background = element_rect(fill = "white"))
    p
    
    file_figure <- tempfile(paste("mean_basal_sum_grouped", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
    ggsave(file_figure, plot = p, device = NULL, path = NULL,
           scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
           units =  "mm")
  } 
  
  ### PLOT MAXIMUM Basal Area Ind Strategy
  {
    p <- ggplot(basal_data, aes(x = time, y = max_basal, color = as.factor(species_id))) + 
      geom_line() +
      geom_point() +
      scale_color_manual("Allocation \nStrategy:",
                         values=foxes_palettes$main[c(2, 1, 4, 3)],
                         labels=c("Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe")) +
      scale_x_continuous("Time (yrs)") +
      scale_y_continuous("Maximum Basal Area per Plant (m2) Per Sub-Environment Repetition") +
      facet_wrap(.~species_id, nrow = 2, ncol = 2, labeller = labeller(species_id = c("1" = "Slow-Risky", 
                                                                                      "2" = "Slow-Safe",
                                                                                      "3" = "Fast-Risky", 
                                                                                      "4" = "Fast-Safe"))) +
      guides(fill = FALSE) +
      theme_bw() +
      theme(panel.grid.major = element_blank(),
            legend.position = "right",
            plot.margin = margin(1,1,1,1, "cm"),
            text=element_text(size=12, color = "#083855", family="Lato Light"),
            axis.text=element_text(size=8, color = "#083855", family="Lato Light"),
            strip.background = element_rect(fill = "white"))
    p
    
    file_figure <- tempfile(paste("max_basal", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
    ggsave(file_figure, plot = p, device = NULL, path = NULL,
           scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
           units =  "mm")
  }
  
  ### PLOT MAXIMUM HEIGHT together
  {
    p <- ggplot(basal_data, aes(x = time, y = max_basal, color = as.factor(species_id))) + 
      geom_line() +
      geom_point() +
      scale_color_manual("Allocation \nStrategy:",
                         values=foxes_palettes$main[c(2, 1, 4, 3)],
                         labels=c("Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe")) +
      scale_x_continuous("Time (yrs)") +
      scale_y_continuous("Maximum Basal Area per Plant (m2) Per Sub-Environment Repetition") +
      
      guides(fill = FALSE) +
      theme_bw() +
      theme(panel.grid.major = element_blank(),
            legend.position = "right",
            plot.margin = margin(1,1,1,1, "cm"),
            text=element_text(size=12, color = "#083855", family="Lato Light"),
            axis.text=element_text(size=8, color = "#083855", family="Lato Light"),
            strip.background = element_rect(fill = "white"))
    p
    
    file_figure <- tempfile(paste("max_grouped_basal", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
    ggsave(file_figure, plot = p, device = NULL, path = NULL,
           scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
           units =  "mm")
  }
  
  ### MEADIN PLANT HEIGHT 
  {
    p <- ggplot(basal_data, aes(x = time, y = median_basal, color = as.factor(species_id))) + 
      geom_line() +
      geom_point() +
      geom_linerange(aes(ymin = quantile_025_basal, ymax = quantile_075_basal), alpha = 0.75) +
      scale_color_manual("Allocation \nStrategy:",
                         values=foxes_palettes$main[c(2, 1, 4, 3)],
                         labels=c("Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe")) +
      scale_x_continuous("Time (yrs)") +
      scale_y_continuous("Median Basal Area per Plant (m2) Per Sub-Environment Repetition") +
      facet_wrap(.~species_id, nrow = 2, ncol = 2, labeller = labeller(species_id = c("1" = "Slow-Risky", 
                                                                                      "2" = "Slow-Safe",
                                                                                      "3" = "Fast-Risky", 
                                                                                      "4" = "Fast-Safe"))) +
      guides(fill = FALSE) +
      theme_bw() +
      theme(panel.grid.major = element_blank(),
            legend.position = "right",
            plot.margin = margin(1,1,1,1, "cm"),
            text=element_text(size=12, color = "#083855", family="Lato Light"),
            axis.text=element_text(size=8, color = "#083855", family="Lato Light"),
            strip.background = element_rect(fill = "white"))
    p
    
    file_figure <- tempfile(paste("basal", "median_iqr", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
    ggsave(file_figure, plot = p, device = NULL, path = NULL,
           scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
           units =  "mm")
  }
  
  ### MEDIAN PLANT HEIGHT (grouped)
  {
    p <- ggplot(basal_data, aes(x = time, y = median_basal, color = as.factor(species_id))) + 
      geom_line() +
      geom_point() +
      geom_linerange(aes(ymin = quantile_025_basal, ymax = quantile_075_basal), alpha = 0.75) +
      scale_color_manual("Allocation \nStrategy:",
                         values=foxes_palettes$main[c(2, 1, 4, 3)],
                         labels=c("Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe")) +
      scale_x_continuous("Time (yrs)") +
      scale_y_continuous("Median Basal Area per Plant (m2) Per Sub-Environment Repetition") +
      guides(fill = FALSE) +
      theme_bw() +
      theme(panel.grid.major = element_blank(),
            legend.position = "right",
            plot.margin = margin(1,1,1,1, "cm"),
            text=element_text(size=12, color = "#083855", family="Lato Light"),
            axis.text=element_text(size=8, color = "#083855", family="Lato Light"),
            strip.background = element_rect(fill = "white"))
    p
    
    file_figure <- tempfile(paste("basal", "median_iqr", "grouped", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
    ggsave(file_figure, plot = p, device = NULL, path = NULL,
           scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
           units =  "mm")
  }
  
  ### MEAN PLANT HEIGHT
  {
    p <- ggplot(basal_data, aes(x = time, y = mean_basal, color = as.factor(species_id))) + 
      geom_line() +
      geom_point() +
      geom_linerange(aes(ymin = quantile_025_basal, ymax = quantile_075_basal), alpha = 0.75) +
      scale_color_manual("Allocation \nStrategy:",
                         values=foxes_palettes$main[c(2, 1, 4, 3)],
                         labels=c("Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe")) +
      scale_x_continuous("Time (yrs)") +
      scale_y_continuous("Mean Basal Area per Plant (m2) Per Sub-Environment Repetition") +
      facet_wrap(.~species_id, nrow = 2, ncol = 2, labeller = labeller(species_id = c("1" = "Slow-Risky", 
                                                                                      "2" = "Slow-Safe",
                                                                                      "3" = "Fast-Risky", 
                                                                                      "4" = "Fast-Safe"))) +
      guides(fill = FALSE) +
      theme_bw() +
      theme(panel.grid.major = element_blank(),
            legend.position = "right",
            plot.margin = margin(1,1,1,1, "cm"),
            text=element_text(size=12, color = "#083855", family="Lato Light"),
            axis.text=element_text(size=8, color = "#083855", family="Lato Light"),
            strip.background = element_rect(fill = "white"))
    p
    
    file_figure <- tempfile(paste("basal", "mean_iqr", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
    ggsave(file_figure, plot = p, device = NULL, path = NULL,
           scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
           units =  "mm")
  }
  
  ### MEAN PLANT HEIGHT (grouped)
  {
    p <- ggplot(basal_data, aes(x = time, y = mean_basal, color = as.factor(species_id))) + 
      geom_line() +
      geom_point() +
      geom_linerange(aes(ymin = quantile_025_basal, ymax = quantile_075_basal), alpha = 0.75) +
      scale_color_manual("Allocation \nStrategy:",
                         values=foxes_palettes$main[c(2, 1, 4, 3)],
                         labels=c("Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe")) +
      scale_x_continuous("Time (yrs)") +
      scale_y_continuous("Mean Basal Area per Plant (m2) Per Sub-Environment Repetition") +
      guides(fill = FALSE) +
      theme_bw() +
      theme(panel.grid.major = element_blank(),
            legend.position = "right",
            plot.margin = margin(1,1,1,1, "cm"),
            text=element_text(size=12, color = "#083855", family="Lato Light"),
            axis.text=element_text(size=8, color = "#083855", family="Lato Light"),
            strip.background = element_rect(fill = "white"))
    p
    
    file_figure <- tempfile(paste("basal", "mean_iqr", "grouped", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
    ggsave(file_figure, plot = p, device = NULL, path = NULL,
           scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
           units =  "mm")
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




