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

figure_dir <- "out/figures/"
dir.create(figure_dir)

data_directories <- list.dirs("data/full_run_processed", recursive = FALSE)
# data_directories <- list.dirs("data/full_run_new_processed", recursive = FALSE)
# data_directories <- list.dirs("data/full_run_new_2_processed", recursive = FALSE)
# data_directories <- list.dirs("data/full_run_4_processed/environment_1_0", recursive = FALSE)

h = 2
h2 = h/2

max_time = 99

  data_rolling_ave <- plyr::rbind.fill(lapply(data_directories, function(dat){
    files <- list.files(path = dat, recursive = TRUE)
    file_names_full <- as.vector(sapply(files, function(x){
      return(file.path(dat, x))
    }))
    
    file_names_full <- file_names_full[grep("extra", file_names_full, invert = TRUE)]
    print(file_names_full)
    
    all_data <- plyr::rbind.fill(lapply(file_names_full, function(file_name){
        data <- readRDS(file_name)
        data <- data %>%
          dplyr::select(species_id, tree_id, env_rep, run_rep, time, env_sd, env_mean, area_stem, height, area_leaf, mass_storage, is_alive,
                        mortality_storage_dependent, mortality_growth_dependent, mortality_new, stress)
        return(data)
      }))
    
    seq_time = seq(from = h2, to = max_time, by = h2)
    
    tree_summary <- plyr::rbind.fill(lapply(seq_time, function(t){
      summary_data <- all_data %>% 
        dplyr::filter((time > (t-h2)) & (time < (t+h2)) & is_alive == TRUE) %>%
        dplyr::group_by(species_id, env_rep, run_rep, time, env_sd, env_mean) %>%
        dplyr::summarise(sum_basal = sum(100 * area_stem, na.rm = TRUE),
                         max_height = max(height, na.rm = TRUE),
                         sum_leaf_area = sum(area_leaf/100, na.rm = TRUE),
                         sum_mass_storage = sum(mass_storage/100, na.rm = TRUE),
                         live = n()) %>%
        dplyr::group_by(species_id, env_sd, env_mean) %>%
        dplyr::summarise(mean_sum_basal = mean(sum_basal, na.rm = TRUE),
                         sd_sum_basal = sd(sum_basal, na.rm=TRUE),
                         se_sum_basal = std.error(sum_basal, na.rm=TRUE),
                         mean_max_height = mean(max_height, na.rm = TRUE),
                         sd_max_height = sd(max_height, na.rm=TRUE),
                         se_max_height = std.error(max_height, na.rm=TRUE),
                         mean_sum_lai = mean(sum_leaf_area, na.rm = TRUE),
                         sd_sum_lai = sd(sum_leaf_area, na.rm=TRUE),
                         se_sum_lai = std.error(sum_leaf_area, na.rm=TRUE),
                         mean_sum_mass_storage_m2 = mean(sum_mass_storage, na.rm = TRUE),
                         sd_sum_mass_storage_m2 = sd(sum_mass_storage, na.rm=TRUE),
                         se_sum_mass_storage_m2 = std.error(sum_mass_storage, na.rm=TRUE),
                         mean_live_count = mean(live, na.rm = TRUE),
                         sd_live_count = sd(live, na.rm=TRUE),
                         se_live_count = std.error(live, na.rm=TRUE))
      mortality_risk <- all_data %>% 
        dplyr::filter((time > (t-h2)) & (time < (t+h2))) %>%
        dplyr::group_by(species_id, env_sd, env_mean) %>%
        dplyr::summarise(mean_mortality_risk = mean(mortality_new, na.rm = TRUE),
                         sd_mortality_risk = sd(mortality_new, na.rm=TRUE),
                         se_mortality_risk = std.error(mortality_new, na.rm=TRUE),
                         mean_mortality_risk_growth = mean(mortality_growth_dependent, na.rm = TRUE),
                         sd_mortality_risk_growth = sd(mortality_growth_dependent, na.rm=TRUE),
                         se_mortality_risk_growth = std.error(mortality_growth_dependent, na.rm=TRUE),
                         mean_mortality_risk_storage = mean(mortality_storage_dependent, na.rm = TRUE),
                         sd_mortality_risk_storage = sd(mortality_storage_dependent, na.rm=TRUE),
                         se_mortality_risk_storage = std.error(mortality_storage_dependent, na.rm=TRUE))
      mortality_count <- all_data %>% 
        dplyr::filter((time > (t-h2)) & (time < (t+h2)) & is_alive == FALSE) %>%
        dplyr::group_by(species_id, env_rep, run_rep, time, env_sd, env_mean) %>%
        dplyr::count() %>%
        dplyr::group_by(species_id, env_sd, env_mean) %>%
        dplyr::summarise(mean_num_deaths = mean(n, na.rm = TRUE),
                         sd_num_deaths = sd(n, na.rm=TRUE),
                         se_num_deaths = std.error(n, na.rm=TRUE))
      ind_summary <- all_data %>% 
        dplyr::filter((time > (t-h2)) & (time < (t+h2)) & is_alive == TRUE) %>%
        dplyr::group_by(species_id, env_sd, env_mean) %>%
        dplyr::summarise(mean_basal = mean(area_stem, na.rm = TRUE),
                         sd_basal = sd(area_stem, na.rm=TRUE),
                         se_basal = std.error(area_stem, na.rm=TRUE),
                         mean_height = mean(height, na.rm = TRUE),
                         sd_height = sd(height, na.rm=TRUE),
                         se_height = std.error(height, na.rm=TRUE),
                         mean_area_leaf = mean(area_leaf, na.rm = TRUE),
                         sd_area_leaf = sd(area_leaf, na.rm=TRUE),
                         se_area_leaf = std.error(area_leaf, na.rm=TRUE),
                         mean_storage_mass = mean(mass_storage, na.rm = TRUE),
                         sd_storage_mass = sd(mass_storage, na.rm=TRUE),
                         se_storage_mass = std.error(mass_storage, na.rm=TRUE),
                         stress_mean = mean(stress, na.rm=TRUE))
      data <- full_join(summary_data, mortality_risk, by = c("species_id", "env_sd", "env_mean"))
      data <- full_join(data, mortality_count, by = c("species_id", "env_sd", "env_mean"))
      data <- full_join(data, ind_summary, by = c("species_id", "env_sd", "env_mean"))
      
      
      data$time = t
        return(data)
      
    }))
    
    
    return(tree_summary)
  }))
  
  max_time = 19
  
  data_individual <- plyr::rbind.fill(lapply(data_directories, function(dat){
    files <- list.files(path = dat, recursive = TRUE)
    file_names_full <- as.vector(sapply(files, function(x){
      return(file.path(dat, x))
    }))
    file_names_full <- file_names_full[grep("extra", file_names_full, invert = TRUE)]
     
    print(file_names_full)
    print(max_time)
    
    all_data <- plyr::rbind.fill(lapply(file_names_full, function(file_name){
      data_file <- readRDS(file_name)
      data <- data_file %>%
        select(c(species_id, tree_id, run_rep, env_rep, time, env_sd, env_mean, area_stem, height, area_leaf, mass_storage, is_alive,
                      mortality_storage_dependent, mortality_growth_dependent, mortality_new)) %>%
        dplyr::filter(time < max_time)
      return(data)
    }))
    
    return(all_data)
    
  }))

  rolling_ave_sub <- data_rolling_ave %>%
    filter(time < 20)
  
  control_ind <- data_individual %>%
    filter(env_mean == 1 & env_sd == 0)
  env_med_no_ind <- data_individual %>%
    filter(env_mean == 85 & env_sd == 0)
  env_med_low_ind <- data_individual %>%
    filter(env_mean == 85 & env_sd == 15)
  env_med_mid_ind <- data_individual %>%
    filter(env_mean == 85 & env_sd == 30)
  env_med_high_ind <- data_individual %>%
    filter(env_mean == 85 & env_sd == 60)
  env_big_no_ind <- data_individual %>%
    filter(env_mean == 75 & env_sd == 0)
  env_big_low_ind <- data_individual %>%
    filter(env_mean == 75 & env_sd == 15)
  env_big_mid_ind <- data_individual %>%
    filter(env_mean == 75 & env_sd == 30)
  env_big_high_ind <- data_individual %>%
    filter(env_mean == 75 & env_sd == 60)
  env_no_ind <- data_individual %>%
    filter(env_sd == 15)
  env_low_ind <- data_individual %>%
    filter(env_sd == 15)
  env_mid_ind <- data_individual %>%
    filter(env_sd == 30)
  env_high_ind <- data_individual %>%
    filter(env_sd == 60)
  env_med_ind <- data_individual %>%
    filter(env_mean == 85)
  env_big_ind <- data_individual %>%
    filter(env_mean == 75)
  
  control_rolling <- rolling_ave_sub %>%
    filter(env_mean == 1 & env_sd == 0)
  env_med_no_rolling <- rolling_ave_sub %>%
    filter(env_mean == 85 & env_sd == 0)
  env_med_low_rolling <- rolling_ave_sub %>%
    filter(env_mean == 85 & env_sd == 15)
  env_med_mid_rolling <- rolling_ave_sub %>%
    filter(env_mean == 85 & env_sd == 30)
  env_med_high_rolling <- rolling_ave_sub %>%
    filter(env_mean == 85 & env_sd == 60)
  env_big_no_rolling <- rolling_ave_sub %>%
    filter(env_mean == 75 & env_sd == 0)
  env_big_low_rolling <- rolling_ave_sub %>%
    filter(env_mean == 75 & env_sd == 15)
  env_big_mid_rolling <- rolling_ave_sub %>%
    filter(env_mean == 75 & env_sd == 30)
  env_big_high_rolling <- rolling_ave_sub %>%
    filter(env_mean == 75 & env_sd == 60)
  env_no_rolling <- rolling_ave_sub %>%
    filter(env_sd == 15)
  env_low_rolling <- rolling_ave_sub %>%
    filter(env_sd == 15)
  env_mid_rolling <- rolling_ave_sub %>%
    filter(env_sd == 30)
  env_high_rolling <- rolling_ave_sub %>%
    filter(env_sd == 60)
  env_med_rolling <- rolling_ave_sub %>%
    filter(env_mean == 85)
  env_big_rolling <- rolling_ave_sub %>%
    filter(env_mean == 75)
  
  
  
  ##
  
  
  p <- ggplot(control_ind, aes(x = time,  colour = as.factor(species_id))) +
    geom_line(aes(y = area_stem, group = as.factor(tree_id * 10 + as.numeric(run_rep))), alpha = 0.1) + 
    scm + 
    species_wrap + 
    foxes_theme
  p
  
  file_figure <- tempfile(paste("max", file_name_in, "group", sep = "_"), tmpdir = new_dir_path, fileext = ".png")
  ggsave(file_figure, plot = p, device = NULL, path = NULL,
         scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
         units =  "mm")
  
  p <- ggplot(control_ind, aes(x = time,  colour = as.factor(species_id))) +
    geom_line(aes(y = mortality_growth_dependent, group = as.factor(tree_id * 10 + as.numeric(run_rep))), alpha = 0.1) + 
    scm + 
    species_wrap + 
    foxes_theme
  p
  
  p <- ggplot(control_ind, aes(x = time,  colour = as.factor(species_id))) +
    geom_line(aes(y = mortality_new, group = as.factor(tree_id * 10 + as.numeric(run_rep))), alpha = 0.1) + 
    scm + 
    species_wrap + 
    foxes_theme
  p
  
  
  p <- ggplot(control_ind, aes(x = time,  colour = as.factor(species_id))) +
    geom_line(aes(y = mean_sum_basal), data = control_rolling) +
    scm + 
    species_wrap + 
    foxes_theme
  p
  
  p <- ggplot(control_ind, aes(x = time,  colour = as.factor(species_id))) +
    geom_line(aes(y = height, group = as.factor(tree_id * 10 + as.numeric(run_rep))), alpha = 0.1) +
    scm + 
    species_wrap + 
    foxes_theme
  p
  
  
  
  
  # data_basal_area_all <- plyr::rbind.fill(lapply("data/full_run_processed", function(dat){
  #   files <- list.files(path = dat, recursive = TRUE)
  #   file_names_full <- as.vector(sapply(files, function(x){
  #     return(file.path(dat, x))
  #   }))
  #   
  #   all_data <- plyr::rbind.fill(lapply(file_names_full, readRDS))
  #   
  #   seq_time = seq(from = h2, to = 99, by = h2)
  #   
  #   tree_sum_basal <- plyr::rbind.fill(lapply(seq_time, function(t){
  #     basal <- all_data %>% 
  #       dplyr::filter((time > (t-h2)) & (time < (t+h2))) %>%
  #       dplyr::group_by(species_id, run_rep, time) %>%
  #       dplyr::summarise(sum_basal = sum(100 * area_stem, na.rm = TRUE),
  #                        max_height = max(height, na.rm = TRUE),
  #                        sum_leaf_area = sum(area_leaf/100, na.rm = TRUE),
  #                        sum_mass_storage = sum(mass_storage/100, na.rm = TRUE)) %>%
  #       dplyr::group_by(species_id) %>%
  #       dplyr::summarise(mean_sum_basal = mean(sum_basal, na.rm = TRUE),
  #                        sd_sum_basal = sd(sum_basal, na.rm=TRUE),
  #                        se_sum_basal = std.error(sum_basal, na.rm=TRUE),
  #                        mean_max_height = mean(max_height, na.rm = TRUE),
  #                        sd_max_height = sd(max_height, na.rm=TRUE),
  #                        se_max_height = std.error(max_height, na.rm=TRUE),
  #                        mean_sum_lai = mean(sum_leaf_area, na.rm = TRUE),
  #                        sd_sum_lai = sd(sum_leaf_area, na.rm=TRUE),
  #                        se_sum_lai = std.error(sum_leaf_area, na.rm=TRUE),
  #                        mean_sum_mass_storage_m2 = mean(sum_mass_storage, na.rm = TRUE),
  #                        sd_sum_mass_storage_m2 = sd(sum_mass_storage, na.rm=TRUE),
  #                        se_sum_mass_storage_m2 = std.error(sum_mass_storage, na.rm=TRUE))
  #     basal$time = t
  #     return(basal)
  #     
  #   }))
  #   return(tree_sum_basal)
  # }))
  # 
  # data_basal_area_sd <- plyr::rbind.fill(lapply("data/full_run_processed", function(dat){
  #   files <- list.files(path = dat, recursive = TRUE)
  #   file_names_full <- as.vector(sapply(files, function(x){
  #     return(file.path(dat, x))
  #   }))
  #   
  #   all_data <- plyr::rbind.fill(lapply(file_names_full, readRDS))
  #   
  #   seq_time = seq(from = h2, to = 99, by = h2)
  #   
  #   tree_sum_basal <- plyr::rbind.fill(lapply(seq_time, function(t){
  #     basal <- all_data %>% 
  #       dplyr::filter((time > (t-h2)) & (time < (t+h2))) %>%
  #       dplyr::group_by(species_id, run_rep, time, env_sd) %>%
  #       dplyr::summarise(sum_basal = sum(100 * area_stem, na.rm = TRUE),
  #                        max_height = max(height, na.rm = TRUE),
  #                        sum_leaf_area = sum(area_leaf/100, na.rm = TRUE),
  #                        sum_mass_storage = sum(mass_storage/100, na.rm = TRUE)) %>%
  #       dplyr::group_by(species_id, env_sd) %>%
  #       dplyr::summarise(mean_sum_basal = mean(sum_basal, na.rm = TRUE),
  #                        sd_sum_basal = sd(sum_basal, na.rm=TRUE),
  #                        se_sum_basal = std.error(sum_basal, na.rm=TRUE),
  #                        mean_max_height = mean(max_height, na.rm = TRUE),
  #                        sd_max_height = sd(max_height, na.rm=TRUE),
  #                        se_max_height = std.error(max_height, na.rm=TRUE),
  #                        mean_sum_lai = mean(sum_leaf_area, na.rm = TRUE),
  #                        sd_sum_lai = sd(sum_leaf_area, na.rm=TRUE),
  #                        se_sum_lai = std.error(sum_leaf_area, na.rm=TRUE),
  #                        mean_sum_mass_storage_m2 = mean(sum_mass_storage, na.rm = TRUE),
  #                        sd_sum_mass_storage_m2 = sd(sum_mass_storage, na.rm=TRUE),
  #                        se_sum_mass_storage_m2 = std.error(sum_mass_storage, na.rm=TRUE))
  #     basal$time = t
  #     return(basal)
  #     
  #   }))
  #   return(tree_sum_basal)
  # }))
  # 
  # data_basal_area_mean <- plyr::rbind.fill(lapply("data/full_run_processed", function(dat){
  #   files <- list.files(path = dat, recursive = TRUE)
  #   file_names_full <- as.vector(sapply(files, function(x){
  #     return(file.path(dat, x))
  #   }))
  #   
  #   all_data <- plyr::rbind.fill(lapply(file_names_full, readRDS))
  #   
  #   seq_time = seq(from = h2, to = 99, by = h2)
  #   
  #   tree_sum_basal <- plyr::rbind.fill(lapply(seq_time, function(t){
  #     basal <- all_data %>% 
  #       dplyr::filter((time > (t-h2)) & (time < (t+h2))) %>%
  #       dplyr::group_by(species_id, run_rep, time, env_mean) %>%
  #       dplyr::summarise(sum_basal = sum(100 * area_stem, na.rm = TRUE),
  #                        max_height = max(height, na.rm = TRUE),
  #                        sum_leaf_area = sum(area_leaf/100, na.rm = TRUE),
  #                        sum_mass_storage = sum(mass_storage/100, na.rm = TRUE)) %>%
  #       dplyr::group_by(species_id, env_mean) %>%
  #       dplyr::summarise(mean_sum_basal = mean(sum_basal, na.rm = TRUE),
  #                        sd_sum_basal = sd(sum_basal, na.rm=TRUE),
  #                        se_sum_basal = std.error(sum_basal, na.rm=TRUE),
  #                        mean_max_height = mean(max_height, na.rm = TRUE),
  #                        sd_max_height = sd(max_height, na.rm=TRUE),
  #                        se_max_height = std.error(max_height, na.rm=TRUE),
  #                        mean_sum_lai = mean(sum_leaf_area, na.rm = TRUE),
  #                        sd_sum_lai = sd(sum_leaf_area, na.rm=TRUE),
  #                        se_sum_lai = std.error(sum_leaf_area, na.rm=TRUE),
  #                        mean_sum_mass_storage_m2 = mean(sum_mass_storage, na.rm = TRUE),
  #                        sd_sum_mass_storage_m2 = sd(sum_mass_storage, na.rm=TRUE),
  #                        se_sum_mass_storage_m2 = std.error(sum_mass_storage, na.rm=TRUE))
  #     basal$time = t
  #     return(basal)
  #     
  #   }))
  #   return(tree_sum_basal)
  # }))
  # 
  
  
  save(data_basal_area, data_basal_area_all, data_basal_area_mean, data_basal_area_sd, file = "rolling_ave_new.RData")
  
  data_basal_area_all_new <- data_basal_area_all
  data_basal_area_all <- data_basal_area_all_new
  load("rolling_ave.RData")
  
  data_basal_area$env = as.factor(data_basal_area$env_mean + data_basal_area$env_sd)
  
  data_basal_area_sub = subset(data_basal_area, env %in% c(90, 100, 135, 145))
  data_basal_area_sub$env_species = as.factor(data_basal_area_sub$env_mean + data_basal_area_sub$env_sd + data_basal_area_sub$species_id)

  data_basal_area_sub$env_species = fct_relevel(data_basal_area_sub$env_species, "101", "102", "91", "92",
                                                                                 "103", "104", "93", "94",
                                                                                 "146", "147", "136", "137",
                                                                                 "148", "149", "138", "139")
    
    
  p1 <- ggplot(data_basal_area_sub, aes(x = time, y = mean_sum_basal * 100, 
                              ymin = mean_sum_basal * 100 - sd_sum_basal * 100, 
                              ymax = mean_sum_basal * 100 + sd_sum_basal * 100,
                              color = as.factor(species_id), fill = as.factor(species_id))) +
    geom_line() +
    gr_ribbon + scm_ribbon + sfm +
    scale_x_continuous("Time (yrs)") +
    scale_y_continuous("Average Basal Area Coverage per Hectar (m2/ha) in Run") +
    facet_wrap(.~env_species, nrow = 4, ncol = 4) +
    guides(fill = FALSE) +
    foxes_theme +
    theme(strip.text = element_blank(),
          panel.spacing = unit(c(0.2,0.5,0.2), "cm"),
          legend.position = "bottom")
  p1
  
  file_figure <- tempfile(paste("basal_area_examples", sep = "_"), tmpdir = figure_dir, fileext = ".png")
  ggsave(file_figure, plot = p1, device = NULL, path = NULL,
         scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
         units =  "mm")

  
  p1 <- ggplot(data_basal_area_sub, aes(x = time, y = mean_max_height, 
                                        ymin = mean_max_height - sd_max_height, 
                                        ymax = mean_max_height + sd_max_height,
                                        color = as.factor(species_id), fill = as.factor(species_id))) +
    geom_line() +
    gr_ribbon + scm_ribbon + sfm +
    scale_x_continuous("Time (yrs)") +
    scale_y_continuous("Average Maximum Height (m) in Run") +
    facet_wrap(.~env_species, nrow = 4, ncol = 4) +
    guides(fill = FALSE) +
    foxes_theme +
    theme(strip.text = element_blank(),
          panel.spacing = unit(c(0.2,0.5,0.2), "cm"),
          legend.position = "bottom")
  p1
  
  file_figure <- tempfile(paste("max_height_examples", sep = "_"), tmpdir = figure_dir, fileext = ".png")
  ggsave(file_figure, plot = p1, device = NULL, path = NULL,
         scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
         units =  "mm")

  
    
  p1 <- ggplot(data_basal_area_sub, aes(x = time, y = mean_sum_lai, 
                                        ymin = mean_sum_lai - sd_sum_lai, 
                                        ymax = mean_sum_lai + sd_sum_lai,
                                        color = as.factor(species_id), fill = as.factor(species_id))) +
    geom_line() +
    gr_ribbon + scm_ribbon + sfm +
    scale_x_continuous("Time (yrs)") +
    scale_y_continuous("Average LAI (m2/m2)") +
    facet_wrap(.~env_species, nrow = 4, ncol = 4) +
    guides(fill = FALSE) +
    foxes_theme +
    theme(strip.text = element_blank(),
          panel.spacing = unit(c(0.2,0.5,0.2), "cm"),
          legend.position = "bottom")
  p1
  
  file_figure <- tempfile(paste("lai_examples", sep = "_"), tmpdir = figure_dir, fileext = ".png")
  ggsave(file_figure, plot = p1, device = NULL, path = NULL,
         scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
         units =  "mm")
  
  
  p1 <- ggplot(data_basal_area_sub, aes(x = time, y = mean_sum_mass_storage_m2, 
                                        ymin = mean_sum_mass_storage_m2 - sd_sum_mass_storage_m2, 
                                        ymax = mean_sum_mass_storage_m2 + sd_sum_mass_storage_m2,
                                        color = as.factor(species_id), fill = as.factor(species_id))) +
    geom_line() +
    gr_ribbon + scm_ribbon + sfm +
    scale_x_continuous("Time (yrs)") +
    scale_y_continuous("Average Storage Mass Per m2 (kg/m2)") +
    facet_wrap(.~env_species, nrow = 4, ncol = 4) +
    guides(fill = FALSE) +
    foxes_theme +
    theme(strip.text = element_blank(),
          panel.spacing = unit(c(0.2,0.5,0.2), "cm"),
          legend.position = "bottom")
  p1
  
  file_figure <- tempfile(paste("mass_storage_examples", sep = "_"), tmpdir = figure_dir, fileext = ".png")
  ggsave(file_figure, plot = p1, device = NULL, path = NULL,
         scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
         units =  "mm")

  
  
  
  ## COMBINED ALL ENVIRONMENTS
  
  
  #### Basal Area
  
  data_basal_area$sp_id_env_mean = as.factor(data_basal_area$species_id + data_basal_area$env_mean)
  data_basal_area_all$sp_id_env_mean = as.factor(data_basal_area_all$species_id)
  data_basal_area_all$env_sd = as.factor(0)
  
  data_basal_area <- subset(data_basal_area, sp_id_env_mean %in% c(76, 77, 78, 79, 86, 87, 88, 89))
  
  
  
  
  
  p1 <- ggplot(data_basal_area, aes(x = time, y = mean_sum_basal, 
                                        color = sp_id_env_mean, linetype = as.factor(env_sd))) +
    geom_line(alpha = 0.5) +
    scale_color_manual("Allocation \nStrategy:",
                              values=c(foxes_palettes$main[c(2, 1, 4, 3)],
                                       foxes_palettes$light[c(2, 1, 4, 3)],
                                       foxes_palettes$dark[c(2, 1, 4, 3)])) +
    geom_line(data = data_basal_area_all, aes(x = time, y = mean_sum_basal, color = sp_id_env_mean), size = 0.75) +
    # scale_color_manual("Allocation \nStrategy:",
                       # values=c(foxes_palettes$main[c(2, 1, 4, 3)])) +
    scale_x_continuous("Time (yrs)") +
    scale_y_continuous("Average Basal Area Coverage per Hectar (m2/ha) in Run") +
    species_wrap + 
    guides(fill = FALSE) +
    foxes_theme 
  p1

  
  file_figure <- tempfile(paste("basal_area_all", sep = "_"), tmpdir = figure_dir, fileext = ".png")
  ggsave(file_figure, plot = p1, device = NULL, path = NULL,
         scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
         units =  "mm")
  
  
  
  
  ### Std deviation
  
  
  data_basal_area$sp_id_env_mean = as.factor(data_basal_area$species_id + data_basal_area$env_mean)
  data_basal_area_sd$sp_id_env_mean = as.factor(data_basal_area_sd$species_id)
  
  
  data_basal_area <- subset(data_basal_area, sp_id_env_mean %in% c(76, 77, 78, 79, 86, 87, 88, 89))
  
  
  
  
  
  p1 <- ggplot(data_basal_area, aes(x = time, y = mean_sum_basal, 
                                    color = sp_id_env_mean, linetype = as.factor(env_sd))) +
    geom_line(alpha = 0.5) +
    scale_color_manual("Allocation \nStrategy:",
                       values=c(foxes_palettes$main[c(2, 1, 4, 3)],
                                foxes_palettes$light[c(2, 1, 4, 3)],
                                foxes_palettes$dark[c(2, 1, 4, 3)])) +
    geom_line(data = data_basal_area_sd, aes(x = time, y = mean_sum_basal, color = sp_id_env_mean), size = 0.75) +
    # scale_color_manual("Allocation \nStrategy:",
    # values=c(foxes_palettes$main[c(2, 1, 4, 3)])) +
    scale_x_continuous("Time (yrs)") +
    scale_y_continuous("Average Basal Area Coverage per Hectar (m2/ha) in Run") +
    species_wrap + 
    guides(fill = FALSE) +
    foxes_theme 
  p1
    
  
  file_figure <- tempfile(paste("basal_area_sd", sep = "_"), tmpdir = figure_dir, fileext = ".png")
  ggsave(file_figure, plot = p1, device = NULL, path = NULL,
         scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
         units =  "mm")
  
  ### Std deviation
  
  
  data_basal_area$sp_id_env_mean = as.factor(data_basal_area$species_id + data_basal_area$env_mean)
  data_basal_area_mean <- subset(data_basal_area_mean, env_mean %in% c(75, 85))
  data_basal_area_mean$sp_id_env_mean = as.factor(data_basal_area_mean$species_id + data_basal_area_mean$env_mean)
  data_basal_area_mean$env_sd = 0
  
  
  data_basal_area <- subset(data_basal_area, sp_id_env_mean %in% c(76, 77, 78, 79, 86, 87, 88, 89))
  
  
  
  
  
  p1 <- ggplot(data_basal_area, aes(x = time, y = mean_sum_basal, 
                                    color = sp_id_env_mean, linetype = as.factor(env_sd))) +
    geom_line(alpha = 0.5) +
    scale_color_manual("Allocation \nStrategy:",
                       values=c(foxes_palettes$light[c(2, 1, 4, 3)],
                                foxes_palettes$dark[c(2, 1, 4, 3)])) +
    geom_line(data = data_basal_area_mean, aes(x = time, y = mean_sum_basal, color = sp_id_env_mean), size = 0.75) +
    # scale_color_manual("Allocation \nStrategy:",
    # values=c(foxes_palettes$main[c(2, 1, 4, 3)])) +
    scale_x_continuous("Time (yrs)") +
    scale_y_continuous("Average Basal Area Coverage per Hectar (m2/ha) in Run") +
    species_wrap + 
    guides(fill = FALSE) +
    foxes_theme 
  p1
  
  
  file_figure <- tempfile(paste("basal_area_mean", sep = "_"), tmpdir = figure_dir, fileext = ".png")
  ggsave(file_figure, plot = p1, device = NULL, path = NULL,
         scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
         units =  "mm")
  
  ##### HEIGHT
  
  
  
  data_basal_area$sp_id_env_mean = as.factor(data_basal_area$species_id + data_basal_area$env_mean)
  data_basal_area_all$sp_id_env_mean = as.factor(data_basal_area_all$species_id)
  data_basal_area_all$env_sd = as.factor(0)
  
  data_basal_area <- subset(data_basal_area, sp_id_env_mean %in% c(76, 77, 78, 79, 86, 87, 88, 89))
  
  
  
  
  
  p1 <- ggplot(data_basal_area, aes(x = time, y = mean_max_height, 
                                    color = sp_id_env_mean, linetype = as.factor(env_sd))) +
    geom_line(alpha = 0.5) +
    scale_color_manual("Allocation \nStrategy:",
                       values=c(foxes_palettes$main[c(2, 1, 4, 3)],
                                foxes_palettes$light[c(2, 1, 4, 3)],
                                foxes_palettes$dark[c(2, 1, 4, 3)])) +
    geom_line(data = data_basal_area_all, aes(x = time, y = mean_max_height, color = sp_id_env_mean), size = 0.75) +
    # scale_color_manual("Allocation \nStrategy:",
    # values=c(foxes_palettes$main[c(2, 1, 4, 3)])) +
    scale_x_continuous("Time (yrs)") +
    scale_y_continuous("Average Maximum Height of individual (m)") +
    species_wrap + 
    guides(fill = FALSE) +
    foxes_theme 
  p1
  
  file_figure <- tempfile(paste("max_height_all", sep = "_"), tmpdir = figure_dir, fileext = ".png")
  ggsave(file_figure, plot = p1, device = NULL, path = NULL,
         scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
         units =  "mm")
  
  
  ### Std deviation
  
  
  data_basal_area$sp_id_env_mean = as.factor(data_basal_area$species_id + data_basal_area$env_mean)
  data_basal_area_sd$sp_id_env_mean = as.factor(data_basal_area_sd$species_id)
  
  
  data_basal_area <- subset(data_basal_area, sp_id_env_mean %in% c(76, 77, 78, 79, 86, 87, 88, 89))
  
  
  
  
  
  p1 <- ggplot(data_basal_area, aes(x = time, y = mean_max_height, 
                                    color = sp_id_env_mean, linetype = as.factor(env_sd))) +
    geom_line(alpha = 0.5) +
    scale_color_manual("Allocation \nStrategy:",
                       values=c(foxes_palettes$main[c(2, 1, 4, 3)],
                                foxes_palettes$light[c(2, 1, 4, 3)],
                                foxes_palettes$dark[c(2, 1, 4, 3)])) +
    geom_line(data = data_basal_area_sd, aes(x = time, y = mean_max_height, color = sp_id_env_mean), size = 0.75) +
    # scale_color_manual("Allocation \nStrategy:",
    # values=c(foxes_palettes$main[c(2, 1, 4, 3)])) +
    scale_x_continuous("Time (yrs)") +
    scale_y_continuous("Average Maximum Height of individual (m)") +
    species_wrap + 
    guides(fill = FALSE) +
    foxes_theme 
  p1
  
  
  file_figure <- tempfile(paste("max_height_sd", sep = "_"), tmpdir = figure_dir, fileext = ".png")
  ggsave(file_figure, plot = p1, device = NULL, path = NULL,
         scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
         units =  "mm")
  
  
  ### Std deviation
  
  
  data_basal_area$sp_id_env_mean = as.factor(data_basal_area$species_id + data_basal_area$env_mean)
  data_basal_area_mean <- subset(data_basal_area_mean, env_mean %in% c(75, 85))
  data_basal_area_mean$sp_id_env_mean = as.factor(data_basal_area_mean$species_id + data_basal_area_mean$env_mean)
  data_basal_area_mean$env_sd = 0
  
  
  data_basal_area <- subset(data_basal_area, sp_id_env_mean %in% c(76, 77, 78, 79, 86, 87, 88, 89))
  
  
  
  
  
  p1 <- ggplot(data_basal_area, aes(x = time, y = mean_max_height, 
                                    color = sp_id_env_mean, linetype = as.factor(env_sd))) +
    geom_line(alpha = 0.5) +
    scale_color_manual("Allocation \nStrategy:",
                       values=c(foxes_palettes$light[c(2, 1, 4, 3)],
                                foxes_palettes$dark[c(2, 1, 4, 3)])) +
    geom_line(data = data_basal_area_mean, aes(x = time, y = mean_max_height, color = sp_id_env_mean), size = 0.75) +
    # scale_color_manual("Allocation \nStrategy:",
    # values=c(foxes_palettes$main[c(2, 1, 4, 3)])) +
    scale_x_continuous("Time (yrs)") +
    scale_y_continuous("Average Maximum Height of individual (m)") +
    species_wrap + 
    guides(fill = FALSE) +
    foxes_theme 
  p1
  
  
  file_figure <- tempfile(paste("max_height_mean", sep = "_"), tmpdir = figure_dir, fileext = ".png")
  ggsave(file_figure, plot = p1, device = NULL, path = NULL,
         scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
         units =  "mm")
  
  ##### LAI
  
  
  
  data_basal_area$sp_id_env_mean = as.factor(data_basal_area$species_id + data_basal_area$env_mean)
  data_basal_area_all$sp_id_env_mean = as.factor(data_basal_area_all$species_id)
  data_basal_area_all$env_sd = as.factor(0)
  
  data_basal_area <- subset(data_basal_area, sp_id_env_mean %in% c(76, 77, 78, 79, 86, 87, 88, 89))
  
  
  
  
  
  p1 <- ggplot(data_basal_area, aes(x = time, y = mean_sum_lai, 
                                    color = sp_id_env_mean, linetype = as.factor(env_sd))) +
    geom_line(alpha = 0.5) +
    scale_color_manual("Allocation \nStrategy:",
                       values=c(foxes_palettes$main[c(2, 1, 4, 3)],
                                foxes_palettes$light[c(2, 1, 4, 3)],
                                foxes_palettes$dark[c(2, 1, 4, 3)])) +
    geom_line(data = data_basal_area_all, aes(x = time, y = mean_sum_lai, color = sp_id_env_mean), size = 0.75) +
    # scale_color_manual("Allocation \nStrategy:",
    # values=c(foxes_palettes$main[c(2, 1, 4, 3)])) +
    scale_x_continuous("Time (yrs)") +
    scale_y_continuous("Average LAI (m2/m2)") +
    species_wrap + 
    guides(fill = FALSE) +
    foxes_theme 
  p1
  
  file_figure <- tempfile(paste("lai_all", sep = "_"), tmpdir = figure_dir, fileext = ".png")
  ggsave(file_figure, plot = p1, device = NULL, path = NULL,
         scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
         units =  "mm")
  
  
  ### Std deviation
  
  
  data_basal_area$sp_id_env_mean = as.factor(data_basal_area$species_id + data_basal_area$env_mean)
  data_basal_area_sd$sp_id_env_mean = as.factor(data_basal_area_sd$species_id)
  
  
  data_basal_area <- subset(data_basal_area, sp_id_env_mean %in% c(76, 77, 78, 79, 86, 87, 88, 89))
  
  
  
  
  
  p1 <- ggplot(data_basal_area, aes(x = time, y = mean_sum_lai, 
                                    color = sp_id_env_mean, linetype = as.factor(env_sd))) +
    geom_line(alpha = 0.5) +
    scale_color_manual("Allocation \nStrategy:",
                       values=c(foxes_palettes$main[c(2, 1, 4, 3)],
                                foxes_palettes$light[c(2, 1, 4, 3)],
                                foxes_palettes$dark[c(2, 1, 4, 3)])) +
    geom_line(data = data_basal_area_sd, aes(x = time, y = mean_sum_lai, color = sp_id_env_mean), size = 0.75) +
    # scale_color_manual("Allocation \nStrategy:",
    # values=c(foxes_palettes$main[c(2, 1, 4, 3)])) +
    scale_x_continuous("Time (yrs)") +
    scale_y_continuous("Average LAI (m2/m2)") +
    species_wrap + 
    guides(fill = FALSE) +
    foxes_theme 
  p1
  
  file_figure <- tempfile(paste("lai_sd", sep = "_"), tmpdir = figure_dir, fileext = ".png")
  ggsave(file_figure, plot = p1, device = NULL, path = NULL,
         scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
         units =  "mm")
  
  
  ### Std deviation
  
  
  data_basal_area$sp_id_env_mean = as.factor(data_basal_area$species_id + data_basal_area$env_mean)
  data_basal_area_mean <- subset(data_basal_area_mean, env_mean %in% c(75, 85))
  data_basal_area_mean$sp_id_env_mean = as.factor(data_basal_area_mean$species_id + data_basal_area_mean$env_mean)
  data_basal_area_mean$env_sd = 0
  
  
  data_basal_area <- subset(data_basal_area, sp_id_env_mean %in% c(76, 77, 78, 79, 86, 87, 88, 89))
  
  
  
  
  
  p1 <- ggplot(data_basal_area, aes(x = time, y = mean_sum_lai, 
                                    color = sp_id_env_mean, linetype = as.factor(env_sd))) +
    geom_line(alpha = 0.5) +
    scale_color_manual("Allocation \nStrategy:",
                       values=c(foxes_palettes$light[c(2, 1, 4, 3)],
                                foxes_palettes$dark[c(2, 1, 4, 3)])) +
    geom_line(data = data_basal_area_mean, aes(x = time, y = mean_sum_lai, color = sp_id_env_mean), size = 0.75) +
    # scale_color_manual("Allocation \nStrategy:",
    # values=c(foxes_palettes$main[c(2, 1, 4, 3)])) +
    scale_x_continuous("Time (yrs)") +
    scale_y_continuous("Average LAI (m2/m2)") +
    species_wrap + 
    guides(fill = FALSE) +
    foxes_theme 
  p1
  
  
  file_figure <- tempfile(paste("lai_mean", sep = "_"), tmpdir = figure_dir, fileext = ".png")
  ggsave(file_figure, plot = p1, device = NULL, path = NULL,
         scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
         units =  "mm")
  
  
  
  ##### Mass Storage
  
  
  
  data_basal_area$sp_id_env_mean = as.factor(data_basal_area$species_id + data_basal_area$env_mean)
  data_basal_area_all$sp_id_env_mean = as.factor(data_basal_area_all$species_id)
  data_basal_area_all$env_sd = as.factor(0)
  
  data_basal_area <- subset(data_basal_area, sp_id_env_mean %in% c(76, 77, 78, 79, 86, 87, 88, 89))
  
  
  
  
  
  p1 <- ggplot(data_basal_area, aes(x = time, y = mean_sum_mass_storage_m2, 
                                    color = sp_id_env_mean, linetype = as.factor(env_sd))) +
    geom_line(alpha = 0.5) +
    scale_color_manual("Allocation \nStrategy:",
                       values=c(foxes_palettes$main[c(2, 1, 4, 3)],
                                foxes_palettes$light[c(2, 1, 4, 3)],
                                foxes_palettes$dark[c(2, 1, 4, 3)])) +
    geom_line(data = data_basal_area_all, aes(x = time, y = mean_sum_mass_storage_m2, color = sp_id_env_mean), size = 0.75) +
    # scale_color_manual("Allocation \nStrategy:",
    # values=c(foxes_palettes$main[c(2, 1, 4, 3)])) +
    scale_x_continuous("Time (yrs)") +
    scale_y_continuous("Average Storage Mass per m2 (kg/m2)") +
    species_wrap + 
    guides(fill = FALSE) +
    foxes_theme 
  p1

  file_figure <- tempfile(paste("mass_storage_all", sep = "_"), tmpdir = figure_dir, fileext = ".png")
  ggsave(file_figure, plot = p1, device = NULL, path = NULL,
         scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
         units =  "mm")
  
  
  ### Std deviation
  
  
  data_basal_area$sp_id_env_mean = as.factor(data_basal_area$species_id + data_basal_area$env_mean)
  data_basal_area_sd$sp_id_env_mean = as.factor(data_basal_area_sd$species_id)
  
  
  data_basal_area <- subset(data_basal_area, sp_id_env_mean %in% c(76, 77, 78, 79, 86, 87, 88, 89))
  
  
  
  
  
  p1 <- ggplot(data_basal_area, aes(x = time, y = mean_sum_mass_storage_m2, 
                                    color = sp_id_env_mean, linetype = as.factor(env_sd))) +
    geom_line(alpha = 0.5) +
    scale_color_manual("Allocation \nStrategy:",
                       values=c(foxes_palettes$main[c(2, 1, 4, 3)],
                                foxes_palettes$light[c(2, 1, 4, 3)],
                                foxes_palettes$dark[c(2, 1, 4, 3)])) +
    geom_line(data = data_basal_area_sd, aes(x = time, y = mean_sum_mass_storage_m2, color = sp_id_env_mean), size = 0.75) +
    # scale_color_manual("Allocation \nStrategy:",
    # values=c(foxes_palettes$main[c(2, 1, 4, 3)])) +
    scale_x_continuous("Time (yrs)") +
    scale_y_continuous("Average Storage Mass per m2 (kg/m2)") +
    species_wrap + 
    guides(fill = FALSE) +
    foxes_theme 
  p1
  
  file_figure <- tempfile(paste("mass_storage_sd", sep = "_"), tmpdir = figure_dir, fileext = ".png")
  ggsave(file_figure, plot = p1, device = NULL, path = NULL,
         scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
         units =  "mm")
  
  
  
  ### Std deviation
  
  
  data_basal_area$sp_id_env_mean = as.factor(data_basal_area$species_id + data_basal_area$env_mean)
  data_basal_area_mean <- subset(data_basal_area_mean, env_mean %in% c(75, 85))
  data_basal_area_mean$sp_id_env_mean = as.factor(data_basal_area_mean$species_id + data_basal_area_mean$env_mean)
  data_basal_area_mean$env_sd = 0
  
  
  data_basal_area <- subset(data_basal_area, sp_id_env_mean %in% c(76, 77, 78, 79, 86, 87, 88, 89))
  
  
  
  
  
  p1 <- ggplot(data_basal_area, aes(x = time, y = mean_sum_mass_storage_m2, 
                                    color = sp_id_env_mean, linetype = as.factor(env_sd))) +
    geom_line(alpha = 0.5) +
    scale_color_manual("Allocation \nStrategy:",
                       values=c(foxes_palettes$light[c(2, 1, 4, 3)],
                                foxes_palettes$dark[c(2, 1, 4, 3)])) +
    geom_line(data = data_basal_area_mean, aes(x = time, y = mean_sum_mass_storage_m2, color = sp_id_env_mean), size = 0.75) +
    # scale_color_manual("Allocation \nStrategy:",
    # values=c(foxes_palettes$main[c(2, 1, 4, 3)])) +
    scale_x_continuous("Time (yrs)") +
    scale_y_continuous("Average Storage Mass per m2 (kg/m2)") +
    species_wrap + 
    guides(fill = FALSE) +
    foxes_theme 
  p1
  
  
  file_figure <- tempfile(paste("mass_storage_mean", sep = "_"), tmpdir = figure_dir, fileext = ".png")
  ggsave(file_figure, plot = p1, device = NULL, path = NULL,
         scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
         units =  "mm")
  
  
  