library(tidyverse)
library(plyr)
library(stringr)

source("code/foxes_pallettes.R")
source("code/function_processing.R")
source("code/plotting.R")

## PREPARE DATA

figure_out_dir <- file.path("out", "figures", "test")
if(!dir.exists(figure_out_dir)){
  dir.create(figure_out_dir, recursive = TRUE)  
}


file_names_075_60 <- list.files(path = "data/full_run/env_075_60d/envrep_3/runrep_1/", pattern = "tree_data", recursive = FALSE, include.dirs = TRUE)
file_names_075_60_full <- sapply(file_names_075_60, function(x){
  return(file.path("data", "full_run", "env_075_60d", "envrep_3", "runrep_1", x))
})
file_names_085_60<- list.files(path = "data/full_run/env_085_60d/envrep_1/runrep_3/", pattern = "tree_data", recursive = FALSE, include.dirs = TRUE)
file_names_085_60_full <- sapply(file_names_085_60, function(x){
  return(file.path("data", "full_run", "env_085_60d", "envrep_1", "runrep_3", x))
})
file_names_075_30<- list.files(path = "data/full_run/env_075_30d/envrep_1/runrep_2/", pattern = "tree_data", recursive = FALSE, include.dirs = TRUE)
file_names_075_30_full <- sapply(file_names_075_30, function(x){
  return(file.path("data", "full_run", "env_075_30d", "envrep_1", "runrep_2", x))
})
file_names_085_30<- list.files(path = "data/full_run/env_085_30d/envrep_1/runrep_5/", pattern = "tree_data", recursive = FALSE, include.dirs = TRUE)
file_names_085_30_full <- sapply(file_names_085_30, function(x){
  return(file.path("data", "full_run", "env_085_30d", "envrep_1", "runrep_5", x))
})

file_names_075_15<- list.files(path = "data/full_run/env_075_15d/envrep_3/runrep_4/", pattern = "tree_data", recursive = FALSE, include.dirs = TRUE)
file_names_075_15_full <- sapply(file_names_075_15, function(x){
  return(file.path("data", "full_run", "env_075_15d", "envrep_3", "runrep_4", x))
})

environment_analysis <- list(#list(file_names = file_names_075_60_full,
     #      stress_average = 0.75,
     #      stress_sd = 60/365,
     #      envrep = 3,
     #      runrep = 1),
     # list(file_names = file_names_085_60_full,
     #      stress_average = 0.85,
     #      stress_sd = 60/365,
     #      envrep = 1,
     #      runrep = 3),
     # list(file_names = file_names_075_30_full,
     #      stress_average = 0.75,
     #      stress_sd = 30/365,
     #      envrep = 1,
     #      runrep = 2),
     # list(file_names = file_names_085_30_full,
     #      stress_average = 0.85,
     #      stress_sd = 30/365,
     #      envrep = 1,
     #      runrep = 5),
     list(file_names = file_names_075_15_full,
          stress_average = 0.75,
          stress_sd = 15/365,
          envrep = 3,
          runrep = 4))



for(env in environment_analysis){
  
  figure_out_dir <- file.path("out", 
                              "figures", 
                              paste("env", env$stress_average, env$stress$sd, sep="_"),
                              paste("envrep", env$envrep, sep = "_"),
                              paste("runrep", env$runrep, sep = "_"))
  if(!dir.exists(figure_out_dir)){
    dir.create(figure_out_dir, recursive = TRUE)  
  }
  
  file_names_full <- env$file_names
  
  file_names_full <- as.vector(file_names_full)
  species_names <- c("Slow-Risky", "Slow-Safe", "Fast-Risky", "Fast-Safe")
  
  
  #### Create the figure for lives and deaths
  
  tree_live_dead <- process_data(file_names_full, subset_live_dead, FALSE)
  
  tree_deaths <- lapply(unique(tree_live_dead$species_id), function(sp_id){
    tree_live_dead_sub <- subset(tree_live_dead, species_id == sp_id)
    return(plyr::rbind.fill(lapply(unique(tree_live_dead_sub$tree_id), function(tr_id){
      tree_live_dead_sub_sub <- subset(tree_live_dead_sub, tree_id == tr_id)
      tree_live_dead_sub_sub <- arrange(tree_live_dead_sub_sub, time)
      diff_alive <- c(0,diff(tree_live_dead_sub_sub$is_alive))
      born <- c(1, rep(0, times =( nrow(tree_live_dead_sub_sub)-1)))
      return(data.frame(time = tree_live_dead_sub_sub$time,
                        species_id = sp_id,
                        tree_id = tr_id,
                        died = -diff_alive,
                        born = born))
    })))
  })
  
  tree_deaths <- plyr::rbind.fill(tree_deaths)
  
  tree_deaths_1 <- tree_deaths %>%
    group_by(time, species_id) %>%
    dplyr::summarise(deaths = sum(died),
                     births = sum(born))
  
  tree_deaths_1$species_id <- factor(tree_deaths_1$species_id, levels = c(1,2,3,4), labels = species_names)
  
  
  p <- ggplot(tree_deaths_1, aes(x = time, y = deaths, colour = as.factor(species_id))) + 
    geom_point(alpha = 0.25) +
    scale_color_manual("Allocation \nStrategy:",
                       values=foxes_palettes$main[c(2, 1, 4, 3)]) +
    scale_x_continuous("Time (yrs)") +
    scale_y_continuous("Number of Deaths") +
    facet_wrap(.~species_id, nrow = 2, ncol = 2) +
    theme_bw() + 
    theme(panel.grid.major = element_blank(),
          legend.position = "right",
          plot.margin = margin(1,1,1,1, "cm"),
          text=element_text(size=12, color = "#083855", family="Lato Light"),
          axis.text=element_text(size=8, color = "#083855", family="Lato Light"),
          strip.background = element_rect(fill = "white"))
  p 
  
  file_figure <- tempfile("number_of_deaths", tmpdir = figure_out_dir, fileext = ".png")
  ggsave(file_figure, plot = p, device = NULL, path = NULL,
         scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
         units =  "mm")
  
  
  rm(tree_deaths_1, tree_deaths, tree_live_dead)
  
  ### Number of trees
  
  tree_nums <- process_data(file_names_full, get_num_tree_height, TRUE)
  tree_nums$species_id <- factor(tree_nums$species_id, levels = c(1,2,3,4), labels = species_names)
  
  p <- ggplot(tree_nums, aes(x = time, y = num_tree, colour = as.factor(species_id))) + 
    geom_line(alpha = 0.75) +
    scale_color_manual("Allocation \nStrategy:",
                       values=foxes_palettes$main[c(2, 1, 4, 3)]) +
    scale_x_continuous("Time (yrs)") +
    scale_y_continuous("Number of Live Individuals") +
    # facet_wrap(.~species_id, nrow = 2, ncol = 2) +
    theme_bw() + 
    theme(panel.grid.major = element_blank(),
          legend.position = "right",
          plot.margin = margin(1,1,1,1, "cm"),
          text=element_text(size=12, color = "#083855", family="Lato Light"),
          axis.text=element_text(size=8, color = "#083855", family="Lato Light"))
  p 
  
  file_figure <- tempfile("number_of_trees_single_plot", tmpdir = figure_out_dir, fileext = ".png")
  ggsave(file_figure, plot = p, device = NULL, path = NULL,
         scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
         units =  "mm")
  
  
  p <- ggplot(tree_nums, aes(x = time, y = num_tree, colour = as.factor(species_id))) + 
    geom_line() +
    scale_color_manual("Allocation \nStrategy:",
                       values=foxes_palettes$main[c(2, 1, 4, 3)]) +
    scale_x_continuous("Time (yrs)") +
    scale_y_continuous("Number of Live Individuals") +
    facet_wrap(.~species_id, nrow = 2, ncol = 2) +
    theme_bw() + 
    theme(panel.grid.major = element_blank(),
          legend.position = "right",
          plot.margin = margin(1,1,1,1, "cm"),
          text=element_text(size=12, color = "#083855", family="Lato Light"),
          axis.text=element_text(size=8, color = "#083855", family="Lato Light"),
          strip.background = element_rect(fill = "white"))
  p 
  
  file_figure <- tempfile("number_of_trees", tmpdir = figure_out_dir, fileext = ".png")
  ggsave(file_figure, plot = p, device = NULL, path = NULL,
         scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
         units =  "mm")
  
  rm(tree_nums)
  
  get_masses <- function(data_t, value){
    return(get_multiple_dist(data_t, value, "mass_value", "mass_type"))
  }
  tree_masses <- process_data(file_names_full, get_masses, TRUE, value = c("mass_sapwood", "mass_leaf", "mass_bark", "mass_root"))
  
  
  
  ##### HEIGHT
  
  plot_basic_figures(file_names_full, "height", "Height [m]", expression(paste("Height [mh", a^2, "]")),  "height", figure_out_dir)
  
  ###### BASAL AREA
  
  plot_basic_figures(file_names_full, "area_stem", expression(paste("Stem Area [", m^2, "]")), expression(paste("Stem Area [", m^2, "h",  a^-2, "]")), "basal_area", figure_out_dir, coeff=100)
  
  ###### Leaf AREA
  
  plot_basic_figures(file_names_full, "area_leaf", expression(paste("Leaf Area [", m^2, "]")), expression(paste("Leaf Area Index [", m^2,  m^-2, "]")), "area_leaf", figure_out_dir, coeff=0.01)
  
  ###### MASS STORE
  
  plot_basic_figures(file_names_full, "mass_storage", "Mass Storage [kgC]", expression(paste("Mass Storage [kgCh",  a^-2, "]")), "mass_storage", figure_out_dir, coeff=100)
  
  ### MASS LEAF | MASS SAPWOOD | MASS ROOT | MASS BARK
  
  plot_basic_figures(file_names_full, "mass_leaf", "Mass Leaf [kgC]", expression(paste("Mass Leaf [kgCh",  a^-2, "]")), "mass_leaf", figure_out_dir, coeff=100)
  plot_basic_figures(file_names_full, "mass_sapwood", "Mass Sapwood [kgC]", expression(paste("Mass Sapwood [kgCh",  a^-2, "]")), "mass_sapwood", figure_out_dir, coeff=100)
  plot_basic_figures(file_names_full, "mass_bark", "Mass Bark [kgC]", expression(paste("Mass Bark [kgCh",  a^-2, "]")), "mass_bark", figure_out_dir, coeff=100)
  plot_basic_figures(file_names_full, "mass_root", "Mass Root [kgC]", expression(paste("Mass Root [kgCh",  a^-2, "]")), "mass_root", figure_out_dir, coeff=100)
}

