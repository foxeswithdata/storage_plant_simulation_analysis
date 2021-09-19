rm(list = ls())

library(tidyverse)

source("code/foxes_pallettes.R")

single_plant_data_075 <- readRDS("data/single_plant/single_plants_075.rds")
single_plant_data_085 <- readRDS("data/single_plant/single_plants_085.rds")
single_plant_data_1 <- readRDS("data/single_plant/single_plants_1.rds")
single_plant_df_1 <- plyr::rbind.fill(single_plant_data_1)
single_plant_df_1$env_mean <- 1
single_plant_df_075 <- plyr::rbind.fill(single_plant_data_075)
single_plant_df_075$env_mean <- 0.75
single_plant_df_085 <- plyr::rbind.fill(single_plant_data_085)
single_plant_df_085$env_mean <- 0.85

single_plant_df <- rbind.fill(single_plant_df_, single_plant_df_085, single_plant_df_075)

single_plant_df_short <- subset(single_plant_df_075, time >= 10 & time < 11)



p <- ggplot(single_plant_df, aes(x = time, y = height, color = as.factor(species_id))) + 
  geom_line() + 
  scm + 
  scale_x_continuous("Time (y)") + 
  scale_y_continuous("Plant Height (m)") + 
  facet_wrap("env_mean", ncol = 3, labeller = labeller("env_mean" = c("0.75" = "Medium  Stress",
                                                                      "0.85" = "Low  Stress",
                                                                      "1" = "No  Stress")) ) + 
  foxes_theme
p

dir.create("out/figures/single_plants")
file_figure <- tempfile(paste("single_plants", "all", sep = "_"), tmpdir = "out/figures/single_plants", fileext = ".png")
ggsave(file_figure, plot = p, device = NULL, path = NULL,
       scale = 1, width = 190, height = 100, dpi = 300, limitsize = TRUE,
       units =  "mm")



p <- ggplot(single_plant_df, aes(x = time, y = storage_portion, color = as.factor(species_id))) + 
  geom_line() + 
  scm + 
  scale_x_continuous("Time (y)") + 
  scale_y_continuous("Storage Concentration (kgC/kgC)") + 
  facet_wrap("env_mean", ncol = 3, labeller = labeller("env_mean" = c("0.75" = "Medium  Stress",
                                                                      "0.85" = "Low  Stress",
                                                                      "1" = "No  Stress")) ) +
  foxes_theme
p
file_figure <- tempfile(paste("single_plants", "storage_concentration", sep = "_"), tmpdir = "out/figures/single_plants", fileext = ".png")
ggsave(file_figure, plot = p, device = NULL, path = NULL,
       scale = 1, width = 190, height = 100, dpi = 300, limitsize = TRUE,
       units =  "mm")




p <- ggplot(single_plant_df_short, aes(x = time, y = height, color = as.factor(species_id))) + 
  geom_line(size = 2) + 
  scm + 
  scale_x_continuous("Time (y)", breaks = c(10, 10.5, 11), labels = c("10", "10.5", "11")) + 
  scale_y_continuous("Plant Height (m)") + 
  facet_wrap("species_id", ncol = 4, labeller = labeller("species_id" = c("1" = "Slow-Risky",
                                                                      "2" = "Slow-Safe",
                                                                      "3" = "Fast-Risky", 
                                                                      "4" = "Fast-Safe")) ) +
  foxes_theme
p

dir.create("out/figures/single_plants")
file_figure <- tempfile(paste("single_plants", "short", sep = "_"), tmpdir = "out/figures/single_plants", fileext = ".png")
ggsave(file_figure, plot = p, device = NULL, path = NULL,
       scale = 1, width = 190, height = 50, dpi = 300, limitsize = TRUE,
       units =  "mm")






p <- ggplot(single_plant_df_short, aes(x = time, y = storage_portion, color = as.factor(species_id))) + 
  geom_line() + 
  scm + 
  scale_x_continuous("Time (y)") + 
  scale_y_continuous("Storage Concentration (kgC/kgC)") + 
  # facet_wrap("env_mean", ncol = 3, labeller = labeller("env_mean" = c("0.75" = "Medium  Stress",
  #                                                                     "0.85" = "Low  Stress",
  #                                                                     "1" = "No  Stress")) ) + 
  foxes_theme
p

file_figure <- tempfile(paste("single_plants", "storage_concentration", "short", sep = "_"), tmpdir = "out/figures/single_plants", fileext = ".png")
ggsave(file_figure, plot = p, device = NULL, path = NULL,
       scale = 1, width = 190, height = 100, dpi = 300, limitsize = TRUE,
       units =  "mm")



p <- ggplot(single_plant_df_short, aes(x = time, y = mass_storage, color = as.factor(species_id))) + 
  geom_line() + 
  scm + 
  scale_x_continuous("Time (y)") + 
  scale_y_continuous("Storage Mass (kgC)") + 
  # facet_wrap("env_mean", ncol = 3, labeller = labeller("env_mean" = c("0.75" = "Medium  Stress",
  #                                                                     "0.85" = "Low  Stress",
  #                                                                     "1" = "No  Stress")) ) + 
  foxes_theme
p

dir.create("out/figures/single_plants")
file_figure <- tempfile(paste("single_plants", "short", sep = "_"), tmpdir = "out/figures/single_plants", fileext = ".png")
ggsave(file_figure, plot = p, device = NULL, path = NULL,
       scale = 1, width = 190, height = 100, dpi = 300, limitsize = TRUE,
       units =  "mm")


