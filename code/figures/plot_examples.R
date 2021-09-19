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

figure_dir <- "out/figures/detailed"
dir.create(figure_dir)

readRDS("rolling_ave_new.rds.RData")



data_rolling_ave$env = as.factor(data_rolling_ave$env_mean + data_rolling_ave$env_sd)

data_rolling_ave_sub = subset(data_rolling_ave, env %in% c(90, 100, 135, 145))
data_rolling_ave_sub$env_species = as.factor(data_rolling_ave_sub$env_mean + data_rolling_ave_sub$env_sd + data_rolling_ave_sub$species_id)

data_rolling_ave_sub$env_species = fct_relevel(data_rolling_ave_sub$env_species, "101", "102", "91", "92",
                                              "103", "104", "93", "94",
                                              "146", "147", "136", "137",
                                              "148", "149", "138", "139")


p1 <- ggplot(data_rolling_ave_sub, aes(x = time, y = mean_sum_basal, 
                                      ymin = mean_sum_basal - sd_sum_basal, 
                                      ymax = mean_sum_basal + sd_sum_basal,
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


p1 <- ggplot(data_rolling_ave_sub, aes(x = time, y = mean_max_height, 
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



p1 <- ggplot(data_rolling_ave_sub, aes(x = time, y = mean_sum_lai, 
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


p1 <- ggplot(data_rolling_ave_sub, aes(x = time, y = mean_sum_mass_storage_m2, 
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



p1 <- ggplot(data_rolling_ave_sub, aes(x = time, y = mean_GPP_abs, 
                                       ymin = mean_GPP_abs - se_GPP_abs, 
                                       ymax = mean_GPP_abs + se_GPP_abs,
                                       color = as.factor(species_id), fill = as.factor(species_id))) +
  geom_line() +
  gr_ribbon + scm_ribbon + sfm +
  scale_x_continuous("Time (yrs)") +
  scale_y_continuous("Average number of dead plants per 2 years") +
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


