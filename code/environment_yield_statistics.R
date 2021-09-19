rm(list = ls())

library(tidyverse)
library(stringr)

source("code/foxes_pallettes.R")
source("code/function_processing.R")
source("code/plotting.R")

data_hist <- readRDS("/Volumes/T7/Chapter 2 - data processing/chapter_2_data_process/out/data_hist.rds")

out_dir <- "out/figures/stats"

dir.create(out_dir)

### Compare Height

## check for normality

data_hist$env = data_hist$env_mean + data_hist$env_sd

data_hist_no_control = subset(data_hist, env_mean != 1)
data_hist_no_control_1 = subset(data_hist_no_control, species_id == 1)
data_hist_no_control_2 = subset(data_hist_no_control, species_id == 2)
data_hist_no_control_3 = subset(data_hist_no_control, species_id == 3)
data_hist_no_control_4 = subset(data_hist_no_control, species_id == 4)

test_vals <- data.frame(species = rep(1:4, each=8),
                        env_sd = rep(rep(c(0, 15, 30, 60), times = 2), times = 4),
                        env_mean = rep(rep(c(75, 85), each = 4), times = 4))

shapiro.tests <- plyr::rbind.fill(lapply(1:nrow(test_vals), function(ind){
  sub_data <- subset(data_hist, env_mean == test_vals$env_mean[ind] & 
                       env_sd == test_vals$env_sd[ind] &
                       species_id == test_vals$species[ind])
  height = sub_data$height
  height_log = log(sub_data$height)
  shapiro_height = shapiro.test(height)
  shapiro_height_log = shapiro.test(height_log)
  
  basal = sub_data$area_stem
  basal_log = log(sub_data$area_stem)
  shapiro_basal = shapiro.test(height)
  shapiro_basal_log = shapiro.test(height_log)
  
  storage = sub_data$mass_storage
  storage_log = log(sub_data$mass_storage)
  shapiro_storage = shapiro.test(storage)
  shapiro_storage_log = shapiro.test(storage_log)
  
  return(data.frame(env_mean = test_vals$env_mean[ind],
                    env_sd = test_vals$env_sd[ind],
                    species_id = test_vals$species[ind],
                    shapiro_test_out = c(shapiro_height$p.value,
                                         shapiro_height_log$p.value,
                                         shapiro_basal$p.value,
                                         shapiro_basal_log$p.value,
                                         shapiro_storage$p.value,
                                         shapiro_storage_log$p.value),
                    value = c("height", "height-log", "basal", "basal-log", "storage", "storage-log")))
  
}))

p <- ggplot(data_hist_no_control, aes(x=as.factor(env), y=height, fill=as.factor(species_id))) + 
  geom_boxplot() +
  scale_fill_manual("Species Id:",
                     values=c(foxes_palettes$main[c(2, 1, 4, 3)]),
                     labels=c("Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe")) +
  species_wrap + 
  scale_y_continuous("Height (m)") +
  foxes_theme
p


p <- ggplot(data_hist_no_control, aes(x=as.factor(env), y=log(height), fill=as.factor(species_id))) + 
  geom_boxplot() +
  scale_fill_manual("Species Id:",
                    values=c(foxes_palettes$main[c(2, 1, 4, 3)]),
                    labels=c("Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe")) +
  species_wrap +
  scale_y_continuous("Log Height") +
  foxes_theme
p



color_1 = foxes_palettes$main[2]
color_2 = foxes_palettes$main[1]
color_3 = foxes_palettes$main[4]
color_4 = foxes_palettes$main[3]

p <- ggplot(data_hist_no_control_1, aes (x = height)) + 
  geom_histogram(mapping = aes(group = as.factor(species_id)), fill = color_1) + 
  facet_grid(env_sd~ env_mean, scales = "free", labeller = labeller(env_mean = c("75" = "High Stress",
                                                                "85" = "Medium Stress"),
                                                   env_sd = c("0" = "No Stochasticity",
                                                              "15" = "Low Stochasticity",
                                                              "30" = "Medium Stochasticity",
                                                              "60" = "High Stochasticity"))) +
  scale_x_continuous("Individual Height (m)", trans = "log10") +
  scale_y_continuous("Number of Individuals") +
  foxes_theme
p

file_figure <- tempfile(paste("Hist", "height", "species", "1", sep = "_"), tmpdir = out_dir, fileext = ".png")
ggsave(file_figure, plot = p, device = NULL, path = NULL,
       scale = 1, width = 120, height = 138, dpi = 300, limitsize = TRUE,
       units =  "mm")


p <- ggplot(data_hist_no_control_2, aes (x = height)) + 
  geom_histogram(mapping = aes(group = as.factor(species_id)), fill = color_2) + 
  facet_grid(env_sd~ env_mean, scales = "free", labeller = labeller(env_mean = c("75" = "High Stress",
                                                                                 "85" = "Medium Stress"),
                                                                    env_sd = c("0" = "No Stochasticity",
                                                                               "15" = "Low Stochasticity",
                                                                               "30" = "Medium Stochasticity",
                                                                               "60" = "High Stochasticity"))) +
  scale_x_continuous("Individual Height (m)", trans = "log10") +
  scale_y_continuous("Number of Individuals") +
  foxes_theme
p

file_figure <- tempfile(paste("Hist", "height", "species", "2", sep = "_"), tmpdir = out_dir, fileext = ".png")
ggsave(file_figure, plot = p, device = NULL, path = NULL,
       scale = 1, width = 120, height = 138, dpi = 300, limitsize = TRUE,
       units =  "mm")
  
p <- ggplot(data_hist_no_control_3, aes (x = height)) + 
  geom_histogram(mapping = aes(group = as.factor(species_id)), fill = color_3) + 
  facet_grid(env_sd~ env_mean, scales = "free", labeller = labeller(env_mean = c("75" = "High Stress",
                                                                                 "85" = "Medium Stress"),
                                                                    env_sd = c("0" = "No Stochasticity",
                                                                               "15" = "Low Stochasticity",
                                                                               "30" = "Medium Stochasticity",
                                                                               "60" = "High Stochasticity"))) +
  scale_x_continuous("Individual Height (m)", trans = "log10") +
  scale_y_continuous("Number of Individuals") +
  foxes_theme
p

file_figure <- tempfile(paste("Hist", "height", "species", "3", sep = "_"), tmpdir = out_dir, fileext = ".png")
ggsave(file_figure, plot = p, device = NULL, path = NULL,
       scale = 1, width = 120, height = 138, dpi = 300, limitsize = TRUE,
       units =  "mm")

p <- ggplot(data_hist_no_control_4, aes (x = height)) + 
  geom_histogram(mapping = aes(group = as.factor(species_id)), fill = color_4) + 
  facet_grid(env_sd~ env_mean, scales = "free", labeller = labeller(env_mean = c("75" = "High Stress",
                                                                                 "85" = "Medium Stress"),
                                                                    env_sd = c("0" = "No Stochasticity",
                                                                               "15" = "Low Stochasticity",
                                                                               "30" = "Medium Stochasticity",
                                                                               "60" = "High Stochasticity"))) +
  scale_x_continuous("Individual Height (m)", trans = "log10") +
  scale_y_continuous("Number of Individuals") +
  foxes_theme
p

file_figure <- tempfile(paste("Hist", "height", "species", "4", sep = "_"), tmpdir = out_dir, fileext = ".png")
ggsave(file_figure, plot = p, device = NULL, path = NULL,
       scale = 1, width = 120, height = 138, dpi = 300, limitsize = TRUE,
       units =  "mm")









p <- ggplot(data_hist_no_control_1, aes (x = mass_storage)) + 
  geom_histogram(mapping = aes(group = as.factor(species_id)), fill = color_1) + 
  facet_grid(env_sd~ env_mean, scales = "free", labeller = labeller(env_mean = c("75" = "High Stress",
                                                                                 "85" = "Medium Stress"),
                                                                    env_sd = c("0" = "No Stochasticity",
                                                                               "15" = "Low Stochasticity",
                                                                               "30" = "Medium Stochasticity",
                                                                               "60" = "High Stochasticity"))) +
  scale_x_continuous("Storage Mass (kgC)", trans = "log10") +
  scale_y_continuous("Number of Individuals") +
  foxes_theme
p

file_figure <- tempfile(paste("Hist", "storage-mass", "species", "1_", sep = "_"), tmpdir = out_dir, fileext = ".png")
ggsave(file_figure, plot = p, device = NULL, path = NULL,
       scale = 1, width = 120, height = 138, dpi = 300, limitsize = TRUE,
       units =  "mm")


p <- ggplot(data_hist_no_control_2, aes (x = mass_storage)) + 
  geom_histogram(mapping = aes(group = as.factor(species_id)), fill = color_2) + 
  facet_grid(env_sd~ env_mean, scales = "free", labeller = labeller(env_mean = c("75" = "High Stress",
                                                                                 "85" = "Medium Stress"),
                                                                    env_sd = c("0" = "No Stochasticity",
                                                                               "15" = "Low Stochasticity",
                                                                               "30" = "Medium Stochasticity",
                                                                               "60" = "High Stochasticity"))) +
  scale_x_continuous("Storage Mass (kgC)", trans = "log10") +
  scale_y_continuous("Number of Individuals") +
  foxes_theme
p

file_figure <- tempfile(paste("Hist", "storage-mass", "species", "2_", sep = "_"), tmpdir = out_dir, fileext = ".png")
ggsave(file_figure, plot = p, device = NULL, path = NULL,
       scale = 1, width = 120, height = 138, dpi = 300, limitsize = TRUE,
       units =  "mm")

p <- ggplot(data_hist_no_control_3, aes (x = mass_storage)) + 
  geom_histogram(mapping = aes(group = as.factor(species_id)), fill = color_3) + 
  facet_grid(env_sd~ env_mean, scales = "free", labeller = labeller(env_mean = c("75" = "High Stress",
                                                                                 "85" = "Medium Stress"),
                                                                    env_sd = c("0" = "No Stochasticity",
                                                                               "15" = "Low Stochasticity",
                                                                               "30" = "Medium Stochasticity",
                                                                               "60" = "High Stochasticity"))) +
  scale_x_continuous("Storage Mass (kgC)", trans = "log10") +
  scale_y_continuous("Number of Individuals") +
  foxes_theme
p

file_figure <- tempfile(paste("Hist", "storage-mass", "species", "3_", sep = "_"), tmpdir = out_dir, fileext = ".png")
ggsave(file_figure, plot = p, device = NULL, path = NULL,
       scale = 1, width = 120, height = 138, dpi = 300, limitsize = TRUE,
       units =  "mm")

p <- ggplot(data_hist_no_control_4, aes (x = mass_storage)) + 
  geom_histogram(mapping = aes(group = as.factor(species_id)), fill = color_4) + 
  facet_grid(env_sd~ env_mean, scales = "free", labeller = labeller(env_mean = c("75" = "High Stress",
                                                                                 "85" = "Medium Stress"),
                                                                    env_sd = c("0" = "No Stochasticity",
                                                                               "15" = "Low Stochasticity",
                                                                               "30" = "Medium Stochasticity",
                                                                               "60" = "High Stochasticity"))) +
  scale_x_continuous("Storage Mass (kgC)", trans = "log10") +
  scale_y_continuous("Number of Individuals") +
  foxes_theme
p

file_figure <- tempfile(paste("Hist", "storage-mass", "species", "4_", sep = "_"), tmpdir = out_dir, fileext = ".png")
ggsave(file_figure, plot = p, device = NULL, path = NULL,
       scale = 1, width = 120, height = 138, dpi = 300, limitsize = TRUE,
       units =  "mm")




#### Linear regression


p <- ggplot(data_hist, aes(color = as.factor(species_id), fill = as.factor(species_id))) + 
  geom_ribbon(data = subset(data_hist, env_mean == 75), aes(x=env_sd, 
                                                                 y = mean_basal_area, 
                                                                 ymin = mean_basal_area - se_basal_area, ymax = mean_basal_area + se_basal_area), alpha = 0.25, linetype = "blank") +
  geom_ribbon(data = subset(data_hist, env_mean == 85), aes(x=env_sd, 
                                                                 y = mean_basal_area, 
                                                                 ymin = mean_basal_area - se_basal_area, ymax = mean_basal_area + se_basal_area), alpha = 0.25, linetype = "blank") +
  geom_point(aes(x=env_sd, y = mean_basal_area)) + 
  geom_line(aes(x=env_sd, y = mean_basal_area, linetype=as.factor(env_mean)), alpha = 0.75) + 
  # geom_line(data = model_data, aes(x=env_sd, y = basal_area, linetype=as.factor(env_mean)), size = 0.75) + 
  scale_fill_manual("Species Id:",
                    values=c(foxes_palettes$main[c(2, 1, 4, 3)]),
                    labels=c("Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe")) +
  scale_color_manual("Species Id:",
                     values=c(foxes_palettes$main[c(2, 1, 4, 3)]),
                     labels=c("Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe")) +
  scale_linetype_discrete("Environment Mean (yr):",
                          labels = c("0.75", "0.85")) +
  ylab("Final Basal Area Per Hectare (m2/ha)") +
  xlab("Standard Deviation of Stress (d)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        legend.position = "right",
        plot.margin = margin(1,1,1,1, "cm"),
        text=element_text(size=12, color = "#083855", family="Lato Light"),
        axis.text=element_text(size=8, color = "#083855", family="Lato Light"),
        strip.background = element_rect(fill = "white"))
p







linearMod_1_75 <- lm(height ~ env_sd, data=data_hist, species_id == 1 & env_mean == 75)
linearMod_1_75_log <- lm(log(height) ~ env_sd, data=data_hist, species_id == 1 & env_mean == 75)
linearMod_2_75 <- lm(log(height) ~ env_sd, data=data_hist, species_id == 2 & env_mean == 75)
linearMod_3_75 <- lm(log(height) ~ env_sd, data=data_hist, species_id == 3 & env_mean == 75)
linearMod_4_75 <- lm(log(height) ~ env_sd, data=data_hist, species_id == 4 & env_mean == 75)
linearMod_1_85 <- lm(log(height) ~ env_sd, data=data_hist, species_id == 1 & env_mean == 85)
linearMod_2_85 <- lm(log(height) ~ env_sd, data=data_hist, species_id == 2 & env_mean == 85)
linearMod_3_85 <- lm(log(height) ~ env_sd, data=data_hist, species_id == 3 & env_mean == 85)
linearMod_4_85 <- lm(log(height) ~ env_sd, data=data_hist, species_id == 4 & env_mean == 85)





