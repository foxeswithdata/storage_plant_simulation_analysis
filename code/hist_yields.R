rm(list = ls())

library(tidyverse)
library(stringr)
library(patchwork)
library(ggh4x)
library(cowplot)
library(effectsize)

source("code/foxes_pallettes.R")
source("code/function_processing.R")
source("code/plotting.R")

data_hist <- readRDS("out/data_hist_new.rds")

figure_dir <- "out/figures/hist"

dir.create(figure_dir)

data_hist_basal <- data_hist %>% 
  dplyr::group_by(species_id, env_sd, env_mean, env_rep, run_rep, time) %>%
  dplyr::summarise(basal_area = sum(area_stem, na.rm = TRUE) * 100) %>%
  dplyr::group_by(species_id, env_sd, env_mean, env_rep, run_rep) %>%
  dplyr::summarise(basal_area = mean(basal_area, na.rm = TRUE)) %>%
  dplyr::group_by(env_sd, env_mean, species_id) %>%
  dplyr::summarise(basal_area_mean = mean(basal_area, na.rm = TRUE),
                   basal_area_sd = sd(basal_area, na.rm = TRUE),
                   count = n())

data_hist_basal_c1 <- data_hist %>% 
  dplyr::group_by(species_id, env_sd, env_mean, env_rep, run_rep, time) %>%
  dplyr::summarise(basal_area = sum(area_stem, na.rm = TRUE) * 100) %>%
  dplyr::group_by(species_id, env_sd, env_mean, env_rep, run_rep) %>%
  dplyr::count()

data_hist_basal_c2 <- data_hist %>% 
  dplyr::group_by(species_id, env_sd, env_mean, env_rep, run_rep, time) %>%
  dplyr::summarise(basal_area = sum(area_stem, na.rm = TRUE) * 100) %>%
  dplyr::group_by(species_id, env_sd, env_mean, env_rep, run_rep) %>%
  dplyr::summarise(basal_area = mean(basal_area, na.rm = TRUE)) %>%
  dplyr::group_by(env_sd, env_mean, species_id) %>%
  dplyr::count()

data_hist_basal_env_mean <- data_hist %>% 
  dplyr::filter(env_mean != 1) %>%
  dplyr::group_by(species_id, env_sd, env_mean, env_rep, run_rep, time) %>%
  dplyr::summarise(basal_area = sum(area_stem, na.rm = TRUE) * 100) %>%
  dplyr::group_by(species_id, env_mean, env_rep, run_rep) %>%
  dplyr::summarise(basal_area = mean(basal_area, na.rm = TRUE)) %>%
  dplyr::group_by(env_mean, species_id) %>%
  dplyr::summarise(basal_area_mean = mean(basal_area, na.rm = TRUE),
                   basal_area_sd = sd(basal_area, na.rm = TRUE))

data_hist_basal_env_sd <- data_hist %>% 
  dplyr::filter(env_mean != 1) %>%
  dplyr::group_by(species_id, env_sd, env_mean, env_rep, run_rep, time) %>%
  dplyr::summarise(basal_area = sum(area_stem, na.rm = TRUE) * 100) %>%
  dplyr::group_by(species_id, env_sd, env_rep, run_rep) %>%
  dplyr::summarise(basal_area = mean(basal_area, na.rm = TRUE)) %>%
  dplyr::group_by(env_sd, species_id) %>%
  dplyr::summarise(basal_area_mean = mean(basal_area, na.rm = TRUE),
                   basal_area_sd = sd(basal_area, na.rm = TRUE))

data_hist_basal_all <- data_hist %>% 
  dplyr::group_by(env_sd, env_mean, env_rep, run_rep, time) %>%
  dplyr::summarise(basal_area = sum(area_stem, na.rm = TRUE) * 100) %>%
  dplyr::group_by(env_sd, env_mean, env_rep, run_rep) %>%
  dplyr::summarise(basal_area = mean(basal_area, na.rm = TRUE)) %>%
  dplyr::group_by(env_sd, env_mean) %>%
  dplyr::summarise(species_id = 0, 
                   basal_area_mean = mean(basal_area, na.rm = TRUE),
                   basal_area_sd = sd(basal_area, na.rm = TRUE), 
                   count = n())

data_hist_basal_all_c1 <- data_hist %>% 
  dplyr::group_by(env_sd, env_mean, env_rep, run_rep, time) %>%
  dplyr::summarise(basal_area = sum(area_stem, na.rm = TRUE) * 100) %>%
  dplyr::group_by(env_sd, env_mean, env_rep, run_rep) %>%
  dplyr::count() 

data_hist_basal_all_c2 <- data_hist %>% 
  dplyr::group_by(env_sd, env_mean, env_rep, run_rep, time) %>%
  dplyr::summarise(basal_area = sum(area_stem, na.rm = TRUE) * 100) %>%
  dplyr::group_by(env_sd, env_mean, env_rep, run_rep) %>%
  dplyr::summarise(basal_area = mean(basal_area, na.rm = TRUE)) %>%
  dplyr::group_by(env_sd, env_mean) %>%
  dplyr::count()


data_hist_basal_all_env_mean <- data_hist %>% 
  dplyr::filter(env_mean != 1) %>%
  dplyr::group_by(env_sd, env_mean, env_rep, run_rep, time) %>%
  dplyr::summarise(basal_area = sum(area_stem, na.rm = TRUE) * 100) %>%
  dplyr::group_by(env_mean, env_rep, run_rep) %>%
  dplyr::summarise(basal_area = mean(basal_area, na.rm = TRUE)) %>%
  dplyr::group_by(env_mean) %>%
  dplyr::summarise(species_id = 0, 
                   basal_area_mean = mean(basal_area, na.rm = TRUE),
                   basal_area_sd = sd(basal_area, na.rm = TRUE))

data_hist_basal_all_env_sd <- data_hist %>% 
  dplyr::filter(env_mean != 1) %>%
  dplyr::group_by(env_sd, env_mean, env_rep, run_rep, time) %>%
  dplyr::summarise(basal_area = sum(area_stem, na.rm = TRUE) * 100) %>%
  dplyr::group_by(env_sd, env_rep, run_rep) %>%
  dplyr::summarise(basal_area = mean(basal_area, na.rm = TRUE)) %>%
  dplyr::group_by(env_sd) %>%
  dplyr::summarise(species_id = 0, 
                   basal_area_mean = mean(basal_area, na.rm = TRUE),
                   basal_area_sd = sd(basal_area, na.rm = TRUE))

data_hist_basal_all_reps <- data_hist %>% 
  dplyr::group_by(env_sd, env_mean, env_rep, run_rep, time) %>%
  dplyr::summarise(basal_area = sum(area_stem, na.rm = TRUE) * 100) %>%
  dplyr::group_by(env_sd, env_mean, env_rep, run_rep) %>%
  dplyr::summarise(basal_area = mean(basal_area, na.rm = TRUE))

data_hist_basal_reps <- data_hist %>% 
  dplyr::group_by(species_id, env_sd, env_mean, env_rep, run_rep, time) %>%
  dplyr::summarise(basal_area = sum(area_stem, na.rm = TRUE) * 100) %>%
  dplyr::group_by(species_id, env_sd, env_mean, env_rep, run_rep) %>%
  dplyr::summarise(basal_area = mean(basal_area, na.rm = TRUE))


data_hist_basal_all_and_ind = rbind(data_hist_basal, data_hist_basal_all)
data_hist_basal_all_and_ind_env_mean = rbind.data.frame(data_hist_basal_env_mean, data_hist_basal_all_env_mean)
data_hist_basal_all_and_ind_env_sd = rbind.data.frame(data_hist_basal_env_sd, data_hist_basal_all_env_sd)

data_hist_basal_reps_all_and_ind = rbind(data_hist_basal_reps, data_hist_basal_all_reps)



data_hist_basal_all_and_ind$env_mean[data_hist_basal_all_and_ind$env_mean == 75] = 0.75
data_hist_basal_all_and_ind$env_mean[data_hist_basal_all_and_ind$env_mean == 85] = 0.85

data_hist_basal_all_and_ind$env_mean <- factor(data_hist_basal_all_and_ind$env_mean, levels = c(1, 0.85, 0.75))

p <- ggplot(data_hist_basal_all_and_ind, aes(fill=as.factor(species_id), 
                                             y=basal_area_mean, 
                                             x=as.factor(env_mean),
                                             group = as.factor(env_sd))) + 
  geom_bar(aes(alpha=(env_sd)),stat="identity", position = "dodge", color = "#FFFFFF") +
  geom_errorbar(aes(ymin=basal_area_mean-basal_area_sd, ymax=basal_area_mean+basal_area_sd), position = "dodge",
                colour="#083855") + 
  scale_fill_manual("Allocation \nStrategy:",
                    values=foxes_palettes$main[c(5, 2, 1, 4, 3)],
                    labels=c("All Strategies", "Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe")) + 
  facet_grid(rows = "species_id", space = "free", labeller = labeller(species_id = c("0" = "All Strategies",
                                                                                     "1" = "Slow-Risky", 
                                                                                     "2" = "Slow-Safe",
                                                                                     "3" = "Fast-Risky", 
                                                                                     "4" = "Fast-Safe"))) +
  scale_alpha("Stochasticity", breaks = c(0, 15, 30, 60), range = c(1, 0.4), labels = c("None", "Low", "Medium", "High")) + 
  geom_text(label="n = ",  nudge_y = 50) + 
  scale_x_discrete("Environmental Treatment", labels = c("Control",
                                                         "Low Stress",
                                                         "Medium Stress")) +
  scale_y_continuous(expression(paste("Average Basal Area per Hectare (", m^2, "h", a^-1, ")"))) +
  foxes_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
p

file_figure <- tempfile(paste("basal_area", "barplot", "final", sep = "_"), tmpdir = figure_dir, fileext = ".png")
ggsave(file_figure, plot = p, device = NULL, path = NULL,
       scale = 1, width = 190, height = 220, dpi = 300, limitsize = TRUE,
       units =  "mm")

data_hist_basal_all_and_ind_no_control <- subset(data_hist_basal_all_and_ind, env_mean != 1)
data_hist_basal_all_and_ind_env_mean_no_control <- subset(data_hist_basal_all_and_ind_env_mean, env_mean != 1)
data_hist_basal_all_and_ind_no_control$species_id <- factor(data_hist_basal_all_and_ind_no_control$species_id, levels = c(1,2,3,4,0))
data_hist_basal_all_and_ind_env_mean_no_control$species_id <- factor(data_hist_basal_all_and_ind_env_mean_no_control$species_id, levels = c(1,2,3,4,0))
data_hist_basal_all_and_ind_env_sd$species_id <- factor(data_hist_basal_all_and_ind_env_sd$species_id, levels = c(1,2,3,4,0))

data_hist_basal_all_and_ind_no_control_species <- subset(data_hist_basal_all_and_ind, species_id != 0)
data_hist_basal_all_and_ind_no_control_all <- subset(data_hist_basal_all_and_ind, species_id == 0)

p <- ggplot(data_hist_basal_all_and_ind_no_control, aes(x=-env_mean, 
                                                                 y = basal_area_mean, 
                                                                 color = as.factor(species_id), 
                                                                 linetype = as.factor(env_sd))) + 
  geom_line(position = position_dodge(width = 2)) + 
  geom_errorbar(aes(ymin = basal_area_mean - basal_area_sd, ymax = basal_area_mean + basal_area_sd), 
                alpha = 0.5,
                position = position_dodge(width = 2), 
                width = 4) + 
  geom_point(position = position_dodge(width = 2)) + 
  scale_color_manual("Species Id:",
                     values=c(foxes_palettes$main[c(2, 1, 4, 3, 5)]),
                     labels=c("Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe", "All Species")) +
  scale_linetype_discrete("Environment SD (d):",
                          labels = c("0", "15", "30", "60")) +
  ylab("Basal Area per Hectare (m2/ha)") +
  xlab("Mean of Stress (yr)") +
  # geom_jitter() + 
  scale_x_continuous(breaks = c(-85, -75), labels = c("low stress", "med stress")) +
  facet_wrap(.~species_id, nrow = 3, ncol = 2, labeller = labeller(species_id = c("1" = "Slow-Risky", 
                                                                                  "2" = "Slow-Safe",
                                                                                  "3" = "Fast-Risky", 
                                                                                  "4" = "Fast-Safe",
                                                                                  "0" = "All Species"))) + 
  force_panelsizes(rows = c(1,1,1),
                   respect = TRUE) +
  foxes_theme

p

file_figure <- tempfile(paste("basal_area", "bigger","sd_var", "all", sep = "_"), tmpdir = figure_dir, fileext = ".png")
ggsave(file_figure, plot = p, device = NULL, path = NULL,
       scale = 1, width = 520, height = 220, dpi = 300, limitsize = TRUE,
       units =  "mm")


file_figure <- tempfile(paste("basal_area", "bigger","sd_var", "all", sep = "_"), tmpdir = figure_dir, fileext = ".png")
ggsave(file_figure, plot = p, device = NULL, path = NULL,
       scale = 1, width = 520, height = 220, dpi = 300, limitsize = TRUE,
       units =  "mm")

p <- ggplot(data_hist_basal_all_and_ind_no_control, aes(x= env_sd, 
                                                        y = basal_area_mean, 
                                                        color = as.factor(species_id), 
                                                        linetype = as.factor(env_mean))) + 
  geom_line(position = position_dodge(width = 2)) + 
  geom_errorbar(aes(ymin = basal_area_mean - basal_area_sd, ymax = basal_area_mean + basal_area_sd), 
                alpha = 0.5,
                position = position_dodge(width = 2), 
                width = 4) + 
  geom_point(position = position_dodge(width = 2)) + 
  scale_color_manual("Species Id:",
                     values=c(foxes_palettes$main[c(2, 1, 4, 3, 5)]),
                     labels=c("Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe", "All Species")) +
  scale_linetype_discrete("Environment Mean:",
                          labels = c("0.75y", "0.85y")) +
  ylab("Basal Area per Hectare (m2/ha)") +
  xlab("Stochasticity Stress (yr)") +
  # geom_jitter() + 
  scale_x_discrete(breaks = c(0, 15, 30, 60), labels = c("None", "Low", "Medium", "High")) +
  facet_wrap(.~species_id, nrow = 3, ncol = 2, labeller = labeller(species_id = c("1" = "Slow-Risky", 
                                                                                  "2" = "Slow-Safe",
                                                                                  "3" = "Fast-Risky", 
                                                                                  "4" = "Fast-Safe",
                                                                                  "0" = "All Species"))) + 
  force_panelsizes(rows = c(1,1,2),
                   respect = TRUE) +
  foxes_theme
p

file_figure <- tempfile(paste("basal_area", "bigger","mean_var", "all", sep = "_"), tmpdir = figure_dir, fileext = ".png")
ggsave(file_figure, plot = p, device = NULL, path = NULL,
       scale = 1, width = 520, height = 220, dpi = 300, limitsize = TRUE,
       units =  "mm")

file_figure <- tempfile(paste("basal_area","mean_var", "all", sep = "_"), tmpdir = figure_dir, fileext = ".png")
ggsave(file_figure, plot = p, device = NULL, path = NULL,
       scale = 1, width = 260, height = 220, dpi = 300, limitsize = TRUE,
       units =  "mm")

data_hist_ave <- data_hist %>%
  dplyr::group_by(tree_id, species_id, env_mean, env_sd, env_rep, run_rep) %>%
  dplyr::summarise(height = mean(height, na.rm=TRUE),
                   area_stem = mean(area_stem, na.rm=TRUE))

data_hist_no_spec = data_hist_ave
data_hist_no_spec$species_id = 0

data_hist_new <- rbind(data_hist_no_spec, data_hist_ave)

p <- ggplot(data_hist_new, aes(x = as.factor(species_id), y = height)) + 
  geom_violin(aes(color=as.factor(species_id), fill=as.factor(species_id)), alpha = 0.75) +
  scale_color_manual("Species Id:",
                     values=c(foxes_palettes$main[c(5, 2, 1, 4, 3)]),
                     labels=c("All Strategies", "Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe")) +
  scale_fill_manual("Species Id:",
                    values=c(foxes_palettes$main[c(5, 2, 1, 4, 3)]),
                    labels=c("All Strategies", "Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe")) +
  ylab("Height (m)") +
  scale_x_discrete("Strategy", labels = c("All Strategies", "Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe")) +
  facet_grid(env_sd~ env_mean, labeller = labeller(env_mean = c("75" = "Med Stress",
                                                                "85" = "Low Stress",
                                                                "1" = "No Stress"),
                                                   env_sd = c("0" = "No Stoch.",
                                                              "15" = "Low Stoch.",
                                                              "30" = "Med Stoch.",
                                                              "60" = "High Stoch"))) +
  foxes_theme + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
p

file_figure <- tempfile(paste("height","violin", "all", sep = "_"), tmpdir = figure_dir, fileext = ".png")
ggsave(file_figure, plot = p, device = NULL, path = NULL,
       scale = 1, width = 260, height = 220, dpi = 300, limitsize = TRUE,
       units =  "mm")

p <- ggplot(data_hist_new, aes(x = as.factor(species_id), y = area_stem)) + 
  geom_violin(aes(color=as.factor(species_id), fill=as.factor(species_id)), alpha = 0.75) +
  scale_color_manual("Species Id:",
                     values=c(foxes_palettes$main[c(5, 2, 1, 4, 3)]),
                     labels=c("All Strategies", "Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe")) +
  scale_fill_manual("Species Id:",
                    values=c(foxes_palettes$main[c(5, 2, 1, 4, 3)]),
                    labels=c("All Strategies", "Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe")) +
  ylab(expression(paste("Basal Area (", m^2, ")"))) +
  scale_x_discrete("Strategy", labels = c("All Strategies", "Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe")) +
  facet_grid(env_sd~ env_mean, labeller = labeller(env_mean = c("75" = "Med Stress",
                                                                "85" = "Low Stress",
                                                                "1" = "No Stress"),
                                                   env_sd = c("0" = "No Stoch.",
                                                              "15" = "Low Stoch.",
                                                              "30" = "Med Stoch.",
                                                              "60" = "High Stoch"))) +
  foxes_theme + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
p

file_figure <- tempfile(paste("area_stem","violin", "all", sep = "_"), tmpdir = figure_dir, fileext = ".png")
ggsave(file_figure, plot = p, device = NULL, path = NULL,
       scale = 1, width = 260, height = 220, dpi = 300, limitsize = TRUE,
       units =  "mm")

data_hist_basal_reps_all_and_ind$env_mean[data_hist_basal_reps_all_and_ind$env_mean == 75] = 0.75
data_hist_basal_reps_all_and_ind$env_mean[data_hist_basal_reps_all_and_ind$env_mean == 85] = 0.85
data_hist_basal_reps_all_and_ind_no_control <- subset(data_hist_basal_reps_all_and_ind, env_mean != 1)
data_hist_basal_reps_all_and_ind_no_control$env_mean <- as.factor(data_hist_basal_reps_all_and_ind_no_control$env_mean)

data_hist_basal_all_and_ind_0 = subset(data_hist_basal_reps_all_and_ind_no_control, species_id == 0)
data_hist_basal_all_and_ind_0_mean_075 = subset(data_hist_basal_reps_all_and_ind_no_control, species_id == 0 & env_mean == 0.75)
data_hist_basal_all_and_ind_0_mean_085 = subset(data_hist_basal_reps_all_and_ind_no_control, species_id == 0 & env_mean == 0.85)
data_hist_basal_all_and_ind_0_sd_0 = subset(data_hist_basal_reps_all_and_ind_no_control, species_id == 0 & env_sd == 0)
data_hist_basal_all_and_ind_0_sd_15 = subset(data_hist_basal_reps_all_and_ind_no_control, species_id == 0 & env_sd == 15)
data_hist_basal_all_and_ind_0_sd_30 = subset(data_hist_basal_reps_all_and_ind_no_control, species_id == 0 & env_sd == 30)
data_hist_basal_all_and_ind_0_sd_60 = subset(data_hist_basal_reps_all_and_ind_no_control, species_id == 0 & env_sd == 60)

data_hist_basal_all_and_ind_1 = subset(data_hist_basal_reps_all_and_ind_no_control, species_id == 1)
data_hist_basal_all_and_ind_1_mean_075 = subset(data_hist_basal_reps_all_and_ind_no_control, species_id == 1 & env_mean == 0.75)
data_hist_basal_all_and_ind_1_mean_085 = subset(data_hist_basal_reps_all_and_ind_no_control, species_id == 1 & env_mean == 0.85)
data_hist_basal_all_and_ind_1_sd_0 = subset(data_hist_basal_reps_all_and_ind_no_control, species_id == 1 & env_sd == 0)
data_hist_basal_all_and_ind_1_sd_15 = subset(data_hist_basal_reps_all_and_ind_no_control, species_id == 1 & env_sd == 15)
data_hist_basal_all_and_ind_1_sd_30 = subset(data_hist_basal_reps_all_and_ind_no_control, species_id == 1 & env_sd == 30)
data_hist_basal_all_and_ind_1_sd_60 = subset(data_hist_basal_reps_all_and_ind_no_control, species_id == 1 & env_sd == 60)

data_hist_basal_all_and_ind_2 = subset(data_hist_basal_reps_all_and_ind_no_control, species_id == 2)
data_hist_basal_all_and_ind_2_mean_075 = subset(data_hist_basal_reps_all_and_ind_no_control, species_id == 2 & env_mean == 0.75)
data_hist_basal_all_and_ind_2_mean_085 = subset(data_hist_basal_reps_all_and_ind_no_control, species_id == 2 & env_mean == 0.85)
data_hist_basal_all_and_ind_2_sd_0 = subset(data_hist_basal_reps_all_and_ind_no_control, species_id == 2 & env_sd == 0)
data_hist_basal_all_and_ind_2_sd_15 = subset(data_hist_basal_reps_all_and_ind_no_control, species_id == 2 & env_sd == 15)
data_hist_basal_all_and_ind_2_sd_30 = subset(data_hist_basal_reps_all_and_ind_no_control, species_id == 2 & env_sd == 30)
data_hist_basal_all_and_ind_2_sd_60 = subset(data_hist_basal_reps_all_and_ind_no_control, species_id == 2 & env_sd == 60)

data_hist_basal_all_and_ind_3 = subset(data_hist_basal_reps_all_and_ind_no_control, species_id == 3)
data_hist_basal_all_and_ind_3_mean_075 = subset(data_hist_basal_reps_all_and_ind_no_control, species_id == 3 & env_mean == 0.75)
data_hist_basal_all_and_ind_3_mean_085 = subset(data_hist_basal_reps_all_and_ind_no_control, species_id == 3 & env_mean == 0.85)
data_hist_basal_all_and_ind_3_sd_0 = subset(data_hist_basal_reps_all_and_ind_no_control, species_id == 3 & env_sd == 0)
data_hist_basal_all_and_ind_3_sd_15 = subset(data_hist_basal_reps_all_and_ind_no_control, species_id == 3 & env_sd == 15)
data_hist_basal_all_and_ind_3_sd_30 = subset(data_hist_basal_reps_all_and_ind_no_control, species_id == 3 & env_sd == 30)
data_hist_basal_all_and_ind_3_sd_60 = subset(data_hist_basal_reps_all_and_ind_no_control, species_id == 3 & env_sd == 60)

data_hist_basal_all_and_ind_4 = subset(data_hist_basal_reps_all_and_ind_no_control, species_id == 4)
data_hist_basal_all_and_ind_4_mean_075 = subset(data_hist_basal_reps_all_and_ind_no_control, species_id == 4 & env_mean == 0.75)
data_hist_basal_all_and_ind_4_mean_085 = subset(data_hist_basal_reps_all_and_ind_no_control, species_id == 4 & env_mean == 0.85)
data_hist_basal_all_and_ind_4_sd_0 = subset(data_hist_basal_reps_all_and_ind_no_control, species_id == 4 & env_sd == 0)
data_hist_basal_all_and_ind_4_sd_15 = subset(data_hist_basal_reps_all_and_ind_no_control, species_id == 4 & env_sd == 15)
data_hist_basal_all_and_ind_4_sd_30 = subset(data_hist_basal_reps_all_and_ind_no_control, species_id == 4 & env_sd == 30)
data_hist_basal_all_and_ind_4_sd_60 = subset(data_hist_basal_reps_all_and_ind_no_control, species_id == 4 & env_sd == 60)

model_0_mean=lm(data_hist_basal_all_and_ind_0$basal_area ~ data_hist_basal_all_and_ind_0$env_sd)
model_0_mean_075=lm(data_hist_basal_all_and_ind_0_mean_075$basal_area ~ data_hist_basal_all_and_ind_0_mean_075$env_sd)
model_0_mean_085=lm(data_hist_basal_all_and_ind_0_mean_085$basal_area ~ data_hist_basal_all_and_ind_0_mean_085$env_sd)
model_0_sd=lm(data_hist_basal_all_and_ind_0$basal_area ~ data_hist_basal_all_and_ind_0$env_mean)
model_0_sd_0=lm(data_hist_basal_all_and_ind_0_sd_0$basal_area ~ data_hist_basal_all_and_ind_0_sd_0$env_mean)
model_0_sd_15=lm(data_hist_basal_all_and_ind_0_sd_15$basal_area ~ data_hist_basal_all_and_ind_0_sd_15$env_mean)
model_0_sd_30=lm(data_hist_basal_all_and_ind_0_sd_30$basal_area ~ data_hist_basal_all_and_ind_0_sd_30$env_mean)
model_0_sd_60=lm(data_hist_basal_all_and_ind_0_sd_60$basal_area ~ data_hist_basal_all_and_ind_0_sd_60$env_mean)

model_1_mean=lm(data_hist_basal_all_and_ind_1$basal_area ~ data_hist_basal_all_and_ind_1$env_sd)
model_1_mean_075=lm(data_hist_basal_all_and_ind_1_mean_075$basal_area ~ data_hist_basal_all_and_ind_1_mean_075$env_sd)
model_1_mean_085=lm(data_hist_basal_all_and_ind_1_mean_085$basal_area ~ data_hist_basal_all_and_ind_1_mean_085$env_sd)
model_1_sd=lm(data_hist_basal_all_and_ind_1$basal_area ~ data_hist_basal_all_and_ind_1$env_mean)
model_1_sd_0=lm(data_hist_basal_all_and_ind_1_sd_0$basal_area ~ data_hist_basal_all_and_ind_1_sd_0$env_mean)
model_1_sd_15=lm(data_hist_basal_all_and_ind_1_sd_15$basal_area ~ data_hist_basal_all_and_ind_1_sd_15$env_mean)
model_1_sd_30=lm(data_hist_basal_all_and_ind_1_sd_30$basal_area ~ data_hist_basal_all_and_ind_1_sd_30$env_mean)
model_1_sd_60=lm(data_hist_basal_all_and_ind_1_sd_60$basal_area ~ data_hist_basal_all_and_ind_1_sd_60$env_mean)

model_2_mean=lm(data_hist_basal_all_and_ind_2$basal_area ~ data_hist_basal_all_and_ind_2$env_sd)
model_2_mean_075=lm(data_hist_basal_all_and_ind_2_mean_075$basal_area ~ data_hist_basal_all_and_ind_2_mean_075$env_sd)
model_2_mean_085=lm(data_hist_basal_all_and_ind_2_mean_085$basal_area ~ data_hist_basal_all_and_ind_2_mean_085$env_sd)
model_2_sd=lm(data_hist_basal_all_and_ind_2$basal_area ~ data_hist_basal_all_and_ind_2$env_mean)
model_2_sd_0=lm(data_hist_basal_all_and_ind_2_sd_0$basal_area ~ data_hist_basal_all_and_ind_2_sd_0$env_mean)
model_2_sd_15=lm(data_hist_basal_all_and_ind_2_sd_15$basal_area ~ data_hist_basal_all_and_ind_2_sd_15$env_mean)
model_2_sd_30=lm(data_hist_basal_all_and_ind_2_sd_30$basal_area ~ data_hist_basal_all_and_ind_2_sd_30$env_mean)
model_2_sd_60=lm(data_hist_basal_all_and_ind_2_sd_60$basal_area ~ data_hist_basal_all_and_ind_2_sd_60$env_mean)

model_3_mean=lm(data_hist_basal_all_and_ind_3$basal_area ~ data_hist_basal_all_and_ind_3$env_sd)
model_3_mean_075=lm(data_hist_basal_all_and_ind_3_mean_075$basal_area ~ data_hist_basal_all_and_ind_3_mean_075$env_sd)
model_3_mean_085=lm(data_hist_basal_all_and_ind_3_mean_085$basal_area ~ data_hist_basal_all_and_ind_3_mean_085$env_sd)
model_3_sd=lm(data_hist_basal_all_and_ind_3$basal_area ~ data_hist_basal_all_and_ind_3$env_mean)
model_3_sd_0=lm(data_hist_basal_all_and_ind_3_sd_0$basal_area ~ data_hist_basal_all_and_ind_3_sd_0$env_mean)
model_3_sd_15=lm(data_hist_basal_all_and_ind_3_sd_15$basal_area ~ data_hist_basal_all_and_ind_3_sd_15$env_mean)
model_3_sd_30=lm(data_hist_basal_all_and_ind_3_sd_30$basal_area ~ data_hist_basal_all_and_ind_3_sd_30$env_mean)
model_3_sd_60=lm(data_hist_basal_all_and_ind_3_sd_60$basal_area ~ data_hist_basal_all_and_ind_3_sd_60$env_mean)

model_4_mean=lm(data_hist_basal_all_and_ind_4$basal_area ~ data_hist_basal_all_and_ind_4$env_sd)
model_4_mean_075=lm(data_hist_basal_all_and_ind_4_mean_075$basal_area ~ data_hist_basal_all_and_ind_4_mean_075$env_sd)
model_4_mean_085=lm(data_hist_basal_all_and_ind_4_mean_085$basal_area ~ data_hist_basal_all_and_ind_4_mean_085$env_sd)
model_4_sd=lm(data_hist_basal_all_and_ind_4$basal_area ~ data_hist_basal_all_and_ind_4$env_mean)
model_4_sd_0=lm(data_hist_basal_all_and_ind_4_sd_0$basal_area ~ data_hist_basal_all_and_ind_4_sd_0$env_mean)
model_4_sd_15=lm(data_hist_basal_all_and_ind_4_sd_15$basal_area ~ data_hist_basal_all_and_ind_4_sd_15$env_mean)
model_4_sd_30=lm(data_hist_basal_all_and_ind_4_sd_30$basal_area ~ data_hist_basal_all_and_ind_4_sd_30$env_mean)
model_4_sd_60=lm(data_hist_basal_all_and_ind_4_sd_60$basal_area ~ data_hist_basal_all_and_ind_4_sd_60$env_mean)

## THIS PART IS THE EXAMINATION OF ENVIRONMENT SD 

params_0_mean <- parameters::model_parameters(model_0_mean)
params_0_mean_075 <- parameters::model_parameters(model_0_mean_075)
params_0_mean_085 <- parameters::model_parameters(model_0_mean_085)

omega_sq_0_mean <- omega_squared(model_0_mean, partial = FALSE, ci = 0.95)
omega_interpret_0_mean <- interpret_omega_squared(omega_sq_0_mean$Omega2, rules = "field2013")
omega_sq_0_mean_075 <- omega_squared(model_0_mean_075, partial = FALSE, ci = 0.95)
omega_interpret_0_mean_075 <- interpret_omega_squared(omega_sq_0_mean_075$Omega2, rules = "field2013")
omega_sq_0_mean_085 <- omega_squared(model_0_mean_085, partial = FALSE, ci = 0.95)
omega_interpret_0_mean_085 <- interpret_omega_squared(omega_sq_0_mean_085$Omega2, rules = "field2013")

sp_0_sd_var <- data.frame(species_id = 0,
                          mean = "all",
                          all_params_1 = params_0_mean$Parameter[1],
                          all_params_2 = params_0_mean$Parameter[2],
                          all_coeff_1 = params_0_mean$Coefficient[1],
                          all_coeff_2 = params_0_mean$Coefficient[2],
                          all_coeff_ci_low_1 = params_0_mean$CI_low[1],
                          all_coeff_ci_low_2 = params_0_mean$CI_low[2],
                          all_coeff_ci_hi_1 = params_0_mean$CI_high[1],
                          all_coeff_ci_hi_2 = params_0_mean$CI_high[2],
                          all_p_val_1 = params_0_mean$p[1],
                          all_p_val_2 = params_0_mean$p[2],
                          all_omega_sq_1 = omega_sq_0_mean$Omega2[1],
                          all_omega_sq_2 = omega_sq_0_mean$Omega2[2],
                          all_omega_sq_ci_low_1 = omega_sq_0_mean$CI_low[1],
                          all_omega_sq_ci_low_2 = omega_sq_0_mean$CI_low[2],
                          all_omega_sq_ci_hi_1 = omega_sq_0_mean$CI_high[1],
                          all_omega_sq_ci_hi_2 = omega_sq_0_mean$CI_high[2],
                          all_omega_sq_interpret = omega_interpret_0_mean)
sp_0_sd_var_075 <- data.frame(species_id = 0,
                          mean = "medium",
                          all_params_1 = params_0_mean_075$Parameter[1],
                          all_params_2 = params_0_mean_075$Parameter[2],
                          all_coeff_1 = params_0_mean_075$Coefficient[1],
                          all_coeff_2 = params_0_mean_075$Coefficient[2],
                          all_coeff_ci_low_1 = params_0_mean_075$CI_low[1],
                          all_coeff_ci_low_2 = params_0_mean_075$CI_low[2],
                          all_coeff_ci_hi_1 = params_0_mean_075$CI_high[1],
                          all_coeff_ci_hi_2 = params_0_mean_075$CI_high[2],
                          all_p_val_1 = params_0_mean_075$p[1],
                          all_p_val_2 = params_0_mean_075$p[2],
                          all_omega_sq_1 = omega_sq_0_mean_075$Omega2[1],
                          all_omega_sq_2 = omega_sq_0_mean_075$Omega2[2],
                          all_omega_sq_ci_low_1 = omega_sq_0_mean_075$CI_low[1],
                          all_omega_sq_ci_low_2 = omega_sq_0_mean_075$CI_low[2],
                          all_omega_sq_ci_hi_1 = omega_sq_0_mean_075$CI_high[1],
                          all_omega_sq_ci_hi_2 = omega_sq_0_mean_075$CI_high[2],
                          all_omega_sq_interpret = omega_interpret_0_mean_075)
sp_0_sd_var_085 <- data.frame(species_id = 0,
                              mean = "low",
                              all_params_1 = params_0_mean_085$Parameter[1],
                              all_params_2 = params_0_mean_085$Parameter[2],
                              all_coeff_1 = params_0_mean_085$Coefficient[1],
                              all_coeff_2 = params_0_mean_085$Coefficient[2],
                              all_coeff_ci_low_1 = params_0_mean_085$CI_low[1],
                              all_coeff_ci_low_2 = params_0_mean_085$CI_low[2],
                              all_coeff_ci_hi_1 = params_0_mean_085$CI_high[1],
                              all_coeff_ci_hi_2 = params_0_mean_085$CI_high[2],
                              all_p_val_1 = params_0_mean_085$p[1],
                              all_p_val_2 = params_0_mean_085$p[2],
                              all_omega_sq_1 = omega_sq_0_mean_085$Omega2[1],
                              all_omega_sq_2 = omega_sq_0_mean_085$Omega2[2],
                              all_omega_sq_ci_low_1 = omega_sq_0_mean_085$CI_low[1],
                              all_omega_sq_ci_low_2 = omega_sq_0_mean_085$CI_low[2],
                              all_omega_sq_ci_hi_1 = omega_sq_0_mean_085$CI_high[1],
                              all_omega_sq_ci_hi_2 = omega_sq_0_mean_085$CI_high[2],
                              all_omega_sq_interpret = omega_interpret_0_mean_085)

sp_0_sd_var <- plyr::rbind.fill(sp_0_sd_var, sp_0_sd_var_075, sp_0_sd_var_085)

params_1_mean <- parameters::model_parameters(model_1_mean)
params_1_mean_075 <- parameters::model_parameters(model_1_mean_075)
params_1_mean_085 <- parameters::model_parameters(model_1_mean_085)

omega_sq_1_mean <- omega_squared(model_1_mean, partial = FALSE, ci = 0.95)
omega_interpret_1_mean <- interpret_omega_squared(omega_sq_1_mean$Omega2, rules = "field2013")
omega_sq_1_mean_075 <- omega_squared(model_1_mean_075, partial = FALSE, ci = 0.95)
omega_interpret_1_mean_075 <- interpret_omega_squared(omega_sq_1_mean_075$Omega2, rules = "field2013")
omega_sq_1_mean_085 <- omega_squared(model_1_mean_085, partial = FALSE, ci = 0.95)
omega_interpret_1_mean_085 <- interpret_omega_squared(omega_sq_1_mean_085$Omega2, rules = "field2013")

sp_1_sd_var <- data.frame(species_id = 1,
                          mean = "all",
                          all_params_1 = params_1_mean$Parameter[1],
                          all_params_2 = params_1_mean$Parameter[2],
                          all_coeff_1 = params_1_mean$Coefficient[1],
                          all_coeff_2 = params_1_mean$Coefficient[2],
                          all_coeff_ci_low_1 = params_1_mean$CI_low[1],
                          all_coeff_ci_low_2 = params_1_mean$CI_low[2],
                          all_coeff_ci_hi_1 = params_1_mean$CI_high[1],
                          all_coeff_ci_hi_2 = params_1_mean$CI_high[2],
                          all_p_val_1 = params_1_mean$p[1],
                          all_p_val_2 = params_1_mean$p[2],
                          all_omega_sq_1 = omega_sq_1_mean$Omega2[1],
                          all_omega_sq_2 = omega_sq_1_mean$Omega2[2],
                          all_omega_sq_ci_low_1 = omega_sq_1_mean$CI_low[1],
                          all_omega_sq_ci_low_2 = omega_sq_1_mean$CI_low[2],
                          all_omega_sq_ci_hi_1 = omega_sq_1_mean$CI_high[1],
                          all_omega_sq_ci_hi_2 = omega_sq_1_mean$CI_high[2],
                          all_omega_sq_interpret = omega_interpret_1_mean)
sp_1_sd_var_075 <- data.frame(species_id = 1,
                              mean = "medium",
                              all_params_1 = params_1_mean_075$Parameter[1],
                              all_params_2 = params_1_mean_075$Parameter[2],
                              all_coeff_1 = params_1_mean_075$Coefficient[1],
                              all_coeff_2 = params_1_mean_075$Coefficient[2],
                              all_coeff_ci_low_1 = params_1_mean_075$CI_low[1],
                              all_coeff_ci_low_2 = params_1_mean_075$CI_low[2],
                              all_coeff_ci_hi_1 = params_1_mean_075$CI_high[1],
                              all_coeff_ci_hi_2 = params_1_mean_075$CI_high[2],
                              all_p_val_1 = params_1_mean_075$p[1],
                              all_p_val_2 = params_1_mean_075$p[2],
                              all_omega_sq_1 = omega_sq_1_mean_075$Omega2[1],
                              all_omega_sq_2 = omega_sq_1_mean_075$Omega2[2],
                              all_omega_sq_ci_low_1 = omega_sq_1_mean_075$CI_low[1],
                              all_omega_sq_ci_low_2 = omega_sq_1_mean_075$CI_low[2],
                              all_omega_sq_ci_hi_1 = omega_sq_1_mean_075$CI_high[1],
                              all_omega_sq_ci_hi_2 = omega_sq_1_mean_075$CI_high[2],
                              all_omega_sq_interpret = omega_interpret_1_mean_075)
sp_1_sd_var_085 <- data.frame(species_id = 1,
                              mean = "low",
                              all_params_1 = params_1_mean_085$Parameter[1],
                              all_params_2 = params_1_mean_085$Parameter[2],
                              all_coeff_1 = params_1_mean_085$Coefficient[1],
                              all_coeff_2 = params_1_mean_085$Coefficient[2],
                              all_coeff_ci_low_1 = params_1_mean_085$CI_low[1],
                              all_coeff_ci_low_2 = params_1_mean_085$CI_low[2],
                              all_coeff_ci_hi_1 = params_1_mean_085$CI_high[1],
                              all_coeff_ci_hi_2 = params_1_mean_085$CI_high[2],
                              all_p_val_1 = params_1_mean_085$p[1],
                              all_p_val_2 = params_1_mean_085$p[2],
                              all_omega_sq_1 = omega_sq_1_mean_085$Omega2[1],
                              all_omega_sq_2 = omega_sq_1_mean_085$Omega2[2],
                              all_omega_sq_ci_low_1 = omega_sq_1_mean_085$CI_low[1],
                              all_omega_sq_ci_low_2 = omega_sq_1_mean_085$CI_low[2],
                              all_omega_sq_ci_hi_1 = omega_sq_1_mean_085$CI_high[1],
                              all_omega_sq_ci_hi_2 = omega_sq_1_mean_085$CI_high[2],
                              all_omega_sq_interpret = omega_interpret_1_mean_085)

sp_1_sd_var <- plyr::rbind.fill(sp_1_sd_var, sp_1_sd_var_075, sp_1_sd_var_085)

params_2_mean <- parameters::model_parameters(model_2_mean)
params_2_mean_075 <- parameters::model_parameters(model_2_mean_075)
params_2_mean_085 <- parameters::model_parameters(model_2_mean_085)

omega_sq_2_mean <- omega_squared(model_2_mean, partial = FALSE, ci = 0.95)
omega_interpret_2_mean <- interpret_omega_squared(omega_sq_2_mean$Omega2, rules = "field2013")
omega_sq_2_mean_075 <- omega_squared(model_2_mean_075, partial = FALSE, ci = 0.95)
omega_interpret_2_mean_075 <- interpret_omega_squared(omega_sq_2_mean_075$Omega2, rules = "field2013")
omega_sq_2_mean_085 <- omega_squared(model_2_mean_085, partial = FALSE, ci = 0.95)
omega_interpret_2_mean_085 <- interpret_omega_squared(omega_sq_2_mean_085$Omega2, rules = "field2013")

sp_2_sd_var <- data.frame(species_id = 2,
                          mean = "all",
                          all_params_1 = params_2_mean$Parameter[1],
                          all_params_2 = params_2_mean$Parameter[2],
                          all_coeff_1 = params_2_mean$Coefficient[1],
                          all_coeff_2 = params_2_mean$Coefficient[2],
                          all_coeff_ci_low_1 = params_2_mean$CI_low[1],
                          all_coeff_ci_low_2 = params_2_mean$CI_low[2],
                          all_coeff_ci_hi_1 = params_2_mean$CI_high[1],
                          all_coeff_ci_hi_2 = params_2_mean$CI_high[2],
                          all_p_val_1 = params_2_mean$p[1],
                          all_p_val_2 = params_2_mean$p[2],
                          all_omega_sq_1 = omega_sq_2_mean$Omega2[1],
                          all_omega_sq_2 = omega_sq_2_mean$Omega2[2],
                          all_omega_sq_ci_low_1 = omega_sq_2_mean$CI_low[1],
                          all_omega_sq_ci_low_2 = omega_sq_2_mean$CI_low[2],
                          all_omega_sq_ci_hi_1 = omega_sq_2_mean$CI_high[1],
                          all_omega_sq_ci_hi_2 = omega_sq_2_mean$CI_high[2],
                          all_omega_sq_interpret = omega_interpret_2_mean)
sp_2_sd_var_075 <- data.frame(species_id = 2,
                              mean = "medium",
                              all_params_1 = params_2_mean_075$Parameter[1],
                              all_params_2 = params_2_mean_075$Parameter[2],
                              all_coeff_1 = params_2_mean_075$Coefficient[1],
                              all_coeff_2 = params_2_mean_075$Coefficient[2],
                              all_coeff_ci_low_1 = params_2_mean_075$CI_low[1],
                              all_coeff_ci_low_2 = params_2_mean_075$CI_low[2],
                              all_coeff_ci_hi_1 = params_2_mean_075$CI_high[1],
                              all_coeff_ci_hi_2 = params_2_mean_075$CI_high[2],
                              all_p_val_1 = params_2_mean_075$p[1],
                              all_p_val_2 = params_2_mean_075$p[2],
                              all_omega_sq_1 = omega_sq_2_mean_075$Omega2[1],
                              all_omega_sq_2 = omega_sq_2_mean_075$Omega2[2],
                              all_omega_sq_ci_low_1 = omega_sq_2_mean_075$CI_low[1],
                              all_omega_sq_ci_low_2 = omega_sq_2_mean_075$CI_low[2],
                              all_omega_sq_ci_hi_1 = omega_sq_2_mean_075$CI_high[1],
                              all_omega_sq_ci_hi_2 = omega_sq_2_mean_075$CI_high[2],
                              all_omega_sq_interpret = omega_interpret_2_mean_075)
sp_2_sd_var_085 <- data.frame(species_id = 2,
                              mean = "low",
                              all_params_1 = params_2_mean_085$Parameter[1],
                              all_params_2 = params_2_mean_085$Parameter[2],
                              all_coeff_1 = params_2_mean_085$Coefficient[1],
                              all_coeff_2 = params_2_mean_085$Coefficient[2],
                              all_coeff_ci_low_1 = params_2_mean_085$CI_low[1],
                              all_coeff_ci_low_2 = params_2_mean_085$CI_low[2],
                              all_coeff_ci_hi_1 = params_2_mean_085$CI_high[1],
                              all_coeff_ci_hi_2 = params_2_mean_085$CI_high[2],
                              all_p_val_1 = params_2_mean_085$p[1],
                              all_p_val_2 = params_2_mean_085$p[2],
                              all_omega_sq_1 = omega_sq_2_mean_085$Omega2[1],
                              all_omega_sq_2 = omega_sq_2_mean_085$Omega2[2],
                              all_omega_sq_ci_low_1 = omega_sq_2_mean_085$CI_low[1],
                              all_omega_sq_ci_low_2 = omega_sq_2_mean_085$CI_low[2],
                              all_omega_sq_ci_hi_1 = omega_sq_2_mean_085$CI_high[1],
                              all_omega_sq_ci_hi_2 = omega_sq_2_mean_085$CI_high[2],
                              all_omega_sq_interpret = omega_interpret_2_mean_085)

sp_2_sd_var <- plyr::rbind.fill(sp_2_sd_var, sp_2_sd_var_075, sp_2_sd_var_085)

params_3_mean <- parameters::model_parameters(model_3_mean)
params_3_mean_075 <- parameters::model_parameters(model_3_mean_075)
params_3_mean_085 <- parameters::model_parameters(model_3_mean_085)

omega_sq_3_mean <- omega_squared(model_3_mean, partial = FALSE, ci = 0.95)
omega_interpret_3_mean <- interpret_omega_squared(omega_sq_3_mean$Omega2, rules = "field2013")
omega_sq_3_mean_075 <- omega_squared(model_3_mean_075, partial = FALSE, ci = 0.95)
omega_interpret_3_mean_075 <- interpret_omega_squared(omega_sq_3_mean_075$Omega2, rules = "field2013")
omega_sq_3_mean_085 <- omega_squared(model_3_mean_085, partial = FALSE, ci = 0.95)
omega_interpret_3_mean_085 <- interpret_omega_squared(omega_sq_3_mean_085$Omega2, rules = "field2013")

sp_3_sd_var <- data.frame(species_id = 3,
                          mean = "all",
                          all_params_1 = params_3_mean$Parameter[1],
                          all_params_2 = params_3_mean$Parameter[2],
                          all_coeff_1 = params_3_mean$Coefficient[1],
                          all_coeff_2 = params_3_mean$Coefficient[2],
                          all_coeff_ci_low_1 = params_3_mean$CI_low[1],
                          all_coeff_ci_low_2 = params_3_mean$CI_low[2],
                          all_coeff_ci_hi_1 = params_3_mean$CI_high[1],
                          all_coeff_ci_hi_2 = params_3_mean$CI_high[2],
                          all_p_val_1 = params_3_mean$p[1],
                          all_p_val_2 = params_3_mean$p[2],
                          all_omega_sq_1 = omega_sq_3_mean$Omega2[1],
                          all_omega_sq_2 = omega_sq_3_mean$Omega2[2],
                          all_omega_sq_ci_low_1 = omega_sq_3_mean$CI_low[1],
                          all_omega_sq_ci_low_2 = omega_sq_3_mean$CI_low[2],
                          all_omega_sq_ci_hi_1 = omega_sq_3_mean$CI_high[1],
                          all_omega_sq_ci_hi_2 = omega_sq_3_mean$CI_high[2],
                          all_omega_sq_interpret = omega_interpret_3_mean)
sp_3_sd_var_075 <- data.frame(species_id = 3,
                              mean = "medium",
                              all_params_1 = params_3_mean_075$Parameter[1],
                              all_params_2 = params_3_mean_075$Parameter[2],
                              all_coeff_1 = params_3_mean_075$Coefficient[1],
                              all_coeff_2 = params_3_mean_075$Coefficient[2],
                              all_coeff_ci_low_1 = params_3_mean_075$CI_low[1],
                              all_coeff_ci_low_2 = params_3_mean_075$CI_low[2],
                              all_coeff_ci_hi_1 = params_3_mean_075$CI_high[1],
                              all_coeff_ci_hi_2 = params_3_mean_075$CI_high[2],
                              all_p_val_1 = params_3_mean_075$p[1],
                              all_p_val_2 = params_3_mean_075$p[2],
                              all_omega_sq_1 = omega_sq_3_mean_075$Omega2[1],
                              all_omega_sq_2 = omega_sq_3_mean_075$Omega2[2],
                              all_omega_sq_ci_low_1 = omega_sq_3_mean_075$CI_low[1],
                              all_omega_sq_ci_low_2 = omega_sq_3_mean_075$CI_low[2],
                              all_omega_sq_ci_hi_1 = omega_sq_3_mean_075$CI_high[1],
                              all_omega_sq_ci_hi_2 = omega_sq_3_mean_075$CI_high[2],
                              all_omega_sq_interpret = omega_interpret_3_mean_075)
sp_3_sd_var_085 <- data.frame(species_id = 3,
                              mean = "low",
                              all_params_1 = params_3_mean_085$Parameter[1],
                              all_params_2 = params_3_mean_085$Parameter[2],
                              all_coeff_1 = params_3_mean_085$Coefficient[1],
                              all_coeff_2 = params_3_mean_085$Coefficient[2],
                              all_coeff_ci_low_1 = params_3_mean_085$CI_low[1],
                              all_coeff_ci_low_2 = params_3_mean_085$CI_low[2],
                              all_coeff_ci_hi_1 = params_3_mean_085$CI_high[1],
                              all_coeff_ci_hi_2 = params_3_mean_085$CI_high[2],
                              all_p_val_1 = params_3_mean_085$p[1],
                              all_p_val_2 = params_3_mean_085$p[2],
                              all_omega_sq_1 = omega_sq_3_mean_085$Omega2[1],
                              all_omega_sq_2 = omega_sq_3_mean_085$Omega2[2],
                              all_omega_sq_ci_low_1 = omega_sq_3_mean_085$CI_low[1],
                              all_omega_sq_ci_low_2 = omega_sq_3_mean_085$CI_low[2],
                              all_omega_sq_ci_hi_1 = omega_sq_3_mean_085$CI_high[1],
                              all_omega_sq_ci_hi_2 = omega_sq_3_mean_085$CI_high[2],
                              all_omega_sq_interpret = omega_interpret_3_mean_085)

sp_3_sd_var <- plyr::rbind.fill(sp_3_sd_var, sp_3_sd_var_075, sp_3_sd_var_085)

params_4_mean <- parameters::model_parameters(model_4_mean)
params_4_mean_075 <- parameters::model_parameters(model_4_mean_075)
params_4_mean_085 <- parameters::model_parameters(model_4_mean_085)

omega_sq_4_mean <- omega_squared(model_4_mean, partial = FALSE, ci = 0.95)
omega_interpret_4_mean <- interpret_omega_squared(omega_sq_4_mean$Omega2, rules = "field2013")
omega_sq_4_mean_075 <- omega_squared(model_4_mean_075, partial = FALSE, ci = 0.95)
omega_interpret_4_mean_075 <- interpret_omega_squared(omega_sq_4_mean_075$Omega2, rules = "field2013")
omega_sq_4_mean_085 <- omega_squared(model_4_mean_085, partial = FALSE, ci = 0.95)
omega_interpret_4_mean_085 <- interpret_omega_squared(omega_sq_4_mean_085$Omega2, rules = "field2013")

sp_4_sd_var <- data.frame(species_id = 4,
                          mean = "all",
                          all_params_1 = params_4_mean$Parameter[1],
                          all_params_2 = params_4_mean$Parameter[2],
                          all_coeff_1 = params_4_mean$Coefficient[1],
                          all_coeff_2 = params_4_mean$Coefficient[2],
                          all_coeff_ci_low_1 = params_4_mean$CI_low[1],
                          all_coeff_ci_low_2 = params_4_mean$CI_low[2],
                          all_coeff_ci_hi_1 = params_4_mean$CI_high[1],
                          all_coeff_ci_hi_2 = params_4_mean$CI_high[2],
                          all_p_val_1 = params_4_mean$p[1],
                          all_p_val_2 = params_4_mean$p[2],
                          all_omega_sq_1 = omega_sq_4_mean$Omega2[1],
                          all_omega_sq_2 = omega_sq_4_mean$Omega2[2],
                          all_omega_sq_ci_low_1 = omega_sq_4_mean$CI_low[1],
                          all_omega_sq_ci_low_2 = omega_sq_4_mean$CI_low[2],
                          all_omega_sq_ci_hi_1 = omega_sq_4_mean$CI_high[1],
                          all_omega_sq_ci_hi_2 = omega_sq_4_mean$CI_high[2],
                          all_omega_sq_interpret = omega_interpret_4_mean)
sp_4_sd_var_075 <- data.frame(species_id = 4,
                              mean = "medium",
                              all_params_1 = params_4_mean_075$Parameter[1],
                              all_params_2 = params_4_mean_075$Parameter[2],
                              all_coeff_1 = params_4_mean_075$Coefficient[1],
                              all_coeff_2 = params_4_mean_075$Coefficient[2],
                              all_coeff_ci_low_1 = params_4_mean_075$CI_low[1],
                              all_coeff_ci_low_2 = params_4_mean_075$CI_low[2],
                              all_coeff_ci_hi_1 = params_4_mean_075$CI_high[1],
                              all_coeff_ci_hi_2 = params_4_mean_075$CI_high[2],
                              all_p_val_1 = params_4_mean_075$p[1],
                              all_p_val_2 = params_4_mean_075$p[2],
                              all_omega_sq_1 = omega_sq_4_mean_075$Omega2[1],
                              all_omega_sq_2 = omega_sq_4_mean_075$Omega2[2],
                              all_omega_sq_ci_low_1 = omega_sq_4_mean_075$CI_low[1],
                              all_omega_sq_ci_low_2 = omega_sq_4_mean_075$CI_low[2],
                              all_omega_sq_ci_hi_1 = omega_sq_4_mean_075$CI_high[1],
                              all_omega_sq_ci_hi_2 = omega_sq_4_mean_075$CI_high[2],
                              all_omega_sq_interpret = omega_interpret_4_mean_075)
sp_4_sd_var_085 <- data.frame(species_id = 4,
                              mean = "low",
                              all_params_1 = params_4_mean_085$Parameter[1],
                              all_params_2 = params_4_mean_085$Parameter[2],
                              all_coeff_1 = params_4_mean_085$Coefficient[1],
                              all_coeff_2 = params_4_mean_085$Coefficient[2],
                              all_coeff_ci_low_1 = params_4_mean_085$CI_low[1],
                              all_coeff_ci_low_2 = params_4_mean_085$CI_low[2],
                              all_coeff_ci_hi_1 = params_4_mean_085$CI_high[1],
                              all_coeff_ci_hi_2 = params_4_mean_085$CI_high[2],
                              all_p_val_1 = params_4_mean_085$p[1],
                              all_p_val_2 = params_4_mean_085$p[2],
                              all_omega_sq_1 = omega_sq_4_mean_085$Omega2[1],
                              all_omega_sq_2 = omega_sq_4_mean_085$Omega2[2],
                              all_omega_sq_ci_low_1 = omega_sq_4_mean_085$CI_low[1],
                              all_omega_sq_ci_low_2 = omega_sq_4_mean_085$CI_low[2],
                              all_omega_sq_ci_hi_1 = omega_sq_4_mean_085$CI_high[1],
                              all_omega_sq_ci_hi_2 = omega_sq_4_mean_085$CI_high[2],
                              all_omega_sq_interpret = omega_interpret_4_mean_085)

sp_4_sd_var <- plyr::rbind.fill(sp_4_sd_var, sp_4_sd_var_075, sp_4_sd_var_085)
basal_area_sd_var <- plyr::rbind.fill(sp_1_sd_var, sp_2_sd_var, sp_3_sd_var, sp_4_sd_var)
basal_area_sd_var <- plyr::rbind.fill(sp_0_sd_var, sp_1_sd_var, sp_2_sd_var, sp_3_sd_var, sp_4_sd_var)
write.csv(basal_area_sd_var, file = "out/basal_area_sd_var_stats_new.csv")

### LOG VALUES

data_hist_basal_reps_all_and_ind_no_control$basal_area = log(data_hist_basal_reps_all_and_ind_no_control$basal_area)
data_hist_basal_reps_all_and_ind_no_control$env_mean = as.numeric(data_hist_basal_reps_all_and_ind_no_control$env_mean)
data_hist_basal_reps_all_and_ind_no_control$env_mean[data_hist_basal_reps_all_and_ind_no_control$env_mean==1] = -0.75
data_hist_basal_reps_all_and_ind_no_control$env_mean[data_hist_basal_reps_all_and_ind_no_control$env_mean==2] = -0.85

data_hist_basal_all_and_ind_0 = subset(data_hist_basal_reps_all_and_ind_no_control, species_id == 0)
data_hist_basal_all_and_ind_0_mean_075 = subset(data_hist_basal_reps_all_and_ind_no_control, species_id == 0 & env_mean == 0.75)
data_hist_basal_all_and_ind_0_mean_085 = subset(data_hist_basal_reps_all_and_ind_no_control, species_id == 0 & env_mean == 0.85)
data_hist_basal_all_and_ind_0_sd_0 = subset(data_hist_basal_reps_all_and_ind_no_control, species_id == 0 & env_sd == 0)
data_hist_basal_all_and_ind_0_sd_15 = subset(data_hist_basal_reps_all_and_ind_no_control, species_id == 0 & env_sd == 15)
data_hist_basal_all_and_ind_0_sd_30 = subset(data_hist_basal_reps_all_and_ind_no_control, species_id == 0 & env_sd == 30)
data_hist_basal_all_and_ind_0_sd_60 = subset(data_hist_basal_reps_all_and_ind_no_control, species_id == 0 & env_sd == 60)

data_hist_basal_all_and_ind_1 = subset(data_hist_basal_reps_all_and_ind_no_control, species_id == 1)
data_hist_basal_all_and_ind_1_mean_075 = subset(data_hist_basal_reps_all_and_ind_no_control, species_id == 1 & env_mean == 0.75)
data_hist_basal_all_and_ind_1_mean_085 = subset(data_hist_basal_reps_all_and_ind_no_control, species_id == 1 & env_mean == 0.85)
data_hist_basal_all_and_ind_1_sd_0 = subset(data_hist_basal_reps_all_and_ind_no_control, species_id == 1 & env_sd == 0)
data_hist_basal_all_and_ind_1_sd_15 = subset(data_hist_basal_reps_all_and_ind_no_control, species_id == 1 & env_sd == 15)
data_hist_basal_all_and_ind_1_sd_30 = subset(data_hist_basal_reps_all_and_ind_no_control, species_id == 1 & env_sd == 30)
data_hist_basal_all_and_ind_1_sd_60 = subset(data_hist_basal_reps_all_and_ind_no_control, species_id == 1 & env_sd == 60)

data_hist_basal_all_and_ind_2 = subset(data_hist_basal_reps_all_and_ind_no_control, species_id == 2)
data_hist_basal_all_and_ind_2_mean_075 = subset(data_hist_basal_reps_all_and_ind_no_control, species_id == 2 & env_mean == 0.75)
data_hist_basal_all_and_ind_2_mean_085 = subset(data_hist_basal_reps_all_and_ind_no_control, species_id == 2 & env_mean == 0.85)
data_hist_basal_all_and_ind_2_sd_0 = subset(data_hist_basal_reps_all_and_ind_no_control, species_id == 2 & env_sd == 0)
data_hist_basal_all_and_ind_2_sd_15 = subset(data_hist_basal_reps_all_and_ind_no_control, species_id == 2 & env_sd == 15)
data_hist_basal_all_and_ind_2_sd_30 = subset(data_hist_basal_reps_all_and_ind_no_control, species_id == 2 & env_sd == 30)
data_hist_basal_all_and_ind_2_sd_60 = subset(data_hist_basal_reps_all_and_ind_no_control, species_id == 2 & env_sd == 60)

data_hist_basal_all_and_ind_3 = subset(data_hist_basal_reps_all_and_ind_no_control, species_id == 3)
data_hist_basal_all_and_ind_3_mean_075 = subset(data_hist_basal_reps_all_and_ind_no_control, species_id == 3 & env_mean == 0.75)
data_hist_basal_all_and_ind_3_mean_085 = subset(data_hist_basal_reps_all_and_ind_no_control, species_id == 3 & env_mean == 0.85)
data_hist_basal_all_and_ind_3_sd_0 = subset(data_hist_basal_reps_all_and_ind_no_control, species_id == 3 & env_sd == 0)
data_hist_basal_all_and_ind_3_sd_15 = subset(data_hist_basal_reps_all_and_ind_no_control, species_id == 3 & env_sd == 15)
data_hist_basal_all_and_ind_3_sd_30 = subset(data_hist_basal_reps_all_and_ind_no_control, species_id == 3 & env_sd == 30)
data_hist_basal_all_and_ind_3_sd_60 = subset(data_hist_basal_reps_all_and_ind_no_control, species_id == 3 & env_sd == 60)

data_hist_basal_all_and_ind_4 = subset(data_hist_basal_reps_all_and_ind_no_control, species_id == 4)
data_hist_basal_all_and_ind_4_mean_075 = subset(data_hist_basal_reps_all_and_ind_no_control, species_id == 4 & env_mean == 0.75)
data_hist_basal_all_and_ind_4_mean_085 = subset(data_hist_basal_reps_all_and_ind_no_control, species_id == 4 & env_mean == 0.85)
data_hist_basal_all_and_ind_4_sd_0 = subset(data_hist_basal_reps_all_and_ind_no_control, species_id == 4 & env_sd == 0)
data_hist_basal_all_and_ind_4_sd_15 = subset(data_hist_basal_reps_all_and_ind_no_control, species_id == 4 & env_sd == 15)
data_hist_basal_all_and_ind_4_sd_30 = subset(data_hist_basal_reps_all_and_ind_no_control, species_id == 4 & env_sd == 30)
data_hist_basal_all_and_ind_4_sd_60 = subset(data_hist_basal_reps_all_and_ind_no_control, species_id == 4 & env_sd == 60)

model_0_mean=lm((data_hist_basal_all_and_ind_0$basal_area) ~ data_hist_basal_all_and_ind_0$env_sd)
model_0_mean_075=lm((data_hist_basal_all_and_ind_0_mean_075$basal_area) ~ data_hist_basal_all_and_ind_0_mean_075$env_sd)
model_0_mean_085=lm((data_hist_basal_all_and_ind_0_mean_085$basal_area) ~ data_hist_basal_all_and_ind_0_mean_085$env_sd)
model_0_sd=lm((data_hist_basal_all_and_ind_0$basal_area) ~ data_hist_basal_all_and_ind_0$env_mean)
model_0_sd_0=lm((data_hist_basal_all_and_ind_0_sd_0$basal_area) ~ data_hist_basal_all_and_ind_0_sd_0$env_mean)
model_0_sd_15=lm((data_hist_basal_all_and_ind_0_sd_15$basal_area) ~ data_hist_basal_all_and_ind_0_sd_15$env_mean)
model_0_sd_30=lm((data_hist_basal_all_and_ind_0_sd_30$basal_area) ~ data_hist_basal_all_and_ind_0_sd_30$env_mean)
model_0_sd_60=lm((data_hist_basal_all_and_ind_0_sd_60$basal_area) ~ data_hist_basal_all_and_ind_0_sd_60$env_mean)

model_1_mean=lm((data_hist_basal_all_and_ind_1$basal_area) ~ data_hist_basal_all_and_ind_1$env_sd)
model_1_mean_075=lm((data_hist_basal_all_and_ind_1_mean_075$basal_area) ~ data_hist_basal_all_and_ind_1_mean_075$env_sd)
model_1_mean_085=lm((data_hist_basal_all_and_ind_1_mean_085$basal_area) ~ data_hist_basal_all_and_ind_1_mean_085$env_sd)
model_1_sd=lm((data_hist_basal_all_and_ind_1$basal_area) ~ data_hist_basal_all_and_ind_1$env_mean)
model_1_sd_0=lm((data_hist_basal_all_and_ind_1_sd_0$basal_area) ~ data_hist_basal_all_and_ind_1_sd_0$env_mean)
model_1_sd_15=lm((data_hist_basal_all_and_ind_1_sd_15$basal_area) ~ data_hist_basal_all_and_ind_1_sd_15$env_mean)
model_1_sd_30=lm((data_hist_basal_all_and_ind_1_sd_30$basal_area) ~ data_hist_basal_all_and_ind_1_sd_30$env_mean)
model_1_sd_60=lm((data_hist_basal_all_and_ind_1_sd_60$basal_area) ~ data_hist_basal_all_and_ind_1_sd_60$env_mean)

data_hist_basal_all_and_ind_2_new <- subset(data_hist_basal_all_and_ind_2, env_sd != 60)
data_hist_basal_all_and_ind_2_mean_075_new <- subset(data_hist_basal_all_and_ind_2_mean_075, env_sd != 60)

model_2_mean=lm((data_hist_basal_all_and_ind_2$basal_area) ~ data_hist_basal_all_and_ind_2$env_sd)
model_2_mean_new=lm((data_hist_basal_all_and_ind_2_new$basal_area) ~ data_hist_basal_all_and_ind_2_new$env_sd)
model_2_mean_075_new=lm((data_hist_basal_all_and_ind_2_mean_075_new$basal_area) ~ data_hist_basal_all_and_ind_2_mean_075_new$env_sd)
model_2_mean_075=lm((data_hist_basal_all_and_ind_2_mean_075$basal_area) ~ data_hist_basal_all_and_ind_2_mean_075$env_sd)
model_2_mean_085=lm((data_hist_basal_all_and_ind_2_mean_085$basal_area) ~ data_hist_basal_all_and_ind_2_mean_085$env_sd)
model_2_sd=lm((data_hist_basal_all_and_ind_2$basal_area) ~ data_hist_basal_all_and_ind_2$env_mean)
model_2_sd_0=lm((data_hist_basal_all_and_ind_2_sd_0$basal_area) ~ data_hist_basal_all_and_ind_2_sd_0$env_mean)
model_2_sd_15=lm((data_hist_basal_all_and_ind_2_sd_15$basal_area) ~ data_hist_basal_all_and_ind_2_sd_15$env_mean)
model_2_sd_30=lm((data_hist_basal_all_and_ind_2_sd_30$basal_area) ~ data_hist_basal_all_and_ind_2_sd_30$env_mean)
model_2_sd_60=lm((data_hist_basal_all_and_ind_2_sd_60$basal_area) ~ data_hist_basal_all_and_ind_2_sd_60$env_mean)

model_3_mean=lm((data_hist_basal_all_and_ind_3$basal_area) ~ data_hist_basal_all_and_ind_3$env_sd)
model_3_mean_075=lm((data_hist_basal_all_and_ind_3_mean_075$basal_area) ~ data_hist_basal_all_and_ind_3_mean_075$env_sd)
model_3_mean_085=lm((data_hist_basal_all_and_ind_3_mean_085$basal_area) ~ data_hist_basal_all_and_ind_3_mean_085$env_sd)
model_3_sd=lm((data_hist_basal_all_and_ind_3$basal_area) ~ data_hist_basal_all_and_ind_3$env_mean)
model_3_sd_0=lm((data_hist_basal_all_and_ind_3_sd_0$basal_area) ~ data_hist_basal_all_and_ind_3_sd_0$env_mean)
model_3_sd_15=lm((data_hist_basal_all_and_ind_3_sd_15$basal_area) ~ data_hist_basal_all_and_ind_3_sd_15$env_mean)
model_3_sd_30=lm((data_hist_basal_all_and_ind_3_sd_30$basal_area) ~ data_hist_basal_all_and_ind_3_sd_30$env_mean)
model_3_sd_60=lm((data_hist_basal_all_and_ind_3_sd_60$basal_area) ~ data_hist_basal_all_and_ind_3_sd_60$env_mean)

model_4_mean=lm((data_hist_basal_all_and_ind_4$basal_area) ~ data_hist_basal_all_and_ind_4$env_sd)
model_4_mean_075=lm((data_hist_basal_all_and_ind_4_mean_075$basal_area) ~ data_hist_basal_all_and_ind_4_mean_075$env_sd)
model_4_mean_085=lm((data_hist_basal_all_and_ind_4_mean_085$basal_area) ~ data_hist_basal_all_and_ind_4_mean_085$env_sd)
model_4_sd=lm((data_hist_basal_all_and_ind_4$basal_area) ~ data_hist_basal_all_and_ind_4$env_mean)
model_4_sd_0=lm((data_hist_basal_all_and_ind_4_sd_0$basal_area) ~ data_hist_basal_all_and_ind_4_sd_0$env_mean)
model_4_sd_15=lm((data_hist_basal_all_and_ind_4_sd_15$basal_area) ~ data_hist_basal_all_and_ind_4_sd_15$env_mean)
model_4_sd_30=lm((data_hist_basal_all_and_ind_4_sd_30$basal_area) ~ data_hist_basal_all_and_ind_4_sd_30$env_mean)
model_4_sd_60=lm((data_hist_basal_all_and_ind_4_sd_60$basal_area) ~ data_hist_basal_all_and_ind_4_sd_60$env_mean)

## THIS PART IS THE EXAMINATION OF ENVIRONMENT SD 

params_0_mean <- parameters::model_parameters(model_0_mean)
params_0_mean_075 <- parameters::model_parameters(model_0_mean_075)
params_0_mean_085 <- parameters::model_parameters(model_0_mean_085)

omega_sq_0_mean <- omega_squared(model_0_mean, partial = FALSE, ci = 0.95)
omega_interpret_0_mean <- interpret_omega_squared(omega_sq_0_mean$Omega2, rules = "field2013")
omega_sq_0_mean_075 <- omega_squared(model_0_mean_075, partial = FALSE, ci = 0.95)
omega_interpret_0_mean_075 <- interpret_omega_squared(omega_sq_0_mean_075$Omega2, rules = "field2013")
omega_sq_0_mean_085 <- omega_squared(model_0_mean_085, partial = FALSE, ci = 0.95)
omega_interpret_0_mean_085 <- interpret_omega_squared(omega_sq_0_mean_085$Omega2, rules = "field2013")

sp_0_sd_var <- data.frame(species_id = 0,
                          mean = "all",
                          all_params_1 = params_0_mean$Parameter[1],
                          all_params_2 = params_0_mean$Parameter[2],
                          all_coeff_1 = params_0_mean$Coefficient[1],
                          all_coeff_2 = params_0_mean$Coefficient[2],
                          all_coeff_ci_low_1 = params_0_mean$CI_low[1],
                          all_coeff_ci_low_2 = params_0_mean$CI_low[2],
                          all_coeff_ci_hi_1 = params_0_mean$CI_high[1],
                          all_coeff_ci_hi_2 = params_0_mean$CI_high[2],
                          all_p_val_1 = params_0_mean$p[1],
                          all_p_val_2 = params_0_mean$p[2],
                          all_omega_sq_1 = omega_sq_0_mean$Omega2[1],
                          all_omega_sq_2 = omega_sq_0_mean$Omega2[2],
                          all_omega_sq_ci_low_1 = omega_sq_0_mean$CI_low[1],
                          all_omega_sq_ci_low_2 = omega_sq_0_mean$CI_low[2],
                          all_omega_sq_ci_hi_1 = omega_sq_0_mean$CI_high[1],
                          all_omega_sq_ci_hi_2 = omega_sq_0_mean$CI_high[2],
                          all_omega_sq_interpret = omega_interpret_0_mean)
sp_0_sd_var_075 <- data.frame(species_id = 0,
                              mean = "medium",
                              all_params_1 = params_0_mean_075$Parameter[1],
                              all_params_2 = params_0_mean_075$Parameter[2],
                              all_coeff_1 = params_0_mean_075$Coefficient[1],
                              all_coeff_2 = params_0_mean_075$Coefficient[2],
                              all_coeff_ci_low_1 = params_0_mean_075$CI_low[1],
                              all_coeff_ci_low_2 = params_0_mean_075$CI_low[2],
                              all_coeff_ci_hi_1 = params_0_mean_075$CI_high[1],
                              all_coeff_ci_hi_2 = params_0_mean_075$CI_high[2],
                              all_p_val_1 = params_0_mean_075$p[1],
                              all_p_val_2 = params_0_mean_075$p[2],
                              all_omega_sq_1 = omega_sq_0_mean_075$Omega2[1],
                              all_omega_sq_2 = omega_sq_0_mean_075$Omega2[2],
                              all_omega_sq_ci_low_1 = omega_sq_0_mean_075$CI_low[1],
                              all_omega_sq_ci_low_2 = omega_sq_0_mean_075$CI_low[2],
                              all_omega_sq_ci_hi_1 = omega_sq_0_mean_075$CI_high[1],
                              all_omega_sq_ci_hi_2 = omega_sq_0_mean_075$CI_high[2],
                              all_omega_sq_interpret = omega_interpret_0_mean_075)
sp_0_sd_var_085 <- data.frame(species_id = 0,
                              mean = "low",
                              all_params_1 = params_0_mean_085$Parameter[1],
                              all_params_2 = params_0_mean_085$Parameter[2],
                              all_coeff_1 = params_0_mean_085$Coefficient[1],
                              all_coeff_2 = params_0_mean_085$Coefficient[2],
                              all_coeff_ci_low_1 = params_0_mean_085$CI_low[1],
                              all_coeff_ci_low_2 = params_0_mean_085$CI_low[2],
                              all_coeff_ci_hi_1 = params_0_mean_085$CI_high[1],
                              all_coeff_ci_hi_2 = params_0_mean_085$CI_high[2],
                              all_p_val_1 = params_0_mean_085$p[1],
                              all_p_val_2 = params_0_mean_085$p[2],
                              all_omega_sq_1 = omega_sq_0_mean_085$Omega2[1],
                              all_omega_sq_2 = omega_sq_0_mean_085$Omega2[2],
                              all_omega_sq_ci_low_1 = omega_sq_0_mean_085$CI_low[1],
                              all_omega_sq_ci_low_2 = omega_sq_0_mean_085$CI_low[2],
                              all_omega_sq_ci_hi_1 = omega_sq_0_mean_085$CI_high[1],
                              all_omega_sq_ci_hi_2 = omega_sq_0_mean_085$CI_high[2],
                              all_omega_sq_interpret = omega_interpret_0_mean_085)

sp_0_sd_var <- plyr::rbind.fill(sp_0_sd_var, sp_0_sd_var_075, sp_0_sd_var_085)

params_1_mean <- parameters::model_parameters(model_1_mean)
params_1_mean_075 <- parameters::model_parameters(model_1_mean_075)
params_1_mean_085 <- parameters::model_parameters(model_1_mean_085)

omega_sq_1_mean <- omega_squared(model_1_mean, partial = FALSE, ci = 0.95)
omega_interpret_1_mean <- interpret_omega_squared(omega_sq_1_mean$Omega2, rules = "field2013")
omega_sq_1_mean_075 <- omega_squared(model_1_mean_075, partial = FALSE, ci = 0.95)
omega_interpret_1_mean_075 <- interpret_omega_squared(omega_sq_1_mean_075$Omega2, rules = "field2013")
omega_sq_1_mean_085 <- omega_squared(model_1_mean_085, partial = FALSE, ci = 0.95)
omega_interpret_1_mean_085 <- interpret_omega_squared(omega_sq_1_mean_085$Omega2, rules = "field2013")

sp_1_sd_var <- data.frame(species_id = 1,
                          mean = "all",
                          all_params_1 = params_1_mean$Parameter[1],
                          all_params_2 = params_1_mean$Parameter[2],
                          all_coeff_1 = params_1_mean$Coefficient[1],
                          all_coeff_2 = params_1_mean$Coefficient[2],
                          all_coeff_ci_low_1 = params_1_mean$CI_low[1],
                          all_coeff_ci_low_2 = params_1_mean$CI_low[2],
                          all_coeff_ci_hi_1 = params_1_mean$CI_high[1],
                          all_coeff_ci_hi_2 = params_1_mean$CI_high[2],
                          all_p_val_1 = params_1_mean$p[1],
                          all_p_val_2 = params_1_mean$p[2],
                          all_omega_sq_1 = omega_sq_1_mean$Omega2[1],
                          all_omega_sq_2 = omega_sq_1_mean$Omega2[2],
                          all_omega_sq_ci_low_1 = omega_sq_1_mean$CI_low[1],
                          all_omega_sq_ci_low_2 = omega_sq_1_mean$CI_low[2],
                          all_omega_sq_ci_hi_1 = omega_sq_1_mean$CI_high[1],
                          all_omega_sq_ci_hi_2 = omega_sq_1_mean$CI_high[2],
                          all_omega_sq_interpret = omega_interpret_1_mean)
sp_1_sd_var_075 <- data.frame(species_id = 1,
                              mean = "medium",
                              all_params_1 = params_1_mean_075$Parameter[1],
                              all_params_2 = params_1_mean_075$Parameter[2],
                              all_coeff_1 = params_1_mean_075$Coefficient[1],
                              all_coeff_2 = params_1_mean_075$Coefficient[2],
                              all_coeff_ci_low_1 = params_1_mean_075$CI_low[1],
                              all_coeff_ci_low_2 = params_1_mean_075$CI_low[2],
                              all_coeff_ci_hi_1 = params_1_mean_075$CI_high[1],
                              all_coeff_ci_hi_2 = params_1_mean_075$CI_high[2],
                              all_p_val_1 = params_1_mean_075$p[1],
                              all_p_val_2 = params_1_mean_075$p[2],
                              all_omega_sq_1 = omega_sq_1_mean_075$Omega2[1],
                              all_omega_sq_2 = omega_sq_1_mean_075$Omega2[2],
                              all_omega_sq_ci_low_1 = omega_sq_1_mean_075$CI_low[1],
                              all_omega_sq_ci_low_2 = omega_sq_1_mean_075$CI_low[2],
                              all_omega_sq_ci_hi_1 = omega_sq_1_mean_075$CI_high[1],
                              all_omega_sq_ci_hi_2 = omega_sq_1_mean_075$CI_high[2],
                              all_omega_sq_interpret = omega_interpret_1_mean_075)
sp_1_sd_var_085 <- data.frame(species_id = 1,
                              mean = "low",
                              all_params_1 = params_1_mean_085$Parameter[1],
                              all_params_2 = params_1_mean_085$Parameter[2],
                              all_coeff_1 = params_1_mean_085$Coefficient[1],
                              all_coeff_2 = params_1_mean_085$Coefficient[2],
                              all_coeff_ci_low_1 = params_1_mean_085$CI_low[1],
                              all_coeff_ci_low_2 = params_1_mean_085$CI_low[2],
                              all_coeff_ci_hi_1 = params_1_mean_085$CI_high[1],
                              all_coeff_ci_hi_2 = params_1_mean_085$CI_high[2],
                              all_p_val_1 = params_1_mean_085$p[1],
                              all_p_val_2 = params_1_mean_085$p[2],
                              all_omega_sq_1 = omega_sq_1_mean_085$Omega2[1],
                              all_omega_sq_2 = omega_sq_1_mean_085$Omega2[2],
                              all_omega_sq_ci_low_1 = omega_sq_1_mean_085$CI_low[1],
                              all_omega_sq_ci_low_2 = omega_sq_1_mean_085$CI_low[2],
                              all_omega_sq_ci_hi_1 = omega_sq_1_mean_085$CI_high[1],
                              all_omega_sq_ci_hi_2 = omega_sq_1_mean_085$CI_high[2],
                              all_omega_sq_interpret = omega_interpret_1_mean_085)

sp_1_sd_var <- plyr::rbind.fill(sp_1_sd_var, sp_1_sd_var_075, sp_1_sd_var_085)

params_2_mean <- parameters::model_parameters(model_2_mean)
params_2_mean_new <- parameters::model_parameters(model_2_mean_new)
params_2_mean_075 <- parameters::model_parameters(model_2_mean_075)
params_2_mean_075_new <- parameters::model_parameters(model_2_mean_075_new)
params_2_mean_085 <- parameters::model_parameters(model_2_mean_085)

omega_sq_2_mean <- omega_squared(model_2_mean, partial = FALSE, ci = 0.95)
omega_interpret_2_mean <- interpret_omega_squared(omega_sq_2_mean$Omega2, rules = "field2013")
omega_sq_2_mean_new <- omega_squared(model_2_mean_new, partial = FALSE, ci = 0.95)
omega_interpret_2_mean_new <- interpret_omega_squared(omega_sq_2_mean_new$Omega2, rules = "field2013")
omega_sq_2_mean_075 <- omega_squared(model_2_mean_075, partial = FALSE, ci = 0.95)
omega_interpret_2_mean_075 <- interpret_omega_squared(omega_sq_2_mean_075$Omega2, rules = "field2013")
omega_sq_2_mean_075_new <- omega_squared(params_2_mean_075_new, partial = FALSE, ci = 0.95)
omega_interpret_2_mean_075_new <- interpret_omega_squared(omega_sq_2_mean_075_new$Omega2, rules = "field2013")
omega_sq_2_mean_085 <- omega_squared(model_2_mean_085, partial = FALSE, ci = 0.95)
omega_interpret_2_mean_085 <- interpret_omega_squared(omega_sq_2_mean_085$Omega2, rules = "field2013")

sp_2_sd_var <- data.frame(species_id = 2,
                          mean = "all",
                          all_params_1 = params_2_mean$Parameter[1],
                          all_params_2 = params_2_mean$Parameter[2],
                          all_coeff_1 = params_2_mean$Coefficient[1],
                          all_coeff_2 = params_2_mean$Coefficient[2],
                          all_coeff_ci_low_1 = params_2_mean$CI_low[1],
                          all_coeff_ci_low_2 = params_2_mean$CI_low[2],
                          all_coeff_ci_hi_1 = params_2_mean$CI_high[1],
                          all_coeff_ci_hi_2 = params_2_mean$CI_high[2],
                          all_p_val_1 = params_2_mean$p[1],
                          all_p_val_2 = params_2_mean$p[2],
                          all_omega_sq_1 = omega_sq_2_mean$Omega2[1],
                          all_omega_sq_2 = omega_sq_2_mean$Omega2[2],
                          all_omega_sq_ci_low_1 = omega_sq_2_mean$CI_low[1],
                          all_omega_sq_ci_low_2 = omega_sq_2_mean$CI_low[2],
                          all_omega_sq_ci_hi_1 = omega_sq_2_mean$CI_high[1],
                          all_omega_sq_ci_hi_2 = omega_sq_2_mean$CI_high[2],
                          all_omega_sq_interpret = omega_interpret_2_mean)
sp_2_sd_var_075 <- data.frame(species_id = 2,
                              mean = "medium",
                              all_params_1 = params_2_mean_075$Parameter[1],
                              all_params_2 = params_2_mean_075$Parameter[2],
                              all_coeff_1 = params_2_mean_075$Coefficient[1],
                              all_coeff_2 = params_2_mean_075$Coefficient[2],
                              all_coeff_ci_low_1 = params_2_mean_075$CI_low[1],
                              all_coeff_ci_low_2 = params_2_mean_075$CI_low[2],
                              all_coeff_ci_hi_1 = params_2_mean_075$CI_high[1],
                              all_coeff_ci_hi_2 = params_2_mean_075$CI_high[2],
                              all_p_val_1 = params_2_mean_075$p[1],
                              all_p_val_2 = params_2_mean_075$p[2],
                              all_omega_sq_1 = omega_sq_2_mean_075$Omega2[1],
                              all_omega_sq_2 = omega_sq_2_mean_075$Omega2[2],
                              all_omega_sq_ci_low_1 = omega_sq_2_mean_075$CI_low[1],
                              all_omega_sq_ci_low_2 = omega_sq_2_mean_075$CI_low[2],
                              all_omega_sq_ci_hi_1 = omega_sq_2_mean_075$CI_high[1],
                              all_omega_sq_ci_hi_2 = omega_sq_2_mean_075$CI_high[2],
                              all_omega_sq_interpret = omega_interpret_2_mean_075)
sp_2_sd_var_085 <- data.frame(species_id = 2,
                              mean = "low",
                              all_params_1 = params_2_mean_085$Parameter[1],
                              all_params_2 = params_2_mean_085$Parameter[2],
                              all_coeff_1 = params_2_mean_085$Coefficient[1],
                              all_coeff_2 = params_2_mean_085$Coefficient[2],
                              all_coeff_ci_low_1 = params_2_mean_085$CI_low[1],
                              all_coeff_ci_low_2 = params_2_mean_085$CI_low[2],
                              all_coeff_ci_hi_1 = params_2_mean_085$CI_high[1],
                              all_coeff_ci_hi_2 = params_2_mean_085$CI_high[2],
                              all_p_val_1 = params_2_mean_085$p[1],
                              all_p_val_2 = params_2_mean_085$p[2],
                              all_omega_sq_1 = omega_sq_2_mean_085$Omega2[1],
                              all_omega_sq_2 = omega_sq_2_mean_085$Omega2[2],
                              all_omega_sq_ci_low_1 = omega_sq_2_mean_085$CI_low[1],
                              all_omega_sq_ci_low_2 = omega_sq_2_mean_085$CI_low[2],
                              all_omega_sq_ci_hi_1 = omega_sq_2_mean_085$CI_high[1],
                              all_omega_sq_ci_hi_2 = omega_sq_2_mean_085$CI_high[2],
                              all_omega_sq_interpret = omega_interpret_2_mean_085)

sp_2_sd_var <- plyr::rbind.fill(sp_2_sd_var, sp_2_sd_var_075, sp_2_sd_var_085)

params_3_mean <- parameters::model_parameters(model_3_mean)
params_3_mean_075 <- parameters::model_parameters(model_3_mean_075)
params_3_mean_085 <- parameters::model_parameters(model_3_mean_085)

omega_sq_3_mean <- omega_squared(model_3_mean, partial = FALSE, ci = 0.95)
omega_interpret_3_mean <- interpret_omega_squared(omega_sq_3_mean$Omega2, rules = "field2013")
omega_sq_3_mean_075 <- omega_squared(model_3_mean_075, partial = FALSE, ci = 0.95)
omega_interpret_3_mean_075 <- interpret_omega_squared(omega_sq_3_mean_075$Omega2, rules = "field2013")
omega_sq_3_mean_085 <- omega_squared(model_3_mean_085, partial = FALSE, ci = 0.95)
omega_interpret_3_mean_085 <- interpret_omega_squared(omega_sq_3_mean_085$Omega2, rules = "field2013")

sp_3_sd_var <- data.frame(species_id = 3,
                          mean = "all",
                          all_params_1 = params_3_mean$Parameter[1],
                          all_params_2 = params_3_mean$Parameter[2],
                          all_coeff_1 = params_3_mean$Coefficient[1],
                          all_coeff_2 = params_3_mean$Coefficient[2],
                          all_coeff_ci_low_1 = params_3_mean$CI_low[1],
                          all_coeff_ci_low_2 = params_3_mean$CI_low[2],
                          all_coeff_ci_hi_1 = params_3_mean$CI_high[1],
                          all_coeff_ci_hi_2 = params_3_mean$CI_high[2],
                          all_p_val_1 = params_3_mean$p[1],
                          all_p_val_2 = params_3_mean$p[2],
                          all_omega_sq_1 = omega_sq_3_mean$Omega2[1],
                          all_omega_sq_2 = omega_sq_3_mean$Omega2[2],
                          all_omega_sq_ci_low_1 = omega_sq_3_mean$CI_low[1],
                          all_omega_sq_ci_low_2 = omega_sq_3_mean$CI_low[2],
                          all_omega_sq_ci_hi_1 = omega_sq_3_mean$CI_high[1],
                          all_omega_sq_ci_hi_2 = omega_sq_3_mean$CI_high[2],
                          all_omega_sq_interpret = omega_interpret_3_mean)
sp_3_sd_var_075 <- data.frame(species_id = 3,
                              mean = "medium",
                              all_params_1 = params_3_mean_075$Parameter[1],
                              all_params_2 = params_3_mean_075$Parameter[2],
                              all_coeff_1 = params_3_mean_075$Coefficient[1],
                              all_coeff_2 = params_3_mean_075$Coefficient[2],
                              all_coeff_ci_low_1 = params_3_mean_075$CI_low[1],
                              all_coeff_ci_low_2 = params_3_mean_075$CI_low[2],
                              all_coeff_ci_hi_1 = params_3_mean_075$CI_high[1],
                              all_coeff_ci_hi_2 = params_3_mean_075$CI_high[2],
                              all_p_val_1 = params_3_mean_075$p[1],
                              all_p_val_2 = params_3_mean_075$p[2],
                              all_omega_sq_1 = omega_sq_3_mean_075$Omega2[1],
                              all_omega_sq_2 = omega_sq_3_mean_075$Omega2[2],
                              all_omega_sq_ci_low_1 = omega_sq_3_mean_075$CI_low[1],
                              all_omega_sq_ci_low_2 = omega_sq_3_mean_075$CI_low[2],
                              all_omega_sq_ci_hi_1 = omega_sq_3_mean_075$CI_high[1],
                              all_omega_sq_ci_hi_2 = omega_sq_3_mean_075$CI_high[2],
                              all_omega_sq_interpret = omega_interpret_3_mean_075)
sp_3_sd_var_085 <- data.frame(species_id = 3,
                              mean = "low",
                              all_params_1 = params_3_mean_085$Parameter[1],
                              all_params_2 = params_3_mean_085$Parameter[2],
                              all_coeff_1 = params_3_mean_085$Coefficient[1],
                              all_coeff_2 = params_3_mean_085$Coefficient[2],
                              all_coeff_ci_low_1 = params_3_mean_085$CI_low[1],
                              all_coeff_ci_low_2 = params_3_mean_085$CI_low[2],
                              all_coeff_ci_hi_1 = params_3_mean_085$CI_high[1],
                              all_coeff_ci_hi_2 = params_3_mean_085$CI_high[2],
                              all_p_val_1 = params_3_mean_085$p[1],
                              all_p_val_2 = params_3_mean_085$p[2],
                              all_omega_sq_1 = omega_sq_3_mean_085$Omega2[1],
                              all_omega_sq_2 = omega_sq_3_mean_085$Omega2[2],
                              all_omega_sq_ci_low_1 = omega_sq_3_mean_085$CI_low[1],
                              all_omega_sq_ci_low_2 = omega_sq_3_mean_085$CI_low[2],
                              all_omega_sq_ci_hi_1 = omega_sq_3_mean_085$CI_high[1],
                              all_omega_sq_ci_hi_2 = omega_sq_3_mean_085$CI_high[2],
                              all_omega_sq_interpret = omega_interpret_3_mean_085)

sp_3_sd_var <- plyr::rbind.fill(sp_3_sd_var, sp_3_sd_var_075, sp_3_sd_var_085)

params_4_mean <- parameters::model_parameters(model_4_mean)
params_4_mean_075 <- parameters::model_parameters(model_4_mean_075)
params_4_mean_085 <- parameters::model_parameters(model_4_mean_085)

omega_sq_4_mean <- omega_squared(model_4_mean, partial = FALSE, ci = 0.95)
omega_interpret_4_mean <- interpret_omega_squared(omega_sq_4_mean$Omega2, rules = "field2013")
omega_sq_4_mean_075 <- omega_squared(model_4_mean_075, partial = FALSE, ci = 0.95)
omega_interpret_4_mean_075 <- interpret_omega_squared(omega_sq_4_mean_075$Omega2, rules = "field2013")
omega_sq_4_mean_085 <- omega_squared(model_4_mean_085, partial = FALSE, ci = 0.95)
omega_interpret_4_mean_085 <- interpret_omega_squared(omega_sq_4_mean_085$Omega2, rules = "field2013")

sp_4_sd_var <- data.frame(species_id = 4,
                          mean = "all",
                          all_params_1 = params_4_mean$Parameter[1],
                          all_params_2 = params_4_mean$Parameter[2],
                          all_coeff_1 = params_4_mean$Coefficient[1],
                          all_coeff_2 = params_4_mean$Coefficient[2],
                          all_coeff_ci_low_1 = params_4_mean$CI_low[1],
                          all_coeff_ci_low_2 = params_4_mean$CI_low[2],
                          all_coeff_ci_hi_1 = params_4_mean$CI_high[1],
                          all_coeff_ci_hi_2 = params_4_mean$CI_high[2],
                          all_p_val_1 = params_4_mean$p[1],
                          all_p_val_2 = params_4_mean$p[2],
                          all_omega_sq_1 = omega_sq_4_mean$Omega2[1],
                          all_omega_sq_2 = omega_sq_4_mean$Omega2[2],
                          all_omega_sq_ci_low_1 = omega_sq_4_mean$CI_low[1],
                          all_omega_sq_ci_low_2 = omega_sq_4_mean$CI_low[2],
                          all_omega_sq_ci_hi_1 = omega_sq_4_mean$CI_high[1],
                          all_omega_sq_ci_hi_2 = omega_sq_4_mean$CI_high[2],
                          all_omega_sq_interpret = omega_interpret_4_mean)
sp_4_sd_var_075 <- data.frame(species_id = 4,
                              mean = "medium",
                              all_params_1 = params_4_mean_075$Parameter[1],
                              all_params_2 = params_4_mean_075$Parameter[2],
                              all_coeff_1 = params_4_mean_075$Coefficient[1],
                              all_coeff_2 = params_4_mean_075$Coefficient[2],
                              all_coeff_ci_low_1 = params_4_mean_075$CI_low[1],
                              all_coeff_ci_low_2 = params_4_mean_075$CI_low[2],
                              all_coeff_ci_hi_1 = params_4_mean_075$CI_high[1],
                              all_coeff_ci_hi_2 = params_4_mean_075$CI_high[2],
                              all_p_val_1 = params_4_mean_075$p[1],
                              all_p_val_2 = params_4_mean_075$p[2],
                              all_omega_sq_1 = omega_sq_4_mean_075$Omega2[1],
                              all_omega_sq_2 = omega_sq_4_mean_075$Omega2[2],
                              all_omega_sq_ci_low_1 = omega_sq_4_mean_075$CI_low[1],
                              all_omega_sq_ci_low_2 = omega_sq_4_mean_075$CI_low[2],
                              all_omega_sq_ci_hi_1 = omega_sq_4_mean_075$CI_high[1],
                              all_omega_sq_ci_hi_2 = omega_sq_4_mean_075$CI_high[2],
                              all_omega_sq_interpret = omega_interpret_4_mean_075)
sp_4_sd_var_085 <- data.frame(species_id = 4,
                              mean = "low",
                              all_params_1 = params_4_mean_085$Parameter[1],
                              all_params_2 = params_4_mean_085$Parameter[2],
                              all_coeff_1 = params_4_mean_085$Coefficient[1],
                              all_coeff_2 = params_4_mean_085$Coefficient[2],
                              all_coeff_ci_low_1 = params_4_mean_085$CI_low[1],
                              all_coeff_ci_low_2 = params_4_mean_085$CI_low[2],
                              all_coeff_ci_hi_1 = params_4_mean_085$CI_high[1],
                              all_coeff_ci_hi_2 = params_4_mean_085$CI_high[2],
                              all_p_val_1 = params_4_mean_085$p[1],
                              all_p_val_2 = params_4_mean_085$p[2],
                              all_omega_sq_1 = omega_sq_4_mean_085$Omega2[1],
                              all_omega_sq_2 = omega_sq_4_mean_085$Omega2[2],
                              all_omega_sq_ci_low_1 = omega_sq_4_mean_085$CI_low[1],
                              all_omega_sq_ci_low_2 = omega_sq_4_mean_085$CI_low[2],
                              all_omega_sq_ci_hi_1 = omega_sq_4_mean_085$CI_high[1],
                              all_omega_sq_ci_hi_2 = omega_sq_4_mean_085$CI_high[2],
                              all_omega_sq_interpret = omega_interpret_4_mean_085)

sp_4_sd_var <- plyr::rbind.fill(sp_4_sd_var, sp_4_sd_var_075, sp_4_sd_var_085)

basal_area_log_sd_var <- plyr::rbind.fill(sp_1_sd_var, sp_2_sd_var, sp_3_sd_var, sp_4_sd_var)
write.csv(basal_area_log_sd_var, file = "out/basal_area_log_sd_var_stats.csv")

## THIS PART IS THE EXAMINATION OF ENVIRONMENT MEAN 

params_0_sd <- parameters::model_parameters((model_0_sd))
params_0_sd_0 <- parameters::model_parameters((model_0_sd_0))
params_0_sd_15 <- parameters::model_parameters((model_0_sd_15))
params_0_sd_30 <- parameters::model_parameters((model_0_sd_30))
params_0_sd_60 <- parameters::model_parameters((model_0_sd_60))

omega_sq_0_sd <- omega_squared(aov(model_0_sd), partial = FALSE, ci = 0.95)
omega_interpret_0_sd <- interpret_omega_squared(omega_sq_0_sd$Omega2, rules = "field2013")
omega_sq_0_sd_0 <- omega_squared(aov(model_0_sd_0), partial = FALSE, ci = 0.95)
omega_interpret_0_sd_0 <- interpret_omega_squared(omega_sq_0_sd_0$Omega2, rules = "field2013")
omega_sq_0_sd_15 <- omega_squared(aov(model_0_sd_15), partial = FALSE, ci = 0.95)
omega_interpret_0_sd_15 <- interpret_omega_squared(omega_sq_0_sd_15$Omega2, rules = "field2013")
omega_sq_0_sd_30 <- omega_squared(aov(model_0_sd_30), partial = FALSE, ci = 0.95)
omega_interpret_0_sd_30 <- interpret_omega_squared(omega_sq_0_sd_30$Omega2, rules = "field2013")
omega_sq_0_sd_60 <- omega_squared(aov(model_0_sd_60), partial = FALSE, ci = 0.95)
omega_interpret_0_sd_60 <- interpret_omega_squared(omega_sq_0_sd_60$Omega2, rules = "field2013")

sp_0_mean_var <- data.frame(species_id = 0,
                          sd = "all",
                          all_params_1 = params_0_sd$Parameter[1],
                          all_params_2 = params_0_sd$Parameter[2],
                          all_coeff_1 = params_0_sd$Coefficient[1],
                          all_coeff_2 = params_0_sd$Coefficient[2],
                          all_coeff_ci_low_1 = params_0_sd$CI_low[1],
                          all_coeff_ci_low_2 = params_0_sd$CI_low[2],
                          all_coeff_ci_hi_1 = params_0_sd$CI_high[1],
                          all_coeff_ci_hi_2 = params_0_sd$CI_high[2],
                          all_p_val_1 = params_0_sd$p[1],
                          all_p_val_2 = params_0_sd$p[2],
                          all_omega_sq_1 = omega_sq_0_sd$Omega2[1],
                          all_omega_sq_2 = omega_sq_0_sd$Omega2[2],
                          all_omega_sq_ci_low_1 = omega_sq_0_sd$CI_low[1],
                          all_omega_sq_ci_low_2 = omega_sq_0_sd$CI_low[2],
                          all_omega_sq_ci_hi_1 = omega_sq_0_sd$CI_high[1],
                          all_omega_sq_ci_hi_2 = omega_sq_0_sd$CI_high[2],
                          all_omega_sq_interpret = omega_interpret_0_sd)
sp_0_mean_var_0 <- data.frame(species_id = 0,
                              sd = "none",
                              all_params_1 = params_0_sd_0$Parameter[1],
                              all_params_2 = params_0_sd_0$Parameter[2],
                              all_coeff_1 = params_0_sd_0$Coefficient[1],
                              all_coeff_2 = params_0_sd_0$Coefficient[2],
                              all_coeff_ci_low_1 = params_0_sd_0$CI_low[1],
                              all_coeff_ci_low_2 = params_0_sd_0$CI_low[2],
                              all_coeff_ci_hi_1 = params_0_sd_0$CI_high[1],
                              all_coeff_ci_hi_2 = params_0_sd_0$CI_high[2],
                              all_p_val_1 = params_0_sd_0$p[1],
                              all_p_val_2 = params_0_sd_0$p[2],
                              all_omega_sq_1 = omega_sq_0_sd_0$Omega2[1],
                              all_omega_sq_2 = omega_sq_0_sd_0$Omega2[2],
                              all_omega_sq_ci_low_1 = omega_sq_0_sd_0$CI_low[1],
                              all_omega_sq_ci_low_2 = omega_sq_0_sd_0$CI_low[2],
                              all_omega_sq_ci_hi_1 = omega_sq_0_sd_0$CI_high[1],
                              all_omega_sq_ci_hi_2 = omega_sq_0_sd_0$CI_high[2],
                              all_omega_sq_interpret = omega_interpret_0_sd_0)
sp_0_mean_var_15 <- data.frame(species_id = 0,
                              sd = "low",
                              all_params_1 = params_0_sd_15$Parameter[1],
                              all_p_val_1 = params_0_sd_15$p[1],
                              all_omega_sq_1 = omega_sq_0_sd_15$Omega2[1],
                              all_omega_sq_ci_low_1 = omega_sq_0_sd_15$CI_low[1],
                              all_omega_sq_ci_hi_1 = omega_sq_0_sd_15$CI_high[1],
                              all_omega_sq_interpret = omega_interpret_0_sd_15)
sp_0_mean_var_30 <- data.frame(species_id = 0,
                               sd = "medium",
                               all_params_1 = params_0_sd_30$Parameter[1],
                               all_p_val_1 = params_0_sd_30$p[1],
                               all_omega_sq_1 = omega_sq_0_sd_30$Omega2[1],
                               all_omega_sq_ci_low_1 = omega_sq_0_sd_30$CI_low[1],
                               all_omega_sq_ci_hi_1 = omega_sq_0_sd_30$CI_high[1],
                               all_omega_sq_interpret = omega_interpret_0_sd_30)
sp_0_mean_var_60 <- data.frame(species_id = 0,
                               sd = "high",
                               all_params_1 = params_0_sd_60$Parameter[1],
                               all_p_val_1 = params_0_sd_60$p[1],
                               all_omega_sq_1 = omega_sq_0_sd_60$Omega2[1],
                               all_omega_sq_ci_low_1 = omega_sq_0_sd_60$CI_low[1],
                               all_omega_sq_ci_hi_1 = omega_sq_0_sd_60$CI_high[1],
                               all_omega_sq_interpret = omega_interpret_0_sd_60)

sp_0_mean_var <- plyr::rbind.fill(sp_0_mean_var, sp_0_mean_var_0, sp_0_mean_var_15, sp_0_mean_var_30, sp_0_mean_var_60)

params_1_sd <- parameters::model_parameters((model_1_sd))
params_1_sd_0 <- parameters::model_parameters((model_1_sd_0))
params_1_sd_15 <- parameters::model_parameters((model_1_sd_15))
params_1_sd_30 <- parameters::model_parameters((model_1_sd_30))
params_1_sd_60 <- parameters::model_parameters((model_1_sd_60))

omega_sq_1_sd <- omega_squared((model_1_sd), partial = FALSE, ci = 0.95)
omega_interpret_1_sd <- interpret_omega_squared(omega_sq_1_sd$Omega2, rules = "field2013")
omega_sq_1_sd_0 <- omega_squared((model_1_sd_0), partial = FALSE, ci = 0.95)
omega_interpret_1_sd_0 <- interpret_omega_squared(omega_sq_1_sd_0$Omega2, rules = "field2013")
omega_sq_1_sd_15 <- omega_squared((model_1_sd_15), partial = FALSE, ci = 0.95)
omega_interpret_1_sd_15 <- interpret_omega_squared(omega_sq_1_sd_15$Omega2, rules = "field2013")
omega_sq_1_sd_30 <- omega_squared((model_1_sd_30), partial = FALSE, ci = 0.95)
omega_interpret_1_sd_30 <- interpret_omega_squared(omega_sq_1_sd_30$Omega2, rules = "field2013")
omega_sq_1_sd_60 <- omega_squared((model_1_sd_60), partial = FALSE, ci = 0.95)
omega_interpret_1_sd_60 <- interpret_omega_squared(omega_sq_1_sd_60$Omega2, rules = "field2013")

sp_1_mean_var <- data.frame(species_id = 1,
                            sd = "all",
                            all_params_1 = params_1_sd$Parameter[1],
                            all_params_2 = params_1_sd$Parameter[2],
                            all_coeff_1 = params_1_sd$Coefficient[1],
                            all_coeff_2 = params_1_sd$Coefficient[2],
                            all_coeff_ci_low_1 = params_1_sd$CI_low[1],
                            all_coeff_ci_low_2 = params_1_sd$CI_low[2],
                            all_coeff_ci_hi_1 = params_1_sd$CI_high[1],
                            all_coeff_ci_hi_2 = params_1_sd$CI_high[2],
                            all_p_val_1 = params_1_sd$p[1],
                            all_p_val_2 = params_1_sd$p[2],
                            all_omega_sq_1 = omega_sq_1_sd$Omega2[1],
                            all_omega_sq_2 = omega_sq_1_sd$Omega2[2],
                            all_omega_sq_ci_low_1 = omega_sq_1_sd$CI_low[1],
                            all_omega_sq_ci_low_2 = omega_sq_1_sd$CI_low[2],
                            all_omega_sq_ci_hi_1 = omega_sq_1_sd$CI_high[1],
                            all_omega_sq_ci_hi_2 = omega_sq_1_sd$CI_high[2],
                            all_omega_sq_interpret = omega_interpret_1_sd)
sp_1_mean_var_0 <- data.frame(species_id = 1,
                              sd = "none",
                              all_params_1 = params_1_sd_0$Parameter[1],
                              all_params_2 = params_1_sd_0$Parameter[2],
                              all_coeff_1 = params_1_sd_0$Coefficient[1],
                              all_coeff_2 = params_1_sd_0$Coefficient[2],
                              all_coeff_ci_low_1 = params_1_sd_0$CI_low[1],
                              all_coeff_ci_low_2 = params_1_sd_0$CI_low[2],
                              all_coeff_ci_hi_1 = params_1_sd_0$CI_high[1],
                              all_coeff_ci_hi_2 = params_1_sd_0$CI_high[2],
                              all_p_val_1 = params_1_sd_0$p[1],
                              all_p_val_2 = params_1_sd_0$p[2],
                              all_omega_sq_1 = omega_sq_1_sd_0$Omega2[1],
                              all_omega_sq_2 = omega_sq_1_sd_0$Omega2[2],
                              all_omega_sq_ci_low_1 = omega_sq_1_sd_0$CI_low[1],
                              all_omega_sq_ci_low_2 = omega_sq_1_sd_0$CI_low[2],
                              all_omega_sq_ci_hi_1 = omega_sq_1_sd_0$CI_high[1],
                              all_omega_sq_ci_hi_2 = omega_sq_1_sd_0$CI_high[2],
                              all_omega_sq_interpret = omega_interpret_1_sd_0)
sp_1_mean_var_15 <- data.frame(species_id = 1,
                               sd = "low",
                               all_params_1 = params_1_sd_15$Parameter[1],
                               all_params_2 = params_1_sd_15$Parameter[2],
                               all_coeff_1 = params_1_sd_15$Coefficient[1],
                               all_coeff_2 = params_1_sd_15$Coefficient[2],
                               all_coeff_ci_low_1 = params_1_sd_15$CI_low[1],
                               all_coeff_ci_low_2 = params_1_sd_15$CI_low[2],
                               all_coeff_ci_hi_1 = params_1_sd_15$CI_high[1],
                               all_coeff_ci_hi_2 = params_1_sd_15$CI_high[2],
                               all_p_val_1 = params_1_sd_15$p[1],
                               all_p_val_2 = params_1_sd_15$p[2],
                               all_omega_sq_1 = omega_sq_1_sd_15$Omega2[1],
                               all_omega_sq_2 = omega_sq_1_sd_15$Omega2[2],
                               all_omega_sq_ci_low_1 = omega_sq_1_sd_15$CI_low[1],
                               all_omega_sq_ci_low_2 = omega_sq_1_sd_15$CI_low[2],
                               all_omega_sq_ci_hi_1 = omega_sq_1_sd_15$CI_high[1],
                               all_omega_sq_ci_hi_2 = omega_sq_1_sd_15$CI_high[2],
                               all_omega_sq_interpret = omega_interpret_1_sd_15)
sp_1_mean_var_30 <- data.frame(species_id = 1,
                               sd = "medium",
                               all_params_1 = params_1_sd_30$Parameter[1],
                               all_params_2 = params_1_sd_30$Parameter[2],
                               all_coeff_1 = params_1_sd_30$Coefficient[1],
                               all_coeff_2 = params_1_sd_30$Coefficient[2],
                               all_coeff_ci_low_1 = params_1_sd_30$CI_low[1],
                               all_coeff_ci_low_2 = params_1_sd_30$CI_low[2],
                               all_coeff_ci_hi_1 = params_1_sd_30$CI_high[1],
                               all_coeff_ci_hi_2 = params_1_sd_30$CI_high[2],
                               all_p_val_1 = params_1_sd_30$p[1],
                               all_p_val_2 = params_1_sd_30$p[2],
                               all_omega_sq_1 = omega_sq_1_sd_30$Omega2[1],
                               all_omega_sq_2 = omega_sq_1_sd_30$Omega2[2],
                               all_omega_sq_ci_low_1 = omega_sq_1_sd_30$CI_low[1],
                               all_omega_sq_ci_low_2 = omega_sq_1_sd_30$CI_low[2],
                               all_omega_sq_ci_hi_1 = omega_sq_1_sd_30$CI_high[1],
                               all_omega_sq_ci_hi_2 = omega_sq_1_sd_30$CI_high[2],
                               all_omega_sq_interpret = omega_interpret_1_sd_30)
sp_1_mean_var_60 <- data.frame(species_id = 1,
                               sd = "high",
                               all_params_1 = params_1_sd_60$Parameter[1],
                               all_params_2 = params_1_sd_60$Parameter[2],
                               all_coeff_1 = params_1_sd_60$Coefficient[1],
                               all_coeff_2 = params_1_sd_60$Coefficient[2],
                               all_coeff_ci_low_1 = params_1_sd_60$CI_low[1],
                               all_coeff_ci_low_2 = params_1_sd_60$CI_low[2],
                               all_coeff_ci_hi_1 = params_1_sd_60$CI_high[1],
                               all_coeff_ci_hi_2 = params_1_sd_60$CI_high[2],
                               all_p_val_1 = params_1_sd_60$p[1],
                               all_p_val_2 = params_1_sd_60$p[2],
                               all_omega_sq_1 = omega_sq_1_sd_60$Omega2[1],
                               all_omega_sq_2 = omega_sq_1_sd_60$Omega2[2],
                               all_omega_sq_ci_low_1 = omega_sq_1_sd_60$CI_low[1],
                               all_omega_sq_ci_low_2 = omega_sq_1_sd_60$CI_low[2],
                               all_omega_sq_ci_hi_1 = omega_sq_1_sd_60$CI_high[1],
                               all_omega_sq_ci_hi_2 = omega_sq_1_sd_60$CI_high[2],
                               all_omega_sq_interpret = omega_interpret_1_sd_60)

sp_1_mean_var <- plyr::rbind.fill(sp_1_mean_var, sp_1_mean_var_0, sp_1_mean_var_15, sp_1_mean_var_30, sp_1_mean_var_60)

params_2_sd <- parameters::model_parameters((model_2_sd))
params_2_sd_0 <- parameters::model_parameters((model_2_sd_0))
params_2_sd_15 <- parameters::model_parameters((model_2_sd_15))
params_2_sd_30 <- parameters::model_parameters((model_2_sd_30))
params_2_sd_60 <- parameters::model_parameters((model_2_sd_60))

omega_sq_2_sd <- omega_squared((model_2_sd), partial = FALSE, ci = 0.95)
omega_interpret_2_sd <- interpret_omega_squared(omega_sq_2_sd$Omega2, rules = "field2013")
omega_sq_2_sd_0 <- omega_squared((model_2_sd_0), partial = FALSE, ci = 0.95)
omega_interpret_2_sd_0 <- interpret_omega_squared(omega_sq_2_sd_0$Omega2, rules = "field2013")
omega_sq_2_sd_15 <- omega_squared((model_2_sd_15), partial = FALSE, ci = 0.95)
omega_interpret_2_sd_15 <- interpret_omega_squared(omega_sq_2_sd_15$Omega2, rules = "field2013")
omega_sq_2_sd_30 <- omega_squared((model_2_sd_30), partial = FALSE, ci = 0.95)
omega_interpret_2_sd_30 <- interpret_omega_squared(omega_sq_2_sd_30$Omega2, rules = "field2013")
omega_sq_2_sd_60 <- omega_squared((model_2_sd_60), partial = FALSE, ci = 0.95)
omega_interpret_2_sd_60 <- interpret_omega_squared(omega_sq_2_sd_60$Omega2, rules = "field2013")

sp_2_mean_var <- data.frame(species_id = 2,
                            sd = "all",
                            all_params_1 = params_2_sd$Parameter[1],
                            all_params_2 = params_2_sd$Parameter[2],
                            all_coeff_1 = params_2_sd$Coefficient[1],
                            all_coeff_2 = params_2_sd$Coefficient[2],
                            all_coeff_ci_low_1 = params_2_sd$CI_low[1],
                            all_coeff_ci_low_2 = params_2_sd$CI_low[2],
                            all_coeff_ci_hi_1 = params_2_sd$CI_high[1],
                            all_coeff_ci_hi_2 = params_2_sd$CI_high[2],
                            all_p_val_1 = params_2_sd$p[1],
                            all_p_val_2 = params_2_sd$p[2],
                            all_omega_sq_1 = omega_sq_2_sd$Omega2[1],
                            all_omega_sq_2 = omega_sq_2_sd$Omega2[2],
                            all_omega_sq_ci_low_1 = omega_sq_2_sd$CI_low[1],
                            all_omega_sq_ci_low_2 = omega_sq_2_sd$CI_low[2],
                            all_omega_sq_ci_hi_1 = omega_sq_2_sd$CI_high[1],
                            all_omega_sq_ci_hi_2 = omega_sq_2_sd$CI_high[2],
                            all_omega_sq_interpret = omega_interpret_2_sd)
sp_2_mean_var_0 <- data.frame(species_id = 2,
                              sd = "none",
                              all_params_1 = params_2_sd_0$Parameter[1],
                              all_params_2 = params_2_sd_0$Parameter[2],
                              all_coeff_1 = params_2_sd_0$Coefficient[1],
                              all_coeff_2 = params_2_sd_0$Coefficient[2],
                              all_coeff_ci_low_1 = params_2_sd_0$CI_low[1],
                              all_coeff_ci_low_2 = params_2_sd_0$CI_low[2],
                              all_coeff_ci_hi_1 = params_2_sd_0$CI_high[1],
                              all_coeff_ci_hi_2 = params_2_sd_0$CI_high[2],
                              all_p_val_1 = params_2_sd_0$p[1],
                              all_p_val_2 = params_2_sd_0$p[2],
                              all_omega_sq_1 = omega_sq_2_sd_0$Omega2[1],
                              all_omega_sq_2 = omega_sq_2_sd_0$Omega2[2],
                              all_omega_sq_ci_low_1 = omega_sq_2_sd_0$CI_low[1],
                              all_omega_sq_ci_low_2 = omega_sq_2_sd_0$CI_low[2],
                              all_omega_sq_ci_hi_1 = omega_sq_2_sd_0$CI_high[1],
                              all_omega_sq_ci_hi_2 = omega_sq_2_sd_0$CI_high[2],
                              all_omega_sq_interpret = omega_interpret_2_sd_0)
sp_2_mean_var_15 <- data.frame(species_id = 2,
                               sd = "low",
                               all_params_1 = params_2_sd_15$Parameter[1],
                               all_params_2 = params_2_sd_15$Parameter[2],
                               all_coeff_1 = params_2_sd_15$Coefficient[1],
                               all_coeff_2 = params_2_sd_15$Coefficient[2],
                               all_coeff_ci_low_1 = params_2_sd_15$CI_low[1],
                               all_coeff_ci_low_2 = params_2_sd_15$CI_low[2],
                               all_coeff_ci_hi_1 = params_2_sd_15$CI_high[1],
                               all_coeff_ci_hi_2 = params_2_sd_15$CI_high[2],
                               all_p_val_1 = params_2_sd_15$p[1],
                               all_p_val_2 = params_2_sd_15$p[2],
                               all_omega_sq_1 = omega_sq_2_sd_15$Omega2[1],
                               all_omega_sq_2 = omega_sq_2_sd_15$Omega2[2],
                               all_omega_sq_ci_low_1 = omega_sq_2_sd_15$CI_low[1],
                               all_omega_sq_ci_low_2 = omega_sq_2_sd_15$CI_low[2],
                               all_omega_sq_ci_hi_1 = omega_sq_2_sd_15$CI_high[1],
                               all_omega_sq_ci_hi_2 = omega_sq_2_sd_15$CI_high[2],
                               all_omega_sq_interpret = omega_interpret_2_sd_15)
sp_2_mean_var_30 <- data.frame(species_id = 2,
                               sd = "medium",
                               all_params_1 = params_2_sd_30$Parameter[1],
                               all_params_2 = params_2_sd_30$Parameter[2],
                               all_coeff_1 = params_2_sd_30$Coefficient[1],
                               all_coeff_2 = params_2_sd_30$Coefficient[2],
                               all_coeff_ci_low_1 = params_2_sd_30$CI_low[1],
                               all_coeff_ci_low_2 = params_2_sd_30$CI_low[2],
                               all_coeff_ci_hi_1 = params_2_sd_30$CI_high[1],
                               all_coeff_ci_hi_2 = params_2_sd_30$CI_high[2],
                               all_p_val_1 = params_2_sd_30$p[1],
                               all_p_val_2 = params_2_sd_30$p[2],
                               all_omega_sq_1 = omega_sq_2_sd_30$Omega2[1],
                               all_omega_sq_2 = omega_sq_2_sd_30$Omega2[2],
                               all_omega_sq_ci_low_1 = omega_sq_2_sd_30$CI_low[1],
                               all_omega_sq_ci_low_2 = omega_sq_2_sd_30$CI_low[2],
                               all_omega_sq_ci_hi_1 = omega_sq_2_sd_30$CI_high[1],
                               all_omega_sq_ci_hi_2 = omega_sq_2_sd_30$CI_high[2],
                               all_omega_sq_interpret = omega_interpret_2_sd_30)
sp_2_mean_var_60 <- data.frame(species_id = 2,
                               sd = "high",
                               all_params_1 = params_2_sd_60$Parameter[1],
                               all_params_2 = params_2_sd_60$Parameter[2],
                               all_coeff_1 = params_2_sd_60$Coefficient[1],
                               all_coeff_2 = params_2_sd_60$Coefficient[2],
                               all_coeff_ci_low_1 = params_2_sd_60$CI_low[1],
                               all_coeff_ci_low_2 = params_2_sd_60$CI_low[2],
                               all_coeff_ci_hi_1 = params_2_sd_60$CI_high[1],
                               all_coeff_ci_hi_2 = params_2_sd_60$CI_high[2],
                               all_p_val_1 = params_2_sd_60$p[1],
                               all_p_val_2 = params_2_sd_60$p[2],
                               all_omega_sq_1 = omega_sq_2_sd_60$Omega2[1],
                               all_omega_sq_2 = omega_sq_2_sd_60$Omega2[2],
                               all_omega_sq_ci_low_1 = omega_sq_2_sd_60$CI_low[1],
                               all_omega_sq_ci_low_2 = omega_sq_2_sd_60$CI_low[2],
                               all_omega_sq_ci_hi_1 = omega_sq_2_sd_60$CI_high[1],
                               all_omega_sq_ci_hi_2 = omega_sq_2_sd_60$CI_high[2],
                               all_omega_sq_interpret = omega_interpret_2_sd_60)

sp_2_mean_var <- plyr::rbind.fill(sp_2_mean_var, sp_2_mean_var_0, sp_2_mean_var_15, sp_2_mean_var_30, sp_2_mean_var_60)

params_3_sd <- parameters::model_parameters((model_3_sd))
params_3_sd_0 <- parameters::model_parameters((model_3_sd_0))
params_3_sd_15 <- parameters::model_parameters((model_3_sd_15))
params_3_sd_30 <- parameters::model_parameters((model_3_sd_30))
params_3_sd_60 <- parameters::model_parameters((model_3_sd_60))

omega_sq_3_sd <- omega_squared((model_3_sd), partial = FALSE, ci = 0.95)
omega_interpret_3_sd <- interpret_omega_squared(omega_sq_3_sd$Omega2, rules = "field2013")
omega_sq_3_sd_0 <- omega_squared((model_3_sd_0), partial = FALSE, ci = 0.95)
omega_interpret_3_sd_0 <- interpret_omega_squared(omega_sq_3_sd_0$Omega2, rules = "field2013")
omega_sq_3_sd_15 <- omega_squared((model_3_sd_15), partial = FALSE, ci = 0.95)
omega_interpret_3_sd_15 <- interpret_omega_squared(omega_sq_3_sd_15$Omega2, rules = "field2013")
omega_sq_3_sd_30 <- omega_squared((model_3_sd_30), partial = FALSE, ci = 0.95)
omega_interpret_3_sd_30 <- interpret_omega_squared(omega_sq_3_sd_30$Omega2, rules = "field2013")
omega_sq_3_sd_60 <- omega_squared((model_3_sd_60), partial = FALSE, ci = 0.95)
omega_interpret_3_sd_60 <- interpret_omega_squared(omega_sq_3_sd_60$Omega2, rules = "field2013")

sp_3_mean_var <- data.frame(species_id = 3,
                            sd = "all",
                            all_params_1 = params_3_sd$Parameter[1],
                            all_params_2 = params_3_sd$Parameter[2],
                            all_coeff_1 = params_3_sd$Coefficient[1],
                            all_coeff_2 = params_3_sd$Coefficient[2],
                            all_coeff_ci_low_1 = params_3_sd$CI_low[1],
                            all_coeff_ci_low_2 = params_3_sd$CI_low[2],
                            all_coeff_ci_hi_1 = params_3_sd$CI_high[1],
                            all_coeff_ci_hi_2 = params_3_sd$CI_high[2],
                            all_p_val_1 = params_3_sd$p[1],
                            all_p_val_2 = params_3_sd$p[2],
                            all_omega_sq_1 = omega_sq_3_sd$Omega2[1],
                            all_omega_sq_2 = omega_sq_3_sd$Omega2[2],
                            all_omega_sq_ci_low_1 = omega_sq_3_sd$CI_low[1],
                            all_omega_sq_ci_low_2 = omega_sq_3_sd$CI_low[2],
                            all_omega_sq_ci_hi_1 = omega_sq_3_sd$CI_high[1],
                            all_omega_sq_ci_hi_2 = omega_sq_3_sd$CI_high[2],
                            all_omega_sq_interpret = omega_interpret_3_sd)
sp_3_mean_var_0 <- data.frame(species_id = 3,
                              sd = "none",
                              all_params_1 = params_3_sd_0$Parameter[1],
                              all_params_2 = params_3_sd_0$Parameter[2],
                              all_coeff_1 = params_3_sd_0$Coefficient[1],
                              all_coeff_2 = params_3_sd_0$Coefficient[2],
                              all_coeff_ci_low_1 = params_3_sd_0$CI_low[1],
                              all_coeff_ci_low_2 = params_3_sd_0$CI_low[2],
                              all_coeff_ci_hi_1 = params_3_sd_0$CI_high[1],
                              all_coeff_ci_hi_2 = params_3_sd_0$CI_high[2],
                              all_p_val_1 = params_3_sd_0$p[1],
                              all_p_val_2 = params_3_sd_0$p[2],
                              all_omega_sq_1 = omega_sq_3_sd_0$Omega2[1],
                              all_omega_sq_2 = omega_sq_3_sd_0$Omega2[2],
                              all_omega_sq_ci_low_1 = omega_sq_3_sd_0$CI_low[1],
                              all_omega_sq_ci_low_2 = omega_sq_3_sd_0$CI_low[2],
                              all_omega_sq_ci_hi_1 = omega_sq_3_sd_0$CI_high[1],
                              all_omega_sq_ci_hi_2 = omega_sq_3_sd_0$CI_high[2],
                              all_omega_sq_interpret = omega_interpret_3_sd_0)
sp_3_mean_var_15 <- data.frame(species_id = 3,
                               sd = "low",
                               all_params_1 = params_3_sd_15$Parameter[1],
                               all_params_2 = params_3_sd_15$Parameter[2],
                               all_coeff_1 = params_3_sd_15$Coefficient[1],
                               all_coeff_2 = params_3_sd_15$Coefficient[2],
                               all_coeff_ci_low_1 = params_3_sd_15$CI_low[1],
                               all_coeff_ci_low_2 = params_3_sd_15$CI_low[2],
                               all_coeff_ci_hi_1 = params_3_sd_15$CI_high[1],
                               all_coeff_ci_hi_2 = params_3_sd_15$CI_high[2],
                               all_p_val_1 = params_3_sd_15$p[1],
                               all_p_val_2 = params_3_sd_15$p[2],
                               all_omega_sq_1 = omega_sq_3_sd_15$Omega2[1],
                               all_omega_sq_2 = omega_sq_3_sd_15$Omega2[2],
                               all_omega_sq_ci_low_1 = omega_sq_3_sd_15$CI_low[1],
                               all_omega_sq_ci_low_2 = omega_sq_3_sd_15$CI_low[2],
                               all_omega_sq_ci_hi_1 = omega_sq_3_sd_15$CI_high[1],
                               all_omega_sq_ci_hi_2 = omega_sq_3_sd_15$CI_high[2],
                               all_omega_sq_interpret = omega_interpret_3_sd_15)
sp_3_mean_var_30 <- data.frame(species_id = 3,
                               sd = "medium",
                               all_params_1 = params_3_sd_30$Parameter[1],
                               all_params_2 = params_3_sd_30$Parameter[2],
                               all_coeff_1 = params_3_sd_30$Coefficient[1],
                               all_coeff_2 = params_3_sd_30$Coefficient[2],
                               all_coeff_ci_low_1 = params_3_sd_30$CI_low[1],
                               all_coeff_ci_low_2 = params_3_sd_30$CI_low[2],
                               all_coeff_ci_hi_1 = params_3_sd_30$CI_high[1],
                               all_coeff_ci_hi_2 = params_3_sd_30$CI_high[2],
                               all_p_val_1 = params_3_sd_30$p[1],
                               all_p_val_2 = params_3_sd_30$p[2],
                               all_omega_sq_1 = omega_sq_3_sd_30$Omega2[1],
                               all_omega_sq_2 = omega_sq_3_sd_30$Omega2[2],
                               all_omega_sq_ci_low_1 = omega_sq_3_sd_30$CI_low[1],
                               all_omega_sq_ci_low_2 = omega_sq_3_sd_30$CI_low[2],
                               all_omega_sq_ci_hi_1 = omega_sq_3_sd_30$CI_high[1],
                               all_omega_sq_ci_hi_2 = omega_sq_3_sd_30$CI_high[2],
                               all_omega_sq_interpret = omega_interpret_3_sd_30)
sp_3_mean_var_60 <- data.frame(species_id = 3,
                               sd = "high",
                               all_params_1 = params_3_sd_60$Parameter[1],
                               all_params_2 = params_3_sd_60$Parameter[2],
                               all_coeff_1 = params_3_sd_60$Coefficient[1],
                               all_coeff_2 = params_3_sd_60$Coefficient[2],
                               all_coeff_ci_low_1 = params_3_sd_60$CI_low[1],
                               all_coeff_ci_low_2 = params_3_sd_60$CI_low[2],
                               all_coeff_ci_hi_1 = params_3_sd_60$CI_high[1],
                               all_coeff_ci_hi_2 = params_3_sd_60$CI_high[2],
                               all_p_val_1 = params_3_sd_60$p[1],
                               all_p_val_2 = params_3_sd_60$p[2],
                               all_omega_sq_1 = omega_sq_3_sd_60$Omega2[1],
                               all_omega_sq_2 = omega_sq_3_sd_60$Omega2[2],
                               all_omega_sq_ci_low_1 = omega_sq_3_sd_60$CI_low[1],
                               all_omega_sq_ci_low_2 = omega_sq_3_sd_60$CI_low[2],
                               all_omega_sq_ci_hi_1 = omega_sq_3_sd_60$CI_high[1],
                               all_omega_sq_ci_hi_2 = omega_sq_3_sd_60$CI_high[2],
                               all_omega_sq_interpret = omega_interpret_3_sd_60)

sp_3_mean_var <- plyr::rbind.fill(sp_3_mean_var, sp_3_mean_var_0, sp_3_mean_var_15, sp_3_mean_var_30, sp_3_mean_var_60)

params_4_sd <- parameters::model_parameters((model_4_sd))
params_4_sd_0 <- parameters::model_parameters((model_4_sd_0))
params_4_sd_15 <- parameters::model_parameters((model_4_sd_15))
params_4_sd_30 <- parameters::model_parameters((model_4_sd_30))
params_4_sd_60 <- parameters::model_parameters((model_4_sd_60))

omega_sq_4_sd <- omega_squared((model_4_sd), partial = FALSE, ci = 0.95)
omega_interpret_4_sd <- interpret_omega_squared(omega_sq_4_sd$Omega2, rules = "field2013")
omega_sq_4_sd_0 <- omega_squared((model_4_sd_0), partial = FALSE, ci = 0.95)
omega_interpret_4_sd_0 <- interpret_omega_squared(omega_sq_4_sd_0$Omega2, rules = "field2013")
omega_sq_4_sd_15 <- omega_squared((model_4_sd_15), partial = FALSE, ci = 0.95)
omega_interpret_4_sd_15 <- interpret_omega_squared(omega_sq_4_sd_15$Omega2, rules = "field2013")
omega_sq_4_sd_30 <- omega_squared((model_4_sd_30), partial = FALSE, ci = 0.95)
omega_interpret_4_sd_30 <- interpret_omega_squared(omega_sq_4_sd_30$Omega2, rules = "field2013")
omega_sq_4_sd_60 <- omega_squared((model_4_sd_60), partial = FALSE, ci = 0.95)
omega_interpret_4_sd_60 <- interpret_omega_squared(omega_sq_4_sd_60$Omega2, rules = "field2013")

sp_4_mean_var <- data.frame(species_id = 4,
                            sd = "all",
                            all_params_1 = params_4_sd$Parameter[1],
                            all_params_2 = params_4_sd$Parameter[2],
                            all_coeff_1 = params_4_sd$Coefficient[1],
                            all_coeff_2 = params_4_sd$Coefficient[2],
                            all_coeff_ci_low_1 = params_4_sd$CI_low[1],
                            all_coeff_ci_low_2 = params_4_sd$CI_low[2],
                            all_coeff_ci_hi_1 = params_4_sd$CI_high[1],
                            all_coeff_ci_hi_2 = params_4_sd$CI_high[2],
                            all_p_val_1 = params_4_sd$p[1],
                            all_p_val_2 = params_4_sd$p[2],
                            all_omega_sq_1 = omega_sq_4_sd$Omega2[1],
                            all_omega_sq_2 = omega_sq_4_sd$Omega2[2],
                            all_omega_sq_ci_low_1 = omega_sq_4_sd$CI_low[1],
                            all_omega_sq_ci_low_2 = omega_sq_4_sd$CI_low[2],
                            all_omega_sq_ci_hi_1 = omega_sq_4_sd$CI_high[1],
                            all_omega_sq_ci_hi_2 = omega_sq_4_sd$CI_high[2],
                            all_omega_sq_interpret = omega_interpret_4_sd)
sp_4_mean_var_0 <- data.frame(species_id = 4, 
                              sd = "none",
                              all_params_1 = params_4_sd_0$Parameter[1],
                              all_params_2 = params_4_sd_0$Parameter[2],
                              all_coeff_1 = params_4_sd_0$Coefficient[1],
                              all_coeff_2 = params_4_sd_0$Coefficient[2],
                              all_coeff_ci_low_1 = params_4_sd_0$CI_low[1],
                              all_coeff_ci_low_2 = params_4_sd_0$CI_low[2],
                              all_coeff_ci_hi_1 = params_4_sd_0$CI_high[1],
                              all_coeff_ci_hi_2 = params_4_sd_0$CI_high[2],
                              all_p_val_1 = params_4_sd_0$p[1],
                              all_p_val_2 = params_4_sd_0$p[2],
                              all_omega_sq_1 = omega_sq_4_sd_0$Omega2[1],
                              all_omega_sq_2 = omega_sq_4_sd_0$Omega2[2],
                              all_omega_sq_ci_low_1 = omega_sq_4_sd_0$CI_low[1],
                              all_omega_sq_ci_low_2 = omega_sq_4_sd_0$CI_low[2],
                              all_omega_sq_ci_hi_1 = omega_sq_4_sd_0$CI_high[1],
                              all_omega_sq_ci_hi_2 = omega_sq_4_sd_0$CI_high[2],
                              all_omega_sq_interpret = omega_interpret_4_sd_0)
sp_4_mean_var_15 <- data.frame(species_id = 4,
                               sd = "low",
                               all_params_1 = params_4_sd_15$Parameter[1],
                               all_params_2 = params_4_sd_15$Parameter[2],
                               all_coeff_1 = params_4_sd_15$Coefficient[1],
                               all_coeff_2 = params_4_sd_15$Coefficient[2],
                               all_coeff_ci_low_1 = params_4_sd_15$CI_low[1],
                               all_coeff_ci_low_2 = params_4_sd_15$CI_low[2],
                               all_coeff_ci_hi_1 = params_4_sd_15$CI_high[1],
                               all_coeff_ci_hi_2 = params_4_sd_15$CI_high[2],
                               all_p_val_1 = params_4_sd_15$p[1],
                               all_p_val_2 = params_4_sd_15$p[2],
                               all_omega_sq_1 = omega_sq_4_sd_15$Omega2[1],
                               all_omega_sq_2 = omega_sq_4_sd_15$Omega2[2],
                               all_omega_sq_ci_low_1 = omega_sq_4_sd_15$CI_low[1],
                               all_omega_sq_ci_low_2 = omega_sq_4_sd_15$CI_low[2],
                               all_omega_sq_ci_hi_1 = omega_sq_4_sd_15$CI_high[1],
                               all_omega_sq_ci_hi_2 = omega_sq_4_sd_15$CI_high[2],
                               all_omega_sq_interpret = omega_interpret_4_sd_15)
sp_4_mean_var_30 <- data.frame(species_id = 4,
                               sd = "medium",
                               all_params_1 = params_4_sd_30$Parameter[1],
                               all_params_2 = params_4_sd_30$Parameter[2],
                               all_coeff_1 = params_4_sd_30$Coefficient[1],
                               all_coeff_2 = params_4_sd_30$Coefficient[2],
                               all_coeff_ci_low_1 = params_4_sd_30$CI_low[1],
                               all_coeff_ci_low_2 = params_4_sd_30$CI_low[2],
                               all_coeff_ci_hi_1 = params_4_sd_30$CI_high[1],
                               all_coeff_ci_hi_2 = params_4_sd_30$CI_high[2],
                               all_p_val_1 = params_4_sd_30$p[1],
                               all_p_val_2 = params_4_sd_30$p[2],
                               all_omega_sq_1 = omega_sq_4_sd_30$Omega2[1],
                               all_omega_sq_2 = omega_sq_4_sd_30$Omega2[2],
                               all_omega_sq_ci_low_1 = omega_sq_4_sd_30$CI_low[1],
                               all_omega_sq_ci_low_2 = omega_sq_4_sd_30$CI_low[2],
                               all_omega_sq_ci_hi_1 = omega_sq_4_sd_30$CI_high[1],
                               all_omega_sq_ci_hi_2 = omega_sq_4_sd_30$CI_high[2],
                               all_omega_sq_interpret = omega_interpret_4_sd_30)
sp_4_mean_var_60 <- data.frame(species_id = 4,
                               sd = "high",
                               all_params_1 = params_4_sd_60$Parameter[1],
                               all_params_2 = params_4_sd_60$Parameter[2],
                               all_coeff_1 = params_4_sd_60$Coefficient[1],
                               all_coeff_2 = params_4_sd_60$Coefficient[2],
                               all_coeff_ci_low_1 = params_4_sd_60$CI_low[1],
                               all_coeff_ci_low_2 = params_4_sd_60$CI_low[2],
                               all_coeff_ci_hi_1 = params_4_sd_60$CI_high[1],
                               all_coeff_ci_hi_2 = params_4_sd_60$CI_high[2],
                               all_p_val_1 = params_4_sd_60$p[1],
                               all_p_val_2 = params_4_sd_60$p[2],
                               all_omega_sq_1 = omega_sq_4_sd_60$Omega2[1],
                               all_omega_sq_2 = omega_sq_4_sd_60$Omega2[2],
                               all_omega_sq_ci_low_1 = omega_sq_4_sd_60$CI_low[1],
                               all_omega_sq_ci_low_2 = omega_sq_4_sd_60$CI_low[2],
                               all_omega_sq_ci_hi_1 = omega_sq_4_sd_60$CI_high[1],
                               all_omega_sq_ci_hi_2 = omega_sq_4_sd_60$CI_high[2],
                               all_omega_sq_interpret = omega_interpret_4_sd_60)

sp_4_mean_var <- plyr::rbind.fill(sp_4_mean_var, sp_4_mean_var_0, sp_4_mean_var_15, sp_4_mean_var_30, sp_4_mean_var_60)

basal_area_log_mean_var <- plyr::rbind.fill(sp_1_mean_var, sp_2_mean_var, sp_3_mean_var, sp_4_mean_var)
write.csv(basal_area_log_mean_var, file = "out/basal_area_log_mean_lm_var_stats.csv")

### STORAGE CONCENTRATION

data_hist_storage <- data_hist %>% 
  dplyr::group_by(species_id, env_sd, env_mean, env_rep, run_rep, time) %>%
  dplyr::summarise(storage = mean(storage_portion, na.rm = TRUE)) %>%
  dplyr::group_by(species_id, env_sd, env_mean, env_rep, run_rep) %>%
  dplyr::summarise(storage = mean(storage, na.rm = TRUE)) %>%
  dplyr::group_by(species_id, env_sd, env_mean) %>%
  dplyr::summarise(storage_mean = mean(storage, na.rm = TRUE),
                   storage_sd = sd(storage, na.rm = TRUE))

data_hist_storage_env_mean <- data_hist %>% 
  dplyr::filter(env_mean != 1) %>%
  dplyr::group_by(species_id, env_sd, env_mean, env_rep, run_rep, time) %>%
  dplyr::summarise(storage = mean(storage_portion, na.rm = TRUE)) %>%
  dplyr::group_by(species_id, env_mean, env_rep, run_rep) %>%
  dplyr::summarise(storage = mean(storage, na.rm = TRUE)) %>%
  dplyr::group_by(species_id, env_mean) %>%
  dplyr::summarise(storage_mean = mean(storage, na.rm = TRUE),
                   storage_sd = sd(storage, na.rm = TRUE))

data_hist_storage_env_sd <-  data_hist %>% 
  dplyr::filter(env_mean != 1) %>%
  dplyr::group_by(species_id, env_sd, env_mean, env_rep, run_rep, time) %>%
  dplyr::summarise(storage = mean(storage_portion, na.rm = TRUE)) %>%
  dplyr::group_by(species_id, env_sd, env_rep, run_rep) %>%
  dplyr::summarise(storage = mean(storage, na.rm = TRUE)) %>%
  dplyr::group_by(species_id, env_sd) %>%
  dplyr::summarise(storage_mean = mean(storage, na.rm = TRUE),
                   storage_sd = sd(storage, na.rm = TRUE))

data_hist_storage_all <- data_hist %>% 
  dplyr::group_by(env_sd, env_mean, env_rep, run_rep, time) %>%
  dplyr::summarise(storage = mean(storage_portion)) %>%
  dplyr::group_by(env_sd, env_mean, env_rep, run_rep) %>%
  dplyr::summarise(storage = mean(storage, na.rm = TRUE)) %>%
  dplyr::group_by(env_sd, env_mean) %>%
  dplyr::summarise(species_id = 0, 
                   storage_mean = mean(storage, na.rm = TRUE),
                   storage_sd = sd(storage, na.rm = TRUE))

data_hist_storage_all_env_mean <- data_hist %>% 
  dplyr::filter(env_mean != 1) %>%
  dplyr::group_by(env_sd, env_mean, env_rep, run_rep, time) %>%
  dplyr::summarise(storage = mean(storage_portion, na.rm = TRUE)) %>%
  dplyr::group_by(env_mean, env_rep, run_rep) %>%
  dplyr::summarise(storage = mean(storage, na.rm = TRUE)) %>%
  dplyr::group_by(env_mean) %>%
  dplyr::summarise(species_id = 0, 
                   storage_mean = mean(storage, na.rm = TRUE),
                   storage_sd = sd(storage, na.rm = TRUE))

data_hist_storage_all_env_sd <-  data_hist %>% 
  dplyr::filter(env_mean != 1) %>%
  dplyr::group_by(env_sd, env_mean, env_rep, run_rep, time) %>%
  dplyr::summarise(storage = mean(storage_portion, na.rm = TRUE)) %>%
  dplyr::group_by(env_sd, env_rep, run_rep) %>%
  dplyr::summarise(storage = mean(storage, na.rm = TRUE)) %>%
  dplyr::group_by(env_sd) %>%
  dplyr::summarise(species_id = 0, 
                   storage_mean = mean(storage, na.rm = TRUE),
                   storage_sd = sd(storage, na.rm = TRUE))

data_hist_storage_reps <- data_hist %>% 
  dplyr::group_by(species_id, env_sd, env_mean, env_rep, run_rep, time) %>%
  dplyr::summarise(storage = mean(storage_portion, na.rm = TRUE)) %>%
  dplyr::group_by(species_id, env_sd, env_mean, env_rep, run_rep) %>%
  dplyr::summarise(storage = mean(storage, na.rm = TRUE))

data_hist_storage_all_reps <- data_hist %>% 
  dplyr::group_by(env_sd, env_mean, env_rep, run_rep, time) %>%
  dplyr::summarise(storage = mean(storage_portion, na.rm = TRUE)) %>%
  dplyr::group_by(env_sd, env_mean, env_rep, run_rep) %>%
  dplyr::summarise(species_id = 0, 
                   storage = mean(storage, na.rm = TRUE))

data_hist_storage$env = data_hist_storage$env_mean + data_hist_storage$env_sd
data_hist_storage_reps$env = data_hist_storage_reps$env_mean + data_hist_storage_reps$env_sd
data_hist_storage_all$env = data_hist_storage_all$env_mean + data_hist_storage_all$env_sd
data_hist_storage_all_reps$env = data_hist_storage_all_reps$env_mean + data_hist_storage_all_reps$env_sd


data_hist_storage_all_and_ind = rbind(data_hist_storage, data_hist_storage_all)
data_hist_storage_all_and_ind_env_mean = rbind.data.frame(data_hist_storage_env_mean, data_hist_storage_all_env_mean)
data_hist_storage_all_and_ind_env_sd = rbind.data.frame(data_hist_storage_env_sd, data_hist_storage_all_env_sd)
data_hist_storage_reps_all_and_ind = rbind(data_hist_storage_reps, data_hist_storage_all_reps)

data_hist_storage_all_and_ind$env_mean[data_hist_storage_all_and_ind$env_mean == 75] = 0.75
data_hist_storage_all_and_ind$env_mean[data_hist_storage_all_and_ind$env_mean == 85] = 0.85

data_hist_storage_all_and_ind$env_mean <- factor(data_hist_storage_all_and_ind$env_mean, levels = c(1, 0.85, 0.75))

p <- ggplot(data_hist_storage_all_and_ind, aes(fill=as.factor(species_id), 
                                               y=storage_mean, 
                                               x=as.factor(env_mean),
                                               group = as.factor(env_sd))) +
  geom_bar(aes(alpha=(env_sd)),stat="identity", position = "dodge", color = "#FFFFFF") +
  geom_errorbar(aes(ymin=storage_mean-storage_sd, ymax=storage_mean+storage_sd), position = "dodge",
                colour="#083855") + 
  scale_fill_manual("Allocation \nStrategy:",
                    values=foxes_palettes$main[c(5, 2, 1, 4, 3)],
                    labels=c("All Strategies", "Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe")) + 
  facet_grid(rows = "species_id", space = "free", labeller = labeller(species_id = c("0" = "All Strategies",
                                                                                     "1" = "Slow-Risky", 
                                                                                     "2" = "Slow-Safe",
                                                                                     "3" = "Fast-Risky", 
                                                                                     "4" = "Fast-Safe"))) +
  scale_alpha("Stochasticity", breaks = c(0, 15, 30, 60), range = c(1, 0.4), labels = c("None", "Low", "Medium", "High")) + 
  scale_x_discrete("Environmental Treatment", labels = c("Control",
                                                         "Low Stress",
                                                         "Medium Stress")) +
  scale_y_continuous("Average Storage Concentration per Tree (kgC/kgC)") +
  foxes_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
p


file_figure <- tempfile(paste("mean_storage_conc", "barplot", "final", sep = "_"), tmpdir = figure_dir, fileext = ".png")
ggsave(file_figure, plot = p, device = NULL, path = NULL,
       scale = 1, width = 190, height = 220, dpi = 300, limitsize = TRUE,
       units =  "mm")

data_hist_storage_all_and_ind = rbind(data_hist_storage, data_hist_storage_all)
data_hist_storage_all_and_ind_no_control <- subset(data_hist_storage_all_and_ind, env_mean != 1)
data_hist_storage_all_and_ind_env_mean_no_control <- subset(data_hist_storage_all_and_ind_env_mean, env_mean != 1)
data_hist_storage_all_and_ind_no_control$species_id <- factor(data_hist_storage_all_and_ind_no_control$species_id, levels = c(1,2,3,4,0))
data_hist_storage_all_and_ind_env_mean_no_control$species_id <- factor(data_hist_storage_all_and_ind_env_mean_no_control$species_id, levels = c(1,2,3,4,0))
data_hist_storage_all_and_ind_env_sd$species_id <- factor(data_hist_storage_all_and_ind_env_sd$species_id, levels = c(1,2,3,4,0))

data_hist_storage_all_and_ind_no_control_species <- subset(data_hist_storage_all_and_ind, species_id != 0)
data_hist_storage_all_and_ind_no_control_all <- subset(data_hist_storage_all_and_ind, species_id == 0)

p <- ggplot(data_hist_storage_all_and_ind_no_control, aes(x=-as.numeric(env_mean), 
                                                        y = storage_mean, 
                                                        color = as.factor(species_id), 
                                                        linetype = as.factor(env_sd))) + 
  geom_line(position = position_dodge(width = 2)) + 
  geom_errorbar(aes(ymin = storage_mean - storage_sd, ymax = storage_mean + storage_sd), 
                alpha = 0.5,
                position = position_dodge(width = 2), 
                width = 4) + 
  geom_point(position = position_dodge(width = 2)) + 
  scale_color_manual("Species Id:",
                     values=c(foxes_palettes$main[c(2, 1, 4, 3, 5)]),
                     labels=c("Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe", "All Species")) +
  scale_linetype_discrete("Environment SD (d):",
                          labels = c("0", "15", "30", "60")) +
  scale_y_continuous("Average Storage Concentration per Tree (kgC/kgC)") +
  xlab("Mean of Stress (yr)") +
  # geom_jitter() + 
  scale_x_continuous(breaks = c(-85, -75), labels = c("low stress", "med stress")) +
  facet_wrap(.~species_id, nrow = 3, ncol = 2, labeller = labeller(species_id = c("1" = "Slow-Risky", 
                                                                                  "2" = "Slow-Safe",
                                                                                  "3" = "Fast-Risky", 
                                                                                  "4" = "Fast-Safe",
                                                                                  "0" = "All Species"))) + 
  force_panelsizes(rows = c(1,1,1),
                   respect = TRUE) +
  foxes_theme

p

file_figure <- tempfile(paste("storage", "sd_var", "all", sep = "_"), tmpdir = figure_dir, fileext = ".png")
ggsave(file_figure, plot = p, device = NULL, path = NULL,
       scale = 1, width = 260, height = 220, dpi = 300, limitsize = TRUE,
       units =  "mm")


file_figure <- tempfile(paste("storage", "bigger","sd_var", "all", sep = "_"), tmpdir = figure_dir, fileext = ".png")
ggsave(file_figure, plot = p, device = NULL, path = NULL,
       scale = 1, width = 520, height = 220, dpi = 300, limitsize = TRUE,
       units =  "mm")


p <- ggplot(data_hist_storage_all_and_ind_no_control, aes(x=env_sd, 
                                                        y = storage_mean, 
                                                        color = as.factor(species_id), 
                                                        linetype = as.factor(env_mean))) + 
  geom_line(position = position_dodge(width = 2)) + 
  geom_errorbar(aes(ymin = storage_mean - storage_sd, ymax = storage_mean + storage_sd), 
                alpha = 0.5,
                position = position_dodge(width = 2), 
                width = 4) + 
  geom_point(position = position_dodge(width = 2)) + 
  scale_color_manual("Species Id:",
                     values=c(foxes_palettes$main[c(2, 1, 4, 3, 5)]),
                     labels=c("Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe", "All Species")) +
  scale_linetype_discrete("Environment Mean:",
                          labels = c("0.75y", "0.85y")) +
  scale_y_continuous("Average Storage Concentration per Tree (kgC/kgC)") +
  xlab("Stochasticity Stress (yr)") +
  # geom_jitter() + 
  scale_x_continuous(breaks = c(0, 15, 30, 60), labels = c("None", "Low", "Medium", "High")) +
  facet_wrap(.~species_id, nrow = 3, ncol = 2, labeller = labeller(species_id = c("1" = "Slow-Risky", 
                                                                                  "2" = "Slow-Safe",
                                                                                  "3" = "Fast-Risky", 
                                                                                  "4" = "Fast-Safe",
                                                                                  "0" = "All Species"))) + 
  force_panelsizes(rows = c(1,1,1),
                   respect = TRUE) +
  foxes_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

p

file_figure <- tempfile(paste("storage", "bigger","mean_var", "all", sep = "_"), tmpdir = figure_dir, fileext = ".png")
ggsave(file_figure, plot = p, device = NULL, path = NULL,
       scale = 1, width = 520, height = 220, dpi = 300, limitsize = TRUE,
       units =  "mm")

file_figure <- tempfile(paste("storage","mean_var", "all", sep = "_"), tmpdir = figure_dir, fileext = ".png")
ggsave(file_figure, plot = p, device = NULL, path = NULL,
       scale = 1, width = 260, height = 220, dpi = 300, limitsize = TRUE,
       units =  "mm")

data_hist_ave <- data_hist %>%
  dplyr::group_by(tree_id, species_id, env_mean, env_sd, env_rep, run_rep) %>%
  dplyr::summarise(storage = mean(storage_portion, na.rm=TRUE))

data_hist_no_spec = data_hist_ave
data_hist_no_spec$species_id = 0

data_hist_new <- rbind(data_hist_no_spec, data_hist_ave)
data_hist_new$storage[data_hist_new$storage < 0] = 0

p <- ggplot(data_hist_new, aes(x = as.factor(species_id), y = storage)) + 
  geom_violin(aes(color=as.factor(species_id), fill=as.factor(species_id)), alpha = 0.75) +
  scale_color_manual("Species Id:",
                     values=c(foxes_palettes$main[c(5, 2, 1, 4, 3)]),
                     labels=c("All Strategies", "Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe")) +
  scale_fill_manual("Species Id:",
                    values=c(foxes_palettes$main[c(5, 2, 1, 4, 3)]),
                    labels=c("All Strategies", "Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe")) +
  scale_y_continuous("Storage Concentration per Tree (kgC/kgC)") +
  scale_x_discrete("Strategy", labels = c("All Strategies", "Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe")) +
  facet_grid(env_sd~ env_mean, labeller = labeller(env_mean = c("75" = "Med Stress",
                                                                "85" = "Low Stress",
                                                                "1" = "No Stress"),
                                                   env_sd = c("0" = "No Stoch.",
                                                              "15" = "Low Stoch.",
                                                              "30" = "Med Stoch.",
                                                              "60" = "High Stoch"))) +
  foxes_theme + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
p

file_figure <- tempfile(paste("storage","violin", "all", sep = "_"), tmpdir = figure_dir, fileext = ".png")
ggsave(file_figure, plot = p, device = NULL, path = NULL,
       scale = 1, width = 260, height = 220, dpi = 300, limitsize = TRUE,
       units =  "mm")


data_hist_storage_reps_all_and_ind$env_mean[data_hist_storage_reps_all_and_ind$env_mean == 75] = 0.75
data_hist_storage_reps_all_and_ind$env_mean[data_hist_storage_reps_all_and_ind$env_mean == 85] = 0.85
data_hist_storage_reps_all_and_ind_no_control <- subset(data_hist_storage_reps_all_and_ind, env_mean != 1)
data_hist_storage_reps_all_and_ind_no_control$env_mean <- as.factor(data_hist_storage_reps_all_and_ind_no_control$env_mean)

data_hist_storage_all_and_ind_0 = subset(data_hist_storage_reps_all_and_ind_no_control, species_id == 0)
data_hist_storage_all_and_ind_0_mean_075 = subset(data_hist_storage_reps_all_and_ind_no_control, species_id == 0 & env_mean == 0.75)
data_hist_storage_all_and_ind_0_mean_085 = subset(data_hist_storage_reps_all_and_ind_no_control, species_id == 0 & env_mean == 0.85)
data_hist_storage_all_and_ind_0_sd_0 = subset(data_hist_storage_reps_all_and_ind_no_control, species_id == 0 & env_sd == 0)
data_hist_storage_all_and_ind_0_sd_15 = subset(data_hist_storage_reps_all_and_ind_no_control, species_id == 0 & env_sd == 15)
data_hist_storage_all_and_ind_0_sd_30 = subset(data_hist_storage_reps_all_and_ind_no_control, species_id == 0 & env_sd == 30)
data_hist_storage_all_and_ind_0_sd_60 = subset(data_hist_storage_reps_all_and_ind_no_control, species_id == 0 & env_sd == 60)

data_hist_storage_all_and_ind_1 = subset(data_hist_storage_reps_all_and_ind_no_control, species_id == 1)
data_hist_storage_all_and_ind_1_mean_075 = subset(data_hist_storage_reps_all_and_ind_no_control, species_id == 1 & env_mean == 0.75)
data_hist_storage_all_and_ind_1_mean_085 = subset(data_hist_storage_reps_all_and_ind_no_control, species_id == 1 & env_mean == 0.85)
data_hist_storage_all_and_ind_1_sd_0 = subset(data_hist_storage_reps_all_and_ind_no_control, species_id == 1 & env_sd == 0)
data_hist_storage_all_and_ind_1_sd_15 = subset(data_hist_storage_reps_all_and_ind_no_control, species_id == 1 & env_sd == 15)
data_hist_storage_all_and_ind_1_sd_30 = subset(data_hist_storage_reps_all_and_ind_no_control, species_id == 1 & env_sd == 30)
data_hist_storage_all_and_ind_1_sd_60 = subset(data_hist_storage_reps_all_and_ind_no_control, species_id == 1 & env_sd == 60)

data_hist_storage_all_and_ind_2 = subset(data_hist_storage_reps_all_and_ind_no_control, species_id == 2)
data_hist_storage_all_and_ind_2_mean_075 = subset(data_hist_storage_reps_all_and_ind_no_control, species_id == 2 & env_mean == 0.75)
data_hist_storage_all_and_ind_2_mean_085 = subset(data_hist_storage_reps_all_and_ind_no_control, species_id == 2 & env_mean == 0.85)
data_hist_storage_all_and_ind_2_sd_0 = subset(data_hist_storage_reps_all_and_ind_no_control, species_id == 2 & env_sd == 0)
data_hist_storage_all_and_ind_2_sd_15 = subset(data_hist_storage_reps_all_and_ind_no_control, species_id == 2 & env_sd == 15)
data_hist_storage_all_and_ind_2_sd_30 = subset(data_hist_storage_reps_all_and_ind_no_control, species_id == 2 & env_sd == 30)
data_hist_storage_all_and_ind_2_sd_60 = subset(data_hist_storage_reps_all_and_ind_no_control, species_id == 2 & env_sd == 60)

data_hist_storage_all_and_ind_3 = subset(data_hist_storage_reps_all_and_ind_no_control, species_id == 3)
data_hist_storage_all_and_ind_3_mean_075 = subset(data_hist_storage_reps_all_and_ind_no_control, species_id == 3 & env_mean == 0.75)
data_hist_storage_all_and_ind_3_mean_085 = subset(data_hist_storage_reps_all_and_ind_no_control, species_id == 3 & env_mean == 0.85)
data_hist_storage_all_and_ind_3_sd_0 = subset(data_hist_storage_reps_all_and_ind_no_control, species_id == 3 & env_sd == 0)
data_hist_storage_all_and_ind_3_sd_15 = subset(data_hist_storage_reps_all_and_ind_no_control, species_id == 3 & env_sd == 15)
data_hist_storage_all_and_ind_3_sd_30 = subset(data_hist_storage_reps_all_and_ind_no_control, species_id == 3 & env_sd == 30)
data_hist_storage_all_and_ind_3_sd_60 = subset(data_hist_storage_reps_all_and_ind_no_control, species_id == 3 & env_sd == 60)

data_hist_storage_all_and_ind_4 = subset(data_hist_storage_reps_all_and_ind_no_control, species_id == 4)
data_hist_storage_all_and_ind_4_mean_075 = subset(data_hist_storage_reps_all_and_ind_no_control, species_id == 4 & env_mean == 0.75)
data_hist_storage_all_and_ind_4_mean_085 = subset(data_hist_storage_reps_all_and_ind_no_control, species_id == 4 & env_mean == 0.85)
data_hist_storage_all_and_ind_4_sd_0 = subset(data_hist_storage_reps_all_and_ind_no_control, species_id == 4 & env_sd == 0)
data_hist_storage_all_and_ind_4_sd_15 = subset(data_hist_storage_reps_all_and_ind_no_control, species_id == 4 & env_sd == 15)
data_hist_storage_all_and_ind_4_sd_30 = subset(data_hist_storage_reps_all_and_ind_no_control, species_id == 4 & env_sd == 30)
data_hist_storage_all_and_ind_4_sd_60 = subset(data_hist_storage_reps_all_and_ind_no_control, species_id == 4 & env_sd == 60)

model_0_mean=lm(data_hist_storage_all_and_ind_0$storage ~ data_hist_storage_all_and_ind_0$env_sd)
model_0_mean_075=lm(data_hist_storage_all_and_ind_0_mean_075$storage ~ data_hist_storage_all_and_ind_0_mean_075$env_sd)
model_0_mean_085=lm(data_hist_storage_all_and_ind_0_mean_085$storage ~ data_hist_storage_all_and_ind_0_mean_085$env_sd)
model_0_sd=lm(data_hist_storage_all_and_ind_0$storage ~ data_hist_storage_all_and_ind_0$env_mean)
model_0_sd_0=lm(data_hist_storage_all_and_ind_0_sd_0$storage ~ data_hist_storage_all_and_ind_0_sd_0$env_mean)
model_0_sd_15=lm(data_hist_storage_all_and_ind_0_sd_15$storage ~ data_hist_storage_all_and_ind_0_sd_15$env_mean)
model_0_sd_30=lm(data_hist_storage_all_and_ind_0_sd_30$storage ~ data_hist_storage_all_and_ind_0_sd_30$env_mean)
model_0_sd_60=lm(data_hist_storage_all_and_ind_0_sd_60$storage ~ data_hist_storage_all_and_ind_0_sd_60$env_mean)

model_1_mean=lm(data_hist_storage_all_and_ind_1$storage ~ data_hist_storage_all_and_ind_1$env_sd)
model_1_mean_075=lm(data_hist_storage_all_and_ind_1_mean_075$storage ~ data_hist_storage_all_and_ind_1_mean_075$env_sd)
model_1_mean_085=lm(data_hist_storage_all_and_ind_1_mean_085$storage ~ data_hist_storage_all_and_ind_1_mean_085$env_sd)
model_1_sd=lm(data_hist_storage_all_and_ind_1$storage ~ data_hist_storage_all_and_ind_1$env_mean)
model_1_sd_0=lm(data_hist_storage_all_and_ind_1_sd_0$storage ~ data_hist_storage_all_and_ind_1_sd_0$env_mean)
model_1_sd_15=lm(data_hist_storage_all_and_ind_1_sd_15$storage ~ data_hist_storage_all_and_ind_1_sd_15$env_mean)
model_1_sd_30=lm(data_hist_storage_all_and_ind_1_sd_30$storage ~ data_hist_storage_all_and_ind_1_sd_30$env_mean)
model_1_sd_60=lm(data_hist_storage_all_and_ind_1_sd_60$storage ~ data_hist_storage_all_and_ind_1_sd_60$env_mean)

model_2_mean=lm(data_hist_storage_all_and_ind_2$storage ~ data_hist_storage_all_and_ind_2$env_sd)
model_2_mean_075=lm(data_hist_storage_all_and_ind_2_mean_075$storage ~ data_hist_storage_all_and_ind_2_mean_075$env_sd)
model_2_mean_085=lm(data_hist_storage_all_and_ind_2_mean_085$storage ~ data_hist_storage_all_and_ind_2_mean_085$env_sd)
model_2_sd=lm(data_hist_storage_all_and_ind_2$storage ~ data_hist_storage_all_and_ind_2$env_mean)
model_2_sd_0=lm(data_hist_storage_all_and_ind_2_sd_0$storage ~ data_hist_storage_all_and_ind_2_sd_0$env_mean)
model_2_sd_15=lm(data_hist_storage_all_and_ind_2_sd_15$storage ~ data_hist_storage_all_and_ind_2_sd_15$env_mean)
model_2_sd_30=lm(data_hist_storage_all_and_ind_2_sd_30$storage ~ data_hist_storage_all_and_ind_2_sd_30$env_mean)
model_2_sd_60=lm(data_hist_storage_all_and_ind_2_sd_60$storage ~ data_hist_storage_all_and_ind_2_sd_60$env_mean)

model_3_mean=lm(data_hist_storage_all_and_ind_3$storage ~ data_hist_storage_all_and_ind_3$env_sd)
model_3_mean_075=lm(data_hist_storage_all_and_ind_3_mean_075$storage ~ data_hist_storage_all_and_ind_3_mean_075$env_sd)
model_3_mean_085=lm(data_hist_storage_all_and_ind_3_mean_085$storage ~ data_hist_storage_all_and_ind_3_mean_085$env_sd)
model_3_sd=lm(data_hist_storage_all_and_ind_3$storage ~ data_hist_storage_all_and_ind_3$env_mean)
model_3_sd_0=lm(data_hist_storage_all_and_ind_3_sd_0$storage ~ data_hist_storage_all_and_ind_3_sd_0$env_mean)
model_3_sd_15=lm(data_hist_storage_all_and_ind_3_sd_15$storage ~ data_hist_storage_all_and_ind_3_sd_15$env_mean)
model_3_sd_30=lm(data_hist_storage_all_and_ind_3_sd_30$storage ~ data_hist_storage_all_and_ind_3_sd_30$env_mean)
model_3_sd_60=lm(data_hist_storage_all_and_ind_3_sd_60$storage ~ data_hist_storage_all_and_ind_3_sd_60$env_mean)

model_4_mean=lm(data_hist_storage_all_and_ind_4$storage ~ data_hist_storage_all_and_ind_4$env_sd)
model_4_mean_075=lm(data_hist_storage_all_and_ind_4_mean_075$storage ~ data_hist_storage_all_and_ind_4_mean_075$env_sd)
model_4_mean_085=lm(data_hist_storage_all_and_ind_4_mean_085$storage ~ data_hist_storage_all_and_ind_4_mean_085$env_sd)
model_4_sd=lm(data_hist_storage_all_and_ind_4$storage ~ data_hist_storage_all_and_ind_4$env_mean)
model_4_sd_0=lm(data_hist_storage_all_and_ind_4_sd_0$storage ~ data_hist_storage_all_and_ind_4_sd_0$env_mean)
model_4_sd_15=lm(data_hist_storage_all_and_ind_4_sd_15$storage ~ data_hist_storage_all_and_ind_4_sd_15$env_mean)
model_4_sd_30=lm(data_hist_storage_all_and_ind_4_sd_30$storage ~ data_hist_storage_all_and_ind_4_sd_30$env_mean)
model_4_sd_60=lm(data_hist_storage_all_and_ind_4_sd_60$storage ~ data_hist_storage_all_and_ind_4_sd_60$env_mean)

## THIS PART IS THE EXAMINATION OF ENVIRONMENT SD 

params_0_mean <- parameters::model_parameters(model_0_mean)
params_0_mean_075 <- parameters::model_parameters(model_0_mean_075)
params_0_mean_085 <- parameters::model_parameters(model_0_mean_085)

omega_sq_0_mean <- omega_squared(model_0_mean, partial = FALSE, ci = 0.95)
omega_interpret_0_mean <- interpret_omega_squared(omega_sq_0_mean$Omega2, rules = "field2013")
omega_sq_0_mean_075 <- omega_squared(model_0_mean_075, partial = FALSE, ci = 0.95)
omega_interpret_0_mean_075 <- interpret_omega_squared(omega_sq_0_mean_075$Omega2, rules = "field2013")
omega_sq_0_mean_085 <- omega_squared(model_0_mean_085, partial = FALSE, ci = 0.95)
omega_interpret_0_mean_085 <- interpret_omega_squared(omega_sq_0_mean_085$Omega2, rules = "field2013")

sp_0_sd_var <- data.frame(species_id = 0,
                          mean = "all",
                          all_params_1 = params_0_mean$Parameter[1],
                          all_params_2 = params_0_mean$Parameter[2],
                          all_coeff_1 = params_0_mean$Coefficient[1],
                          all_coeff_2 = params_0_mean$Coefficient[2],
                          all_coeff_ci_low_1 = params_0_mean$CI_low[1],
                          all_coeff_ci_low_2 = params_0_mean$CI_low[2],
                          all_coeff_ci_hi_1 = params_0_mean$CI_high[1],
                          all_coeff_ci_hi_2 = params_0_mean$CI_high[2],
                          all_p_val_1 = params_0_mean$p[1],
                          all_p_val_2 = params_0_mean$p[2],
                          all_omega_sq_1 = omega_sq_0_mean$Omega2[1],
                          all_omega_sq_2 = omega_sq_0_mean$Omega2[2],
                          all_omega_sq_ci_low_1 = omega_sq_0_mean$CI_low[1],
                          all_omega_sq_ci_low_2 = omega_sq_0_mean$CI_low[2],
                          all_omega_sq_ci_hi_1 = omega_sq_0_mean$CI_high[1],
                          all_omega_sq_ci_hi_2 = omega_sq_0_mean$CI_high[2],
                          all_omega_sq_interpret = omega_interpret_0_mean)
sp_0_sd_var_075 <- data.frame(species_id = 0,
                              mean = "medium",
                              all_params_1 = params_0_mean_075$Parameter[1],
                              all_params_2 = params_0_mean_075$Parameter[2],
                              all_coeff_1 = params_0_mean_075$Coefficient[1],
                              all_coeff_2 = params_0_mean_075$Coefficient[2],
                              all_coeff_ci_low_1 = params_0_mean_075$CI_low[1],
                              all_coeff_ci_low_2 = params_0_mean_075$CI_low[2],
                              all_coeff_ci_hi_1 = params_0_mean_075$CI_high[1],
                              all_coeff_ci_hi_2 = params_0_mean_075$CI_high[2],
                              all_p_val_1 = params_0_mean_075$p[1],
                              all_p_val_2 = params_0_mean_075$p[2],
                              all_omega_sq_1 = omega_sq_0_mean_075$Omega2[1],
                              all_omega_sq_2 = omega_sq_0_mean_075$Omega2[2],
                              all_omega_sq_ci_low_1 = omega_sq_0_mean_075$CI_low[1],
                              all_omega_sq_ci_low_2 = omega_sq_0_mean_075$CI_low[2],
                              all_omega_sq_ci_hi_1 = omega_sq_0_mean_075$CI_high[1],
                              all_omega_sq_ci_hi_2 = omega_sq_0_mean_075$CI_high[2],
                              all_omega_sq_interpret = omega_interpret_0_mean_075)
sp_0_sd_var_085 <- data.frame(species_id = 0,
                              mean = "low",
                              all_params_1 = params_0_mean_085$Parameter[1],
                              all_params_2 = params_0_mean_085$Parameter[2],
                              all_coeff_1 = params_0_mean_085$Coefficient[1],
                              all_coeff_2 = params_0_mean_085$Coefficient[2],
                              all_coeff_ci_low_1 = params_0_mean_085$CI_low[1],
                              all_coeff_ci_low_2 = params_0_mean_085$CI_low[2],
                              all_coeff_ci_hi_1 = params_0_mean_085$CI_high[1],
                              all_coeff_ci_hi_2 = params_0_mean_085$CI_high[2],
                              all_p_val_1 = params_0_mean_085$p[1],
                              all_p_val_2 = params_0_mean_085$p[2],
                              all_omega_sq_1 = omega_sq_0_mean_085$Omega2[1],
                              all_omega_sq_2 = omega_sq_0_mean_085$Omega2[2],
                              all_omega_sq_ci_low_1 = omega_sq_0_mean_085$CI_low[1],
                              all_omega_sq_ci_low_2 = omega_sq_0_mean_085$CI_low[2],
                              all_omega_sq_ci_hi_1 = omega_sq_0_mean_085$CI_high[1],
                              all_omega_sq_ci_hi_2 = omega_sq_0_mean_085$CI_high[2],
                              all_omega_sq_interpret = omega_interpret_0_mean_085)

sp_0_sd_var <- plyr::rbind.fill(sp_0_sd_var, sp_0_sd_var_075, sp_0_sd_var_085)

params_1_mean <- parameters::model_parameters(model_1_mean)
params_1_mean_075 <- parameters::model_parameters(model_1_mean_075)
params_1_mean_085 <- parameters::model_parameters(model_1_mean_085)

omega_sq_1_mean <- omega_squared(model_1_mean, partial = FALSE, ci = 0.95)
omega_interpret_1_mean <- interpret_omega_squared(omega_sq_1_mean$Omega2, rules = "field2013")
omega_sq_1_mean_075 <- omega_squared(model_1_mean_075, partial = FALSE, ci = 0.95)
omega_interpret_1_mean_075 <- interpret_omega_squared(omega_sq_1_mean_075$Omega2, rules = "field2013")
omega_sq_1_mean_085 <- omega_squared(model_1_mean_085, partial = FALSE, ci = 0.95)
omega_interpret_1_mean_085 <- interpret_omega_squared(omega_sq_1_mean_085$Omega2, rules = "field2013")

sp_1_sd_var <- data.frame(species_id = 1,
                          mean = "all",
                          all_params_1 = params_1_mean$Parameter[1],
                          all_params_2 = params_1_mean$Parameter[2],
                          all_coeff_1 = params_1_mean$Coefficient[1],
                          all_coeff_2 = params_1_mean$Coefficient[2],
                          all_coeff_ci_low_1 = params_1_mean$CI_low[1],
                          all_coeff_ci_low_2 = params_1_mean$CI_low[2],
                          all_coeff_ci_hi_1 = params_1_mean$CI_high[1],
                          all_coeff_ci_hi_2 = params_1_mean$CI_high[2],
                          all_p_val_1 = params_1_mean$p[1],
                          all_p_val_2 = params_1_mean$p[2],
                          all_omega_sq_1 = omega_sq_1_mean$Omega2[1],
                          all_omega_sq_2 = omega_sq_1_mean$Omega2[2],
                          all_omega_sq_ci_low_1 = omega_sq_1_mean$CI_low[1],
                          all_omega_sq_ci_low_2 = omega_sq_1_mean$CI_low[2],
                          all_omega_sq_ci_hi_1 = omega_sq_1_mean$CI_high[1],
                          all_omega_sq_ci_hi_2 = omega_sq_1_mean$CI_high[2],
                          all_omega_sq_interpret = omega_interpret_1_mean)
sp_1_sd_var_075 <- data.frame(species_id = 1,
                              mean = "medium",
                              all_params_1 = params_1_mean_075$Parameter[1],
                              all_params_2 = params_1_mean_075$Parameter[2],
                              all_coeff_1 = params_1_mean_075$Coefficient[1],
                              all_coeff_2 = params_1_mean_075$Coefficient[2],
                              all_coeff_ci_low_1 = params_1_mean_075$CI_low[1],
                              all_coeff_ci_low_2 = params_1_mean_075$CI_low[2],
                              all_coeff_ci_hi_1 = params_1_mean_075$CI_high[1],
                              all_coeff_ci_hi_2 = params_1_mean_075$CI_high[2],
                              all_p_val_1 = params_1_mean_075$p[1],
                              all_p_val_2 = params_1_mean_075$p[2],
                              all_omega_sq_1 = omega_sq_1_mean_075$Omega2[1],
                              all_omega_sq_2 = omega_sq_1_mean_075$Omega2[2],
                              all_omega_sq_ci_low_1 = omega_sq_1_mean_075$CI_low[1],
                              all_omega_sq_ci_low_2 = omega_sq_1_mean_075$CI_low[2],
                              all_omega_sq_ci_hi_1 = omega_sq_1_mean_075$CI_high[1],
                              all_omega_sq_ci_hi_2 = omega_sq_1_mean_075$CI_high[2],
                              all_omega_sq_interpret = omega_interpret_1_mean_075)
sp_1_sd_var_085 <- data.frame(species_id = 1,
                              mean = "low",
                              all_params_1 = params_1_mean_085$Parameter[1],
                              all_params_2 = params_1_mean_085$Parameter[2],
                              all_coeff_1 = params_1_mean_085$Coefficient[1],
                              all_coeff_2 = params_1_mean_085$Coefficient[2],
                              all_coeff_ci_low_1 = params_1_mean_085$CI_low[1],
                              all_coeff_ci_low_2 = params_1_mean_085$CI_low[2],
                              all_coeff_ci_hi_1 = params_1_mean_085$CI_high[1],
                              all_coeff_ci_hi_2 = params_1_mean_085$CI_high[2],
                              all_p_val_1 = params_1_mean_085$p[1],
                              all_p_val_2 = params_1_mean_085$p[2],
                              all_omega_sq_1 = omega_sq_1_mean_085$Omega2[1],
                              all_omega_sq_2 = omega_sq_1_mean_085$Omega2[2],
                              all_omega_sq_ci_low_1 = omega_sq_1_mean_085$CI_low[1],
                              all_omega_sq_ci_low_2 = omega_sq_1_mean_085$CI_low[2],
                              all_omega_sq_ci_hi_1 = omega_sq_1_mean_085$CI_high[1],
                              all_omega_sq_ci_hi_2 = omega_sq_1_mean_085$CI_high[2],
                              all_omega_sq_interpret = omega_interpret_1_mean_085)

sp_1_sd_var <- plyr::rbind.fill(sp_1_sd_var, sp_1_sd_var_075, sp_1_sd_var_085)

params_2_mean <- parameters::model_parameters(model_1_mean)
params_2_mean_075 <- parameters::model_parameters(model_1_mean_075)
params_2_mean_085 <- parameters::model_parameters(model_1_mean_085)

omega_sq_2_mean <- omega_squared(model_2_mean, partial = FALSE, ci = 0.95)
omega_interpret_2_mean <- interpret_omega_squared(omega_sq_2_mean$Omega2, rules = "field2013")
omega_sq_2_mean_075 <- omega_squared(model_2_mean_075, partial = FALSE, ci = 0.95)
omega_interpret_2_mean_075 <- interpret_omega_squared(omega_sq_2_mean_075$Omega2, rules = "field2013")
omega_sq_2_mean_085 <- omega_squared(model_2_mean_085, partial = FALSE, ci = 0.95)
omega_interpret_2_mean_085 <- interpret_omega_squared(omega_sq_2_mean_085$Omega2, rules = "field2013")

sp_2_sd_var <- data.frame(species_id = 2,
                          mean = "all",
                          all_params_1 = params_2_mean$Parameter[1],
                          all_params_2 = params_2_mean$Parameter[2],
                          all_coeff_1 = params_2_mean$Coefficient[1],
                          all_coeff_2 = params_2_mean$Coefficient[2],
                          all_coeff_ci_low_1 = params_2_mean$CI_low[1],
                          all_coeff_ci_low_2 = params_2_mean$CI_low[2],
                          all_coeff_ci_hi_1 = params_2_mean$CI_high[1],
                          all_coeff_ci_hi_2 = params_2_mean$CI_high[2],
                          all_p_val_1 = params_2_mean$p[1],
                          all_p_val_2 = params_2_mean$p[2],
                          all_omega_sq_1 = omega_sq_2_mean$Omega2[1],
                          all_omega_sq_2 = omega_sq_2_mean$Omega2[2],
                          all_omega_sq_ci_low_1 = omega_sq_2_mean$CI_low[1],
                          all_omega_sq_ci_low_2 = omega_sq_2_mean$CI_low[2],
                          all_omega_sq_ci_hi_1 = omega_sq_2_mean$CI_high[1],
                          all_omega_sq_ci_hi_2 = omega_sq_2_mean$CI_high[2],
                          all_omega_sq_interpret = omega_interpret_2_mean)
sp_2_sd_var_075 <- data.frame(species_id = 2,
                              mean = "medium",
                              all_params_1 = params_2_mean_075$Parameter[1],
                              all_params_2 = params_2_mean_075$Parameter[2],
                              all_coeff_1 = params_2_mean_075$Coefficient[1],
                              all_coeff_2 = params_2_mean_075$Coefficient[2],
                              all_coeff_ci_low_1 = params_2_mean_075$CI_low[1],
                              all_coeff_ci_low_2 = params_2_mean_075$CI_low[2],
                              all_coeff_ci_hi_1 = params_2_mean_075$CI_high[1],
                              all_coeff_ci_hi_2 = params_2_mean_075$CI_high[2],
                              all_p_val_1 = params_2_mean_075$p[1],
                              all_p_val_2 = params_2_mean_075$p[2],
                              all_omega_sq_1 = omega_sq_2_mean_075$Omega2[1],
                              all_omega_sq_2 = omega_sq_2_mean_075$Omega2[2],
                              all_omega_sq_ci_low_1 = omega_sq_2_mean_075$CI_low[1],
                              all_omega_sq_ci_low_2 = omega_sq_2_mean_075$CI_low[2],
                              all_omega_sq_ci_hi_1 = omega_sq_2_mean_075$CI_high[1],
                              all_omega_sq_ci_hi_2 = omega_sq_2_mean_075$CI_high[2],
                              all_omega_sq_interpret = omega_interpret_2_mean_075)
sp_2_sd_var_085 <- data.frame(species_id = 2,
                              mean = "low",
                              all_params_1 = params_2_mean_085$Parameter[1],
                              all_params_2 = params_2_mean_085$Parameter[2],
                              all_coeff_1 = params_2_mean_085$Coefficient[1],
                              all_coeff_2 = params_2_mean_085$Coefficient[2],
                              all_coeff_ci_low_1 = params_2_mean_085$CI_low[1],
                              all_coeff_ci_low_2 = params_2_mean_085$CI_low[2],
                              all_coeff_ci_hi_1 = params_2_mean_085$CI_high[1],
                              all_coeff_ci_hi_2 = params_2_mean_085$CI_high[2],
                              all_p_val_1 = params_2_mean_085$p[1],
                              all_p_val_2 = params_2_mean_085$p[2],
                              all_omega_sq_1 = omega_sq_2_mean_085$Omega2[1],
                              all_omega_sq_2 = omega_sq_2_mean_085$Omega2[2],
                              all_omega_sq_ci_low_1 = omega_sq_2_mean_085$CI_low[1],
                              all_omega_sq_ci_low_2 = omega_sq_2_mean_085$CI_low[2],
                              all_omega_sq_ci_hi_1 = omega_sq_2_mean_085$CI_high[1],
                              all_omega_sq_ci_hi_2 = omega_sq_2_mean_085$CI_high[2],
                              all_omega_sq_interpret = omega_interpret_2_mean_085)

sp_2_sd_var <- plyr::rbind.fill(sp_2_sd_var, sp_2_sd_var_075, sp_2_sd_var_085)

params_3_mean <- parameters::model_parameters(model_3_mean)
params_3_mean_075 <- parameters::model_parameters(model_3_mean_075)
params_3_mean_085 <- parameters::model_parameters(model_3_mean_085)

omega_sq_3_mean <- omega_squared(model_3_mean, partial = FALSE, ci = 0.95)
omega_interpret_3_mean <- interpret_omega_squared(omega_sq_3_mean$Omega2, rules = "field2013")
omega_sq_3_mean_075 <- omega_squared(model_3_mean_075, partial = FALSE, ci = 0.95)
omega_interpret_3_mean_075 <- interpret_omega_squared(omega_sq_3_mean_075$Omega2, rules = "field2013")
omega_sq_3_mean_085 <- omega_squared(model_3_mean_085, partial = FALSE, ci = 0.95)
omega_interpret_3_mean_085 <- interpret_omega_squared(omega_sq_3_mean_085$Omega2, rules = "field2013")

sp_3_sd_var <- data.frame(species_id = 3,
                          mean = "all",
                          all_params_1 = params_3_mean$Parameter[1],
                          all_params_2 = params_3_mean$Parameter[2],
                          all_coeff_1 = params_3_mean$Coefficient[1],
                          all_coeff_2 = params_3_mean$Coefficient[2],
                          all_coeff_ci_low_1 = params_3_mean$CI_low[1],
                          all_coeff_ci_low_2 = params_3_mean$CI_low[2],
                          all_coeff_ci_hi_1 = params_3_mean$CI_high[1],
                          all_coeff_ci_hi_2 = params_3_mean$CI_high[2],
                          all_p_val_1 = params_3_mean$p[1],
                          all_p_val_2 = params_3_mean$p[2],
                          all_omega_sq_1 = omega_sq_3_mean$Omega2[1],
                          all_omega_sq_2 = omega_sq_3_mean$Omega2[2],
                          all_omega_sq_ci_low_1 = omega_sq_3_mean$CI_low[1],
                          all_omega_sq_ci_low_2 = omega_sq_3_mean$CI_low[2],
                          all_omega_sq_ci_hi_1 = omega_sq_3_mean$CI_high[1],
                          all_omega_sq_ci_hi_2 = omega_sq_3_mean$CI_high[2],
                          all_omega_sq_interpret = omega_interpret_3_mean)
sp_3_sd_var_075 <- data.frame(species_id = 3,
                              mean = "medium",
                              all_params_1 = params_3_mean_075$Parameter[1],
                              all_params_2 = params_3_mean_075$Parameter[2],
                              all_coeff_1 = params_3_mean_075$Coefficient[1],
                              all_coeff_2 = params_3_mean_075$Coefficient[2],
                              all_coeff_ci_low_1 = params_3_mean_075$CI_low[1],
                              all_coeff_ci_low_2 = params_3_mean_075$CI_low[2],
                              all_coeff_ci_hi_1 = params_3_mean_075$CI_high[1],
                              all_coeff_ci_hi_2 = params_3_mean_075$CI_high[2],
                              all_p_val_1 = params_3_mean_075$p[1],
                              all_p_val_2 = params_3_mean_075$p[2],
                              all_omega_sq_1 = omega_sq_3_mean_075$Omega2[1],
                              all_omega_sq_2 = omega_sq_3_mean_075$Omega2[2],
                              all_omega_sq_ci_low_1 = omega_sq_3_mean_075$CI_low[1],
                              all_omega_sq_ci_low_2 = omega_sq_3_mean_075$CI_low[2],
                              all_omega_sq_ci_hi_1 = omega_sq_3_mean_075$CI_high[1],
                              all_omega_sq_ci_hi_2 = omega_sq_3_mean_075$CI_high[2],
                              all_omega_sq_interpret = omega_interpret_3_mean_075)
sp_3_sd_var_085 <- data.frame(species_id = 3,
                              mean = "low",
                              all_params_1 = params_3_mean_085$Parameter[1],
                              all_params_2 = params_3_mean_085$Parameter[2],
                              all_coeff_1 = params_3_mean_085$Coefficient[1],
                              all_coeff_2 = params_3_mean_085$Coefficient[2],
                              all_coeff_ci_low_1 = params_3_mean_085$CI_low[1],
                              all_coeff_ci_low_2 = params_3_mean_085$CI_low[2],
                              all_coeff_ci_hi_1 = params_3_mean_085$CI_high[1],
                              all_coeff_ci_hi_2 = params_3_mean_085$CI_high[2],
                              all_p_val_1 = params_3_mean_085$p[1],
                              all_p_val_2 = params_3_mean_085$p[2],
                              all_omega_sq_1 = omega_sq_3_mean_085$Omega2[1],
                              all_omega_sq_2 = omega_sq_3_mean_085$Omega2[2],
                              all_omega_sq_ci_low_1 = omega_sq_3_mean_085$CI_low[1],
                              all_omega_sq_ci_low_2 = omega_sq_3_mean_085$CI_low[2],
                              all_omega_sq_ci_hi_1 = omega_sq_3_mean_085$CI_high[1],
                              all_omega_sq_ci_hi_2 = omega_sq_3_mean_085$CI_high[2],
                              all_omega_sq_interpret = omega_interpret_3_mean_085)

sp_3_sd_var <- plyr::rbind.fill(sp_3_sd_var, sp_3_sd_var_075, sp_3_sd_var_085)

params_4_mean <- parameters::model_parameters(model_4_mean)
params_4_mean_075 <- parameters::model_parameters(model_4_mean_075)
params_4_mean_085 <- parameters::model_parameters(model_4_mean_085)

omega_sq_4_mean <- omega_squared(model_4_mean, partial = FALSE, ci = 0.95)
omega_interpret_4_mean <- interpret_omega_squared(omega_sq_4_mean$Omega2, rules = "field2013")
omega_sq_4_mean_075 <- omega_squared(model_4_mean_075, partial = FALSE, ci = 0.95)
omega_interpret_4_mean_075 <- interpret_omega_squared(omega_sq_4_mean_075$Omega2, rules = "field2013")
omega_sq_4_mean_085 <- omega_squared(model_4_mean_085, partial = FALSE, ci = 0.95)
omega_interpret_4_mean_085 <- interpret_omega_squared(omega_sq_4_mean_085$Omega2, rules = "field2013")

sp_4_sd_var <- data.frame(species_id = 4,
                          mean = "all",
                          all_params_1 = params_4_mean$Parameter[1],
                          all_params_2 = params_4_mean$Parameter[2],
                          all_coeff_1 = params_4_mean$Coefficient[1],
                          all_coeff_2 = params_4_mean$Coefficient[2],
                          all_coeff_ci_low_1 = params_4_mean$CI_low[1],
                          all_coeff_ci_low_2 = params_4_mean$CI_low[2],
                          all_coeff_ci_hi_1 = params_4_mean$CI_high[1],
                          all_coeff_ci_hi_2 = params_4_mean$CI_high[2],
                          all_p_val_1 = params_4_mean$p[1],
                          all_p_val_2 = params_4_mean$p[2],
                          all_omega_sq_1 = omega_sq_4_mean$Omega2[1],
                          all_omega_sq_2 = omega_sq_4_mean$Omega2[2],
                          all_omega_sq_ci_low_1 = omega_sq_4_mean$CI_low[1],
                          all_omega_sq_ci_low_2 = omega_sq_4_mean$CI_low[2],
                          all_omega_sq_ci_hi_1 = omega_sq_4_mean$CI_high[1],
                          all_omega_sq_ci_hi_2 = omega_sq_4_mean$CI_high[2],
                          all_omega_sq_interpret = omega_interpret_4_mean)
sp_4_sd_var_075 <- data.frame(species_id = 4,
                              mean = "medium",
                              all_params_1 = params_4_mean_075$Parameter[1],
                              all_params_2 = params_4_mean_075$Parameter[2],
                              all_coeff_1 = params_4_mean_075$Coefficient[1],
                              all_coeff_2 = params_4_mean_075$Coefficient[2],
                              all_coeff_ci_low_1 = params_4_mean_075$CI_low[1],
                              all_coeff_ci_low_2 = params_4_mean_075$CI_low[2],
                              all_coeff_ci_hi_1 = params_4_mean_075$CI_high[1],
                              all_coeff_ci_hi_2 = params_4_mean_075$CI_high[2],
                              all_p_val_1 = params_4_mean_075$p[1],
                              all_p_val_2 = params_4_mean_075$p[2],
                              all_omega_sq_1 = omega_sq_4_mean_075$Omega2[1],
                              all_omega_sq_2 = omega_sq_4_mean_075$Omega2[2],
                              all_omega_sq_ci_low_1 = omega_sq_4_mean_075$CI_low[1],
                              all_omega_sq_ci_low_2 = omega_sq_4_mean_075$CI_low[2],
                              all_omega_sq_ci_hi_1 = omega_sq_4_mean_075$CI_high[1],
                              all_omega_sq_ci_hi_2 = omega_sq_4_mean_075$CI_high[2],
                              all_omega_sq_interpret = omega_interpret_4_mean_075)
sp_4_sd_var_085 <- data.frame(species_id = 4,
                              mean = "low",
                              all_params_1 = params_4_mean_085$Parameter[1],
                              all_params_2 = params_4_mean_085$Parameter[2],
                              all_coeff_1 = params_4_mean_085$Coefficient[1],
                              all_coeff_2 = params_4_mean_085$Coefficient[2],
                              all_coeff_ci_low_1 = params_4_mean_085$CI_low[1],
                              all_coeff_ci_low_2 = params_4_mean_085$CI_low[2],
                              all_coeff_ci_hi_1 = params_4_mean_085$CI_high[1],
                              all_coeff_ci_hi_2 = params_4_mean_085$CI_high[2],
                              all_p_val_1 = params_4_mean_085$p[1],
                              all_p_val_2 = params_4_mean_085$p[2],
                              all_omega_sq_1 = omega_sq_4_mean_085$Omega2[1],
                              all_omega_sq_2 = omega_sq_4_mean_085$Omega2[2],
                              all_omega_sq_ci_low_1 = omega_sq_4_mean_085$CI_low[1],
                              all_omega_sq_ci_low_2 = omega_sq_4_mean_085$CI_low[2],
                              all_omega_sq_ci_hi_1 = omega_sq_4_mean_085$CI_high[1],
                              all_omega_sq_ci_hi_2 = omega_sq_4_mean_085$CI_high[2],
                              all_omega_sq_interpret = omega_interpret_4_mean_085)

sp_4_sd_var <- plyr::rbind.fill(sp_4_sd_var, sp_4_sd_var_075, sp_4_sd_var_085)

storage_sd_var <- plyr::rbind.fill(sp_0_sd_var, sp_1_sd_var, sp_2_sd_var, sp_3_sd_var, sp_4_sd_var)
write.csv(storage_sd_var, file = "out/storage_conc_sd_var_stats.csv")

## THIS PART IS THE EXAMINATION OF ENVIRONMENT MEAN 

params_0_sd <- parameters::model_parameters(aov(model_0_sd))
params_0_sd_0 <- parameters::model_parameters(aov(model_0_sd_0))
params_0_sd_15 <- parameters::model_parameters(aov(model_0_sd_15))
params_0_sd_30 <- parameters::model_parameters(aov(model_0_sd_30))
params_0_sd_60 <- parameters::model_parameters(aov(model_0_sd_60))

omega_sq_0_sd <- omega_squared(aov(model_0_sd), partial = FALSE, ci = 0.95)
omega_interpret_0_sd <- interpret_omega_squared(omega_sq_0_sd$Omega2, rules = "field2013")
omega_sq_0_sd_0 <- omega_squared(aov(model_0_sd_0), partial = FALSE, ci = 0.95)
omega_interpret_0_sd_0 <- interpret_omega_squared(omega_sq_0_sd_0$Omega2, rules = "field2013")
omega_sq_0_sd_15 <- omega_squared(aov(model_0_sd_15), partial = FALSE, ci = 0.95)
omega_interpret_0_sd_15 <- interpret_omega_squared(omega_sq_0_sd_15$Omega2, rules = "field2013")
omega_sq_0_sd_30 <- omega_squared(aov(model_0_sd_30), partial = FALSE, ci = 0.95)
omega_interpret_0_sd_30 <- interpret_omega_squared(omega_sq_0_sd_30$Omega2, rules = "field2013")
omega_sq_0_sd_60 <- omega_squared(aov(model_0_sd_60), partial = FALSE, ci = 0.95)
omega_interpret_0_sd_60 <- interpret_omega_squared(omega_sq_0_sd_60$Omega2, rules = "field2013")

sp_0_mean_var <- data.frame(species_id = 0,
                            sd = "all",
                            all_params_1 = params_0_sd$Parameter[1],
                            all_p_val_1 = params_0_sd$p[1],
                            all_omega_sq_1 = omega_sq_0_sd$Omega2[1],
                            all_omega_sq_ci_low_1 = omega_sq_0_sd$CI_low[1],
                            all_omega_sq_ci_hi_1 = omega_sq_0_sd$CI_high[1],
                            all_omega_sq_interpret = omega_interpret_0_sd)
sp_0_mean_var_0 <- data.frame(species_id = 0,
                              sd = "none",
                              all_params_1 = params_0_sd_0$Parameter[1],
                              all_p_val_1 = params_0_sd_0$p[1],
                              all_omega_sq_1 = omega_sq_0_sd_0$Omega2[1],
                              all_omega_sq_ci_low_1 = omega_sq_0_sd_0$CI_low[1],
                              all_omega_sq_ci_hi_1 = omega_sq_0_sd_0$CI_high[1],
                              all_omega_sq_interpret = omega_interpret_0_sd_0)
sp_0_mean_var_15 <- data.frame(species_id = 0,
                               sd = "low",
                               all_params_1 = params_0_sd_15$Parameter[1],
                               all_p_val_1 = params_0_sd_15$p[1],
                               all_omega_sq_1 = omega_sq_0_sd_15$Omega2[1],
                               all_omega_sq_ci_low_1 = omega_sq_0_sd_15$CI_low[1],
                               all_omega_sq_ci_hi_1 = omega_sq_0_sd_15$CI_high[1],
                               all_omega_sq_interpret = omega_interpret_0_sd_15)
sp_0_mean_var_30 <- data.frame(species_id = 0,
                               sd = "medium",
                               all_params_1 = params_0_sd_30$Parameter[1],
                               all_p_val_1 = params_0_sd_30$p[1],
                               all_omega_sq_1 = omega_sq_0_sd_30$Omega2[1],
                               all_omega_sq_ci_low_1 = omega_sq_0_sd_30$CI_low[1],
                               all_omega_sq_ci_hi_1 = omega_sq_0_sd_30$CI_high[1],
                               all_omega_sq_interpret = omega_interpret_0_sd_30)
sp_0_mean_var_60 <- data.frame(species_id = 0,
                               sd = "high",
                               all_params_1 = params_0_sd_60$Parameter[1],
                               all_p_val_1 = params_0_sd_60$p[1],
                               all_omega_sq_1 = omega_sq_0_sd_60$Omega2[1],
                               all_omega_sq_ci_low_1 = omega_sq_0_sd_60$CI_low[1],
                               all_omega_sq_ci_hi_1 = omega_sq_0_sd_60$CI_high[1],
                               all_omega_sq_interpret = omega_interpret_0_sd_60)

sp_0_mean_var <- plyr::rbind.fill(sp_0_mean_var, sp_0_mean_var_0, sp_0_mean_var_15, sp_0_mean_var_30, sp_0_mean_var_60)

params_1_sd <- parameters::model_parameters(aov(model_1_sd))
params_1_sd_0 <- parameters::model_parameters(aov(model_1_sd_0))
params_1_sd_15 <- parameters::model_parameters(aov(model_1_sd_15))
params_1_sd_30 <- parameters::model_parameters(aov(model_1_sd_30))
params_1_sd_60 <- parameters::model_parameters(aov(model_1_sd_60))

omega_sq_1_sd <- omega_squared(aov(model_1_sd), partial = FALSE, ci = 0.95)
omega_interpret_1_sd <- interpret_omega_squared(omega_sq_1_sd$Omega2, rules = "field2013")
omega_sq_1_sd_0 <- omega_squared(aov(model_1_sd_0), partial = FALSE, ci = 0.95)
omega_interpret_1_sd_0 <- interpret_omega_squared(omega_sq_1_sd_0$Omega2, rules = "field2013")
omega_sq_1_sd_15 <- omega_squared(aov(model_1_sd_15), partial = FALSE, ci = 0.95)
omega_interpret_1_sd_15 <- interpret_omega_squared(omega_sq_1_sd_15$Omega2, rules = "field2013")
omega_sq_1_sd_30 <- omega_squared(aov(model_1_sd_30), partial = FALSE, ci = 0.95)
omega_interpret_1_sd_30 <- interpret_omega_squared(omega_sq_1_sd_30$Omega2, rules = "field2013")
omega_sq_1_sd_60 <- omega_squared(aov(model_1_sd_60), partial = FALSE, ci = 0.95)
omega_interpret_1_sd_60 <- interpret_omega_squared(omega_sq_1_sd_60$Omega2, rules = "field2013")

sp_1_mean_var <- data.frame(species_id = 1,
                            sd = "all",
                            all_params_1 = params_1_sd$Parameter[1],
                            all_p_val_1 = params_1_sd$p[1],
                            all_omega_sq_1 = omega_sq_1_sd$Omega2[1],
                            all_omega_sq_ci_low_1 = omega_sq_1_sd$CI_low[1],
                            all_omega_sq_ci_hi_1 = omega_sq_1_sd$CI_high[1],
                            all_omega_sq_interpret = omega_interpret_1_sd)
sp_1_mean_var_0 <- data.frame(species_id = 1,
                              sd = "none",
                              all_params_1 = params_1_sd_0$Parameter[1],
                              all_p_val_1 = params_1_sd_0$p[1],
                              all_omega_sq_1 = omega_sq_1_sd_0$Omega2[1],
                              all_omega_sq_ci_low_1 = omega_sq_1_sd_0$CI_low[1],
                              all_omega_sq_ci_hi_1 = omega_sq_1_sd_0$CI_high[1],
                              all_omega_sq_interpret = omega_interpret_1_sd_0)
sp_1_mean_var_15 <- data.frame(species_id = 1,
                               sd = "low",
                               all_params_1 = params_1_sd_15$Parameter[1],
                               all_p_val_1 = params_1_sd_15$p[1],
                               all_omega_sq_1 = omega_sq_1_sd_15$Omega2[1],
                               all_omega_sq_ci_low_1 = omega_sq_1_sd_15$CI_low[1],
                               all_omega_sq_ci_hi_1 = omega_sq_1_sd_15$CI_high[1],
                               all_omega_sq_interpret = omega_interpret_1_sd_15)
sp_1_mean_var_30 <- data.frame(species_id = 1,
                               sd = "medium",
                               all_params_1 = params_1_sd_30$Parameter[1],
                               all_p_val_1 = params_1_sd_30$p[1],
                               all_omega_sq_1 = omega_sq_1_sd_30$Omega2[1],
                               all_omega_sq_ci_low_1 = omega_sq_1_sd_30$CI_low[1],
                               all_omega_sq_ci_hi_1 = omega_sq_1_sd_30$CI_high[1],
                               all_omega_sq_interpret = omega_interpret_1_sd_30)
sp_1_mean_var_60 <- data.frame(species_id = 1,
                               sd = "high",
                               all_params_1 = params_1_sd_60$Parameter[1],
                               all_p_val_1 = params_1_sd_60$p[1],
                               all_omega_sq_1 = omega_sq_1_sd_60$Omega2[1],
                               all_omega_sq_ci_low_1 = omega_sq_1_sd_60$CI_low[1],
                               all_omega_sq_ci_hi_1 = omega_sq_1_sd_60$CI_high[1],
                               all_omega_sq_interpret = omega_interpret_1_sd_60)

sp_1_mean_var <- plyr::rbind.fill(sp_1_mean_var, sp_1_mean_var_0, sp_1_mean_var_15, sp_1_mean_var_30, sp_1_mean_var_60)

params_2_sd <- parameters::model_parameters(aov(model_2_sd))
params_2_sd_0 <- parameters::model_parameters(aov(model_2_sd_0))
params_2_sd_15 <- parameters::model_parameters(aov(model_2_sd_15))
params_2_sd_30 <- parameters::model_parameters(aov(model_2_sd_30))
params_2_sd_60 <- parameters::model_parameters(aov(model_2_sd_60))

omega_sq_2_sd <- omega_squared(aov(model_2_sd), partial = FALSE, ci = 0.95)
omega_interpret_2_sd <- interpret_omega_squared(omega_sq_2_sd$Omega2, rules = "field2013")
omega_sq_2_sd_0 <- omega_squared(aov(model_2_sd_0), partial = FALSE, ci = 0.95)
omega_interpret_2_sd_0 <- interpret_omega_squared(omega_sq_2_sd_0$Omega2, rules = "field2013")
omega_sq_2_sd_15 <- omega_squared(aov(model_2_sd_15), partial = FALSE, ci = 0.95)
omega_interpret_2_sd_15 <- interpret_omega_squared(omega_sq_2_sd_15$Omega2, rules = "field2013")
omega_sq_2_sd_30 <- omega_squared(aov(model_2_sd_30), partial = FALSE, ci = 0.95)
omega_interpret_2_sd_30 <- interpret_omega_squared(omega_sq_2_sd_30$Omega2, rules = "field2013")
omega_sq_2_sd_60 <- omega_squared(aov(model_2_sd_60), partial = FALSE, ci = 0.95)
omega_interpret_2_sd_60 <- interpret_omega_squared(omega_sq_2_sd_60$Omega2, rules = "field2013")

sp_2_mean_var <- data.frame(species_id = 2,
                            sd = "all",
                            all_params_1 = params_2_sd$Parameter[1],
                            all_p_val_1 = params_2_sd$p[1],
                            all_omega_sq_1 = omega_sq_2_sd$Omega2[1],
                            all_omega_sq_ci_low_1 = omega_sq_2_sd$CI_low[1],
                            all_omega_sq_ci_hi_1 = omega_sq_2_sd$CI_high[1],
                            all_omega_sq_interpret = omega_interpret_2_sd)
sp_2_mean_var_0 <- data.frame(species_id = 2,
                              sd = "none",
                              all_params_1 = params_2_sd_0$Parameter[1],
                              all_p_val_1 = params_2_sd_0$p[1],
                              all_omega_sq_1 = omega_sq_2_sd_0$Omega2[1],
                              all_omega_sq_ci_low_1 = omega_sq_2_sd_0$CI_low[1],
                              all_omega_sq_ci_hi_1 = omega_sq_2_sd_0$CI_high[1],
                              all_omega_sq_interpret = omega_interpret_2_sd_0)
sp_2_mean_var_15 <- data.frame(species_id = 2,
                               sd = "low",
                               all_params_1 = params_2_sd_15$Parameter[1],
                               all_p_val_1 = params_2_sd_15$p[1],
                               all_omega_sq_1 = omega_sq_2_sd_15$Omega2[1],
                               all_omega_sq_ci_low_1 = omega_sq_2_sd_15$CI_low[1],
                               all_omega_sq_ci_hi_1 = omega_sq_2_sd_15$CI_high[1],
                               all_omega_sq_interpret = omega_interpret_2_sd_15)
sp_2_mean_var_30 <- data.frame(species_id = 2,
                               sd = "medium",
                               all_params_1 = params_2_sd_30$Parameter[1],
                               all_p_val_1 = params_2_sd_30$p[1],
                               all_omega_sq_1 = omega_sq_2_sd_30$Omega2[1],
                               all_omega_sq_ci_low_1 = omega_sq_2_sd_30$CI_low[1],
                               all_omega_sq_ci_hi_1 = omega_sq_2_sd_30$CI_high[1],
                               all_omega_sq_interpret = omega_interpret_2_sd_30)
sp_2_mean_var_60 <- data.frame(species_id = 2,
                               sd = "high",
                               all_params_1 = params_2_sd_60$Parameter[1],
                               all_p_val_1 = params_2_sd_60$p[1],
                               all_omega_sq_1 = omega_sq_2_sd_60$Omega2[1],
                               all_omega_sq_ci_low_1 = omega_sq_2_sd_60$CI_low[1],
                               all_omega_sq_ci_hi_1 = omega_sq_2_sd_60$CI_high[1],
                               all_omega_sq_interpret = omega_interpret_2_sd_60)

sp_2_mean_var <- plyr::rbind.fill(sp_2_mean_var, sp_2_mean_var_0, sp_2_mean_var_15, sp_2_mean_var_30, sp_2_mean_var_60)

params_3_sd <- parameters::model_parameters(aov(model_3_sd))
params_3_sd_0 <- parameters::model_parameters(aov(model_3_sd_0))
params_3_sd_15 <- parameters::model_parameters(aov(model_3_sd_15))
params_3_sd_30 <- parameters::model_parameters(aov(model_3_sd_30))
params_3_sd_60 <- parameters::model_parameters(aov(model_3_sd_60))

omega_sq_3_sd <- omega_squared(aov(model_3_sd), partial = FALSE, ci = 0.95)
omega_interpret_3_sd <- interpret_omega_squared(omega_sq_3_sd$Omega2, rules = "field2013")
omega_sq_3_sd_0 <- omega_squared(aov(model_3_sd_0), partial = FALSE, ci = 0.95)
omega_interpret_3_sd_0 <- interpret_omega_squared(omega_sq_3_sd_0$Omega2, rules = "field2013")
omega_sq_3_sd_15 <- omega_squared(aov(model_3_sd_15), partial = FALSE, ci = 0.95)
omega_interpret_3_sd_15 <- interpret_omega_squared(omega_sq_3_sd_15$Omega2, rules = "field2013")
omega_sq_3_sd_30 <- omega_squared(aov(model_3_sd_30), partial = FALSE, ci = 0.95)
omega_interpret_3_sd_30 <- interpret_omega_squared(omega_sq_3_sd_30$Omega2, rules = "field2013")
omega_sq_3_sd_60 <- omega_squared(aov(model_3_sd_60), partial = FALSE, ci = 0.95)
omega_interpret_3_sd_60 <- interpret_omega_squared(omega_sq_3_sd_60$Omega2, rules = "field2013")

sp_3_mean_var <- data.frame(species_id = 3,
                            sd = "all",
                            all_params_1 = params_3_sd$Parameter[1],
                            all_p_val_1 = params_3_sd$p[1],
                            all_omega_sq_1 = omega_sq_3_sd$Omega2[1],
                            all_omega_sq_ci_low_1 = omega_sq_3_sd$CI_low[1],
                            all_omega_sq_ci_hi_1 = omega_sq_3_sd$CI_high[1],
                            all_omega_sq_interpret = omega_interpret_3_sd)
sp_3_mean_var_0 <- data.frame(species_id = 3,
                              sd = "none",
                              all_params_1 = params_3_sd_0$Parameter[1],
                              all_p_val_1 = params_3_sd_0$p[1],
                              all_omega_sq_1 = omega_sq_3_sd_0$Omega2[1],
                              all_omega_sq_ci_low_1 = omega_sq_3_sd_0$CI_low[1],
                              all_omega_sq_ci_hi_1 = omega_sq_3_sd_0$CI_high[1],
                              all_omega_sq_interpret = omega_interpret_3_sd_0)
sp_3_mean_var_15 <- data.frame(species_id = 3,
                               sd = "low",
                               all_params_1 = params_3_sd_15$Parameter[1],
                               all_p_val_1 = params_3_sd_15$p[1],
                               all_omega_sq_1 = omega_sq_3_sd_15$Omega2[1],
                               all_omega_sq_ci_low_1 = omega_sq_3_sd_15$CI_low[1],
                               all_omega_sq_ci_hi_1 = omega_sq_3_sd_15$CI_high[1],
                               all_omega_sq_interpret = omega_interpret_3_sd_15)
sp_3_mean_var_30 <- data.frame(species_id = 3,
                               sd = "medium",
                               all_params_1 = params_3_sd_30$Parameter[1],
                               all_p_val_1 = params_3_sd_30$p[1],
                               all_omega_sq_1 = omega_sq_3_sd_30$Omega2[1],
                               all_omega_sq_ci_low_1 = omega_sq_3_sd_30$CI_low[1],
                               all_omega_sq_ci_hi_1 = omega_sq_3_sd_30$CI_high[1],
                               all_omega_sq_interpret = omega_interpret_3_sd_30)
sp_3_mean_var_60 <- data.frame(species_id = 3,
                               sd = "high",
                               all_params_1 = params_3_sd_60$Parameter[1],
                               all_p_val_1 = params_3_sd_60$p[1],
                               all_omega_sq_1 = omega_sq_3_sd_60$Omega2[1],
                               all_omega_sq_ci_low_1 = omega_sq_3_sd_60$CI_low[1],
                               all_omega_sq_ci_hi_1 = omega_sq_3_sd_60$CI_high[1],
                               all_omega_sq_interpret = omega_interpret_3_sd_60)

sp_3_mean_var <- plyr::rbind.fill(sp_3_mean_var, sp_3_mean_var_0, sp_3_mean_var_15, sp_3_mean_var_30, sp_3_mean_var_60)

params_4_sd <- parameters::model_parameters(aov(model_4_sd))
params_4_sd_0 <- parameters::model_parameters(aov(model_4_sd_0))
params_4_sd_15 <- parameters::model_parameters(aov(model_4_sd_15))
params_4_sd_30 <- parameters::model_parameters(aov(model_4_sd_30))
params_4_sd_60 <- parameters::model_parameters(aov(model_4_sd_60))

omega_sq_4_sd <- omega_squared(aov(model_4_sd), partial = FALSE, ci = 0.95)
omega_interpret_4_sd <- interpret_omega_squared(omega_sq_4_sd$Omega2, rules = "field2013")
omega_sq_4_sd_0 <- omega_squared(aov(model_4_sd_0), partial = FALSE, ci = 0.95)
omega_interpret_4_sd_0 <- interpret_omega_squared(omega_sq_4_sd_0$Omega2, rules = "field2013")
omega_sq_4_sd_15 <- omega_squared(aov(model_4_sd_15), partial = FALSE, ci = 0.95)
omega_interpret_4_sd_15 <- interpret_omega_squared(omega_sq_4_sd_15$Omega2, rules = "field2013")
omega_sq_4_sd_30 <- omega_squared(aov(model_4_sd_30), partial = FALSE, ci = 0.95)
omega_interpret_4_sd_30 <- interpret_omega_squared(omega_sq_4_sd_30$Omega2, rules = "field2013")
omega_sq_4_sd_60 <- omega_squared(aov(model_4_sd_60), partial = FALSE, ci = 0.95)
omega_interpret_4_sd_60 <- interpret_omega_squared(omega_sq_4_sd_60$Omega2, rules = "field2013")

sp_4_mean_var <- data.frame(species_id = 4,
                            sd = "all",
                            all_params_1 = params_4_sd$Parameter[1],
                            all_p_val_1 = params_4_sd$p[1],
                            all_omega_sq_1 = omega_sq_4_sd$Omega2[1],
                            all_omega_sq_ci_low_1 = omega_sq_4_sd$CI_low[1],
                            all_omega_sq_ci_hi_1 = omega_sq_4_sd$CI_high[1],
                            all_omega_sq_interpret = omega_interpret_4_sd)
sp_4_mean_var_0 <- data.frame(species_id = 4, 
                              sd = "none",
                              all_params_1 = params_4_sd_0$Parameter[1],
                              all_p_val_1 = params_4_sd_0$p[1],
                              all_omega_sq_1 = omega_sq_4_sd_0$Omega2[1],
                              all_omega_sq_ci_low_1 = omega_sq_4_sd_0$CI_low[1],
                              all_omega_sq_ci_hi_1 = omega_sq_4_sd_0$CI_high[1],
                              all_omega_sq_interpret = omega_interpret_4_sd_0)
sp_4_mean_var_15 <- data.frame(species_id = 4,
                               sd = "low",
                               all_params_1 = params_4_sd_15$Parameter[1],
                               all_p_val_1 = params_4_sd_15$p[1],
                               all_omega_sq_1 = omega_sq_4_sd_15$Omega2[1],
                               all_omega_sq_ci_low_1 = omega_sq_4_sd_15$CI_low[1],
                               all_omega_sq_ci_hi_1 = omega_sq_4_sd_15$CI_high[1],
                               all_omega_sq_interpret = omega_interpret_4_sd_15)
sp_4_mean_var_30 <- data.frame(species_id = 4,
                               sd = "medium",
                               all_params_1 = params_4_sd_30$Parameter[1],
                               all_p_val_1 = params_4_sd_30$p[1],
                               all_omega_sq_1 = omega_sq_4_sd_30$Omega2[1],
                               all_omega_sq_ci_low_1 = omega_sq_4_sd_30$CI_low[1],
                               all_omega_sq_ci_hi_1 = omega_sq_4_sd_30$CI_high[1],
                               all_omega_sq_interpret = omega_interpret_4_sd_30)
sp_4_mean_var_60 <- data.frame(species_id = 4,
                               sd = "high",
                               all_params_1 = params_4_sd_60$Parameter[1],
                               all_p_val_1 = params_4_sd_60$p[1],
                               all_omega_sq_1 = omega_sq_4_sd_60$Omega2[1],
                               all_omega_sq_ci_low_1 = omega_sq_4_sd_60$CI_low[1],
                               all_omega_sq_ci_hi_1 = omega_sq_4_sd_60$CI_high[1],
                               all_omega_sq_interpret = omega_interpret_4_sd_60)

sp_4_mean_var <- plyr::rbind.fill(sp_4_mean_var, sp_4_mean_var_0, sp_4_mean_var_15, sp_4_mean_var_30, sp_4_mean_var_60)

storage_mean_var <- plyr::rbind.fill(sp_0_mean_var, sp_1_mean_var, sp_2_mean_var, sp_3_mean_var, sp_4_mean_var)
write.csv(storage_mean_var, file = "out/storage_conc_mean_var_stats.csv")

### Log values

model_0_mean=lm(log(data_hist_storage_all_and_ind_0$storage) ~ data_hist_storage_all_and_ind_0$env_sd)
model_0_mean_075=lm(log(data_hist_storage_all_and_ind_0_mean_075$storage) ~ data_hist_storage_all_and_ind_0_mean_075$env_sd)
model_0_mean_085=lm(log(data_hist_storage_all_and_ind_0_mean_085$storage) ~ data_hist_storage_all_and_ind_0_mean_085$env_sd)
model_0_sd=lm(log(data_hist_storage_all_and_ind_0$storage) ~ data_hist_storage_all_and_ind_0$env_mean)
model_0_sd_0=lm(log(data_hist_storage_all_and_ind_0_sd_0$storage) ~ data_hist_storage_all_and_ind_0_sd_0$env_mean)
model_0_sd_15=lm(log(data_hist_storage_all_and_ind_0_sd_15$storage) ~ data_hist_storage_all_and_ind_0_sd_15$env_mean)
model_0_sd_30=lm(log(data_hist_storage_all_and_ind_0_sd_30$storage) ~ data_hist_storage_all_and_ind_0_sd_30$env_mean)
model_0_sd_60=lm(log(data_hist_storage_all_and_ind_0_sd_60$storage) ~ data_hist_storage_all_and_ind_0_sd_60$env_mean)

model_1_mean=lm(log(data_hist_storage_all_and_ind_1$storage) ~ data_hist_storage_all_and_ind_1$env_sd)
model_1_mean_075=lm(log(data_hist_storage_all_and_ind_1_mean_075$storage) ~ data_hist_storage_all_and_ind_1_mean_075$env_sd)
model_1_mean_085=lm(log(data_hist_storage_all_and_ind_1_mean_085$storage) ~ data_hist_storage_all_and_ind_1_mean_085$env_sd)
model_1_sd=lm(log(data_hist_storage_all_and_ind_1$storage) ~ data_hist_storage_all_and_ind_1$env_mean)
model_1_sd_0=lm(log(data_hist_storage_all_and_ind_1_sd_0$storage) ~ data_hist_storage_all_and_ind_1_sd_0$env_mean)
model_1_sd_15=lm(log(data_hist_storage_all_and_ind_1_sd_15$storage) ~ data_hist_storage_all_and_ind_1_sd_15$env_mean)
model_1_sd_30=lm(log(data_hist_storage_all_and_ind_1_sd_30$storage) ~ data_hist_storage_all_and_ind_1_sd_30$env_mean)
model_1_sd_60=lm(log(data_hist_storage_all_and_ind_1_sd_60$storage) ~ data_hist_storage_all_and_ind_1_sd_60$env_mean)

model_2_mean=lm(log(data_hist_storage_all_and_ind_2$storage) ~ data_hist_storage_all_and_ind_2$env_sd)
model_2_mean_075=lm(log(data_hist_storage_all_and_ind_2_mean_075$storage) ~ data_hist_storage_all_and_ind_2_mean_075$env_sd)
model_2_mean_085=lm(log(data_hist_storage_all_and_ind_2_mean_085$storage) ~ data_hist_storage_all_and_ind_2_mean_085$env_sd)
model_2_sd=lm(log(data_hist_storage_all_and_ind_2$storage) ~ data_hist_storage_all_and_ind_2$env_mean)
model_2_sd_0=lm(log(data_hist_storage_all_and_ind_2_sd_0$storage) ~ data_hist_storage_all_and_ind_2_sd_0$env_mean)
model_2_sd_15=lm(log(data_hist_storage_all_and_ind_2_sd_15$storage) ~ data_hist_storage_all_and_ind_2_sd_15$env_mean)
model_2_sd_30=lm(log(data_hist_storage_all_and_ind_2_sd_30$storage) ~ data_hist_storage_all_and_ind_2_sd_30$env_mean)
model_2_sd_60=lm(log(data_hist_storage_all_and_ind_2_sd_60$storage) ~ data_hist_storage_all_and_ind_2_sd_60$env_mean)

model_3_mean=lm(log(data_hist_storage_all_and_ind_3$storage) ~ data_hist_storage_all_and_ind_3$env_sd)
model_3_mean_075=lm(log(data_hist_storage_all_and_ind_3_mean_075$storage) ~ data_hist_storage_all_and_ind_3_mean_075$env_sd)
model_3_mean_085=lm(log(data_hist_storage_all_and_ind_3_mean_085$storage) ~ data_hist_storage_all_and_ind_3_mean_085$env_sd)
model_3_sd=lm(log(data_hist_storage_all_and_ind_3$storage) ~ data_hist_storage_all_and_ind_3$env_mean)
model_3_sd_0=lm(log(data_hist_storage_all_and_ind_3_sd_0$storage) ~ data_hist_storage_all_and_ind_3_sd_0$env_mean)
model_3_sd_15=lm(log(data_hist_storage_all_and_ind_3_sd_15$storage) ~ data_hist_storage_all_and_ind_3_sd_15$env_mean)
model_3_sd_30=lm(log(data_hist_storage_all_and_ind_3_sd_30$storage) ~ data_hist_storage_all_and_ind_3_sd_30$env_mean)
model_3_sd_60=lm(log(data_hist_storage_all_and_ind_3_sd_60$storage) ~ data_hist_storage_all_and_ind_3_sd_60$env_mean)

model_4_mean=lm(log(data_hist_storage_all_and_ind_4$storage) ~ data_hist_storage_all_and_ind_4$env_sd)
model_4_mean_075=lm(log(data_hist_storage_all_and_ind_4_mean_075$storage) ~ data_hist_storage_all_and_ind_4_mean_075$env_sd)
model_4_mean_085=lm(log(data_hist_storage_all_and_ind_4_mean_085$storage) ~ data_hist_storage_all_and_ind_4_mean_085$env_sd)
model_4_sd=lm(log(data_hist_storage_all_and_ind_4$storage) ~ data_hist_storage_all_and_ind_4$env_mean)
model_4_sd_0=lm(log(data_hist_storage_all_and_ind_4_sd_0$storage) ~ data_hist_storage_all_and_ind_4_sd_0$env_mean)
model_4_sd_15=lm(log(data_hist_storage_all_and_ind_4_sd_15$storage) ~ data_hist_storage_all_and_ind_4_sd_15$env_mean)
model_4_sd_30=lm(log(data_hist_storage_all_and_ind_4_sd_30$storage) ~ data_hist_storage_all_and_ind_4_sd_30$env_mean)
model_4_sd_60=lm(log(data_hist_storage_all_and_ind_4_sd_60$storage) ~ data_hist_storage_all_and_ind_4_sd_60$env_mean)

## THIS PART IS THE EXAMINATION OF ENVIRONMENT SD 

params_0_mean <- parameters::model_parameters(model_0_mean)
params_0_mean_075 <- parameters::model_parameters(model_0_mean_075)
params_0_mean_085 <- parameters::model_parameters(model_0_mean_085)

omega_sq_0_mean <- omega_squared(model_0_mean, partial = FALSE, ci = 0.95)
omega_interpret_0_mean <- interpret_omega_squared(omega_sq_0_mean$Omega2, rules = "field2013")
omega_sq_0_mean_075 <- omega_squared(model_0_mean_075, partial = FALSE, ci = 0.95)
omega_interpret_0_mean_075 <- interpret_omega_squared(omega_sq_0_mean_075$Omega2, rules = "field2013")
omega_sq_0_mean_085 <- omega_squared(model_0_mean_085, partial = FALSE, ci = 0.95)
omega_interpret_0_mean_085 <- interpret_omega_squared(omega_sq_0_mean_085$Omega2, rules = "field2013")

sp_0_sd_var <- data.frame(species_id = 0,
                          mean = "all",
                          all_params_1 = params_0_mean$Parameter[1],
                          all_params_2 = params_0_mean$Parameter[2],
                          all_coeff_1 = params_0_mean$Coefficient[1],
                          all_coeff_2 = params_0_mean$Coefficient[2],
                          all_coeff_ci_low_1 = params_0_mean$CI_low[1],
                          all_coeff_ci_low_2 = params_0_mean$CI_low[2],
                          all_coeff_ci_hi_1 = params_0_mean$CI_high[1],
                          all_coeff_ci_hi_2 = params_0_mean$CI_high[2],
                          all_p_val_1 = params_0_mean$p[1],
                          all_p_val_2 = params_0_mean$p[2],
                          all_omega_sq_1 = omega_sq_0_mean$Omega2[1],
                          all_omega_sq_2 = omega_sq_0_mean$Omega2[2],
                          all_omega_sq_ci_low_1 = omega_sq_0_mean$CI_low[1],
                          all_omega_sq_ci_low_2 = omega_sq_0_mean$CI_low[2],
                          all_omega_sq_ci_hi_1 = omega_sq_0_mean$CI_high[1],
                          all_omega_sq_ci_hi_2 = omega_sq_0_mean$CI_high[2],
                          all_omega_sq_interpret = omega_interpret_0_mean)
sp_0_sd_var_075 <- data.frame(species_id = 0,
                              mean = "medium",
                              all_params_1 = params_0_mean_075$Parameter[1],
                              all_params_2 = params_0_mean_075$Parameter[2],
                              all_coeff_1 = params_0_mean_075$Coefficient[1],
                              all_coeff_2 = params_0_mean_075$Coefficient[2],
                              all_coeff_ci_low_1 = params_0_mean_075$CI_low[1],
                              all_coeff_ci_low_2 = params_0_mean_075$CI_low[2],
                              all_coeff_ci_hi_1 = params_0_mean_075$CI_high[1],
                              all_coeff_ci_hi_2 = params_0_mean_075$CI_high[2],
                              all_p_val_1 = params_0_mean_075$p[1],
                              all_p_val_2 = params_0_mean_075$p[2],
                              all_omega_sq_1 = omega_sq_0_mean_075$Omega2[1],
                              all_omega_sq_2 = omega_sq_0_mean_075$Omega2[2],
                              all_omega_sq_ci_low_1 = omega_sq_0_mean_075$CI_low[1],
                              all_omega_sq_ci_low_2 = omega_sq_0_mean_075$CI_low[2],
                              all_omega_sq_ci_hi_1 = omega_sq_0_mean_075$CI_high[1],
                              all_omega_sq_ci_hi_2 = omega_sq_0_mean_075$CI_high[2],
                              all_omega_sq_interpret = omega_interpret_0_mean_075)
sp_0_sd_var_085 <- data.frame(species_id = 0,
                              mean = "low",
                              all_params_1 = params_0_mean_085$Parameter[1],
                              all_params_2 = params_0_mean_085$Parameter[2],
                              all_coeff_1 = params_0_mean_085$Coefficient[1],
                              all_coeff_2 = params_0_mean_085$Coefficient[2],
                              all_coeff_ci_low_1 = params_0_mean_085$CI_low[1],
                              all_coeff_ci_low_2 = params_0_mean_085$CI_low[2],
                              all_coeff_ci_hi_1 = params_0_mean_085$CI_high[1],
                              all_coeff_ci_hi_2 = params_0_mean_085$CI_high[2],
                              all_p_val_1 = params_0_mean_085$p[1],
                              all_p_val_2 = params_0_mean_085$p[2],
                              all_omega_sq_1 = omega_sq_0_mean_085$Omega2[1],
                              all_omega_sq_2 = omega_sq_0_mean_085$Omega2[2],
                              all_omega_sq_ci_low_1 = omega_sq_0_mean_085$CI_low[1],
                              all_omega_sq_ci_low_2 = omega_sq_0_mean_085$CI_low[2],
                              all_omega_sq_ci_hi_1 = omega_sq_0_mean_085$CI_high[1],
                              all_omega_sq_ci_hi_2 = omega_sq_0_mean_085$CI_high[2],
                              all_omega_sq_interpret = omega_interpret_0_mean_085)

sp_0_sd_var <- plyr::rbind.fill(sp_0_sd_var, sp_0_sd_var_075, sp_0_sd_var_085)

params_1_mean <- parameters::model_parameters(model_1_mean)
params_1_mean_075 <- parameters::model_parameters(model_1_mean_075)
params_1_mean_085 <- parameters::model_parameters(model_1_mean_085)

omega_sq_1_mean <- omega_squared(model_1_mean, partial = FALSE, ci = 0.95)
omega_interpret_1_mean <- interpret_omega_squared(omega_sq_1_mean$Omega2, rules = "field2013")
omega_sq_1_mean_075 <- omega_squared(model_1_mean_075, partial = FALSE, ci = 0.95)
omega_interpret_1_mean_075 <- interpret_omega_squared(omega_sq_1_mean_075$Omega2, rules = "field2013")
omega_sq_1_mean_085 <- omega_squared(model_1_mean_085, partial = FALSE, ci = 0.95)
omega_interpret_1_mean_085 <- interpret_omega_squared(omega_sq_1_mean_085$Omega2, rules = "field2013")

sp_1_sd_var <- data.frame(species_id = 1,
                          mean = "all",
                          all_params_1 = params_1_mean$Parameter[1],
                          all_params_2 = params_1_mean$Parameter[2],
                          all_coeff_1 = params_1_mean$Coefficient[1],
                          all_coeff_2 = params_1_mean$Coefficient[2],
                          all_coeff_ci_low_1 = params_1_mean$CI_low[1],
                          all_coeff_ci_low_2 = params_1_mean$CI_low[2],
                          all_coeff_ci_hi_1 = params_1_mean$CI_high[1],
                          all_coeff_ci_hi_2 = params_1_mean$CI_high[2],
                          all_p_val_1 = params_1_mean$p[1],
                          all_p_val_2 = params_1_mean$p[2],
                          all_omega_sq_1 = omega_sq_1_mean$Omega2[1],
                          all_omega_sq_2 = omega_sq_1_mean$Omega2[2],
                          all_omega_sq_ci_low_1 = omega_sq_1_mean$CI_low[1],
                          all_omega_sq_ci_low_2 = omega_sq_1_mean$CI_low[2],
                          all_omega_sq_ci_hi_1 = omega_sq_1_mean$CI_high[1],
                          all_omega_sq_ci_hi_2 = omega_sq_1_mean$CI_high[2],
                          all_omega_sq_interpret = omega_interpret_1_mean)
sp_1_sd_var_075 <- data.frame(species_id = 1,
                              mean = "medium",
                              all_params_1 = params_1_mean_075$Parameter[1],
                              all_params_2 = params_1_mean_075$Parameter[2],
                              all_coeff_1 = params_1_mean_075$Coefficient[1],
                              all_coeff_2 = params_1_mean_075$Coefficient[2],
                              all_coeff_ci_low_1 = params_1_mean_075$CI_low[1],
                              all_coeff_ci_low_2 = params_1_mean_075$CI_low[2],
                              all_coeff_ci_hi_1 = params_1_mean_075$CI_high[1],
                              all_coeff_ci_hi_2 = params_1_mean_075$CI_high[2],
                              all_p_val_1 = params_1_mean_075$p[1],
                              all_p_val_2 = params_1_mean_075$p[2],
                              all_omega_sq_1 = omega_sq_1_mean_075$Omega2[1],
                              all_omega_sq_2 = omega_sq_1_mean_075$Omega2[2],
                              all_omega_sq_ci_low_1 = omega_sq_1_mean_075$CI_low[1],
                              all_omega_sq_ci_low_2 = omega_sq_1_mean_075$CI_low[2],
                              all_omega_sq_ci_hi_1 = omega_sq_1_mean_075$CI_high[1],
                              all_omega_sq_ci_hi_2 = omega_sq_1_mean_075$CI_high[2],
                              all_omega_sq_interpret = omega_interpret_1_mean_075)
sp_1_sd_var_085 <- data.frame(species_id = 1,
                              mean = "low",
                              all_params_1 = params_1_mean_085$Parameter[1],
                              all_params_2 = params_1_mean_085$Parameter[2],
                              all_coeff_1 = params_1_mean_085$Coefficient[1],
                              all_coeff_2 = params_1_mean_085$Coefficient[2],
                              all_coeff_ci_low_1 = params_1_mean_085$CI_low[1],
                              all_coeff_ci_low_2 = params_1_mean_085$CI_low[2],
                              all_coeff_ci_hi_1 = params_1_mean_085$CI_high[1],
                              all_coeff_ci_hi_2 = params_1_mean_085$CI_high[2],
                              all_p_val_1 = params_1_mean_085$p[1],
                              all_p_val_2 = params_1_mean_085$p[2],
                              all_omega_sq_1 = omega_sq_1_mean_085$Omega2[1],
                              all_omega_sq_2 = omega_sq_1_mean_085$Omega2[2],
                              all_omega_sq_ci_low_1 = omega_sq_1_mean_085$CI_low[1],
                              all_omega_sq_ci_low_2 = omega_sq_1_mean_085$CI_low[2],
                              all_omega_sq_ci_hi_1 = omega_sq_1_mean_085$CI_high[1],
                              all_omega_sq_ci_hi_2 = omega_sq_1_mean_085$CI_high[2],
                              all_omega_sq_interpret = omega_interpret_1_mean_085)

sp_1_sd_var <- plyr::rbind.fill(sp_1_sd_var, sp_1_sd_var_075, sp_1_sd_var_085)

params_2_mean <- parameters::model_parameters(model_1_mean)
params_2_mean_075 <- parameters::model_parameters(model_1_mean_075)
params_2_mean_085 <- parameters::model_parameters(model_1_mean_085)

omega_sq_2_mean <- omega_squared(model_2_mean, partial = FALSE, ci = 0.95)
omega_interpret_2_mean <- interpret_omega_squared(omega_sq_2_mean$Omega2, rules = "field2013")
omega_sq_2_mean_075 <- omega_squared(model_2_mean_075, partial = FALSE, ci = 0.95)
omega_interpret_2_mean_075 <- interpret_omega_squared(omega_sq_2_mean_075$Omega2, rules = "field2013")
omega_sq_2_mean_085 <- omega_squared(model_2_mean_085, partial = FALSE, ci = 0.95)
omega_interpret_2_mean_085 <- interpret_omega_squared(omega_sq_2_mean_085$Omega2, rules = "field2013")

sp_2_sd_var <- data.frame(species_id = 2,
                          mean = "all",
                          all_params_1 = params_2_mean$Parameter[1],
                          all_params_2 = params_2_mean$Parameter[2],
                          all_coeff_1 = params_2_mean$Coefficient[1],
                          all_coeff_2 = params_2_mean$Coefficient[2],
                          all_coeff_ci_low_1 = params_2_mean$CI_low[1],
                          all_coeff_ci_low_2 = params_2_mean$CI_low[2],
                          all_coeff_ci_hi_1 = params_2_mean$CI_high[1],
                          all_coeff_ci_hi_2 = params_2_mean$CI_high[2],
                          all_p_val_1 = params_2_mean$p[1],
                          all_p_val_2 = params_2_mean$p[2],
                          all_omega_sq_1 = omega_sq_2_mean$Omega2[1],
                          all_omega_sq_2 = omega_sq_2_mean$Omega2[2],
                          all_omega_sq_ci_low_1 = omega_sq_2_mean$CI_low[1],
                          all_omega_sq_ci_low_2 = omega_sq_2_mean$CI_low[2],
                          all_omega_sq_ci_hi_1 = omega_sq_2_mean$CI_high[1],
                          all_omega_sq_ci_hi_2 = omega_sq_2_mean$CI_high[2],
                          all_omega_sq_interpret = omega_interpret_2_mean)
sp_2_sd_var_075 <- data.frame(species_id = 2,
                              mean = "medium",
                              all_params_1 = params_2_mean_075$Parameter[1],
                              all_params_2 = params_2_mean_075$Parameter[2],
                              all_coeff_1 = params_2_mean_075$Coefficient[1],
                              all_coeff_2 = params_2_mean_075$Coefficient[2],
                              all_coeff_ci_low_1 = params_2_mean_075$CI_low[1],
                              all_coeff_ci_low_2 = params_2_mean_075$CI_low[2],
                              all_coeff_ci_hi_1 = params_2_mean_075$CI_high[1],
                              all_coeff_ci_hi_2 = params_2_mean_075$CI_high[2],
                              all_p_val_1 = params_2_mean_075$p[1],
                              all_p_val_2 = params_2_mean_075$p[2],
                              all_omega_sq_1 = omega_sq_2_mean_075$Omega2[1],
                              all_omega_sq_2 = omega_sq_2_mean_075$Omega2[2],
                              all_omega_sq_ci_low_1 = omega_sq_2_mean_075$CI_low[1],
                              all_omega_sq_ci_low_2 = omega_sq_2_mean_075$CI_low[2],
                              all_omega_sq_ci_hi_1 = omega_sq_2_mean_075$CI_high[1],
                              all_omega_sq_ci_hi_2 = omega_sq_2_mean_075$CI_high[2],
                              all_omega_sq_interpret = omega_interpret_2_mean_075)
sp_2_sd_var_085 <- data.frame(species_id = 2,
                              mean = "low",
                              all_params_1 = params_2_mean_085$Parameter[1],
                              all_params_2 = params_2_mean_085$Parameter[2],
                              all_coeff_1 = params_2_mean_085$Coefficient[1],
                              all_coeff_2 = params_2_mean_085$Coefficient[2],
                              all_coeff_ci_low_1 = params_2_mean_085$CI_low[1],
                              all_coeff_ci_low_2 = params_2_mean_085$CI_low[2],
                              all_coeff_ci_hi_1 = params_2_mean_085$CI_high[1],
                              all_coeff_ci_hi_2 = params_2_mean_085$CI_high[2],
                              all_p_val_1 = params_2_mean_085$p[1],
                              all_p_val_2 = params_2_mean_085$p[2],
                              all_omega_sq_1 = omega_sq_2_mean_085$Omega2[1],
                              all_omega_sq_2 = omega_sq_2_mean_085$Omega2[2],
                              all_omega_sq_ci_low_1 = omega_sq_2_mean_085$CI_low[1],
                              all_omega_sq_ci_low_2 = omega_sq_2_mean_085$CI_low[2],
                              all_omega_sq_ci_hi_1 = omega_sq_2_mean_085$CI_high[1],
                              all_omega_sq_ci_hi_2 = omega_sq_2_mean_085$CI_high[2],
                              all_omega_sq_interpret = omega_interpret_2_mean_085)

sp_2_sd_var <- plyr::rbind.fill(sp_2_sd_var, sp_2_sd_var_075, sp_2_sd_var_085)

params_3_mean <- parameters::model_parameters(model_3_mean)
params_3_mean_075 <- parameters::model_parameters(model_3_mean_075)
params_3_mean_085 <- parameters::model_parameters(model_3_mean_085)

omega_sq_3_mean <- omega_squared(model_3_mean, partial = FALSE, ci = 0.95)
omega_interpret_3_mean <- interpret_omega_squared(omega_sq_3_mean$Omega2, rules = "field2013")
omega_sq_3_mean_075 <- omega_squared(model_3_mean_075, partial = FALSE, ci = 0.95)
omega_interpret_3_mean_075 <- interpret_omega_squared(omega_sq_3_mean_075$Omega2, rules = "field2013")
omega_sq_3_mean_085 <- omega_squared(model_3_mean_085, partial = FALSE, ci = 0.95)
omega_interpret_3_mean_085 <- interpret_omega_squared(omega_sq_3_mean_085$Omega2, rules = "field2013")

sp_3_sd_var <- data.frame(species_id = 3,
                          mean = "all",
                          all_params_1 = params_3_mean$Parameter[1],
                          all_params_2 = params_3_mean$Parameter[2],
                          all_coeff_1 = params_3_mean$Coefficient[1],
                          all_coeff_2 = params_3_mean$Coefficient[2],
                          all_coeff_ci_low_1 = params_3_mean$CI_low[1],
                          all_coeff_ci_low_2 = params_3_mean$CI_low[2],
                          all_coeff_ci_hi_1 = params_3_mean$CI_high[1],
                          all_coeff_ci_hi_2 = params_3_mean$CI_high[2],
                          all_p_val_1 = params_3_mean$p[1],
                          all_p_val_2 = params_3_mean$p[2],
                          all_omega_sq_1 = omega_sq_3_mean$Omega2[1],
                          all_omega_sq_2 = omega_sq_3_mean$Omega2[2],
                          all_omega_sq_ci_low_1 = omega_sq_3_mean$CI_low[1],
                          all_omega_sq_ci_low_2 = omega_sq_3_mean$CI_low[2],
                          all_omega_sq_ci_hi_1 = omega_sq_3_mean$CI_high[1],
                          all_omega_sq_ci_hi_2 = omega_sq_3_mean$CI_high[2],
                          all_omega_sq_interpret = omega_interpret_3_mean)
sp_3_sd_var_075 <- data.frame(species_id = 3,
                              mean = "medium",
                              all_params_1 = params_3_mean_075$Parameter[1],
                              all_params_2 = params_3_mean_075$Parameter[2],
                              all_coeff_1 = params_3_mean_075$Coefficient[1],
                              all_coeff_2 = params_3_mean_075$Coefficient[2],
                              all_coeff_ci_low_1 = params_3_mean_075$CI_low[1],
                              all_coeff_ci_low_2 = params_3_mean_075$CI_low[2],
                              all_coeff_ci_hi_1 = params_3_mean_075$CI_high[1],
                              all_coeff_ci_hi_2 = params_3_mean_075$CI_high[2],
                              all_p_val_1 = params_3_mean_075$p[1],
                              all_p_val_2 = params_3_mean_075$p[2],
                              all_omega_sq_1 = omega_sq_3_mean_075$Omega2[1],
                              all_omega_sq_2 = omega_sq_3_mean_075$Omega2[2],
                              all_omega_sq_ci_low_1 = omega_sq_3_mean_075$CI_low[1],
                              all_omega_sq_ci_low_2 = omega_sq_3_mean_075$CI_low[2],
                              all_omega_sq_ci_hi_1 = omega_sq_3_mean_075$CI_high[1],
                              all_omega_sq_ci_hi_2 = omega_sq_3_mean_075$CI_high[2],
                              all_omega_sq_interpret = omega_interpret_3_mean_075)
sp_3_sd_var_085 <- data.frame(species_id = 3,
                              mean = "low",
                              all_params_1 = params_3_mean_085$Parameter[1],
                              all_params_2 = params_3_mean_085$Parameter[2],
                              all_coeff_1 = params_3_mean_085$Coefficient[1],
                              all_coeff_2 = params_3_mean_085$Coefficient[2],
                              all_coeff_ci_low_1 = params_3_mean_085$CI_low[1],
                              all_coeff_ci_low_2 = params_3_mean_085$CI_low[2],
                              all_coeff_ci_hi_1 = params_3_mean_085$CI_high[1],
                              all_coeff_ci_hi_2 = params_3_mean_085$CI_high[2],
                              all_p_val_1 = params_3_mean_085$p[1],
                              all_p_val_2 = params_3_mean_085$p[2],
                              all_omega_sq_1 = omega_sq_3_mean_085$Omega2[1],
                              all_omega_sq_2 = omega_sq_3_mean_085$Omega2[2],
                              all_omega_sq_ci_low_1 = omega_sq_3_mean_085$CI_low[1],
                              all_omega_sq_ci_low_2 = omega_sq_3_mean_085$CI_low[2],
                              all_omega_sq_ci_hi_1 = omega_sq_3_mean_085$CI_high[1],
                              all_omega_sq_ci_hi_2 = omega_sq_3_mean_085$CI_high[2],
                              all_omega_sq_interpret = omega_interpret_3_mean_085)

sp_3_sd_var <- plyr::rbind.fill(sp_3_sd_var, sp_3_sd_var_075, sp_3_sd_var_085)

params_4_mean <- parameters::model_parameters(model_4_mean)
params_4_mean_075 <- parameters::model_parameters(model_4_mean_075)
params_4_mean_085 <- parameters::model_parameters(model_4_mean_085)

omega_sq_4_mean <- omega_squared(model_4_mean, partial = FALSE, ci = 0.95)
omega_interpret_4_mean <- interpret_omega_squared(omega_sq_4_mean$Omega2, rules = "field2013")
omega_sq_4_mean_075 <- omega_squared(model_4_mean_075, partial = FALSE, ci = 0.95)
omega_interpret_4_mean_075 <- interpret_omega_squared(omega_sq_4_mean_075$Omega2, rules = "field2013")
omega_sq_4_mean_085 <- omega_squared(model_4_mean_085, partial = FALSE, ci = 0.95)
omega_interpret_4_mean_085 <- interpret_omega_squared(omega_sq_4_mean_085$Omega2, rules = "field2013")

sp_4_sd_var <- data.frame(species_id = 4,
                          mean = "all",
                          all_params_1 = params_4_mean$Parameter[1],
                          all_params_2 = params_4_mean$Parameter[2],
                          all_coeff_1 = params_4_mean$Coefficient[1],
                          all_coeff_2 = params_4_mean$Coefficient[2],
                          all_coeff_ci_low_1 = params_4_mean$CI_low[1],
                          all_coeff_ci_low_2 = params_4_mean$CI_low[2],
                          all_coeff_ci_hi_1 = params_4_mean$CI_high[1],
                          all_coeff_ci_hi_2 = params_4_mean$CI_high[2],
                          all_p_val_1 = params_4_mean$p[1],
                          all_p_val_2 = params_4_mean$p[2],
                          all_omega_sq_1 = omega_sq_4_mean$Omega2[1],
                          all_omega_sq_2 = omega_sq_4_mean$Omega2[2],
                          all_omega_sq_ci_low_1 = omega_sq_4_mean$CI_low[1],
                          all_omega_sq_ci_low_2 = omega_sq_4_mean$CI_low[2],
                          all_omega_sq_ci_hi_1 = omega_sq_4_mean$CI_high[1],
                          all_omega_sq_ci_hi_2 = omega_sq_4_mean$CI_high[2],
                          all_omega_sq_interpret = omega_interpret_4_mean)
sp_4_sd_var_075 <- data.frame(species_id = 4,
                              mean = "medium",
                              all_params_1 = params_4_mean_075$Parameter[1],
                              all_params_2 = params_4_mean_075$Parameter[2],
                              all_coeff_1 = params_4_mean_075$Coefficient[1],
                              all_coeff_2 = params_4_mean_075$Coefficient[2],
                              all_coeff_ci_low_1 = params_4_mean_075$CI_low[1],
                              all_coeff_ci_low_2 = params_4_mean_075$CI_low[2],
                              all_coeff_ci_hi_1 = params_4_mean_075$CI_high[1],
                              all_coeff_ci_hi_2 = params_4_mean_075$CI_high[2],
                              all_p_val_1 = params_4_mean_075$p[1],
                              all_p_val_2 = params_4_mean_075$p[2],
                              all_omega_sq_1 = omega_sq_4_mean_075$Omega2[1],
                              all_omega_sq_2 = omega_sq_4_mean_075$Omega2[2],
                              all_omega_sq_ci_low_1 = omega_sq_4_mean_075$CI_low[1],
                              all_omega_sq_ci_low_2 = omega_sq_4_mean_075$CI_low[2],
                              all_omega_sq_ci_hi_1 = omega_sq_4_mean_075$CI_high[1],
                              all_omega_sq_ci_hi_2 = omega_sq_4_mean_075$CI_high[2],
                              all_omega_sq_interpret = omega_interpret_4_mean_075)
sp_4_sd_var_085 <- data.frame(species_id = 4,
                              mean = "low",
                              all_params_1 = params_4_mean_085$Parameter[1],
                              all_params_2 = params_4_mean_085$Parameter[2],
                              all_coeff_1 = params_4_mean_085$Coefficient[1],
                              all_coeff_2 = params_4_mean_085$Coefficient[2],
                              all_coeff_ci_low_1 = params_4_mean_085$CI_low[1],
                              all_coeff_ci_low_2 = params_4_mean_085$CI_low[2],
                              all_coeff_ci_hi_1 = params_4_mean_085$CI_high[1],
                              all_coeff_ci_hi_2 = params_4_mean_085$CI_high[2],
                              all_p_val_1 = params_4_mean_085$p[1],
                              all_p_val_2 = params_4_mean_085$p[2],
                              all_omega_sq_1 = omega_sq_4_mean_085$Omega2[1],
                              all_omega_sq_2 = omega_sq_4_mean_085$Omega2[2],
                              all_omega_sq_ci_low_1 = omega_sq_4_mean_085$CI_low[1],
                              all_omega_sq_ci_low_2 = omega_sq_4_mean_085$CI_low[2],
                              all_omega_sq_ci_hi_1 = omega_sq_4_mean_085$CI_high[1],
                              all_omega_sq_ci_hi_2 = omega_sq_4_mean_085$CI_high[2],
                              all_omega_sq_interpret = omega_interpret_4_mean_085)

sp_4_sd_var <- plyr::rbind.fill(sp_4_sd_var, sp_4_sd_var_075, sp_4_sd_var_085)

storage_log_sd_var <- plyr::rbind.fill(sp_0_sd_var, sp_1_sd_var, sp_2_sd_var, sp_3_sd_var, sp_4_sd_var)
write.csv(storage_log_sd_var, file = "out/storage_conc_log_sd_var_stats.csv")

## THIS PART IS THE EXAMINATION OF ENVIRONMENT MEAN 

params_0_sd <- parameters::model_parameters(aov(model_0_sd))
params_0_sd_0 <- parameters::model_parameters(aov(model_0_sd_0))
params_0_sd_15 <- parameters::model_parameters(aov(model_0_sd_15))
params_0_sd_30 <- parameters::model_parameters(aov(model_0_sd_30))
params_0_sd_60 <- parameters::model_parameters(aov(model_0_sd_60))

omega_sq_0_sd <- omega_squared(aov(model_0_sd), partial = FALSE, ci = 0.95)
omega_interpret_0_sd <- interpret_omega_squared(omega_sq_0_sd$Omega2, rules = "field2013")
omega_sq_0_sd_0 <- omega_squared(aov(model_0_sd_0), partial = FALSE, ci = 0.95)
omega_interpret_0_sd_0 <- interpret_omega_squared(omega_sq_0_sd_0$Omega2, rules = "field2013")
omega_sq_0_sd_15 <- omega_squared(aov(model_0_sd_15), partial = FALSE, ci = 0.95)
omega_interpret_0_sd_15 <- interpret_omega_squared(omega_sq_0_sd_15$Omega2, rules = "field2013")
omega_sq_0_sd_30 <- omega_squared(aov(model_0_sd_30), partial = FALSE, ci = 0.95)
omega_interpret_0_sd_30 <- interpret_omega_squared(omega_sq_0_sd_30$Omega2, rules = "field2013")
omega_sq_0_sd_60 <- omega_squared(aov(model_0_sd_60), partial = FALSE, ci = 0.95)
omega_interpret_0_sd_60 <- interpret_omega_squared(omega_sq_0_sd_60$Omega2, rules = "field2013")

sp_0_mean_var <- data.frame(species_id = 0,
                            sd = "all",
                            all_params_1 = params_0_sd$Parameter[1],
                            all_p_val_1 = params_0_sd$p[1],
                            all_omega_sq_1 = omega_sq_0_sd$Omega2[1],
                            all_omega_sq_ci_low_1 = omega_sq_0_sd$CI_low[1],
                            all_omega_sq_ci_hi_1 = omega_sq_0_sd$CI_high[1],
                            all_omega_sq_interpret = omega_interpret_0_sd)
sp_0_mean_var_0 <- data.frame(species_id = 0,
                              sd = "none",
                              all_params_1 = params_0_sd_0$Parameter[1],
                              all_p_val_1 = params_0_sd_0$p[1],
                              all_omega_sq_1 = omega_sq_0_sd_0$Omega2[1],
                              all_omega_sq_ci_low_1 = omega_sq_0_sd_0$CI_low[1],
                              all_omega_sq_ci_hi_1 = omega_sq_0_sd_0$CI_high[1],
                              all_omega_sq_interpret = omega_interpret_0_sd_0)
sp_0_mean_var_15 <- data.frame(species_id = 0,
                               sd = "low",
                               all_params_1 = params_0_sd_15$Parameter[1],
                               all_p_val_1 = params_0_sd_15$p[1],
                               all_omega_sq_1 = omega_sq_0_sd_15$Omega2[1],
                               all_omega_sq_ci_low_1 = omega_sq_0_sd_15$CI_low[1],
                               all_omega_sq_ci_hi_1 = omega_sq_0_sd_15$CI_high[1],
                               all_omega_sq_interpret = omega_interpret_0_sd_15)
sp_0_mean_var_30 <- data.frame(species_id = 0,
                               sd = "medium",
                               all_params_1 = params_0_sd_30$Parameter[1],
                               all_p_val_1 = params_0_sd_30$p[1],
                               all_omega_sq_1 = omega_sq_0_sd_30$Omega2[1],
                               all_omega_sq_ci_low_1 = omega_sq_0_sd_30$CI_low[1],
                               all_omega_sq_ci_hi_1 = omega_sq_0_sd_30$CI_high[1],
                               all_omega_sq_interpret = omega_interpret_0_sd_30)
sp_0_mean_var_60 <- data.frame(species_id = 0,
                               sd = "high",
                               all_params_1 = params_0_sd_60$Parameter[1],
                               all_p_val_1 = params_0_sd_60$p[1],
                               all_omega_sq_1 = omega_sq_0_sd_60$Omega2[1],
                               all_omega_sq_ci_low_1 = omega_sq_0_sd_60$CI_low[1],
                               all_omega_sq_ci_hi_1 = omega_sq_0_sd_60$CI_high[1],
                               all_omega_sq_interpret = omega_interpret_0_sd_60)

sp_0_mean_var <- plyr::rbind.fill(sp_0_mean_var, sp_0_mean_var_0, sp_0_mean_var_15, sp_0_mean_var_30, sp_0_mean_var_60)

params_1_sd <- parameters::model_parameters(aov(model_1_sd))
params_1_sd_0 <- parameters::model_parameters(aov(model_1_sd_0))
params_1_sd_15 <- parameters::model_parameters(aov(model_1_sd_15))
params_1_sd_30 <- parameters::model_parameters(aov(model_1_sd_30))
params_1_sd_60 <- parameters::model_parameters(aov(model_1_sd_60))

omega_sq_1_sd <- omega_squared(aov(model_1_sd), partial = FALSE, ci = 0.95)
omega_interpret_1_sd <- interpret_omega_squared(omega_sq_1_sd$Omega2, rules = "field2013")
omega_sq_1_sd_0 <- omega_squared(aov(model_1_sd_0), partial = FALSE, ci = 0.95)
omega_interpret_1_sd_0 <- interpret_omega_squared(omega_sq_1_sd_0$Omega2, rules = "field2013")
omega_sq_1_sd_15 <- omega_squared(aov(model_1_sd_15), partial = FALSE, ci = 0.95)
omega_interpret_1_sd_15 <- interpret_omega_squared(omega_sq_1_sd_15$Omega2, rules = "field2013")
omega_sq_1_sd_30 <- omega_squared(aov(model_1_sd_30), partial = FALSE, ci = 0.95)
omega_interpret_1_sd_30 <- interpret_omega_squared(omega_sq_1_sd_30$Omega2, rules = "field2013")
omega_sq_1_sd_60 <- omega_squared(aov(model_1_sd_60), partial = FALSE, ci = 0.95)
omega_interpret_1_sd_60 <- interpret_omega_squared(omega_sq_1_sd_60$Omega2, rules = "field2013")

sp_1_mean_var <- data.frame(species_id = 1,
                            sd = "all",
                            all_params_1 = params_1_sd$Parameter[1],
                            all_p_val_1 = params_1_sd$p[1],
                            all_omega_sq_1 = omega_sq_1_sd$Omega2[1],
                            all_omega_sq_ci_low_1 = omega_sq_1_sd$CI_low[1],
                            all_omega_sq_ci_hi_1 = omega_sq_1_sd$CI_high[1],
                            all_omega_sq_interpret = omega_interpret_1_sd)
sp_1_mean_var_0 <- data.frame(species_id = 1,
                              sd = "none",
                              all_params_1 = params_1_sd_0$Parameter[1],
                              all_p_val_1 = params_1_sd_0$p[1],
                              all_omega_sq_1 = omega_sq_1_sd_0$Omega2[1],
                              all_omega_sq_ci_low_1 = omega_sq_1_sd_0$CI_low[1],
                              all_omega_sq_ci_hi_1 = omega_sq_1_sd_0$CI_high[1],
                              all_omega_sq_interpret = omega_interpret_1_sd_0)
sp_1_mean_var_15 <- data.frame(species_id = 1,
                               sd = "low",
                               all_params_1 = params_1_sd_15$Parameter[1],
                               all_p_val_1 = params_1_sd_15$p[1],
                               all_omega_sq_1 = omega_sq_1_sd_15$Omega2[1],
                               all_omega_sq_ci_low_1 = omega_sq_1_sd_15$CI_low[1],
                               all_omega_sq_ci_hi_1 = omega_sq_1_sd_15$CI_high[1],
                               all_omega_sq_interpret = omega_interpret_1_sd_15)
sp_1_mean_var_30 <- data.frame(species_id = 1,
                               sd = "medium",
                               all_params_1 = params_1_sd_30$Parameter[1],
                               all_p_val_1 = params_1_sd_30$p[1],
                               all_omega_sq_1 = omega_sq_1_sd_30$Omega2[1],
                               all_omega_sq_ci_low_1 = omega_sq_1_sd_30$CI_low[1],
                               all_omega_sq_ci_hi_1 = omega_sq_1_sd_30$CI_high[1],
                               all_omega_sq_interpret = omega_interpret_1_sd_30)
sp_1_mean_var_60 <- data.frame(species_id = 1,
                               sd = "high",
                               all_params_1 = params_1_sd_60$Parameter[1],
                               all_p_val_1 = params_1_sd_60$p[1],
                               all_omega_sq_1 = omega_sq_1_sd_60$Omega2[1],
                               all_omega_sq_ci_low_1 = omega_sq_1_sd_60$CI_low[1],
                               all_omega_sq_ci_hi_1 = omega_sq_1_sd_60$CI_high[1],
                               all_omega_sq_interpret = omega_interpret_1_sd_60)

sp_1_mean_var <- plyr::rbind.fill(sp_1_mean_var, sp_1_mean_var_0, sp_1_mean_var_15, sp_1_mean_var_30, sp_1_mean_var_60)

params_2_sd <- parameters::model_parameters(aov(model_2_sd))
params_2_sd_0 <- parameters::model_parameters(aov(model_2_sd_0))
params_2_sd_15 <- parameters::model_parameters(aov(model_2_sd_15))
params_2_sd_30 <- parameters::model_parameters(aov(model_2_sd_30))
params_2_sd_60 <- parameters::model_parameters(aov(model_2_sd_60))

omega_sq_2_sd <- omega_squared(aov(model_2_sd), partial = FALSE, ci = 0.95)
omega_interpret_2_sd <- interpret_omega_squared(omega_sq_2_sd$Omega2, rules = "field2013")
omega_sq_2_sd_0 <- omega_squared(aov(model_2_sd_0), partial = FALSE, ci = 0.95)
omega_interpret_2_sd_0 <- interpret_omega_squared(omega_sq_2_sd_0$Omega2, rules = "field2013")
omega_sq_2_sd_15 <- omega_squared(aov(model_2_sd_15), partial = FALSE, ci = 0.95)
omega_interpret_2_sd_15 <- interpret_omega_squared(omega_sq_2_sd_15$Omega2, rules = "field2013")
omega_sq_2_sd_30 <- omega_squared(aov(model_2_sd_30), partial = FALSE, ci = 0.95)
omega_interpret_2_sd_30 <- interpret_omega_squared(omega_sq_2_sd_30$Omega2, rules = "field2013")
omega_sq_2_sd_60 <- omega_squared(aov(model_2_sd_60), partial = FALSE, ci = 0.95)
omega_interpret_2_sd_60 <- interpret_omega_squared(omega_sq_2_sd_60$Omega2, rules = "field2013")

sp_2_mean_var <- data.frame(species_id = 2,
                            sd = "all",
                            all_params_1 = params_2_sd$Parameter[1],
                            all_p_val_1 = params_2_sd$p[1],
                            all_omega_sq_1 = omega_sq_2_sd$Omega2[1],
                            all_omega_sq_ci_low_1 = omega_sq_2_sd$CI_low[1],
                            all_omega_sq_ci_hi_1 = omega_sq_2_sd$CI_high[1],
                            all_omega_sq_interpret = omega_interpret_2_sd)
sp_2_mean_var_0 <- data.frame(species_id = 2,
                              sd = "none",
                              all_params_1 = params_2_sd_0$Parameter[1],
                              all_p_val_1 = params_2_sd_0$p[1],
                              all_omega_sq_1 = omega_sq_2_sd_0$Omega2[1],
                              all_omega_sq_ci_low_1 = omega_sq_2_sd_0$CI_low[1],
                              all_omega_sq_ci_hi_1 = omega_sq_2_sd_0$CI_high[1],
                              all_omega_sq_interpret = omega_interpret_2_sd_0)
sp_2_mean_var_15 <- data.frame(species_id = 2,
                               sd = "low",
                               all_params_1 = params_2_sd_15$Parameter[1],
                               all_p_val_1 = params_2_sd_15$p[1],
                               all_omega_sq_1 = omega_sq_2_sd_15$Omega2[1],
                               all_omega_sq_ci_low_1 = omega_sq_2_sd_15$CI_low[1],
                               all_omega_sq_ci_hi_1 = omega_sq_2_sd_15$CI_high[1],
                               all_omega_sq_interpret = omega_interpret_2_sd_15)
sp_2_mean_var_30 <- data.frame(species_id = 2,
                               sd = "medium",
                               all_params_1 = params_2_sd_30$Parameter[1],
                               all_p_val_1 = params_2_sd_30$p[1],
                               all_omega_sq_1 = omega_sq_2_sd_30$Omega2[1],
                               all_omega_sq_ci_low_1 = omega_sq_2_sd_30$CI_low[1],
                               all_omega_sq_ci_hi_1 = omega_sq_2_sd_30$CI_high[1],
                               all_omega_sq_interpret = omega_interpret_2_sd_30)
sp_2_mean_var_60 <- data.frame(species_id = 2,
                               sd = "high",
                               all_params_1 = params_2_sd_60$Parameter[1],
                               all_p_val_1 = params_2_sd_60$p[1],
                               all_omega_sq_1 = omega_sq_2_sd_60$Omega2[1],
                               all_omega_sq_ci_low_1 = omega_sq_2_sd_60$CI_low[1],
                               all_omega_sq_ci_hi_1 = omega_sq_2_sd_60$CI_high[1],
                               all_omega_sq_interpret = omega_interpret_2_sd_60)

sp_2_mean_var <- plyr::rbind.fill(sp_2_mean_var, sp_2_mean_var_0, sp_2_mean_var_15, sp_2_mean_var_30, sp_2_mean_var_60)

params_3_sd <- parameters::model_parameters(aov(model_3_sd))
params_3_sd_0 <- parameters::model_parameters(aov(model_3_sd_0))
params_3_sd_15 <- parameters::model_parameters(aov(model_3_sd_15))
params_3_sd_30 <- parameters::model_parameters(aov(model_3_sd_30))
params_3_sd_60 <- parameters::model_parameters(aov(model_3_sd_60))

omega_sq_3_sd <- omega_squared(aov(model_3_sd), partial = FALSE, ci = 0.95)
omega_interpret_3_sd <- interpret_omega_squared(omega_sq_3_sd$Omega2, rules = "field2013")
omega_sq_3_sd_0 <- omega_squared(aov(model_3_sd_0), partial = FALSE, ci = 0.95)
omega_interpret_3_sd_0 <- interpret_omega_squared(omega_sq_3_sd_0$Omega2, rules = "field2013")
omega_sq_3_sd_15 <- omega_squared(aov(model_3_sd_15), partial = FALSE, ci = 0.95)
omega_interpret_3_sd_15 <- interpret_omega_squared(omega_sq_3_sd_15$Omega2, rules = "field2013")
omega_sq_3_sd_30 <- omega_squared(aov(model_3_sd_30), partial = FALSE, ci = 0.95)
omega_interpret_3_sd_30 <- interpret_omega_squared(omega_sq_3_sd_30$Omega2, rules = "field2013")
omega_sq_3_sd_60 <- omega_squared(aov(model_3_sd_60), partial = FALSE, ci = 0.95)
omega_interpret_3_sd_60 <- interpret_omega_squared(omega_sq_3_sd_60$Omega2, rules = "field2013")

sp_3_mean_var <- data.frame(species_id = 3,
                            sd = "all",
                            all_params_1 = params_3_sd$Parameter[1],
                            all_p_val_1 = params_3_sd$p[1],
                            all_omega_sq_1 = omega_sq_3_sd$Omega2[1],
                            all_omega_sq_ci_low_1 = omega_sq_3_sd$CI_low[1],
                            all_omega_sq_ci_hi_1 = omega_sq_3_sd$CI_high[1],
                            all_omega_sq_interpret = omega_interpret_3_sd)
sp_3_mean_var_0 <- data.frame(species_id = 3,
                              sd = "none",
                              all_params_1 = params_3_sd_0$Parameter[1],
                              all_p_val_1 = params_3_sd_0$p[1],
                              all_omega_sq_1 = omega_sq_3_sd_0$Omega2[1],
                              all_omega_sq_ci_low_1 = omega_sq_3_sd_0$CI_low[1],
                              all_omega_sq_ci_hi_1 = omega_sq_3_sd_0$CI_high[1],
                              all_omega_sq_interpret = omega_interpret_3_sd_0)
sp_3_mean_var_15 <- data.frame(species_id = 3,
                               sd = "low",
                               all_params_1 = params_3_sd_15$Parameter[1],
                               all_p_val_1 = params_3_sd_15$p[1],
                               all_omega_sq_1 = omega_sq_3_sd_15$Omega2[1],
                               all_omega_sq_ci_low_1 = omega_sq_3_sd_15$CI_low[1],
                               all_omega_sq_ci_hi_1 = omega_sq_3_sd_15$CI_high[1],
                               all_omega_sq_interpret = omega_interpret_3_sd_15)
sp_3_mean_var_30 <- data.frame(species_id = 3,
                               sd = "medium",
                               all_params_1 = params_3_sd_30$Parameter[1],
                               all_p_val_1 = params_3_sd_30$p[1],
                               all_omega_sq_1 = omega_sq_3_sd_30$Omega2[1],
                               all_omega_sq_ci_low_1 = omega_sq_3_sd_30$CI_low[1],
                               all_omega_sq_ci_hi_1 = omega_sq_3_sd_30$CI_high[1],
                               all_omega_sq_interpret = omega_interpret_3_sd_30)
sp_3_mean_var_60 <- data.frame(species_id = 3,
                               sd = "high",
                               all_params_1 = params_3_sd_60$Parameter[1],
                               all_p_val_1 = params_3_sd_60$p[1],
                               all_omega_sq_1 = omega_sq_3_sd_60$Omega2[1],
                               all_omega_sq_ci_low_1 = omega_sq_3_sd_60$CI_low[1],
                               all_omega_sq_ci_hi_1 = omega_sq_3_sd_60$CI_high[1],
                               all_omega_sq_interpret = omega_interpret_3_sd_60)

sp_3_mean_var <- plyr::rbind.fill(sp_3_mean_var, sp_3_mean_var_0, sp_3_mean_var_15, sp_3_mean_var_30, sp_3_mean_var_60)

params_4_sd <- parameters::model_parameters(aov(model_4_sd))
params_4_sd_0 <- parameters::model_parameters(aov(model_4_sd_0))
params_4_sd_15 <- parameters::model_parameters(aov(model_4_sd_15))
params_4_sd_30 <- parameters::model_parameters(aov(model_4_sd_30))
params_4_sd_60 <- parameters::model_parameters(aov(model_4_sd_60))

omega_sq_4_sd <- omega_squared(aov(model_4_sd), partial = FALSE, ci = 0.95)
omega_interpret_4_sd <- interpret_omega_squared(omega_sq_4_sd$Omega2, rules = "field2013")
omega_sq_4_sd_0 <- omega_squared(aov(model_4_sd_0), partial = FALSE, ci = 0.95)
omega_interpret_4_sd_0 <- interpret_omega_squared(omega_sq_4_sd_0$Omega2, rules = "field2013")
omega_sq_4_sd_15 <- omega_squared(aov(model_4_sd_15), partial = FALSE, ci = 0.95)
omega_interpret_4_sd_15 <- interpret_omega_squared(omega_sq_4_sd_15$Omega2, rules = "field2013")
omega_sq_4_sd_30 <- omega_squared(aov(model_4_sd_30), partial = FALSE, ci = 0.95)
omega_interpret_4_sd_30 <- interpret_omega_squared(omega_sq_4_sd_30$Omega2, rules = "field2013")
omega_sq_4_sd_60 <- omega_squared(aov(model_4_sd_60), partial = FALSE, ci = 0.95)
omega_interpret_4_sd_60 <- interpret_omega_squared(omega_sq_4_sd_60$Omega2, rules = "field2013")

sp_4_mean_var <- data.frame(species_id = 4,
                            sd = "all",
                            all_params_1 = params_4_sd$Parameter[1],
                            all_p_val_1 = params_4_sd$p[1],
                            all_omega_sq_1 = omega_sq_4_sd$Omega2[1],
                            all_omega_sq_ci_low_1 = omega_sq_4_sd$CI_low[1],
                            all_omega_sq_ci_hi_1 = omega_sq_4_sd$CI_high[1],
                            all_omega_sq_interpret = omega_interpret_4_sd)
sp_4_mean_var_0 <- data.frame(species_id = 4, 
                              sd = "none",
                              all_params_1 = params_4_sd_0$Parameter[1],
                              all_p_val_1 = params_4_sd_0$p[1],
                              all_omega_sq_1 = omega_sq_4_sd_0$Omega2[1],
                              all_omega_sq_ci_low_1 = omega_sq_4_sd_0$CI_low[1],
                              all_omega_sq_ci_hi_1 = omega_sq_4_sd_0$CI_high[1],
                              all_omega_sq_interpret = omega_interpret_4_sd_0)
sp_4_mean_var_15 <- data.frame(species_id = 4,
                               sd = "low",
                               all_params_1 = params_4_sd_15$Parameter[1],
                               all_p_val_1 = params_4_sd_15$p[1],
                               all_omega_sq_1 = omega_sq_4_sd_15$Omega2[1],
                               all_omega_sq_ci_low_1 = omega_sq_4_sd_15$CI_low[1],
                               all_omega_sq_ci_hi_1 = omega_sq_4_sd_15$CI_high[1],
                               all_omega_sq_interpret = omega_interpret_4_sd_15)
sp_4_mean_var_30 <- data.frame(species_id = 4,
                               sd = "medium",
                               all_params_1 = params_4_sd_30$Parameter[1],
                               all_p_val_1 = params_4_sd_30$p[1],
                               all_omega_sq_1 = omega_sq_4_sd_30$Omega2[1],
                               all_omega_sq_ci_low_1 = omega_sq_4_sd_30$CI_low[1],
                               all_omega_sq_ci_hi_1 = omega_sq_4_sd_30$CI_high[1],
                               all_omega_sq_interpret = omega_interpret_4_sd_30)
sp_4_mean_var_60 <- data.frame(species_id = 4,
                               sd = "high",
                               all_params_1 = params_4_sd_60$Parameter[1],
                               all_p_val_1 = params_4_sd_60$p[1],
                               all_omega_sq_1 = omega_sq_4_sd_60$Omega2[1],
                               all_omega_sq_ci_low_1 = omega_sq_4_sd_60$CI_low[1],
                               all_omega_sq_ci_hi_1 = omega_sq_4_sd_60$CI_high[1],
                               all_omega_sq_interpret = omega_interpret_4_sd_60)

sp_4_mean_var <- plyr::rbind.fill(sp_4_mean_var, sp_4_mean_var_0, sp_4_mean_var_15, sp_4_mean_var_30, sp_4_mean_var_60)

storage_log_mean_var <- plyr::rbind.fill(sp_0_mean_var, sp_1_mean_var, sp_2_mean_var, sp_3_mean_var, sp_4_mean_var)
write.csv(storage_log_mean_var, file = "out/storage_conc_log_mean_var_stats.csv")

##### Now for the histograms

## basal area distribution


data_hist_basal_reps_ind_and_all = rbind(data_hist_basal_reps, data_hist_basal_all_reps)
data_hist_basal_reps_ind_and_all$env_mean[data_hist_basal_reps_ind_and_all$env_mean == 75] = 0.75
data_hist_basal_reps_ind_and_all$env_mean[data_hist_basal_reps_ind_and_all$env_mean == 85] = 0.85
data_hist_basal_reps_ind_and_all$env_mean = -data_hist_basal_reps_ind_and_all$env_mean

p <- ggplot(data_hist_basal_reps_ind_and_all, aes(x = basal_area, color= as.factor(species_id))) + 
  geom_density() + 
  scale_y_continuous("Desnity") +
  scale_color_manual("Allocation \nStrategy:",
                    values=foxes_palettes$main[c(5, 2, 1, 4, 3)],
                    labels=c("All Strategies", "Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe")) + 
  scale_x_continuous("Mean Basal Area Coverage (m2/ha)", trans="log10") +
  facet_grid(env_sd~ env_mean, labeller = labeller(env_mean = c("-0.75" = "Med Stress",
                                                                "-0.85" = "Low Stress",
                                                                "-1" = "No Stress"),
                                                   env_sd = c("0" = "No Stoch.",
                                                              "15" = "Low Stoch.",
                                                              "30" = "Med Stoch.",
                                                              "60" = "High Stoch"))) +
  
  foxes_theme
p

file_figure <- tempfile(paste("basal_area", "density", "all", sep = "_"), tmpdir = figure_dir, fileext = ".png")
ggsave(file_figure, plot = p, device = NULL, path = NULL,
       scale = 1, width = 260, height = 220, dpi = 300, limitsize = TRUE,
       units =  "mm")




min(data_hist$height)
max(data_hist$height)

height_measurements = seq(from = 0, to = 32, by = 2)

height_hists <- plyr::rbind.fill(lapply(1:(length(height_measurements)-1), function(h){
  out <- data_hist %>%
    dplyr::filter(height >= height_measurements[h] & height < height_measurements[h+1]) %>%
    dplyr::group_by(env_sd, env_mean, env, species_id, env_rep, run_rep) %>%
    dplyr::count()
  out_2 <- data_hist %>%
    dplyr::filter(height >= height_measurements[h] & height < height_measurements[h+1]) %>%
    dplyr::group_by(env_sd, env_mean, env, env_rep, run_rep) %>%
    dplyr::count()
  out_2$species_id = 0
  out <- rbind(out, out_2)
  out$height = (height_measurements[h] + height_measurements[h+1])/2
  return(out)
}))


height_hists_summ <- height_hists %>%
  dplyr::group_by(env_sd, env_mean, env, species_id, height) %>%
  dplyr::summarise(mean_num_ind = mean(n),
                   sd_num_ind = sd(n))

# Reorder following a precise order
height_hists_summ$env_mean[height_hists_summ$env_mean == 75] <- 0.75
height_hists_summ$env_mean[height_hists_summ$env_mean == 85] <- 0.85

height_hists_summ$env_mean <- as.factor(height_hists_summ$env_mean)




p <- ggplot(height_hists_summ, aes(x=height, y = mean_num_ind)) + 
  geom_line(aes(color=as.factor(species_id))) +
  geom_ribbon(aes(ymin = mean_num_ind - sd_num_ind, 
                  ymax = mean_num_ind + sd_num_ind, 
                  fill = as.factor(species_id)), alpha = 0.15) +
  scale_color_manual("Species Id:",
                     values=c(foxes_palettes$main[c(5, 2, 1, 4, 3)]),
                     labels=c("All Strategies", "Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe")) +
  scale_fill_manual("Species Id:",
                     values=c(foxes_palettes$main[c(5, 2, 1, 4, 3)]),
                     labels=c("All Strategies", "Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe")) +
  ylab("Average number of individual in 100m2 plot") +
  scale_x_continuous("Height (m)") +
  facet_grid(env_sd~ env_mean, labeller = labeller(env_mean = c("0.75" = "Med Stress",
                                                                "0.85" = "Low Stress",
                                                                "1" = "No Stress"),
                                                   env_sd = c("0" = "No Stoch.",
                                                              "15" = "Low Stoch.",
                                                              "30" = "Med Stoch.",
                                                              "60" = "High Stoch"))) + 
  guides(fill = FALSE) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        legend.position = "right",
        plot.margin = margin(1,1,1,1, "cm"),
        text=element_text(size=12, color = "#083855", family="Lato Light"),
        axis.text=element_text(size=8, color = "#083855", family="Lato Light"),
        strip.background = element_rect(fill = "white"))
p

file_figure <- tempfile(paste("height_dist", "hist", "all", sep = "_"), tmpdir = figure_dir, fileext = ".png")
ggsave(file_figure, plot = p, device = NULL, path = NULL,
       scale = 1, width = 260, height = 220, dpi = 300, limitsize = TRUE,
       units =  "mm")





min(data_hist$height)
max(data_hist$height)

height_measurements = seq(from = 0, to = 32, by = 2)

height_hists <- plyr::rbind.fill(lapply(1:(length(height_measurements)-1), function(h){
  out <- data_hist %>%
    dplyr::filter(height >= height_measurements[h] & height < height_measurements[h+1]) %>%
    dplyr::group_by(env_sd, env_mean, env, species_id, env_rep, run_rep) %>%
    dplyr::count()
  out_2 <- data_hist %>%
    dplyr::filter(height >= height_measurements[h] & height < height_measurements[h+1]) %>%
    dplyr::group_by(env_sd, env_mean, env, env_rep, run_rep) %>%
    dplyr::count()
  out_2$species_id = 0
  out <- rbind(out, out_2)
  out$height = (height_measurements[h] + height_measurements[h+1])/2
  return(out)
}))


height_hists_summ <- height_hists %>%
  dplyr::group_by(env_sd, env_mean, env, species_id, height) %>%
  dplyr::summarise(mean_num_ind = mean(n),
                   sd_num_ind = sd(n))

# Reorder following a precise order
height_hists_summ$env_mean[height_hists_summ$env_mean == 75] <- 0.75
height_hists_summ$env_mean[height_hists_summ$env_mean == 85] <- 0.85

height_hists_summ$env_mean <- as.factor(height_hists_summ$env_mean)




p <- ggplot(height_hists_summ, aes(x=height, y = mean_num_ind)) + 
  geom_line(aes(color=as.factor(species_id))) +
  geom_point(aes(color=as.factor(species_id))) +
  geom_ribbon(aes(ymin = mean_num_ind - sd_num_ind, 
                  ymax = mean_num_ind + sd_num_ind, 
                  fill = as.factor(species_id)), alpha = 0.15) +
  scale_color_manual("Species Id:",
                     values=c(foxes_palettes$main[c(5, 2, 1, 4, 3)]),
                     labels=c("All Strategies", "Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe")) +
  scale_fill_manual("Species Id:",
                    values=c(foxes_palettes$main[c(5, 2, 1, 4, 3)]),
                    labels=c("All Strategies", "Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe")) +
  ylab("Average number of individual in 100m2 plot") +
  scale_x_continuous("Height (m)") +
  facet_grid(env_sd~ env_mean, labeller = labeller(env_mean = c("0.75" = "Med Stress",
                                                                "0.85" = "Low Stress",
                                                                "1" = "No Stress"),
                                                   env_sd = c("0" = "No Stoch.",
                                                              "15" = "Low Stoch.",
                                                              "30" = "Med Stoch.",
                                                              "60" = "High Stoch"))) + 
  guides(fill = FALSE) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        legend.position = "right",
        plot.margin = margin(1,1,1,1, "cm"),
        text=element_text(size=12, color = "#083855", family="Lato Light"),
        axis.text=element_text(size=8, color = "#083855", family="Lato Light"),
        strip.background = element_rect(fill = "white"))
p

file_figure <- tempfile(paste("height_dist", "hist", "all", sep = "_"), tmpdir = figure_dir, fileext = ".png")
ggsave(file_figure, plot = p, device = NULL, path = NULL,
       scale = 1, width = 260, height = 220, dpi = 300, limitsize = TRUE,
       units =  "mm")

data_hist_no_spec = data_hist
data_hist$species_id = 0

data_hist_new <- rbind(data_hist_no_spec, data_hist)

p <- ggplot(data_hist_new, aes(x = as.factor(species_id), y = height)) + 
  geom_violin(aes(color=as.factor(species_id), fill=as.factor(species_id)), alpha = 0.75) +
  scale_color_manual("Species Id:",
                     values=c(foxes_palettes$main[c(5, 2, 1, 4, 3)]),
                     labels=c("All Strategies", "Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe")) +
  scale_fill_manual("Species Id:",
                    values=c(foxes_palettes$main[c(5, 2, 1, 4, 3)]),
                    labels=c("All Strategies", "Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe")) +
  ylab("Height (m)") +
  scale_x_discrete("Strategy", labels = c("All Strategies", "Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe")) +
  facet_grid(env_sd~ env_mean, labeller = labeller(env_mean = c("75" = "Med Stress",
                                                                "85" = "Low Stress",
                                                                "1" = "No Stress"),
                                                   env_sd = c("0" = "No Stoch.",
                                                              "15" = "Low Stoch.",
                                                              "30" = "Med Stoch.",
                                                              "60" = "High Stoch"))) +
  foxes_theme + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
p

file_figure <- tempfile(paste("height_dist", "violin", "all", sep = "_"), tmpdir = figure_dir, fileext = ".png")
ggsave(file_figure, plot = p, device = NULL, path = NULL,
       scale = 1, width = 260, height = 220, dpi = 300, limitsize = TRUE,
       units =  "mm")


min(data_hist$mass_storage)
max(data_hist$mass_storage)
mass_store_measurements = seq(from = -2, to = 20, by = 1)

mass_store_hists <- plyr::rbind.fill(lapply(1:(length(mass_store_measurements)-1), function(h){
  out <- data_hist %>%
    dplyr::filter(mass_storage >= mass_store_measurements[h] & mass_storage < mass_store_measurements[h+1]) %>%
    dplyr::group_by(env_sd, env_mean, env, species_id, env_rep, run_rep) %>%
    dplyr::count()
  out_2 <- data_hist %>%
    dplyr::filter(mass_storage >= mass_store_measurements[h] & mass_storage < mass_store_measurements[h+1]) %>%
    dplyr::group_by(env_sd, env_mean, env, env_rep, run_rep) %>%
    dplyr::count()
  out_2$species_id = 0
  out <- rbind(out, out_2)
  out$mass_storage = (mass_store_measurements[h] + mass_store_measurements[h+1])/2
  return(out)
}))


mass_store_hists_summ <- mass_store_hists %>%
  dplyr::group_by(env_sd, env_mean, env, species_id, mass_storage) %>%
  dplyr::summarise(mean_num_ind = mean(n),
                   sd_num_ind = sd(n))

# Reorder following a precise order
mass_store_hists_summ$env_mean[mass_store_hists_summ$env_mean == 75] <- 0.75
mass_store_hists_summ$env_mean[mass_store_hists_summ$env_mean == 85] <- 0.85

mass_store_hists_summ$env_mean <- as.factor(mass_store_hists_summ$env_mean)




p <- ggplot(mass_store_hists_summ, aes(x=mass_storage, y = mean_num_ind)) + 
  geom_line(aes(color=as.factor(species_id))) +
  geom_point(aes(color=as.factor(species_id))) +
  geom_ribbon(aes(ymin = mean_num_ind - sd_num_ind, 
                  ymax = mean_num_ind + sd_num_ind, 
                  fill = as.factor(species_id)), alpha = 0.15) +
  scale_color_manual("Species Id:",
                     values=c(foxes_palettes$main[c(5, 2, 1, 4, 3)]),
                     labels=c("All Strategies", "Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe")) +
  scale_fill_manual("Species Id:",
                    values=c(foxes_palettes$main[c(5, 2, 1, 4, 3)]),
                    labels=c("All Strategies", "Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe")) +
  ylab("Average number of individual in 100m2 plot") +
  scale_x_continuous("Storage Mass (kgC)") +
  facet_grid(env_sd~ env_mean, labeller = labeller(env_mean = c("0.75" = "Med Stress",
                                                                "0.85" = "Low Stress",
                                                                "1" = "No Stress"),
                                                   env_sd = c("0" = "No Stoch.",
                                                              "15" = "Low Stoch.",
                                                              "30" = "Med Stoch.",
                                                              "60" = "High Stoch"))) + 
  guides(fill = FALSE) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        legend.position = "right",
        plot.margin = margin(1,1,1,1, "cm"),
        text=element_text(size=12, color = "#083855", family="Lato Light"),
        axis.text=element_text(size=8, color = "#083855", family="Lato Light"),
        strip.background = element_rect(fill = "white"))
p

file_figure <- tempfile(paste("mass_store_dist", "hist", "all", sep = "_"), tmpdir = figure_dir, fileext = ".png")
ggsave(file_figure, plot = p, device = NULL, path = NULL,
       scale = 1, width = 260, height = 220, dpi = 300, limitsize = TRUE,
       units =  "mm")


p <- ggplot(data_hist_new, aes(x = as.factor(species_id), y = mass_storage)) + 
  geom_violin(aes(color=as.factor(species_id), fill=as.factor(species_id)), alpha = 0.75) +
  scale_color_manual("Species Id:",
                     values=c(foxes_palettes$main[c(5, 2, 1, 4, 3)]),
                     labels=c("All Strategies", "Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe")) +
  scale_fill_manual("Species Id:",
                    values=c(foxes_palettes$main[c(5, 2, 1, 4, 3)]),
                    labels=c("All Strategies", "Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe")) +
  ylab("Storage Mass (kgC)") +
  scale_x_discrete("Strategy", labels = c("All Strategies", "Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe")) +
  facet_grid(env_sd~ env_mean, labeller = labeller(env_mean = c("75" = "Med Stress",
                                                                "85" = "Low Stress",
                                                                "1" = "No Stress"),
                                                   env_sd = c("0" = "No Stoch.",
                                                              "15" = "Low Stoch.",
                                                              "30" = "Med Stoch.",
                                                              "60" = "High Stoch"))) +
  foxes_theme + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
p

file_figure <- tempfile(paste("mass_store_dist", "violin", "all", sep = "_"), tmpdir = figure_dir, fileext = ".png")
ggsave(file_figure, plot = p, device = NULL, path = NULL,
       scale = 1, width = 260, height = 220, dpi = 300, limitsize = TRUE,
       units =  "mm")


p <- ggplot(data_hist_new, aes(x = as.factor(species_id), y = storage_portion)) + 
  geom_violin(aes(color=as.factor(species_id), fill=as.factor(species_id)), alpha = 0.75) +
  scale_color_manual("Species Id:",
                     values=c(foxes_palettes$main[c(5, 2, 1, 4, 3)]),
                     labels=c("All Strategies", "Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe")) +
  scale_fill_manual("Species Id:",
                    values=c(foxes_palettes$main[c(5, 2, 1, 4, 3)]),
                    labels=c("All Strategies", "Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe")) +
  ylab("Storage Concentration (kgC/kgC)") +
  scale_x_discrete("Strategy", labels = c("All Strategies", "Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe")) +
  facet_grid(env_sd~ env_mean, labeller = labeller(env_mean = c("75" = "Med Stress",
                                                                "85" = "Low Stress",
                                                                "1" = "No Stress"),
                                                   env_sd = c("0" = "No Stoch.",
                                                              "15" = "Low Stoch.",
                                                              "30" = "Med Stoch.",
                                                              "60" = "High Stoch"))) +
  foxes_theme + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
p

file_figure <- tempfile(paste("storage_conc", "violin", "all", sep = "_"), tmpdir = figure_dir, fileext = ".png")
ggsave(file_figure, plot = p, device = NULL, path = NULL,
       scale = 1, width = 260, height = 220, dpi = 300, limitsize = TRUE,
       units =  "mm")

  

test_vals <- data.frame(species = rep(0:4, each=9),
                        env = rep(c(1, 75, 85, 90, 100, 105, 115, 135, 145), times = 5))



### Check for normalities

shapiro.tests <- plyr::rbind.fill(lapply(1:nrow(test_vals), function(ind){
  sub_data <- data_hist %>%
    filter(env == test_vals$env[ind] & time_slice == 100) %>%
    dplyr::group_by(species_id, env_rep, run_rep) %>%
    dplyr::summarise(height_max = max(height),
                     height_ave = mean(height),
                     basal_area = sum(area_stem) * 100,
                     basal_area_ave = mean(area_stem),
                     storage_conc = mean(storage_portion),
                     storage_mean_plot = sum(mass_storage)/100,
                     storage_mean_ind = mean(mass_storage))
  
  height_max = sub_data$height_max
  height_max_log = log(sub_data$height_max)
  shapiro_height_max = shapiro.test(height_max)
  shapiro_height_max_log = shapiro.test(height_max_log)
  
  height_ave = sub_data$height_ave
  height_ave_log = log(sub_data$height_ave)
  shapiro_height_ave = shapiro.test(height_ave)
  shapiro_height_ave_log = shapiro.test(height_ave_log)
  
  basal_area = sub_data$basal_area
  basal_area_log = log(sub_data$basal_area)
  shapiro_basal_area = shapiro.test(basal_area)
  shapiro_basal_area_log = shapiro.test(basal_area_log)
  
  basal_area_ind = sub_data$basal_area_ave
  basal_area_ind_log = log(sub_data$basal_area_ave)
  shapiro_basal_area_ind = shapiro.test(basal_area_ind)
  shapiro_basal_area_ind_log = shapiro.test(basal_area_ind_log)
  
  storage_conc = sub_data$storage_conc
  storage_conc_log = log(sub_data$storage_conc)
  shapiro_storage_conc = shapiro.test(storage_conc)
  shapiro_storage_conc_log = shapiro.test(storage_conc_log)
  
  storage_mean_plot = sub_data$storage_mean_plot
  storage_mean_plot_log = log(sub_data$storage_mean_plot)
  shapiro_storage_mean_plot = shapiro.test(storage_mean_plot)
  shapiro_storage_mean_plot_log = shapiro.test(storage_mean_plot_log)
  
  storage_mean_ind = sub_data$storage_mean_ind
  storage_mean_ind_log = log(sub_data$storage_mean_ind)
  shapiro_storage_mean_ind = shapiro.test(storage_mean_ind)
  shapiro_storage_mean_ind_log = shapiro.test(storage_mean_ind_log)
  
  return(data.frame(env = test_vals$env[ind],
                    species_id = test_vals$species[ind],
                    shapiro_test_out = c(shapiro_height_max$p.value,
                                         shapiro_height_max_log$p.value,
                                         shapiro_height_ave$p.value,
                                         shapiro_height_ave_log$p.value,
                                         shapiro_basal_area$p.value,
                                         shapiro_basal_area_log$p.value,
                                         shapiro_basal_area_ind$p.value,
                                         shapiro_basal_area_ind_log$p.value,
                                         shapiro_storage_conc$p.value,
                                         shapiro_storage_conc_log$p.value,
                                         shapiro_storage_mean_plot$p.value,
                                         shapiro_storage_mean_plot_log$p.value,
                                         shapiro_storage_mean_ind$p.value,
                                         shapiro_storage_mean_ind_log$p.value),
                    value = c("height-max", "height-max-log", 
                              "height-ave", "height-ave-log", 
                              "basal-area", "basal-area-log",
                              "basal-area-ind", "basal-area-ind-log",
                              "storage-conc", "storage-conc-log",
                              "storage-mean-plot", "storage-mean-plot-log",
                              "storage-mean-ind", "storage-mean-ind-log")))
}))


dir.create("out/stats")

write.csv(shapiro.tests, file = "out/stats/shapiro_tests.csv")



sub_data <- data_hist %>%
  filter(env_mean != 1 & time_slice == 100) %>%
  dplyr::group_by(species_id, env_rep, run_rep, env_mean, env_sd) %>%
  dplyr::summarise(height_ave = mean(height))

aov_general_out_1 <- aov(height_ave ~ as.factor(env_mean) * env_sd, data = subset(sub_data, species_id == 1))
aov_general_out_2 <- aov(height_ave ~ as.factor(env_mean) * env_sd, data = subset(sub_data, species_id == 2))
aov_general_out_3 <- aov(height_ave ~ as.factor(env_mean) * env_sd, data = subset(sub_data, species_id == 3))
aov_general_out_4 <- aov(height_ave ~ as.factor(env_mean) * env_sd, data = subset(sub_data, species_id == 4))
anova(aov_general_out_1)
anova(aov_general_out_2)
anova(aov_general_out_3)
anova(aov_general_out_4)

data_hist_basal_100 <- subset(data_hist_basal, time_slice == 100 & env_mean != 1)
data_hist_basal_all_100 <- subset(data_hist_basal_all, time_slice == 100 & env_mean != 1)
p <- ggplot(data_hist_basal_100, aes(x=env_sd, 
                                y = basal_area_mean, 
                                color = as.factor(species_id), 
                                linetype=as.factor(env_mean))) + 
  geom_line() + 
  geom_errorbar(aes(ymin = basal_area_mean - basal_area_sd, ymax = basal_area_mean + basal_area_sd), alpha = 0.5) + 
  geom_point() + 
  scale_color_manual("Species Id:",
                     values=c(foxes_palettes$main[c(2, 1, 4, 3)]),
                     labels=c("Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe")) +
  scale_linetype_discrete("Environment Mean (yr):",
                          labels = c("0.75", "0.85")) +
  ylab("Final Basal Area Per Hectare (m2/ha)") +
  xlab("Standard Deviation of Stress (d)") +
  species_wrap + 
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        legend.position = "right",
        plot.margin = margin(1,1,1,1, "cm"),
        text=element_text(size=12, color = "#083855", family="Lato Light"),
        axis.text=element_text(size=8, color = "#083855", family="Lato Light"),
        strip.background = element_rect(fill = "white"))
p


file_figure <- tempfile(paste("basal_mean_sd", sep = "_"), tmpdir = figure_dir, fileext = ".png")
ggsave(file_figure, plot = p, device = NULL, path = NULL,
       scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
       units =  "mm")

p <- ggplot(data_hist_basal_all_100, aes(x=env_sd, 
                                     y = basal_area_mean, 
                                     linetype=as.factor(env_mean))) + 
  geom_line(color = "#083855") + 
  geom_errorbar(aes(ymin = basal_area_mean - basal_area_sd, ymax = basal_area_mean + basal_area_sd), alpha = 0.5, color = "#083855") + 
  geom_point() + 
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


file_figure <- tempfile(paste("basal_mean_sd", "all", sep = "_"), tmpdir = figure_dir, fileext = ".png")
ggsave(file_figure, plot = p, device = NULL, path = NULL,
       scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
       units =  "mm")



p <- ggplot(data_hist_basal_100, aes(x=env_mean, 
                                y = basal_area_mean, 
                                color = as.factor(species_id), 
                                linetype = as.factor(env_sd))) + 
  geom_line() + 
  geom_point() + 
  scale_color_manual("Species Id:",
                     values=c(foxes_palettes$main[c(2, 1, 4, 3)]),
                     labels=c("Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe")) +
  scale_linetype_discrete("Environment SD (d):",
                          labels = c("0", "15", "30", "60")) +
  ylab("Final Basal Area Per Hectare (m2/ha)") +
  xlab("Mean of Stress (yr)") +
  scale_x_continuous(breaks = c(85, 75), labels = c("low stress", "med stress")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        legend.position = "right",
        plot.margin = margin(1,1,1,1, "cm"),
        text=element_text(size=12, color = "#083855", family="Lato Light"),
        axis.text=element_text(size=8, color = "#083855", family="Lato Light"),
        strip.background = element_rect(fill = "white"))
p



file_figure <- tempfile(paste("basal_mean_mean", sep = "_"), tmpdir = figure_dir, fileext = ".png")
ggsave(file_figure, plot = p, device = NULL, path = NULL,
       scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
       units =  "mm")






p <- ggplot(data_hist_basal_all_100, aes(x=env_mean, 
                                     y = basal_area_mean, 
                                     linetype = as.factor(env_sd))) + 
  geom_line(color = "#083855") + 
  geom_point(color = "#083855") + 
  # scale_color_manual("Species Id:",
  #                    values=c(foxes_palettes$main[c(2, 1, 4, 3)]),
  #                    labels=c("Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe")) +
  scale_linetype_discrete("Environment SD (d):",
                          labels = c("0", "15", "30", "60")) +
  ylab("Final Basal Area Per Hectare (m2/ha)") +
  xlab("Mean of Stress (yr)") +
  scale_x_continuous(breaks = c(85, 75), labels = c("low stress", "med stress")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        legend.position = "right",
        plot.margin = margin(1,1,1,1, "cm"),
        text=element_text(size=12, color = "#083855", family="Lato Light"),
        axis.text=element_text(size=8, color = "#083855", family="Lato Light"),
        strip.background = element_rect(fill = "white"))
p



file_figure <- tempfile(paste("basal_mean_mean", "all", sep = "_"), tmpdir = figure_dir, fileext = ".png")
ggsave(file_figure, plot = p, device = NULL, path = NULL,
       scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
       units =  "mm")



## storage concentration

data_hist_storage_100 <- subset(data_hist_storage, time_slice == 100 & env_mean != 1)
data_hist_storage_all_100 <- subset(data_hist_storage_all, time_slice == 100 & env_mean != 1)
p <- ggplot(data_hist_storage_100, aes(x=env_sd, 
                                     y = storage_mean, 
                                     color = as.factor(species_id), 
                                     linetype=as.factor(env_mean))) + 
  geom_line() + 
  geom_errorbar(aes(ymin = storage_mean - storage_sd, ymax = storage_mean + storage_sd), alpha = 0.5) + 
  geom_point() + 
  scale_color_manual("Species Id:",
                     values=c(foxes_palettes$main[c(2, 1, 4, 3)]),
                     labels=c("Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe")) +
  scale_linetype_discrete("Environment Mean (yr):",
                          labels = c("0.75", "0.85")) +
  ylab("Average Storage Concentration (kgC/kgC)") +
  xlab("Standard Deviation of Stress (d)") +
  species_wrap + 
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        legend.position = "right",
        plot.margin = margin(1,1,1,1, "cm"),
        text=element_text(size=12, color = "#083855", family="Lato Light"),
        axis.text=element_text(size=8, color = "#083855", family="Lato Light"),
        strip.background = element_rect(fill = "white"))
p


file_figure <- tempfile(paste("store_conc_sd", sep = "_"), tmpdir = figure_dir, fileext = ".png")
ggsave(file_figure, plot = p, device = NULL, path = NULL,
       scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
       units =  "mm")

p <- ggplot(data_hist_storage_all_100, aes(x=env_sd, 
                                         y = storage_mean, 
                                         linetype=as.factor(env_mean))) + 
  geom_line(color = "#083855") + 
  geom_errorbar(aes(ymin = storage_mean - storage_sd, ymax = storage_mean + storage_sd), alpha = 0.5, color = "#083855") + 
  geom_point() + 
  scale_linetype_discrete("Environment Mean (yr):",
                          labels = c("0.75", "0.85")) +
  ylab("Average Storage Concentration (kgC/kgC)") +
  xlab("Standard Deviation of Stress (d)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        legend.position = "right",
        plot.margin = margin(1,1,1,1, "cm"),
        text=element_text(size=12, color = "#083855", family="Lato Light"),
        axis.text=element_text(size=8, color = "#083855", family="Lato Light"),
        strip.background = element_rect(fill = "white"))
p


file_figure <- tempfile(paste("store_conc_sd", "all", sep = "_"), tmpdir = figure_dir, fileext = ".png")
ggsave(file_figure, plot = p, device = NULL, path = NULL,
       scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
       units =  "mm")



p <- ggplot(data_hist_storage_100, aes(x=env_mean, 
                                     y = storage_mean, 
                                     color = as.factor(species_id), 
                                     linetype = as.factor(env_sd))) + 
  geom_line() + 
  geom_errorbar(aes(ymin = storage_mean - storage_sd, ymax = storage_mean + storage_sd), alpha = 0.5) + 
  geom_point() + 
  scale_color_manual("Species Id:",
                     values=c(foxes_palettes$main[c(2, 1, 4, 3)]),
                     labels=c("Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe")) +
  scale_linetype_discrete("Environment SD (d):",
                          labels = c("0", "15", "30", "60")) +
  ylab("Average Storage Concentration (kgC/kgC)") +
  xlab("Mean of Stress (yr)") +
  scale_x_continuous(breaks = c(85, 75), labels = c("low stress", "med stress")) +
  species_wrap + 
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        legend.position = "right",
        plot.margin = margin(1,1,1,1, "cm"),
        text=element_text(size=12, color = "#083855", family="Lato Light"),
        axis.text=element_text(size=8, color = "#083855", family="Lato Light"),
        strip.background = element_rect(fill = "white"))
p



file_figure <- tempfile(paste("store_conc_mean", sep = "_"), tmpdir = figure_dir, fileext = ".png")
ggsave(file_figure, plot = p, device = NULL, path = NULL,
       scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
       units =  "mm")






p <- ggplot(data_hist_storage_all_100, aes(x=env_mean, 
                                         y = storage_mean, 
                                         linetype = as.factor(env_sd))) + 
  geom_line(color = "#083855") + 
  geom_point(color = "#083855") + 
  geom_errorbar(aes(ymin = storage_mean - storage_sd, ymax = storage_mean + storage_sd), alpha = 0.5, color = "#083855") + 
  scale_linetype_discrete("Environment SD (d):",
                          labels = c("0", "15", "30", "60")) +
  ylab("Average Storage Concentration (kgC/kgC)") +
  xlab("Mean of Stress (yr)") +
  scale_x_continuous(breaks = c(85, 75), labels = c("low stress", "med stress")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        legend.position = "right",
        plot.margin = margin(1,1,1,1, "cm"),
        text=element_text(size=12, color = "#083855", family="Lato Light"),
        axis.text=element_text(size=8, color = "#083855", family="Lato Light"),
        strip.background = element_rect(fill = "white"))
p



file_figure <- tempfile(paste("store_conc_mean", "all", sep = "_"), tmpdir = figure_dir, fileext = ".png")
ggsave(file_figure, plot = p, device = NULL, path = NULL,
       scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
       units =  "mm")

