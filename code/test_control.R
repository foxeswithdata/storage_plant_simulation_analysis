rm(list = ls())

library(tidyverse)
library(ggExtra)

source('/Volumes/T7/Chapter 2 - data processing/chapter_2_data_process/code/foxes_pallettes.R', echo=TRUE)

fig_dir = "out/control"
dir.create(fig_dir)

tree_data <- readRDS("data/full_run_processed/env_1_0d/envrep_1/runrep_1/tree_data_processed.rds")
env_data <- readRDS("data/full_run_processed/env_1_0d/envrep_1/runrep_1/env_data_processed.rds")
param_data <- readRDS("data/full_run/env_1_0d/envrep_1/runrep_1/run_parametersa7674121334.rds")
# tree_data_files <- list.files("data/full_run/env_075_30d/envrep_6/runrep_2/", pattern = "tree_data")
 

env_stress <- param_data$parameters$control$stress_regime
env_stress <- data.frame(time = 0:105,
                         stress = (env_stress - (0:105)),
                         env_stress = env_stress, 
                         temp = 1)

tree_data_live <- subset(tree_data, is_alive == TRUE)

tree_data_ave <- tree_data_live %>% 
  dplyr::group_by(species_id, time) %>%
  dplyr::summarise(height = mean(height),
            height_sd = sd(height))

tree_data_ave_all <- tree_data_live %>% 
  dplyr::group_by(time) %>%
  dplyr::summarise(height = mean(height),
            height_sd = sd(height))

p <- ggplot(tree_data_live, aes(x= time, y = height)) + 
  geom_line(aes(group = as.factor(species_id * 10 + tree_id), color = as.factor(species_id)), 
            alpha = 0.15) +
  geom_line(data = tree_data_ave, aes(color = as.factor(species_id))) +
  geom_ribbon(data = tree_data_ave, aes(color = as.factor(species_id),
  fill = as.factor(species_id),
  ymin = height - height_sd,
  ymax = height + height_sd), alpha = 0.35, linetype = "blank") +
  geom_line(data = tree_data_ave_all, color = "#083855", alpha = 0.75) +
  geom_ribbon(data = tree_data_ave_all, aes(ymin = height - height_sd, ymax = height + height_sd), alpha = 0.35, linetype = "blank") +
  scm_ribbon + 
  sfm + 
  species_wrap +
  scale_x_continuous("Time (yrs)") +
  scale_y_continuous("Individual Plant Height (m/plant)") +
  foxes_theme +
  theme(legend.position = "bottom")
p

file_figure <- tempfile(paste("individual_height", sep = "_"), tmpdir = fig_dir, fileext = ".png")
ggsave(file_figure, plot = p, device = NULL, path = NULL,
       scale = 1, width = 240, height = 138, dpi = 600, limitsize = TRUE,
       units =  "mm")


tree_data_ave <- tree_data_live %>% 
  dplyr::group_by(species_id, time) %>%
  dplyr::summarise(num_live = n())

p <- ggplot(tree_data_ave, aes(x=time, y=num_live, fill = as.factor(species_id),
                               color = as.factor(species_id))) + 
  geom_bar(stat = "identity", position = "stack") + 
  scale_color_manual("Allocation \nStrategy:",
                     values=foxes_palettes$main[c(2, 1, 4, 3)],
                     labels=c("Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe")) +
  scale_fill_manual("Allocation \nStrategy:",
                    values=foxes_palettes$main[c(2, 1, 4, 3)],
                    labels=c("Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe"))+
  foxes_theme + 
  scale_x_continuous("Time (yrs)") +
  scale_y_continuous("Number of individuals") +
  theme(legend.position = "bottom")
p

file_figure <- tempfile(paste("numbers_of_species", sep = "_"), tmpdir = fig_dir, fileext = ".png")
ggsave(file_figure, plot = p, device = NULL, path = NULL,
       scale = 1, width = 190, height = 138, dpi = 600, limitsize = TRUE,
       units =  "mm")


#summaries:

num_ind_sp_1 <- subset(tree_data_ave, species_id == 1 & time > 20)
num_ind_sp_2 <- subset(tree_data_ave, species_id == 2 & time > 20)
num_ind_sp_3 <- subset(tree_data_ave, species_id == 3 & time > 20)
num_ind_sp_4 <- subset(tree_data_ave, species_id == 4 & time > 20)

max(subset(num_ind_sp_1, time > 20, select=num_live))
max(subset(num_ind_sp_2, time > 20, select=num_live))
max(subset(num_ind_sp_3, time > 20, select=num_live))
max(subset(num_ind_sp_4, time > 20, select=num_live))

mean(num_ind_sp_1$num_live)
mean(num_ind_sp_2$num_live)
mean(num_ind_sp_3$num_live)
mean(num_ind_sp_4$num_live)

### MORTALITY and STORAGE


seq_time = seq(from = 1, to = 104, by = 1)


tree_data_live_ave <- plyr::rbind.fill(lapply(seq_time, function(t){
  dat <- tree_data_live %>% 
    filter(time > (t-1) & time < (t+1)) %>%
    dplyr::group_by(species_id, tree_id) %>%
    dplyr::summarise(storage_portion = mean(storage_portion, na.rm=TRUE),
              storage_portion_sd = sd(storage_portion, na.rm=TRUE)) 
  dat$time = t
  return(dat)
}))

tree_data_ave <- plyr::rbind.fill(lapply(seq_time, function(t){
  dat <- tree_data_live %>% 
    filter(time > (t-1) & time < (t+1)) %>%
    dplyr::group_by(species_id) %>%
    dplyr::summarise(storage_portion = mean(storage_portion, na.rm=TRUE),
              storage_portion_sd = sd(storage_portion, na.rm=TRUE))
  dat$time = t
  return(dat)
}))

tree_data_ave_all <- plyr::rbind.fill(lapply(seq_time, function(t){
  dat <- tree_data_live %>% 
    filter(time > (t-1) & time < (t+1)) %>%
    dplyr::summarise(storage_portion = mean(storage_portion, na.rm=TRUE),
              storage_portion_sd = sd(storage_portion, na.rm=TRUE)) 
  dat$time = t
  return(dat)
}))



p <- ggplot(tree_data_live_ave, aes(x= time, y = storage_portion)) + 
  geom_line(aes(group = as.factor(species_id * 10 + tree_id), color = as.factor(species_id)), 
            alpha = 0.15) +
  geom_line(data = tree_data_ave, aes(color = as.factor(species_id))) + 
  geom_ribbon(data = tree_data_ave, aes(color = as.factor(species_id), 
                                        fill = as.factor(species_id), 
                                        ymin = storage_portion - storage_portion_sd, 
                                        ymax = storage_portion + storage_portion_sd), alpha = 0.35, linetype = "blank") + 
  geom_line(data = tree_data_ave_all, color = "#083855", alpha = 0.75) + 
  geom_ribbon(data = tree_data_ave_all, aes(ymin = storage_portion - storage_portion_sd, 
                                            ymax = storage_portion + storage_portion_sd), 
              alpha = 0.35, linetype = "blank") + 
  scm + 
  species_wrap +
  scale_x_continuous("Time (yrs)") +
  scale_y_continuous("Storage Portion per kg of live mass (kg/kg)") +
  foxes_theme +
  theme(legend.position = "bottom")
p



file_figure <- tempfile(paste("storage_portion_ind", sep = "_"), tmpdir = fig_dir, fileext = ".png")
ggsave(file_figure, plot = p, device = NULL, path = NULL,
       scale = 1, width = 190, height = 138, dpi = 600, limitsize = TRUE,
       units =  "mm")


seq_time = seq(from = 1, to = 104, by = 1)

tree_data_live_ave <- plyr::rbind.fill(lapply(seq_time, function(t){
  dat <- tree_data_live %>% 
    dplyr::filter(time > (t-1) & time < (t+1)) %>%
    dplyr::group_by(species_id, tree_id) %>%
    dplyr::summarise(mass_storage = mean(mass_storage, na.rm=TRUE),
              mass_storage_sd = sd(mass_storage, na.rm=TRUE)) 
  dat$time = t
  return(dat)
}))

tree_data_ave <- plyr::rbind.fill(lapply(seq_time, function(t){
  dat <- tree_data_live %>% 
    dplyr::filter(time > (t-1) & time < (t+1)) %>%
    dplyr::group_by(species_id) %>%
    dplyr::summarise(mass_storage = mean(mass_storage, na.rm=TRUE),
              mass_storage_sd = sd(mass_storage, na.rm=TRUE)) 
  dat$time = t
  return(dat)
}))

tree_data_ave_all <- plyr::rbind.fill(lapply(seq_time, function(t){
  dat <- tree_data_live %>% 
    dplyr::filter(time > (t-1) & time < (t+1)) %>%
    dplyr::summarise(mass_storage = mean(mass_storage, na.rm=TRUE),
              mass_storage_sd = sd(mass_storage, na.rm=TRUE))  
  dat$time = t
  return(dat)
}))



p <- ggplot(tree_data_live_ave, aes(x= time, y = mass_storage)) + 
  geom_line(aes(group = as.factor(species_id * 10 + tree_id), color = as.factor(species_id)), 
            alpha = 0.15) +
  geom_line(data = tree_data_ave, aes(color = as.factor(species_id))) + 
  geom_ribbon(data = tree_data_ave, aes(color = as.factor(species_id), 
                                        fill = as.factor(species_id), 
                                        ymin = mass_storage - mass_storage_sd, 
                                        ymax = mass_storage + mass_storage_sd), alpha = 0.35, linetype = "blank") + 
  geom_line(data = tree_data_ave_all, color = "#083855", alpha = 0.75) + 
  geom_ribbon(data = tree_data_ave_all, aes(ymin = mass_storage - mass_storage_sd, 
                                            ymax = mass_storage + mass_storage_sd), 
              alpha = 0.35, linetype = "blank") + 
  scm_ribbon + 
  sfm + 
  species_wrap +
  scale_x_continuous("Time (yrs)") +
  scale_y_continuous("Individual Storage Mass (kg/plant)") +
  foxes_theme +
  theme(legend.position = "bottom")
p

file_figure <- tempfile(paste("storage_mass_ind", sep = "_"), tmpdir = fig_dir, fileext = ".png")
ggsave(file_figure, plot = p, device = NULL, path = NULL,
       scale = 1, width = 240, height = 138, dpi = 600, limitsize = TRUE,
       units =  "mm")




tree_data_live_M_ave <- plyr::rbind.fill(lapply(seq_time, function(t){
  dat <- tree_data_live %>% 
    dplyr::filter(time > (t-1) & time < (t+1)) %>%
    dplyr::group_by(species_id, tree_id) %>%
    dplyr::summarise(mortality = mean((1 - exp(- 2 * mortality_new)), na.rm=TRUE),
                     mortality_sd = sd((1 - exp(- 2 * mortality_new)), na.rm=TRUE)) 
  dat$time = t
  return(dat)
}))

tree_data_M_ave <- plyr::rbind.fill(lapply(seq_time, function(t){
  dat <- tree_data_live %>% 
    dplyr::filter(time > (t-1) & time < (t+1)) %>%
    dplyr::group_by(species_id) %>%
    dplyr::summarise(mortality = mean((1 - exp(- 2 * mortality_new)), na.rm=TRUE),
                     mortality_sd = sd((1 - exp(- 2 * mortality_new)), na.rm=TRUE)) 
  dat$time = t
  return(dat)
}))

tree_data_ave_M_all <- plyr::rbind.fill(lapply(seq_time, function(t){
  dat <- tree_data_live %>% 
    dplyr::filter(time > (t-1) & time < (t+1)) %>%
    dplyr::summarise(mortality = mean((1 - exp(- 2 * mortality_new)), na.rm=TRUE),
                     mortality_sd = sd((1 - exp(- 2 * mortality_new)), na.rm=TRUE)) 
  dat$time = t
  return(dat)
}))




tree_data_live_SM_ave <- plyr::rbind.fill(lapply(seq_time, function(t){
  dat <- tree_data_live %>% 
    dplyr::filter(time > (t-1) & time < (t+1)) %>%
    dplyr::group_by(species_id, tree_id) %>%
    dplyr::summarise(mortality = mean((1 - exp(- 2 * mortality_storage_dependent_dt)), na.rm=TRUE),
                     mortality_sd = sd((1 - exp(- 2 * mortality_storage_dependent_dt)), na.rm=TRUE))
  dat$time = t
  return(dat)
}))

tree_data_SM_ave <- plyr::rbind.fill(lapply(seq_time, function(t){
  dat <- tree_data_live %>% 
    dplyr::filter(time > (t-1) & time < (t+1)) %>%
    dplyr::group_by(species_id) %>%
    dplyr::summarise(mortality = mean((1 - exp(- 2 * mortality_storage_dependent_dt)), na.rm=TRUE),
                     mortality_sd = sd((1 - exp(- 2 * mortality_storage_dependent_dt)), na.rm=TRUE))
  dat$time = t
  return(dat)
}))

tree_data_ave_SM_all <- plyr::rbind.fill(lapply(seq_time, function(t){
  dat <- tree_data_live %>% 
    dplyr::filter(time > (t-1) & time < (t+1)) %>%
    dplyr::summarise(mortality = mean((1 - exp(- 2 * mortality_storage_dependent_dt)), na.rm=TRUE),
                     mortality_sd = sd((1 - exp(- 2 * mortality_storage_dependent_dt)), na.rm=TRUE))  
  dat$time = t
  return(dat)
}))


tree_data_live_GM_ave <- plyr::rbind.fill(lapply(seq_time, function(t){
  dat <- tree_data_live %>% 
    dplyr::filter(time > (t-1) & time < (t+1)) %>%
    dplyr::group_by(species_id, tree_id) %>%
    dplyr::summarise(mortality = mean((1 - exp(- 2 * mortality_growth_dependent_dt)), na.rm=TRUE),
                     mortality_sd = sd((1 - exp(- 2 * mortality_growth_dependent_dt)), na.rm=TRUE)) 
  dat$time = t
  return(dat)
}))

tree_data_GM_ave <- plyr::rbind.fill(lapply(seq_time, function(t){
  dat <- tree_data_live %>% 
    dplyr::filter(time > (t-1) & time < (t+1)) %>%
    dplyr::group_by(species_id) %>%
    dplyr::summarise(mortality = mean((1 - exp(- 2 * mortality_growth_dependent_dt)), na.rm=TRUE),
                     mortality_sd = sd((1 - exp(- 2 * mortality_growth_dependent_dt)), na.rm=TRUE)) 
  dat$time = t
  return(dat)
}))

tree_data_ave_GM_all <- plyr::rbind.fill(lapply(seq_time, function(t){
  dat <- tree_data_live %>% 
    dplyr::filter(time > (t-1) & time < (t+1)) %>%
    dplyr::summarise(mortality = mean((1 - exp(- 2 * mortality_growth_dependent_dt)), na.rm=TRUE),
                     mortality_sd = sd((1 - exp(- 2 * mortality_growth_dependent_dt)), na.rm=TRUE)) 
  dat$time = t
  return(dat)
}))



p <- ggplot(tree_data_live_M_ave, aes(x= time, y = mortality)) + 
  geom_line(aes(group = as.factor(species_id * 10 + tree_id), color = as.factor(species_id)),
            alpha = 0.15) +
  geom_line(data = tree_data_M_ave, aes(color = as.factor(species_id))) +
  geom_line(data = tree_data_ave_M_all, color = "#083855", alpha = 0.75) +
  geom_line(data = tree_data_live_GM_ave, aes(group = as.factor(species_id * 10 + tree_id), color = as.factor(species_id)),
            alpha = 0.15, linetype = "dashed") +
  geom_line(data = tree_data_GM_ave, aes(color = as.factor(species_id)), linetype = "dashed") +
  geom_line(data = tree_data_ave_GM_all, color = "#083855", alpha = 0.75, linetype = "dashed") +
  geom_line(data = tree_data_live_SM_ave, aes(group = as.factor(species_id * 10 + tree_id), color = as.factor(species_id)),
            alpha = 0.15, linetype = "dotted") +
  geom_line(data = tree_data_SM_ave, aes(color = as.factor(species_id)), linetype = "dotted") +
  geom_line(data = tree_data_ave_SM_all, color = "#083855", alpha = 0.75, linetype = "dotted") +
  sfm + scm + 
  species_wrap +
  scale_x_continuous("Time (yrs)") +
  scale_y_continuous("Individual Mortality Likelihood", trans = "log10") +
  foxes_theme +
  theme(legend.position = "bottom")
p


file_figure <- tempfile(paste("mortality_likelihood_growth", sep = "_"), tmpdir = fig_dir, fileext = ".png")
ggsave(file_figure, plot = p, device = NULL, path = NULL,
       scale = 1, width = 190, height = 138, dpi = 600, limitsize = TRUE,
       units =  "mm")




tree_data_live_M_rate_ave <- plyr::rbind.fill(lapply(seq_time, function(t){
  dat <- tree_data_live %>% 
    dplyr::filter(time > (t-1) & time < (t+1)) %>%
    dplyr::group_by(species_id, tree_id) %>%
    dplyr::summarise(mortality = mean(mortality_new, na.rm=TRUE),
                     mortality_sd = sd(mortality_new, na.rm=TRUE)) 
  dat$time = t
  return(dat)
}))

tree_data_M_rate_ave <- plyr::rbind.fill(lapply(seq_time, function(t){
  dat <- tree_data_live %>% 
    dplyr::filter(time > (t-1) & time < (t+1)) %>%
    dplyr::group_by(species_id) %>%
    dplyr::summarise(mortality = mean(mortality_new, na.rm=TRUE),
                     mortality_sd = sd(mortality_new, na.rm=TRUE))  
  dat$time = t
  return(dat)
}))

tree_data_ave_M_rate_all <- plyr::rbind.fill(lapply(seq_time, function(t){
  dat <- tree_data_live %>% 
    dplyr::filter(time > (t-1) & time < (t+1)) %>%
    dplyr::summarise(mortality = mean(mortality_new, na.rm=TRUE),
                     mortality_sd = sd(mortality_new, na.rm=TRUE))  
  dat$time = t
  return(dat)
}))




tree_data_live_SM_rate_ave <- plyr::rbind.fill(lapply(seq_time, function(t){
  dat <- tree_data_live %>% 
    dplyr::filter(time > (t-1) & time < (t+1)) %>%
    dplyr::group_by(species_id, tree_id) %>%
    dplyr::summarise(mortality = mean(mortality_storage_dependent_dt, na.rm=TRUE),
                     mortality_sd = sd(mortality_storage_dependent_dt, na.rm=TRUE))  
  dat$time = t
  return(dat)
}))

tree_data_SM_rate_ave <- plyr::rbind.fill(lapply(seq_time, function(t){
  dat <- tree_data_live %>% 
    dplyr::filter(time > (t-1) & time < (t+1)) %>%
    dplyr::group_by(species_id) %>%
    dplyr::summarise(mortality = mean(mortality_storage_dependent_dt, na.rm=TRUE),
                     mortality_sd = sd(mortality_storage_dependent_dt, na.rm=TRUE))  
  dat$time = t
  return(dat)
}))

tree_data_ave_SM_rate_all <- plyr::rbind.fill(lapply(seq_time, function(t){
  dat <- tree_data_live %>% 
    dplyr::filter(time > (t-1) & time < (t+1)) %>%
    dplyr::summarise(mortality = mean(mortality_storage_dependent_dt, na.rm=TRUE),
                     mortality_sd = sd(mortality_storage_dependent_dt, na.rm=TRUE))  
  dat$time = t
  return(dat)
}))


tree_data_live_GM_rate_ave <- plyr::rbind.fill(lapply(seq_time, function(t){
  dat <- tree_data_live %>% 
    dplyr::filter(time > (t-1) & time < (t+1)) %>%
    dplyr::group_by(species_id, tree_id) %>%
    dplyr::summarise(mortality = mean(mortality_growth_dependent_dt, na.rm=TRUE),
                     mortality_sd = sd(mortality_growth_dependent_dt, na.rm=TRUE))  
  dat$time = t
  return(dat)
}))

tree_data_GM_rate_ave <- plyr::rbind.fill(lapply(seq_time, function(t){
  dat <- tree_data_live %>% 
    dplyr::filter(time > (t-1) & time < (t+1)) %>%
    dplyr::group_by(species_id) %>%
    dplyr::summarise(mortality = mean(mortality_growth_dependent_dt, na.rm=TRUE),
                     mortality_sd = sd(mortality_growth_dependent_dt, na.rm=TRUE))  
  dat$time = t
  return(dat)
}))

tree_data_ave_GM_rate_all <- plyr::rbind.fill(lapply(seq_time, function(t){
  dat <- tree_data_live %>% 
    dplyr::filter(time > (t-1) & time < (t+1)) %>%
    dplyr::summarise(mortality = mean(mortality_growth_dependent_dt, na.rm=TRUE),
                     mortality_sd = sd(mortality_growth_dependent_dt, na.rm=TRUE))
  dat$time = t
  return(dat)
}))



p <- ggplot(tree_data_live_M_rate_ave, aes(x= time, y = mortality)) + 
  geom_line(aes(group = as.factor(species_id * 10 + tree_id), color = as.factor(species_id)),
            alpha = 0.15) +
  geom_line(data = tree_data_M_rate_ave, aes(color = as.factor(species_id))) +
  geom_line(data = tree_data_ave_M_rate_all, color = "#083855", alpha = 0.75) +
  geom_line(data = tree_data_live_GM_rate_ave, aes(group = as.factor(species_id * 10 + tree_id), color = as.factor(species_id)),
            alpha = 0.15, linetype = "dashed") +
  geom_line(data = tree_data_GM_rate_ave, aes(color = as.factor(species_id)), linetype = "dashed") +
  geom_line(data = tree_data_ave_GM_rate_all, color = "#083855", alpha = 0.75, linetype = "dashed") +
  geom_line(data = tree_data_live_SM_rate_ave, aes(group = as.factor(species_id * 10 + tree_id), color = as.factor(species_id)),
            alpha = 0.15, linetype = "dotted") +
  geom_line(data = tree_data_SM_rate_ave, aes(color = as.factor(species_id)), linetype = "dotted") +
  geom_line(data = tree_data_ave_SM_rate_all, color = "#083855", alpha = 0.75, linetype = "dotted") +
  sfm + scm + 
  species_wrap +
  scale_x_continuous("Time (yrs)") +
  scale_y_continuous("Individual Mortality Rate", trans="log10") +
  foxes_theme +
  theme(legend.position = "bottom")
p




tree_data_live_SM_rate_ave <- plyr::rbind.fill(lapply(seq_time, function(t){
  dat <- tree_data_live %>% 
    dplyr::filter(time > (t-1) & time < (t+1)) %>%
    dplyr::group_by(species_id, tree_id) %>%
    dplyr::summarise(mortality = mean((1 - exp(-mortality_storage_dependent_dt)), na.rm=TRUE),
                     mortality_sd = sd((1 - exp(-mortality_storage_dependent_dt)), na.rm=TRUE)) 
  dat$time = t
  dat$mortality_type = "storage"
  return(dat)
}))

tree_data_SM_rate_ave <- plyr::rbind.fill(lapply(seq_time, function(t){
  dat <- tree_data_live %>% 
    dplyr::filter(time > (t-1) & time < (t+1)) %>%
    dplyr::group_by(species_id) %>%
    dplyr::summarise(mortality = mean((1 - exp(-mortality_storage_dependent_dt)), na.rm=TRUE),
                     mortality_sd = sd((1 - exp(-mortality_storage_dependent_dt)), na.rm=TRUE)) 
  dat$time = t
  dat$mortality_type = "storage"
  return(dat)
}))

tree_data_ave_SM_rate_all <- plyr::rbind.fill(lapply(seq_time, function(t){
  dat <- tree_data_live %>% 
    dplyr::filter(time > (t-1) & time < (t+1)) %>%
    dplyr::summarise(mortality = mean((1 - exp(-mortality_storage_dependent_dt)), na.rm=TRUE),
                     mortality_sd = sd((1 - exp(-mortality_storage_dependent_dt)), na.rm=TRUE)) 
  dat$time = t
  dat$mortality_type = "storage"
  return(dat)
}))


tree_data_live_GM_rate_ave <- plyr::rbind.fill(lapply(seq_time, function(t){
  dat <- tree_data_live %>% 
    dplyr::filter(time > (t-1) & time < (t+1)) %>%
    dplyr::group_by(species_id, tree_id) %>%
    dplyr::summarise(mortality = mean((1 - exp(-mortality_growth_dependent_dt)), na.rm=TRUE),
                     mortality_sd = sd((1 - exp(-mortality_growth_dependent_dt)), na.rm=TRUE)) 
  dat$time = t
  dat$mortality_type = "Productivity"
  return(dat)
}))

tree_data_GM_rate_ave <- plyr::rbind.fill(lapply(seq_time, function(t){
  dat <- tree_data_live %>% 
    dplyr::filter(time > (t-1) & time < (t+1)) %>%
    dplyr::group_by(species_id) %>%
    dplyr::summarise(mortality = mean((1 - exp(-mortality_growth_dependent_dt)), na.rm=TRUE),
                     mortality_sd = sd((1 - exp(-mortality_growth_dependent_dt)), na.rm=TRUE)) 
  dat$time = t
  dat$mortality_type = "Productivity"
  return(dat)
}))

tree_data_ave_GM_rate_all <- plyr::rbind.fill(lapply(seq_time, function(t){
  dat <- tree_data_live %>% 
    dplyr::filter(time > (t-1) & time < (t+1)) %>%
    dplyr::summarise(mortality = mean((1 - exp(-mortality_growth_dependent_dt)), na.rm=TRUE),
                     mortality_sd = sd((1 - exp(-mortality_growth_dependent_dt)), na.rm=TRUE)) 
  dat$time = t
  dat$mortality_type = "Productivity"
  return(dat)
}))
tree_data_live_SM_rate_ave$mortality_type_num = 1
tree_data_live_GM_rate_ave$mortality_type_num = 2
tree_data_live_rate_ave <- rbind(tree_data_live_SM_rate_ave, tree_data_live_GM_rate_ave)
tree_data_live_rate_ave$mortality_type <- as.factor(tree_data_live_rate_ave$mortality_type)
tree_data_live_rate_ave$ind <- as.factor(tree_data_live_rate_ave$mortality_type_num * 100 + 
                                           tree_data_live_rate_ave$species_id * 10 + 
                                           tree_data_live_rate_ave$tree_id)

tree_data_SM_rate_ave$mortality_type_num = 1
tree_data_GM_rate_ave$mortality_type_num = 2
tree_data_rate_ave <- rbind(tree_data_SM_rate_ave, tree_data_GM_rate_ave)

tree_data_ave_SM_rate_all$mortality_type_num = 1
tree_data_ave_GM_rate_all$mortality_type_num = 2
tree_data_ave_rate_all <- rbind(tree_data_ave_SM_rate_all, tree_data_ave_GM_rate_all)

p <- ggplot(tree_data_rate_ave, aes(x= time, y = mortality)) + 
  geom_line(data = tree_data_live_GM_rate_ave, aes(color = as.factor(species_id), group = as.factor(species_id * 10 + tree_id)), linetype = "dashed", alpha = 0.15) +
  geom_line(data = tree_data_live_SM_rate_ave, aes(color = as.factor(species_id), group = as.factor(species_id * 10 + tree_id)), linetype = "dotted", alpha = 0.15) +
  geom_line(data = tree_data_rate_ave, aes(color = as.factor(species_id),  linetype = as.factor(mortality_type))) +
  geom_line(data = tree_data_ave_rate_all, aes(linetype = as.factor(mortality_type)), color = "#083855", alpha = 0.85) +
  sfm + scm + 
  scale_linetype_manual("Mortality type", values=c("dashed", "dotted"), labels = c("Productivity-Driven","Storage-Driven")) +
  species_wrap +
  scale_x_continuous("Time (yrs)") +
  scale_y_continuous("Individual Mortality Rate (Likelihood/yr)", trans="log10", limits=c(1*10^-8, 1*10^-0.5)) +
  foxes_theme 
p


file_figure <- tempfile(paste("mortality_likelihood", sep = "_"), tmpdir = fig_dir, fileext = ".png")
ggsave(file_figure, plot = p, device = NULL, path = NULL,
       scale = 1, width = 190, height = 138, dpi = 600, limitsize = TRUE,
       units =  "mm")




### CANOPY OPENNESS 
time <- unique(env_data$time)

canopy_openness <- plyr::rbind.fill(lapply(time, function(t){
  env_data_temp <- env_data %>%
    filter(time == t)
  max_height = max(env_data_temp$height)
  quartile3_height = env_data_temp$height[which.min(abs(env_data_temp$canopy_openness - 0.75))]
  quartile2_height = env_data_temp$height[which.min(abs(env_data_temp$canopy_openness - 0.5))]
  quartile1_height = env_data_temp$height[which.min(abs(env_data_temp$canopy_openness - 0.25))]
  
  out_df <- data.frame(time = t,
                       height = c(quartile1_height, quartile2_height, quartile3_height, max_height),
                       openness = c(0.25, 0.5, 0.75, 1))
  return(out_df)
}))


seq_time = seq(from = 1, to = 104, by = 1)


canopy_openness_ave <- plyr::rbind.fill(lapply(seq_time, function(t){
  dat <- canopy_openness %>% 
    filter(time > (t-1) & time < (t+1)) %>%
    dplyr::group_by(openness) %>%
    dplyr::summarise(height = mean(height, na.rm=TRUE),
              height_sd = sd(height, na.rm=TRUE)) 
  dat$time = t
  return(dat)
}))


p <- ggplot(canopy_openness_ave, aes(x= time, y = height, linetype = as.factor(openness))) + 
  geom_line() +
  scale_x_continuous("Time (yrs)") +
  scale_y_continuous("Height (m)") +
  foxes_theme 
p





p <- ggplot(tree_data_live, aes(x= time, y = height)) + 
  geom_line(data = canopy_openness_ave, aes(linetype = as.factor(openness)), alpha = 0.5) + 
  geom_line(aes(group = as.factor(species_id * 10 + tree_id), color = as.factor(species_id)), 
            alpha = 0.15) +
  geom_line(data = tree_data_ave, aes(color = as.factor(species_id))) + 
  geom_ribbon(data = tree_data_ave, aes(color = as.factor(species_id), 
                                        fill = as.factor(species_id), 
                                        ymin = height - height_sd, 
                                        ymax = height + height_sd), alpha = 0.35, linetype = "blank") + 
  geom_line(data = tree_data_ave_all, color = "#083855", alpha = 0.75) + 
  geom_ribbon(data = tree_data_ave_all, aes(ymin = height - height_sd, ymax = height + height_sd), alpha = 0.35, linetype = "blank") + 
  scm_ribbon + 
  sfm + 
  scale_linetype_discrete("Canopy Openness") +
  species_wrap +
  scale_x_continuous("Time (yrs)") +
  scale_y_continuous("Individual Plant Height (m/plant)") +
  foxes_theme 
p


file_figure <- tempfile(paste("height", "canopy", sep = "_"), tmpdir = fig_dir, fileext = ".png")
ggsave(file_figure, plot = p, device = NULL, path = NULL,
       scale = 1, width = 190, height = 138, dpi = 600, limitsize = TRUE,
       units =  "mm")

