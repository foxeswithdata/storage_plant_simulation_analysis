rm(list=ls())

library(plyr)
library(tidyverse)
library(stringr)
library(plotrix) 
library(DescTools)
library(gridExtra)
library(broom)
library(ggnewscale)
require(ISLR)

source("code/foxes_pallettes.R")
source("code/function_processing.R")
source("code/plotting.R")

directories <- list.dirs("data/full_run", recursive = TRUE)


out_dirs_env <- as.vector(unlist(sapply(directories, function(dir){
  path_parts <- unlist(str_split(dir, "/"))
  if(length(path_parts) == 4){
    return(dir)
  }
  return(NULL)
})))

env_stress_data <- lapply(out_dirs_env, function(dir){
  path_parts <- unlist(str_split(dir, "/"))
  env_det <- as.numeric(str_extract(unlist(str_split(path_parts[3], pattern = "_")), "[:digit:]*"))
  
  file_names <- list.files(path = dir, pattern = "parameters", recursive = TRUE, include.dirs = TRUE)
  file_names_full <- as.vector(sapply(file_names, function(x){
    return(file.path(dir, x))
  }))
  file_names_full <- file_names_full[1]
  files <- process_tree_data(file_names_full)
  
  param_data <- readRDS(file_names_full)
  env_stress <- param_data$parameters$control$stress_regime
  env_stress <- data.frame(time = 0:105,
                           stress = (env_stress - (0:105)),
                           env_stress = env_stress, 
                           temp = 1)
  
  env_stress$env_mean = env_det[2]
  env_stress$env_sd = env_det[3]
  env_stress$env_rep = unique(files$env_rep)
  
  return(env_stress)
})


env_stress_data_df <- plyr::rbind.fill(env_stress_data)
env_stress_data_df$stress_duration <- (1 - env_stress_data_df$stress) * 365
env_stress_data_df$stress_duration[env_stress_data_df$stress_duration < 0] = 0


env_stress_ave <- env_stress_data_df %>%
  dplyr::group_by(env_sd, env_mean) %>%
  dplyr::summarise(env_sd_real = sd(stress),
            env_mean_real = mean(stress),
            env_stress_min = min(stress))

env_stress_ave$env_stress_min = 365 * (1 - env_stress_ave$env_stress_min)



env_stress_data_df_stressed <- subset(env_stress_data_df, env_sd != 0)




p <- ggplot(env_stress_data_df_stressed, aes(x = stress_duration)) + 
  geom_histogram(fill = "#083855" , color = "#083855") +
  ylab("Number of Years") +
  scale_x_continuous("Duration of Stress (days)") +
  facet_grid(env_sd~ env_mean, labeller = labeller(env_mean = c("75" = "High Stress",
                                                                "85" = "Medium Stress"),
                                                   env_sd = c("15" = "Low Stochasticity",
                                                              "30" = "Medium Stochasticity",
                                                              "60" = "High Stochasticity"))) +
  # guides(fill = FALSE) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        legend.position = "right",
        plot.margin = margin(1,1,1,1, "cm"),
        text=element_text(size=12, color = "#083855", family="Lato Light"),
        axis.text=element_text(size=8, color = "#083855", family="Lato Light"),
        strip.background = element_rect(fill = "white"))
p

file_figure <- tempfile(paste("stress", "dist", sep = "_"), tmpdir = "out", fileext = ".png")
ggsave(file_figure, plot = p, device = NULL, path = NULL,
       scale = 1, width = 120, height = 138, dpi = 300, limitsize = TRUE,
       units =  "mm")



env_stresses <- unique(select(env_stress_data_df_stressed, c("env_sd", "env_mean")))


env_summary <- plyr::rbind.fill(lapply(1:nrow(env_stresses), function(n){
  env_stress_temp <- env_stress_data_df %>%
    filter(env_mean == env_stresses$env_mean[n] & env_sd == env_stresses$env_sd[n])
  
  env_very_low <- length(which((1 - env_stress_temp$stress) <= 30 / 365))
  env_low <- length(which((1 - env_stress_temp$stress) > 30 / 365 & (1 - env_stress_temp$stress) <= 75 / 365))
  env_med <- length(which((1 - env_stress_temp$stress) > 75 / 365 & (1 - env_stress_temp$stress) <= 105 / 365))
  env_high <- length(which((1 - env_stress_temp$stress) > 105 / 365 & (1 - env_stress_temp$stress) <= 150 / 365))
  env_very_heavy <- length(which((1 - env_stress_temp$stress) > 150 / 365))
    
  total = env_very_low + env_low + env_med + env_high + env_very_heavy
  
  return(data.frame(env_sd = env_stresses$env_sd[n],
             env_mean = env_stresses$env_mean[n],
             number_of_ind = c(env_very_low, env_low, env_med, env_high, env_very_heavy),
             stress_intensity = c("very light", "light", "medium", "high", "very high"),
             prop_of_ind = c(env_very_low, env_low, env_med, env_high, env_very_heavy)/total))
}))

View(env_summary)

env_summary <- env_summary %>% 
  mutate(stress_intensity = fct_relevel(stress_intensity, 
                            "very light", "light", "medium", 
                            "high", "very high")) 

p <- ggplot(env_summary, aes(y = prop_of_ind, x= as.factor(stress_intensity))) + 
  geom_bar(stat = "identity", position = "dodge", fill = "#083855" , color = "#083855") +
  ylab("Proportion of Years") +
  xlab("Stress intensity") +
  facet_grid(env_sd~ env_mean, labeller = labeller(env_mean = c("75" = "High Stress",
                                                                "85" = "Medium Stress"),
                                                   env_sd = c("15" = "Low Stochasticity",
                                                              "30" = "Medium Stochasticity",
                                                              "60" = "High Stochasticity"))) +
  # guides(fill = FALSE) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        legend.position = "right",
        plot.margin = margin(1,1,1,1, "cm"),
        text=element_text(size=12, color = "#083855", family="Lato Light"),
        axis.text=element_text(size=8, color = "#083855", family="Lato Light"),
        strip.background = element_rect(fill = "white"))
p


file_figure <- tempfile(paste("stress", "summary", sep = "_"), tmpdir = "out", fileext = ".png")
ggsave(file_figure, plot = p, device = NULL, path = NULL,
       scale = 1, width = 120, height = 138, dpi = 300, limitsize = TRUE,
       units =  "mm")



## Number of plants that died during the stress

directories <- list.dirs("data/full_run_processed", recursive = TRUE)

out_dirs_runs <- as.vector(unlist(sapply(directories, function(dir){
  path_parts <- unlist(str_split(dir, "/"))
  if(length(path_parts) == 5){
    return(dir)
  }
  return(NULL)
})))

out_dirs_runs <- out_dirs_runs[grep("extra", out_dirs_runs, invert = TRUE)]

out_dirs_runs <- out_dirs_runs[1:128]


all_stress_deaths <- plyr::rbind.fill(lapply(out_dirs_runs, function(dir){
  files_tree <- list.files(path = dir, pattern = "tree", recursive = FALSE)
  print(dir)
  print(files_tree)
  files_stress <- list.files(path = dir, pattern = "stress", recursive = FALSE)
  print(files_stress)
  if(length(files_tree) > 0 && length(files_stress) > 0){
    files_tree_full <- as.vector(sapply(files_tree, function(x){
      return(file.path(dir, x))
    }))
    files_stress_full <- as.vector(sapply(files_stress, function(x){
      return(file.path(dir, x))
    }))
    
    tree_data <- readRDS(files_tree_full)
    stress_data <- readRDS(files_stress_full)
    
    env_very_low <- (which((1 - stress_data$stress) <= 30 / 365))
    env_low <- (which((1 - stress_data$stress) > 30 / 365 & (1 - stress_data$stress) <= 75 / 365))
    env_med <- (which((1 - stress_data$stress) > 75 / 365 & (1 - stress_data$stress) <= 105 / 365))
    env_high <- length(which((1 - stress_data$stress) > 105 / 365 & (1 - stress_data$stress) <= 150 / 365))
    env_very_heavy <- length(which((1 - stress_data$stress) > 150 / 365))
    
    tree_data_stressed <- tree_data %>%
      filter(stress < 0.5)
    
    time = unique(stress_data$time)
    
    out <- plyr::rbind.fill(lapply(time, function(t){
      tree_data_stressed_temp <- tree_data_stressed %>%
        dplyr::filter(time >= t & time < (t+1))
      
      tree_data_stressed_temp_deaths <- tree_data_stressed_temp %>%
        dplyr::filter(is_alive == FALSE)
      
      num_deaths <- tree_data_stressed_temp_deaths %>%
        dplyr::group_by(species_id) %>%
        dplyr::summarise(num_deaths = n(),
                         mortality_new = mean(mortality, na.rm = TRUE),
                         mortality_growth_dependent = mean(mortality_growth_dependent_dt),
                         mortality_storage_dependent = mean(mortality_storage_dependent_dt))
      
      num_ind <- tree_data_stressed_temp %>% select(species_id, tree_id)
      num_ind <- unique(num_ind)
      num_ind <- num_ind %>%
        dplyr::group_by(species_id) %>%
        dplyr::count(name = "num_ind")
      
      
      out_stress <- merge(num_deaths, num_ind, by = "species_id")
      current_stress <- 0
      if(t %in% env_very_low){
        current_stress = "very_low"
      }
      else if(t %in% env_low){
        current_stress = "low"
      }
      else if(t %in% env_med){
        current_stress = "medium"
      }
      else if(t %in% env_high){
        current_stress = "high"
      }
      else{
        current_stress = "very_high"
      }
      if(nrow(out_stress) > 0){
        out_stress$time = t  
        out_stress$stress_intensity = current_stress
        out_stress$stress = stress_data$stress[stress_data$time == t]
      }
      return(out_stress)
    }))
    
    out$env_sd = unique(tree_data$env_sd)
    out$env_mean = unique(tree_data$env_mean)
    out$env_rep = unique(tree_data$env_rep)
    out$run_rep = unique(tree_data$run_rep)
    
    return(out)
  }
}))



saveRDS(all_stress_deaths, file = "out/mortality_stress_data.rds")
all_stress_deaths <- readRDS("out/mortality_stress_data.rds")


mortality_stress_data_ave <- all_stress_deaths %>%
  dplyr::filter(env_sd != 0) %>%
  dplyr::group_by(env_mean, env_sd, stress_intensity, species_id) %>%
  dplyr::summarise(mean_num_deaths = mean(num_deaths, na.rm = TRUE),
            sd_num_deaths = sd(num_deaths, na.rm = TRUE),
            mean_prop_deaths = mean(num_deaths/num_ind, na.rm = TRUE),
            sd_prop_deaths = sd(num_deaths/num_ind, na.rm = TRUE))


all_stress_deaths$prop_deaths = all_stress_deaths$num_deaths/all_stress_deaths$num_ind

stress_int = seq(from = log(min(all_stress_deaths$stress)), to=log(1), length.out = 25)

stress_int[25] = log(max(all_stress_deaths$stress))
stress_int = exp(stress_int)

stress_averages <- plyr::rbind.fill(lapply(1:(length(stress_int) - 1), function(ind){
  out <- all_stress_deaths %>% 
    dplyr::filter(stress >= stress_int[ind] & stress > stress_int[ind+1]) %>%
    group_by(species_id, env_mean, env_sd) %>%
    dplyr::summarise(mean_num_deaths = mean(num_deaths, na.rm = TRUE),
              sd_num_deaths = sd(num_deaths, na.rm = TRUE),
              mean_prop_deaths = mean(prop_deaths, na.rm = TRUE),
              sd_prop_deaths = sd(prop_deaths, na.rm = TRUE))
    if(nrow(out) > 0){
      out$stress = (stress_int[ind] + stress_int[ind+1])/2
    }
    return(out)
}))




p <- ggplot(all_stress_deaths, aes(x = 365 * (1 - stress), color = as.factor(species_id))) +
  geom_point(aes(y = prop_deaths), alpha = 0.2) + 
  scm + 
  sfm + 
  species_wrap +
  scale_x_continuous("Length of Stress (days)") +
  scale_y_continuous("Proportion of deaths in the population") +
  foxes_theme +
  theme(legend.position = "bottom")
p
  

file_figure <- tempfile(paste("num_death", "length_of_stress", sep = "_"), tmpdir = "out", fileext = ".png")
ggsave(file_figure, plot = p, device = NULL, path = NULL,
       scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
       units =  "mm")


all_stress_deaths$dur_stress = (365 * (1 - all_stress_deaths$stress))
all_stress_deaths_sub <- subset(all_stress_deaths, dur_stress > 0 & prop_deaths < 1)
all_stress_deaths_sub_1 <- subset(all_stress_deaths_sub, species_id == 1)
all_stress_deaths_sub_2 <- subset(all_stress_deaths_sub, species_id == 2)
all_stress_deaths_sub_3 <- subset(all_stress_deaths_sub, species_id == 3)
all_stress_deaths_sub_4 <- subset(all_stress_deaths_sub, species_id == 4)
# 
glm.fit <- glm(prop_deaths ~ dur_stress + as.factor(species_id),
               data = all_stress_deaths_sub,
               family = "binomial")

fit_1 <- nls(prop_deaths ~ 1/ (1 +  exp(-alpha * (dur_stress - x0))), 
             data = all_stress_deaths_sub_1,
             start = list(x0 = 160, alpha = 10))
fit_2 <- nls(prop_deaths ~ 1/ (1 +  exp(-alpha * (dur_stress - x0))), 
             data = all_stress_deaths_sub_2,
             start = list(x0 = 190, alpha = 1))
fit_3 <- nls(prop_deaths ~ 1/ (1 +  exp(-alpha * (dur_stress - x0))), 
             data = all_stress_deaths_sub_3,
             start = list(x0 = 160, alpha = 2))
fit_4 <- nls(prop_deaths ~ 1/ (1 +  exp(-alpha * (dur_stress - x0))), 
             data = all_stress_deaths_sub_4,
             start = list(x0 = 190, alpha = 1))

fit_1_ci <- parameters::ci(fit_1)
fit_2_ci <- parameters::ci(fit_2)
fit_3_ci <- parameters::ci(fit_3)
fit_4_ci <- parameters::ci(fit_4)

fit_1_par <- parameters::parameters(fit_1)
fit_1_par <- data.frame(alpha = fit_1_par$Coefficient[2], x0 = fit_1_par$Coefficient[1], species_id = 1)
fit_2_par <- parameters::parameters(fit_2)
fit_2_par <- data.frame(alpha = fit_2_par$Coefficient[2], x0 = fit_2_par$Coefficient[1], species_id = 2)
fit_3_par <- parameters::parameters(fit_3)
fit_3_par <- data.frame(alpha = fit_3_par$Coefficient[2], x0 = fit_3_par$Coefficient[1], species_id = 3)
fit_4_par <- parameters::parameters(fit_4)
fit_4_par <- data.frame(alpha = fit_4_par$Coefficient[2], x0 = fit_4_par$Coefficient[1], species_id = 4)
fit_par <- plyr::rbind.fill(fit_1_par, fit_2_par, fit_3_par, fit_4_par)
# 
# fit_1 = nls(prop_deaths ~ SSlogis(dur_stress, a, b, c), data = all_stress_deaths_sub_1)
# fit_2 = nls(prop_deaths ~ SSlogis(dur_stress, a, b, c), data = all_stress_deaths_sub_2, control = nls.control(tol = 0.0001, 
#                                                                                                               warnOnly = TRUE,
#                                                                                                               minFactor = 0.0001),
#             algorithm = "plinear")
# fit_3 = nls(prop_deaths ~ SSlogis(dur_stress, a, b, c), data = all_stress_deaths_sub_3)
# fit_4 = nls(prop_deaths ~ SSlogis(dur_stress, a, b, c), data = all_stress_deaths_sub_4)

plot(data$x, data$y)
lines(data$x, predict(model))
# 
# fit_1 <- glm(prop_deaths ~ dur_stress,
#                data = all_stress_deaths_sub_1,
#                family = "binomial")
# fit_2 <- glm(prop_deaths ~ dur_stress,
#              data = all_stress_deaths_sub_2,
#              family = "binomial")
# fit_3 <- glm(prop_deaths ~ dur_stress,
#              data = all_stress_deaths_sub_3,
#              family = "binomial")
# fit_4 <- glm(prop_deaths ~ dur_stress,
#              data = all_stress_deaths_sub_4,
#              family = "binomial")

fit_1_df <- augment(fit_1)
fit_1_df$species_id = 1
fit_1_df$ci_lower = 1/(1 + exp(- fit_1_ci$CI_low[2] * (fit_1_df$dur_stress - fit_1_ci$CI_low[1])))
fit_1_df$ci_higher = 1/(1 + exp(- fit_1_ci$CI_high[2] * (fit_1_df$dur_stress - fit_1_ci$CI_high[1])))
fit_2_df <- augment(fit_2)
fit_2_df$species_id = 2
fit_2_df$ci_lower = 1/(1 + exp(- fit_2_ci$CI_low[2] * (fit_2_df$dur_stress - fit_2_ci$CI_low[1])))
fit_2_df$ci_higher = 1/(1 + exp(- fit_2_ci$CI_high[2] * (fit_2_df$dur_stress - fit_2_ci$CI_high[1])))
fit_3_df <- augment(fit_3)
fit_3_df$species_id = 3
fit_3_df$ci_lower = 1/(1 + exp(- fit_3_ci$CI_low[2] * (fit_3_df$dur_stress - fit_3_ci$CI_low[1])))
fit_3_df$ci_higher = 1/(1 + exp(- fit_3_ci$CI_high[2] * (fit_3_df$dur_stress - fit_3_ci$CI_high[1])))
fit_4_df <- augment(fit_4)
fit_4_df$species_id = 4
fit_4_df$ci_lower = 1/(1 + exp(- fit_4_ci$CI_low[2] * (fit_4_df$dur_stress - fit_4_ci$CI_low[1])))
fit_4_df$ci_higher = 1/(1 + exp(- fit_4_ci$CI_high[2] * (fit_4_df$dur_stress - fit_4_ci$CI_high[1])))

fit <- plyr::rbind.fill(fit_1_df, fit_2_df, fit_3_df, fit_4_df)


p <- ggplot(all_stress_deaths, aes(x = (dur_stress), color = as.factor(species_id))) +
  geom_point(aes(y = (prop_deaths)), alpha = 0.2) + 
  scale_color_manual("Allocation \nStrategy:",
                     values=foxes_palettes$light[c(2, 1, 4, 3)],
                     labels=c("Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe")) +
  new_scale_color() +
  geom_vline(data = fit_par, aes(xintercept = x0, color = as.factor(species_id)), linetype = "dashed") +
  geom_line(data = fit, aes(y = .fitted, color = as.factor(species_id))) + 
  scale_color_manual("Allocation \nStrategy:",
                     values=foxes_palettes$dark[c(2, 1, 4, 3)],
                     labels=c("Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe")) +
  species_wrap +
  scale_x_continuous("Duration of Stress (days)", limits = c(0, 220)) +
  scale_y_continuous("Proportion of Deaths in Strategy Population in Stress Period") +
  foxes_theme +
  theme(legend.position = "bottom")
p

summary(fit_1)
summary(fit_2)
summary(fit_3)
summary(fit_4)



file_figure <- tempfile(paste("num_death", "length_of_stress", "fit", sep = "_"), tmpdir = "out", fileext = ".png")
ggsave(file_figure, plot = p, device = NULL, path = NULL,
       scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
       units =  "mm")






all_growth_deaths <- plyr::rbind.fill(lapply(out_dirs_runs, function(dir){
  files_tree <- list.files(path = dir, pattern = "tree", recursive = FALSE)
  print(dir)
  print(files_tree)
  files_env <- list.files(path = dir, pattern = "env_data", recursive = FALSE)
  print(files_env)
  if(length(files_tree) > 0 && length(files_env) > 0){
    files_tree_full <- as.vector(sapply(files_tree, function(x){
      return(file.path(dir, x))
    }))
    files_growth_full <- as.vector(sapply(files_env, function(x){
      return(file.path(dir, x))
    }))
    
    tree_data <- readRDS(files_tree_full)
    env_data <- readRDS(files_growth_full)
    
    tree_data_not_stressed <- tree_data %>%
      filter(stress > 0.5)
    
    tree_data_not_stressed_dead <- tree_data_not_stressed %>%
      dplyr::filter(is_alive == FALSE)
    
    canopy_height_seq = seq(0, 1, length.out = 51)
    
    out <- plyr::rbind.fill(lapply(1:(length(canopy_height_seq)-1), function(ind){
      canopy_data_temp <- env_data %>%
        dplyr::filter(canopy_openness >= canopy_height_seq[ind] & 
                        canopy_openness < canopy_height_seq[ind + 1])
      
      if(nrow(canopy_data_temp) > 0){
        canopy_data_temp_time <- canopy_data_temp %>%
          dplyr::group_by(time) %>%
          dplyr::summarise(min_height = min(height),
                    max_height = max(height))
        
        tree_data_dead_temp <- merge(canopy_data_temp_time, tree_data_not_stressed_dead, by = c("time"))
        tree_data_live_temp <- merge(canopy_data_temp_time, tree_data_not_stressed, by = c("time"))
        
        tree_data_dead_temp_valid <- tree_data_dead_temp %>%
          dplyr::filter(height >= min_height & height <= max_height)
        
        tree_data_dead_all_valid <- tree_data_live_temp %>%
          dplyr::filter(height >= min_height & height <= max_height)
        
        num_deaths <- tree_data_dead_temp_valid %>%
          dplyr::group_by(species_id) %>%
          dplyr::summarise(num_deaths = n(),
                           mortality_new = mean(mortality, na.rm = TRUE),
                           mortality_growth_dependent = mean(mortality_growth_dependent_dt),
                           mortality_storage_dependent = mean(mortality_storage_dependent_dt))
        
        num_ind <- tree_data_dead_all_valid %>% select(species_id, tree_id)
        num_ind <- unique(num_ind)
        num_ind <- num_ind %>%
          dplyr::group_by(species_id) %>%
          dplyr::count(name = "num_ind")
        
        
        out_growth <- merge(num_deaths, num_ind, by = "species_id")
        
        
        
        if(nrow(out_growth) > 0){
          out_growth$prop_death = out_growth$num_deaths / out_growth$num_ind
          out_growth$canopy_openness = (canopy_height_seq[ind]  + canopy_height_seq[ind + 1]) /2
        }
        return(out_growth) 
      }
    }))
    
    out$env_sd = unique(tree_data$env_sd)
    out$env_mean = unique(tree_data$env_mean)
    out$env_rep = unique(tree_data$env_rep)
    out$run_rep = unique(tree_data$run_rep)
    
    return(out)
  }
}))

saveRDS(all_growth_deaths, file = "out/mortality_growth_data.rds")


all_growth_deaths <- readRDS("out/mortality_growth_data.rds")




p <- ggplot(all_growth_deaths, aes(x = canopy_openness, color = as.factor(species_id))) +
  geom_point(aes(y = prop_death), alpha = 0.2) + 
  scm + 
  sfm + 
  species_wrap +
  scale_x_continuous("Canopy Openness") +
  scale_y_continuous("Proportion of deaths in the population", trans = "log10") +
  foxes_theme +
  theme(legend.position = "bottom")
p


file_figure <- tempfile(paste("num_death", "canopy_openness", sep = "_"), tmpdir = "out", fileext = ".png")
ggsave(file_figure, plot = p, device = NULL, path = NULL,
       scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
       units =  "mm")


all_growth_deaths_sub <- subset(all_growth_deaths, canopy_openness != 0.99)

p <- ggplot(all_growth_deaths_sub, aes(x = canopy_openness, color = as.factor(species_id))) +
  geom_point(aes(y = prop_death), alpha = 0.2) + 
  scm + 
  sfm + 
  species_wrap +
  scale_x_continuous("Canopy Openness") +
  scale_y_continuous("Proportion of deaths in the population") +
  foxes_theme +
  theme(legend.position = "bottom")
p

all_growth_deaths_sub_1 <- subset(all_growth_deaths_sub, species_id == 1)
all_growth_deaths_sub_2 <- subset(all_growth_deaths_sub, species_id == 2)
all_growth_deaths_sub_3 <- subset(all_growth_deaths_sub, species_id == 3)
all_growth_deaths_sub_4 <- subset(all_growth_deaths_sub, species_id == 4)

fit_1 <- nls(prop_death ~ yf + (y0 - yf) * exp(-alpha * canopy_openness), 
    data = all_growth_deaths_sub_1,
    start = list(y0 = 1, yf = 0, alpha = 10))
fit_2 <- nls(prop_death ~ yf + (y0 - yf) * exp(-alpha * canopy_openness), 
             data = all_growth_deaths_sub_2,
             start = list(y0 = 1, yf = 0, alpha = 10))
fit_3 <- nls(prop_death ~ yf + (y0 - yf) * exp(-alpha * canopy_openness), 
             data = all_growth_deaths_sub_3,
             start = list(y0 = 1, yf = 0, alpha = 20))
fit_4 <- nls(prop_death ~ yf + (y0 - yf) * exp(-alpha * canopy_openness), 
             data = all_growth_deaths_sub_4,
             start = list(y0 = 1, yf = 0, alpha = 30))

fit_1_df <- augment(fit_1)
fit_1_df$species_id = 1
fit_2_df <- augment(fit_2)
fit_2_df$species_id = 2
fit_3_df <- augment(fit_3)
fit_3_df$species_id = 3
fit_4_df <- augment(fit_4)
fit_4_df$species_id = 4


fit_1_ci <- parameters::ci(fit_1)
fit_2_ci <- parameters::ci(fit_2)
fit_3_ci <- parameters::ci(fit_3)
fit_4_ci <- parameters::ci(fit_4)

fit <- plyr::rbind.fill(fit_1_df, fit_2_df, fit_3_df, fit_4_df)

qplot(canopy_openness, prop_death, data = augment(fit)) + geom_line(aes(y = .fitted))
 
p <- ggplot(all_growth_deaths, aes(x = canopy_openness)) +
  geom_point(aes(y = prop_death, color = as.factor(species_id)), alpha = 0.2) + 
  scale_color_manual("Allocation \nStrategy:",
                     values=foxes_palettes$light[c(2, 1, 4, 3)],
                     labels=c("Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe")) +
  new_scale_color() +
  geom_line(data = fit, aes(y = .fitted, color = as.factor(species_id))) + 
  scale_color_manual("Allocation \nStrategy:",
                     values=foxes_palettes$dark[c(2, 1, 4, 3)],
                     labels=c("Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe")) +
  species_wrap +
  scale_x_continuous("Canopy Openness") +
  scale_y_continuous("Proportion of Deaths in Strategy Population in Light Environment") +
  foxes_theme +
  theme(legend.position = "bottom")
p


file_figure <- tempfile(paste("num_death", "canopy_openness", "fit", sep = "_"), tmpdir = "out", fileext = ".png")
ggsave(file_figure, plot = p, device = NULL, path = NULL,
       scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
       units =  "mm")





