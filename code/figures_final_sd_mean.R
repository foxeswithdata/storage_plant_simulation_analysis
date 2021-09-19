
library(tidyverse)

figure_dir <- "out/figures_manual/sd_mean"

data_directories <- list.dirs("data/full_run_processed", recursive = FALSE)

# hist_all <- plyr::rbind.fill(lapply(seq(20, 100, by= 20), function(num){
  hist <- plyr::rbind.fill(lapply(data_directories, function(dat){
    files <- list.files(path = dat, recursive = TRUE)
    file_names_full <- as.vector(sapply(files, function(x){
      return(file.path(dat, x))
    }))
    
    all_data <- plyr::rbind.fill(lapply(file_names_full, readRDS))
    
    
    all_data_times <- all_data %>% 
      filter(time >= 100 & is_alive == TRUE) %>%
      dplyr::group_by(env_rep, run_rep, species_id) %>%
      dplyr::summarise(time = min(time))
    
    all_data_sub <- all_data %>% select(time, env_rep, run_rep, species_id, tree_id, mass_storage, area_stem, is_alive, height, area_leaf, env_mean, env_sd)
    all_data_det <- left_join(all_data_times, all_data_sub, by = c("env_rep", "run_rep", "species_id", "time"))
    all_data_det$time_slice = 100
    return(all_data_det)
  }))
  # return(hist)
# }))

  
  
  hist$env <- hist$env_mean + hist$env_sd
  hist_data <- hist
  
  
  
  sum_basal_data <- hist_data %>%
    dplyr::group_by(env, run_rep, env_rep, species_id, env_mean, env_sd, time_slice) %>% 
    dplyr::summarise(basal_area = sum(area_stem, na.rm= TRUE) * 100) %>%
    dplyr::group_by(env, species_id,  env_mean, env_sd, time_slice) %>%
    dplyr::summarise(mean_basal_area = mean(basal_area, na.rm = TRUE),
                     sd_basal_area = sd(basal_area, na.rm = TRUE),
                     se_basal_area = std.error(basal_area, na.rm = TRUE))
  
  sum_basal_data <- subset(sum_basal_data, (env_mean %in% c(75, 85)))
  
  sum_basal_data_2 <- hist_data %>%
    dplyr::group_by(env, run_rep, env_rep, species_id, env_mean, env_sd, time_slice) %>% 
    dplyr::summarise(basal_area = sum(area_stem) * 100) 
  
  sum_basal_data_all_sp <- hist_data %>%
    dplyr::group_by(env, run_rep, env_rep, env_mean, env_sd, time_slice) %>% 
    dplyr::summarise(basal_area = sum(area_stem) * 100) 
  
  linearMod_1_75 <- lm(basal_area ~ env_sd, data=sum_basal_data_2, species_id == 1 & env_mean == 75)
  linearMod_2_75 <- lm(basal_area ~ env_sd, data=sum_basal_data_2, species_id == 2 & env_mean == 75)
  linearMod_3_75 <- lm(basal_area ~ env_sd, data=sum_basal_data_2, species_id == 3 & env_mean == 75)
  linearMod_4_75 <- lm(basal_area ~ env_sd, data=sum_basal_data_2, species_id == 4 & env_mean == 75)
  linearMod_1_85 <- lm(basal_area ~ env_sd, data=sum_basal_data_2, species_id == 1 & env_mean == 85)
  linearMod_2_85 <- lm(basal_area ~ env_sd, data=sum_basal_data_2, species_id == 2 & env_mean == 85)
  linearMod_3_85 <- lm(basal_area ~ env_sd, data=sum_basal_data_2, species_id == 3 & env_mean == 85)
  linearMod_4_85 <- lm(basal_area ~ env_sd, data=sum_basal_data_2, species_id == 4 & env_mean == 85)
  env_sd = c(0, 15, 30, 60)
  model_data_1_75 <- data.frame(env_sd = env_sd,
                           basal_area = linearMod_1_75$coefficients[1] + env_sd * linearMod_1_75$coefficients[2],
                           species_id = 1,
                           env_mean = 0.75)
  model_data_2_75 <- data.frame(env_sd = env_sd,
                                basal_area = linearMod_2_75$coefficients[1] + env_sd * linearMod_2_75$coefficients[2],
                                species_id = 2,
                                env_mean = 0.75)
  model_data_3_75 <- data.frame(env_sd = env_sd,
                                basal_area = linearMod_3_75$coefficients[1] + env_sd * linearMod_3_75$coefficients[2],
                                species_id = 3,
                                env_mean = 0.75)
  model_data_4_75 <- data.frame(env_sd = env_sd,
                                basal_area = linearMod_4_75$coefficients[1] + env_sd * linearMod_4_75$coefficients[2],
                                species_id = 4,
                                env_mean = 0.75)
  model_data_1_85 <- data.frame(env_sd = env_sd,
                                basal_area = linearMod_1_85$coefficients[1] + env_sd * linearMod_1_85$coefficients[2],
                                species_id = 1,
                                env_mean = 0.85)
  model_data_2_85 <- data.frame(env_sd = env_sd,
                                basal_area = linearMod_2_85$coefficients[1] + env_sd * linearMod_2_85$coefficients[2],
                                species_id = 2,
                                env_mean = 0.85)
  model_data_3_85 <- data.frame(env_sd = env_sd,
                                basal_area = linearMod_3_85$coefficients[1] + env_sd * linearMod_3_85$coefficients[2],
                                species_id = 3,
                                env_mean = 0.85)
  model_data_4_85 <- data.frame(env_sd = env_sd,
                                basal_area = linearMod_4_85$coefficients[1] + env_sd * linearMod_4_85$coefficients[2],
                                species_id = 4,
                                env_mean = 0.85)
  model_data <- rbind.fill(model_data_1_75, model_data_2_75, model_data_3_75, model_data_4_75,
                           model_data_1_85, model_data_2_85, model_data_3_85, model_data_4_85)
  model_data$species_id <- as.factor(model_data$species_id)
  model_data$env_mean <- as.factor(model_data$env_mean)
  
  
  data_env_sp_1 <- sum_basal_data_2 %>%
    filter(species_id == 1 & env_mean == 75)

  one_way_anova <- aov(basal_area ~ (env_sd + env_mean), 
                       data = sum_basal_data_all_sp)
  summary_out <- summary(one_way_anova)
  
  one_way_anova <- aov(basal_area ~ (env_sd + env_mean + species_id), 
                       data = sum_basal_data_2)
  summary(one_way_anova)
  
  one_way_anova <- aov(basal_area ~ (env_sd * env_mean), 
                       data = sum_basal_data_all_sp)
  summary(one_way_anova)
  
  one_way_anova <- aov(basal_area ~ (env_sd * env_mean * species_id), 
                       data = sum_basal_data_2)
  summary(one_way_anova)
  
  save.image("statistics-basal.RData")
  
  p <- ggplot(sum_basal_data, aes(color = as.factor(species_id), fill = as.factor(species_id))) + 
    geom_ribbon(data = subset(sum_basal_data, env_mean == 75), aes(x=env_sd, 
                                                                   y = mean_basal_area, 
                                                                   ymin = mean_basal_area - se_basal_area, ymax = mean_basal_area + se_basal_area), alpha = 0.25, linetype = "blank") +
    geom_ribbon(data = subset(sum_basal_data, env_mean == 85), aes(x=env_sd, 
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
  
  
  
  file_figure <- tempfile(paste("basal_mean_sd", sep = "_"), tmpdir = figure_dir, fileext = ".png")
  ggsave(file_figure, plot = p, device = NULL, path = NULL,
         scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
         units =  "mm")
  
  linearMod_1_75 <- lm(basal_area ~ env_mean, data=sum_basal_data_2, species_id == 1 & env_sd == 75)
  linearMod_2_75 <- lm(basal_area ~ env_sd, data=sum_basal_data_2, species_id == 2 & env_mean == 75)
  linearMod_3_75 <- lm(basal_area ~ env_sd, data=sum_basal_data_2, species_id == 3 & env_mean == 75)
  linearMod_4_75 <- lm(basal_area ~ env_sd, data=sum_basal_data_2, species_id == 4 & env_mean == 75)
  linearMod_1_85 <- lm(basal_area ~ env_sd, data=sum_basal_data_2, species_id == 1 & env_mean == 85)
  linearMod_2_85 <- lm(basal_area ~ env_sd, data=sum_basal_data_2, species_id == 2 & env_mean == 85)
  linearMod_3_85 <- lm(basal_area ~ env_sd, data=sum_basal_data_2, species_id == 3 & env_mean == 85)
  linearMod_4_85 <- lm(basal_area ~ env_sd, data=sum_basal_data_2, species_id == 4 & env_mean == 85)  
  

  
  
  p <- ggplot(sum_basal_data, aes(x=env_mean, 
                                  y = mean_basal_area, 
                                  color = as.factor(species_id), 
                                  linetype=as.factor(env_sd))) + 
    geom_line() + 
    geom_point() + 
    scale_color_manual("Species Id:",
                       values=c(foxes_palettes$main[c(2, 1, 4, 3)]),
                       labels=c("Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe")) +
    scale_linetype_discrete("Environment SD (d):",
                            labels = c("0", "15", "30", "60")) +
    ylab("Final Basal Area Per Hectare (m2/ha)") +
    xlab("Mean of Stress (yr)") +
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
  
  
  
  
  
  
  
  
  
  
  hist$env <- hist$env_mean + hist$env_sd
  hist_data <- hist
  
  
  
  sum_basal_data <- hist_data %>%
    dplyr::group_by(env, run_rep, env_rep, species_id, env_mean, env_sd, time_slice) %>% 
    dplyr::summarise(max_height = max(height, na.rm= TRUE)) %>%
    dplyr::group_by(env, species_id,  env_mean, env_sd, time_slice) %>%
    dplyr::summarise(mean_max_height = mean(max_height, na.rm = TRUE),
                     sd_max_height = sd(max_height, na.rm = TRUE),
                     se_max_height = std.error(max_height, na.rm = TRUE))
  
  sum_basal_data <- subset(sum_basal_data, (env_mean %in% c(75, 85)))
  
  sum_basal_data_2 <- hist_data %>%
    dplyr::group_by(env, run_rep, env_rep, species_id, env_mean, env_sd, time_slice) %>% 
    dplyr::summarise(max_height = max(area_stem)) 
  
  sum_basal_data_all_sp <- hist_data %>%
    dplyr::group_by(env, run_rep, env_rep, env_mean, env_sd, time_slice) %>% 
    dplyr::summarise(max_height = max(area_stem)) 
  
  linearMod_1_75 <- lm(basal_area ~ env_sd, data=sum_basal_data_2, species_id == 1 & env_mean == 75)
  linearMod_2_75 <- lm(basal_area ~ env_sd, data=sum_basal_data_2, species_id == 2 & env_mean == 75)
  linearMod_3_75 <- lm(basal_area ~ env_sd, data=sum_basal_data_2, species_id == 3 & env_mean == 75)
  linearMod_4_75 <- lm(basal_area ~ env_sd, data=sum_basal_data_2, species_id == 4 & env_mean == 75)
  linearMod_1_85 <- lm(basal_area ~ env_sd, data=sum_basal_data_2, species_id == 1 & env_mean == 85)
  linearMod_2_85 <- lm(basal_area ~ env_sd, data=sum_basal_data_2, species_id == 2 & env_mean == 85)
  linearMod_3_85 <- lm(basal_area ~ env_sd, data=sum_basal_data_2, species_id == 3 & env_mean == 85)
  linearMod_4_85 <- lm(basal_area ~ env_sd, data=sum_basal_data_2, species_id == 4 & env_mean == 85)
  env_sd = c(0, 15, 30, 60)
  model_data_1_75 <- data.frame(env_sd = env_sd,
                                basal_area = linearMod_1_75$coefficients[1] + env_sd * linearMod_1_75$coefficients[2],
                                species_id = 1,
                                env_mean = 0.75)
  model_data_2_75 <- data.frame(env_sd = env_sd,
                                basal_area = linearMod_2_75$coefficients[1] + env_sd * linearMod_2_75$coefficients[2],
                                species_id = 2,
                                env_mean = 0.75)
  model_data_3_75 <- data.frame(env_sd = env_sd,
                                basal_area = linearMod_3_75$coefficients[1] + env_sd * linearMod_3_75$coefficients[2],
                                species_id = 3,
                                env_mean = 0.75)
  model_data_4_75 <- data.frame(env_sd = env_sd,
                                basal_area = linearMod_4_75$coefficients[1] + env_sd * linearMod_4_75$coefficients[2],
                                species_id = 4,
                                env_mean = 0.75)
  model_data_1_85 <- data.frame(env_sd = env_sd,
                                basal_area = linearMod_1_85$coefficients[1] + env_sd * linearMod_1_85$coefficients[2],
                                species_id = 1,
                                env_mean = 0.85)
  model_data_2_85 <- data.frame(env_sd = env_sd,
                                basal_area = linearMod_2_85$coefficients[1] + env_sd * linearMod_2_85$coefficients[2],
                                species_id = 2,
                                env_mean = 0.85)
  model_data_3_85 <- data.frame(env_sd = env_sd,
                                basal_area = linearMod_3_85$coefficients[1] + env_sd * linearMod_3_85$coefficients[2],
                                species_id = 3,
                                env_mean = 0.85)
  model_data_4_85 <- data.frame(env_sd = env_sd,
                                basal_area = linearMod_4_85$coefficients[1] + env_sd * linearMod_4_85$coefficients[2],
                                species_id = 4,
                                env_mean = 0.85)
  model_data <- rbind.fill(model_data_1_75, model_data_2_75, model_data_3_75, model_data_4_75,
                           model_data_1_85, model_data_2_85, model_data_3_85, model_data_4_85)
  model_data$species_id <- as.factor(model_data$species_id)
  model_data$env_mean <- as.factor(model_data$env_mean)
  
  
  data_env_sp_1 <- sum_basal_data_2 %>%
    filter(species_id == 1 & env_mean == 75)
  
  one_way_anova <- aov(max_height ~ (env_sd + env ++_mean), 
                       data = sum_basal_data_all_sp)
  summary_out <- summary(one_way_anova)
  
  one_way_anova <- aov(max_height ~ (env_sd + env_mean + species_id), 
                       data = sum_basal_data_2)
  summary(one_way_anova)
  
  one_way_anova <- aov(max_height ~ (env_sd * env_mean), 
                       data = sum_basal_data_all_sp)
  summary(one_way_anova)
  
  one_way_anova <- aov(max_height ~ (env_sd * env_mean * species_id), 
                       data = sum_basal_data_2)
  summary(one_way_anova)
  
  ttest_sp_1_height <- t.test(subset(sum_basal_data_2, species_id == 1 & env_mean == 75 & env_sd == 60, select = max_height), 
                              subset(sum_basal_data_2, species_id == 1 & env_mean == 85 & env_sd == 60, select = max_height))
  ttest_sp_2_height <- t.test(subset(sum_basal_data_2, species_id == 2 & env_mean == 75 & env_sd == 60, select = max_height), 
                              subset(sum_basal_data_2, species_id == 2 & env_mean == 85 & env_sd == 60, select = max_height))
  ttest_sp_3_height <- t.test(subset(sum_basal_data_2, species_id == 3 & env_mean == 75 & env_sd == 60, select = max_height), 
                              subset(sum_basal_data_2, species_id == 3 & env_mean == 85 & env_sd == 60, select = max_height))
  ttest_sp_4_height <- t.test(subset(sum_basal_data_2, species_id == 4 & env_mean == 75 & env_sd == 60, select = max_height), 
                              subset(sum_basal_data_2, species_id == 4 & env_mean == 85 & env_sd == 60, select = max_height))
  
  
  save.image("statistics-basal.RData")
  
  p <- ggplot(sum_basal_data, aes(color = as.factor(species_id), fill = as.factor(species_id))) + 
    geom_ribbon(data = subset(sum_basal_data, env_mean == 75), aes(x=env_sd, 
                                                                   y = mean_basal_area, 
                                                                   ymin = mean_basal_area - se_basal_area, ymax = mean_basal_area + se_basal_area), alpha = 0.25, linetype = "blank") +
    geom_ribbon(data = subset(sum_basal_data, env_mean == 85), aes(x=env_sd, 
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
  
  
  
  file_figure <- tempfile(paste("basal_mean_sd", sep = "_"), tmpdir = figure_dir, fileext = ".png")
  ggsave(file_figure, plot = p, device = NULL, path = NULL,
         scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
         units =  "mm")
  
  linearMod_1_75 <- lm(basal_area ~ env_mean, data=sum_basal_data_2, species_id == 1 & env_sd == 75)
  linearMod_2_75 <- lm(basal_area ~ env_sd, data=sum_basal_data_2, species_id == 2 & env_mean == 75)
  linearMod_3_75 <- lm(basal_area ~ env_sd, data=sum_basal_data_2, species_id == 3 & env_mean == 75)
  linearMod_4_75 <- lm(basal_area ~ env_sd, data=sum_basal_data_2, species_id == 4 & env_mean == 75)
  linearMod_1_85 <- lm(basal_area ~ env_sd, data=sum_basal_data_2, species_id == 1 & env_mean == 85)
  linearMod_2_85 <- lm(basal_area ~ env_sd, data=sum_basal_data_2, species_id == 2 & env_mean == 85)
  linearMod_3_85 <- lm(basal_area ~ env_sd, data=sum_basal_data_2, species_id == 3 & env_mean == 85)
  linearMod_4_85 <- lm(basal_area ~ env_sd, data=sum_basal_data_2, species_id == 4 & env_mean == 85)  
  
  
  
  
  p <- ggplot(sum_basal_data, aes(x=env_mean, 
                                  y = mean_basal_area, 
                                  color = as.factor(species_id), 
                                  linetype=as.factor(env_sd))) + 
    geom_line() + 
    geom_point() + 
    scale_color_manual("Species Id:",
                       values=c(foxes_palettes$main[c(2, 1, 4, 3)]),
                       labels=c("Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe")) +
    scale_linetype_discrete("Environment SD (d):",
                            labels = c("0", "15", "30", "60")) +
    ylab("Final Basal Area Per Hectare (m2/ha)") +
    xlab("Mean of Stress (yr)") +
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
  