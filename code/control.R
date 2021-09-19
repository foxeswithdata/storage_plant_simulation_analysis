data_directories <- list.dirs("data/full_run_new_processed/env_1_0", recursive = FALSE)


data_basal_area <- plyr::rbind.fill(lapply(data_directories, function(dat){
  files <- list.files(path = dat, recursive = TRUE)
  file_names_full <- as.vector(sapply(files, function(x){
    return(file.path(dat, x))
  }))
  
  all_data <- plyr::rbind.fill(lapply(file_names_full, readRDS))
  
  seq_time = seq(from = h2, to = 99, by = h2)
  
  tree_sum_basal <- plyr::rbind.fill(lapply(seq_time, function(t){
    basal <- all_data %>% 
      dplyr::filter((time > (t-h2)) & (time < (t+h2))) %>%
      dplyr::group_by(species_id, run_rep, env_rep, time, env_sd, env_mean) %>%
      dplyr::summarise(sum_basal = sum(100 * area_stem, na.rm = TRUE),
                       max_height = max(height, na.rm = TRUE),
                       sum_leaf_area = sum(area_leaf/100, na.rm = TRUE),
                       sum_mass_storage = sum(mass_storage/100, na.rm = TRUE)) %>%
      dplyr::group_by(species_id, env_rep, env_sd, env_mean) %>%
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
                       se_sum_mass_storage_m2 = std.error(sum_mass_storage, na.rm=TRUE))
    basal$time = t
    return(basal)
    
  }))
  return(tree_sum_basal)
}))



data_basal_area$sp_id_env_mean = as.factor(data_basal_area$species_id + data_basal_area$env_mean)

p1 <- ggplot(data_basal_area, aes(x = time, y = mean_sum_basal, 
                                  ymin = mean_sum_basal - sd_sum_basal, 
                                  ymax = mean_sum_basal + sd_sum_basal,
                                  color = sp_id_env_mean, linetype = as.factor(env_rep),
                                  fill = sp_id_env_mean, group = env_rep)) +
  geom_line() +
  geom_ribbon(alpha = 0.35, linetype = "blank") + 
  sfm +
  scale_color_manual("Allocation \nStrategy:",
                     values=c(foxes_palettes$light[c(2, 1, 4, 3)], foxes_palettes$main[c(2, 1, 4, 3)],foxes_palettes$dark[c(2, 1, 4, 3)])) +
  scale_x_continuous("Time (yrs)") +
  scale_y_continuous("Average Basal Area Coverage per Hectar (m2/ha) in Run") +
  species_wrap + 
  guides(fill = FALSE) +
  foxes_theme 
p1

