data_directories <- list.dirs("data/full_run_processed", recursive = FALSE)

hist_all <- plyr::rbind.fill(lapply(seq(100, 100, by= 1), function(num){
  hist <- plyr::rbind.fill(lapply(data_directories, function(dat){
    files <- list.files(path = dat, pattern = "tree", recursive = TRUE)
    file_names_full <- as.vector(sapply(files, function(x){
      return(file.path(dat, x))
    }))
    
    file_names_full <- file_names_full[grep("extra", file_names_full, invert = TRUE)]
    
    all_data <- plyr::rbind.fill(lapply(file_names_full, readRDS))
    
    all_data_det <- all_data %>% 
      filter((time >= (num-10)) & (time < num)& is_alive == TRUE)
      # dplyr::group_by(env_rep, run_rep, species_id) %>%
      # dplyr::summarise(time = min(time))
    
    # all_data_sub <- all_data %>% select(time, env_rep, run_rep, species_id, tree_id, mass_storage, area_stem, is_alive, height, area_leaf, env_mean, env_sd,
    #                                     storage_portion, mortality_new, mortality_growth_dependent, mortality_storage_dependent, net_mass_production_dt, respiration_dt,
    #                                     dbiomass_dt, mass_leaf, mass_root, mass_sapwood, mass_bark, dt)
    # all_data_det <- left_join(all_data_times, all_data_sub, by = c("env_rep", "run_rep", "species_id", "time"))
    all_data_det$time_slice = num
    return(all_data_det)
  }))
  return(hist)
}))


hist_all$env <- hist_all$env_mean + hist_all$env_sd

hist_data <- hist_all

saveRDS(hist_all, "out/data_hist_new.rds")
