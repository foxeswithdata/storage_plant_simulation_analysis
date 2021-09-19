# rm(list = ls())

extract_trees <- function(all_results){
  species_list <- lapply(1:length(all_results[[length(all_results)]]$species), function(species_ind){
    species_df <- data.frame()
    for(i in 1:length(all_results)){
      new_df <- as.data.frame(t(all_results[[i]]$species[[species_ind]]))
      if(nrow(new_df) > 0){
        new_df$time <- rep(all_results[[i]]$time, times = nrow(new_df))
        new_df$tree_id <- 1:nrow(new_df)
        new_df$species_id <- rep(species_ind, times = nrow(new_df))
      }
      species_df <- rbind(species_df, new_df)
    }
    return(species_df)
  })
  
  species_df <- data.frame()
  
  for(i in length(species_list)){
    species_df <- rbind(species_df, species_list[[i]])
  }
  
  return(species_df)  
  
}



load("data/simulation_tests/results_stochastic_test_4_years_100m2.RData")


library(ggplot2)
library(dplyr)

tree_data <- extract_trees(res)

live_v2 <- subset(tree_data, is_alive == TRUE)
live <- subset(tree_data, dead_flag != 1)

# live$tree_id <- as.factor(live$tree_id)
live_v2$tree_id <- as.factor(live_v2$tree_id)


# any

# p <- ggplot(live, aes(x = time, y = height, color = tree_id)) +
#   geom_line() + 
#   scale_x_continuous("Time [yr]", breaks = 0:10)
# p

p <- ggplot(live_v2, aes(x = time, y = height, color = tree_id)) +
  geom_line() + 
  scale_x_continuous("Time [yr]") + 
  theme(legend.position = "none")
p


p <- ggplot(live_v2, aes(x = time, y = mortality, color = tree_id)) +
  geom_line() + 
  scale_x_continuous("Time [yr]", breaks = 0:35) + 
  theme(legend.position = "none")
p


p <- ggplot(live_v2, aes(x = time, y = mass_storage, color = tree_id)) +
  geom_line() + 
  scale_x_continuous("Time [yr]", breaks = 0:35) + 
  theme(legend.position = "none")
p

p <- ggplot(tree_data, aes(x = mass_storage, y = mortality)) +
  geom_point() + 
  scale_x_continuous("Storage", breaks = 0:35) + 
  theme(legend.position = "none")
p


any(live_v2$mass_storage < 0)
any(tree_data$mass_storage < 0, na.rm = TRUE)


min(tree_data$height, na.rm = TRUE)

max(as.numeric(live_v2$tree_id))




#### Examine last elements and whats going on 

trees_final <- subset(tree_data, time == max(tree_data$time))

trees_final_is_alive <- subset(trees_final, is_alive ==TRUE)

trees_at_end <- subset(tree_data, tree_id %in% trees_final_is_alive$tree_id)
trees_at_end$tree_id <- as.factor(trees_at_end$tree_id)

nrow(trees_at_end)


p <- ggplot(trees_at_end, aes(x = time, y = height, color = tree_id)) +
  geom_line() + 
  # geom_line(data = data.frame(times = times, stress = (0.84 + stress/1000)), mapping = aes(times, stress), color = "black", alpha = 0.5) +
  scale_x_continuous("Time [yr]") + 
  theme(legend.position = "none")
p




p <- ggplot(trees_at_end, aes(x = time, y = dbiomass_dt, color = tree_id)) +
  geom_line() +
  scale_x_continuous("Time [yr]") + 
  theme(legend.position = "none")
p

p <- ggplot(trees_at_end, aes(x = time, y = mass_storage, color = tree_id)) +
  geom_line() +
  scale_x_continuous("Time [yr]") + 
  theme(legend.position = "none")
p

p <- ggplot(trees_at_end, aes(x = time, y = storage_portion, color = tree_id)) +
  geom_line() +
  scale_x_continuous("Time [yr]") + 
  theme(legend.position = "none")
p

p <- ggplot(trees_at_end, aes(x = time, y = area_leaf, color = tree_id)) +
  geom_line() +
  scale_x_continuous("Time [yr]") + 
  theme(legend.position = "none")
p

p <- ggplot(trees_at_end, aes(x = time, y = mortality, color = tree_id)) +
  geom_line() +
  scale_x_continuous("Time [yr]") + 
  scale_y_continuous(limits = c(-3,3)) +
  theme(legend.position = "none")
p


p1$environment$stress_regime









