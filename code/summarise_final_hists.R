 
library(tidyverse)

figure_dir <- "out/figures_manual/"

data_directories <- list.dirs("data/full_run_processed", recursive = FALSE)

hist_all <- plyr::rbind.fill(lapply(seq(20, 100, by= 20), function(num){
  hist <- plyr::rbind.fill(lapply(data_directories, function(dat){
    files <- list.files(path = dat, recursive = TRUE)
    file_names_full <- as.vector(sapply(files, function(x){
      return(file.path(dat, x))
    }))
    
    all_data <- plyr::rbind.fill(lapply(file_names_full, readRDS))
    
    
    all_data_times <- all_data %>% 
      filter(time >= num & is_alive == TRUE) %>%
      dplyr::group_by(env_rep, run_rep, species_id) %>%
      dplyr::summarise(time = min(time))
    
    all_data_sub <- all_data %>% select(time, env_rep, run_rep, species_id, tree_id, mass_storage, area_stem, is_alive, height, area_leaf, env_mean, env_sd)
    all_data_det <- left_join(all_data_times, all_data_sub, by = c("env_rep", "run_rep", "species_id", "time"))
    all_data_det$time_slice = num
    return(all_data_det)
  }))
  return(hist)
}))


hist_all$env <- hist_all$env_mean + hist_all$env_sd

hist_data <- hist_all

saveRDS(hist_all, "out/data_hist.rds")

### histograms

### height data
hist_data_100 <- subset(hist_data, time_slice == 100)
min(hist_data_100$height)
max(hist_data_100$height)

binsize = 2

# hist_data_100
bin_data_height <- hist_data_100 %>%
  dplyr::mutate(bin = floor(height/binsize) * binsize + binsize/2) %>%
  dplyr::group_by(env, run_rep, env_rep, species_id, bin, env_mean, env_sd) %>% 
  dplyr::count() %>%
  dplyr::group_by(env, species_id, bin,  env_mean, env_sd) %>%
  dplyr::summarise(mean_num = mean(n) * 100)


p <- ggplot(bin_data_height, aes(x=bin, y = mean_num, fill=as.factor(species_id))) + 
  geom_bar(stat = 'identity', position="identity", alpha = 0.45) + 
  geom_line(aes(color=as.factor(species_id))) +
  geom_point(aes(color=as.factor(species_id))) +
  scale_fill_manual("Species Id:",
                    values=c(foxes_palettes$main[c(2, 1, 4, 3)]),
                    labels=c("Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe")) +
  scale_color_manual("Species Id:",
                     values=c(foxes_palettes$main[c(2, 1, 4, 3)]),
                     labels=c("Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe")) +
  ylab("Average number of individual in hectare") +
  scale_x_continuous("height") +
  facet_grid(env_sd~ env_mean) +
  # guides(fill = FALSE) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        legend.position = "right",
        plot.margin = margin(1,1,1,1, "cm"),
        text=element_text(size=12, color = "#083855", family="Lato Light"),
        axis.text=element_text(size=8, color = "#083855", family="Lato Light"),
        strip.background = element_rect(fill = "white"))
p


file_figure <- tempfile(paste("height_hist", sep = "_"), tmpdir = figure_dir, fileext = ".png")
ggsave(file_figure, plot = p, device = NULL, path = NULL,
       scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
       units =  "mm")



##basal area
hist_data_100 <- subset(hist_data, time_slice == 100)
min(hist_data_100$mass_storage)
max(hist_data_100$mass_storage)

binsize = 10

bin_data_mass_storage <- hist_data_100 %>%
  dplyr::mutate(bin = floor(mass_storage/binsize) * binsize + binsize/2) %>%
  dplyr::group_by(env, run_rep, env_rep, species_id, bin, env_mean, env_sd) %>% 
  dplyr::count() %>%
  dplyr::group_by(env, species_id, bin,  env_mean, env_sd) %>%
  dplyr::summarise(mean_num = mean(n))


p <- ggplot(bin_data_mass_storage, aes(x=bin, y = mean_num, fill=as.factor(species_id))) + 
  geom_bar(stat = 'identity', position="identity", alpha = 0.45) + 
  geom_line(aes(color=as.factor(species_id))) +
  geom_point(aes(color=as.factor(species_id))) +
  scale_fill_manual("Species Id:",
                    values=c(foxes_palettes$main[c(2, 1, 4, 3)]),
                    labels=c("Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe")) +
  scale_color_manual("Species Id:",
                     values=c(foxes_palettes$main[c(2, 1, 4, 3)]),
                     labels=c("Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe")) +
  ylab("Average number of individual in hectar") +
  scale_x_continuous("Storage Mass (kg)") +
  facet_grid(env_sd~ env_mean) +
  # guides(fill = FALSE) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        legend.position = "right",
        plot.margin = margin(1,1,1,1, "cm"),
        text=element_text(size=12, color = "#083855", family="Lato Light"),
        axis.text=element_text(size=8, color = "#083855", family="Lato Light"),
        strip.background = element_rect(fill = "white"))
p


file_figure <- tempfile(paste("storage_hist", sep = "_"), tmpdir = figure_dir, fileext = ".png")
ggsave(file_figure, plot = p, device = NULL, path = NULL,
       scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
       units =  "mm")


#####


sum_basal_data <- hist_data %>%
  dplyr::group_by(env, run_rep, env_rep, species_id, env_mean, env_sd, time_slice) %>% 
  dplyr::summarise(basal_area = sum(area_stem)) %>%
  dplyr::group_by(env, species_id,  env_mean, env_sd, time_slice) %>%
  dplyr::summarise(mean_basal_area = mean(basal_area))


# sum_basal_data

# scale_fill_manual("Species Id:",
#                   values=c(foxes_palettes$main[c(2, 1, 4, 3)]),
#                   labels=c("Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe")) +
#   scale_color_manual("Species Id:",
#                      values=c(foxes_palettes$main[c(2, 1, 4, 3)]),
#                      labels=c("Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe")) +

p <- ggplot(sum_basal_data, aes(x=as.factor(species_id), y = mean_basal_area, fill=as.factor(env))) + 
  geom_bar(stat = 'identity', position="dodge", alpha = 0.75) + 
  scale_fill_manual("Environment Id:",
                    values=c(foxes_palettes$extra[2],
                             foxes_palettes$light[5],
                             foxes_palettes$dark[5],
                             foxes_palettes$light[1],
                             foxes_palettes$dark[1],
                             foxes_palettes$light[2],
                             foxes_palettes$dark[2],
                             foxes_palettes$light[4],
                             foxes_palettes$dark[4]),
                    labels = c("Control - no stress",
                               "No Stoch - heavy stress",
                               "No Stoch - medium stress",
                               "Low Stoch - heavy stress",
                               "Low Stoch - medium stress",
                               "Med Stoch - heavy stress",
                               "Med Stoch - medium stress",
                               "High Stoch - heavy stress",
                               "High Stoch - medium stress")) +
  ylab("Basal Area Cover Per Hectar (m2/ha)") +
  scale_x_discrete("Species", labels = c("Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe")) +
  facet_grid(time_slice~.) +
  # guides(fill = FALSE) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        legend.position = "right",
        plot.margin = margin(1,1,1,1, "cm"),
        text=element_text(size=12, color = "#083855", family="Lato Light"),
        axis.text=element_text(size=8, color = "#083855", family="Lato Light"),
        strip.background = element_rect(fill = "white"))
p


file_figure <- tempfile(paste("basal_area_per_hectare_at_100yr", sep = "_"), tmpdir = figure_dir, fileext = ".png")
ggsave(file_figure, plot = p, device = NULL, path = NULL,
       scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
       units =  "mm")

### leaf area


lai_data <- hist_data %>%
  dplyr::group_by(env, run_rep, env_rep, species_id, env_mean, env_sd, time_slice) %>% 
  dplyr::summarise(basal_area = sum(area_leaf)) %>%
  dplyr::group_by(env, species_id,  env_mean, env_sd, time_slice) %>%
  dplyr::summarise(mean_lai = mean(basal_area)/100)


# sum_basal_data

# scale_fill_manual("Species Id:",
#                   values=c(foxes_palettes$main[c(2, 1, 4, 3)]),
#                   labels=c("Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe")) +
#   scale_color_manual("Species Id:",
#                      values=c(foxes_palettes$main[c(2, 1, 4, 3)]),
#                      labels=c("Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe")) +

p <- ggplot(lai_data, aes(x=as.factor(species_id), y = mean_lai, fill=as.factor(env))) + 
  geom_bar(stat = 'identity', position="dodge", alpha = 0.75) + 
  scale_fill_manual("Environment Id:",
                    values=c(foxes_palettes$extra[2],
                             foxes_palettes$light[5],
                             foxes_palettes$dark[5],
                             foxes_palettes$light[1],
                             foxes_palettes$dark[1],
                             foxes_palettes$light[2],
                             foxes_palettes$dark[2],
                             foxes_palettes$light[4],
                             foxes_palettes$dark[4]),
                    labels = c("Control - no stress",
                               "No Stoch - heavy stress",
                               "No Stoch - medium stress",
                               "Low Stoch - heavy stress",
                               "Low Stoch - medium stress",
                               "Med Stoch - heavy stress",
                               "Med Stoch - medium stress",
                               "High Stoch - heavy stress",
                               "High Stoch - medium stress")) +
  ylab("Leaf Area Index (m2/m2)") +
  scale_x_discrete("Species", labels = c("Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe")) +
  facet_grid(time_slice~.) +
  # guides(fill = FALSE) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        legend.position = "right",
        plot.margin = margin(1,1,1,1, "cm"),
        text=element_text(size=12, color = "#083855", family="Lato Light"),
        axis.text=element_text(size=8, color = "#083855", family="Lato Light"),
        strip.background = element_rect(fill = "white"))
p


file_figure <- tempfile(paste("lai_at_100yr", sep = "_"), tmpdir = figure_dir, fileext = ".png")
ggsave(file_figure, plot = p, device = NULL, path = NULL,
       scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
       units =  "mm")


### height


height_data <- hist_data %>%
  dplyr::group_by(env, run_rep, env_rep, species_id, env_mean, env_sd, time_slice) %>% 
  dplyr::summarise(basal_area = max(height)) %>%
  dplyr::group_by(env, species_id,  env_mean, env_sd, time_slice) %>%
  dplyr::summarise(max_heihgt = mean(basal_area))


# sum_basal_data

# scale_fill_manual("Species Id:",
#                   values=c(foxes_palettes$main[c(2, 1, 4, 3)]),
#                   labels=c("Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe")) +
#   scale_color_manual("Species Id:",
#                      values=c(foxes_palettes$main[c(2, 1, 4, 3)]),
#                      labels=c("Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe")) +

p <- ggplot(height_data, aes(x=as.factor(species_id), y = max_heihgt, fill=as.factor(env))) + 
  geom_bar(stat = 'identity', position="dodge", alpha = 0.75) + 
  scale_fill_manual("Environment Id:",
                    values=c(foxes_palettes$extra[2],
                             foxes_palettes$light[5],
                             foxes_palettes$dark[5],
                             foxes_palettes$light[1],
                             foxes_palettes$dark[1],
                             foxes_palettes$light[2],
                             foxes_palettes$dark[2],
                             foxes_palettes$light[4],
                             foxes_palettes$dark[4]),
                    labels = c("Control - no stress",
                               "No Stoch - heavy stress",
                               "No Stoch - medium stress",
                               "Low Stoch - heavy stress",
                               "Low Stoch - medium stress",
                               "Med Stoch - heavy stress",
                               "Med Stoch - medium stress",
                               "High Stoch - heavy stress",
                               "High Stoch - medium stress")) +
  ylab("Maximum Height (m)") +
  scale_x_discrete("Species", labels = c("Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe")) +
  facet_grid(time_slice~.) +
  # guides(fill = FALSE) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        legend.position = "right",
        plot.margin = margin(1,1,1,1, "cm"),
        text=element_text(size=12, color = "#083855", family="Lato Light"),
        axis.text=element_text(size=8, color = "#083855", family="Lato Light"),
        strip.background = element_rect(fill = "white"))
p


file_figure <- tempfile(paste("height_max_at_100yr", sep = "_"), tmpdir = figure_dir, fileext = ".png")
ggsave(file_figure, plot = p, device = NULL, path = NULL,
       scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
       units =  "mm")


### mass storage

store_data <- hist_data %>%
  dplyr::group_by(env, run_rep, env_rep, species_id, env_mean, env_sd, time_slice) %>% 
  dplyr::summarise(basal_area = sum(mass_storage)) %>%
  dplyr::group_by(env, species_id,  env_mean, env_sd, time_slice) %>%
  dplyr::summarise(mean_storage = mean(basal_area)/100)


# sum_basal_data

# scale_fill_manual("Species Id:",
#                   values=c(foxes_palettes$main[c(2, 1, 4, 3)]),
#                   labels=c("Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe")) +
#   scale_color_manual("Species Id:",
#                      values=c(foxes_palettes$main[c(2, 1, 4, 3)]),
#                      labels=c("Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe")) +

p <- ggplot(store_data, aes(x=as.factor(species_id), y = mean_storage, fill=as.factor(env))) + 
  geom_bar(stat = 'identity', position="dodge", alpha = 0.75) + 
  scale_fill_manual("Environment Id:",
                    values=c(foxes_palettes$extra[2],
                             foxes_palettes$light[5],
                             foxes_palettes$dark[5],
                             foxes_palettes$light[1],
                             foxes_palettes$dark[1],
                             foxes_palettes$light[2],
                             foxes_palettes$dark[2],
                             foxes_palettes$light[4],
                             foxes_palettes$dark[4]),
                    labels = c("Control - no stress",
                               "No Stoch - heavy stress",
                               "No Stoch - medium stress",
                               "Low Stoch - heavy stress",
                               "Low Stoch - medium stress",
                               "Med Stoch - heavy stress",
                               "Med Stoch - medium stress",
                               "High Stoch - heavy stress",
                               "High Stoch - medium stress")) +
  ylab("Mean Mass Storage per m2 (kg/m2)") +
  scale_x_discrete("Species", labels = c("Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe")) +
  facet_grid(time_slice~.) +
  # guides(fill = FALSE) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        legend.position = "right",
        plot.margin = margin(1,1,1,1, "cm"),
        text=element_text(size=12, color = "#083855", family="Lato Light"),
        axis.text=element_text(size=8, color = "#083855", family="Lato Light"),
        strip.background = element_rect(fill = "white"))
p


file_figure <- tempfile(paste("mass_storage_at_100yr", sep = "_"), tmpdir = figure_dir, fileext = ".png")
ggsave(file_figure, plot = p, device = NULL, path = NULL,
       scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
       units =  "mm")



### sd versus basal area sum

sum_basal_data <- hist_data_100 %>%
  dplyr::group_by(env, run_rep, env_rep, species_id, env_mean, env_sd, time_slice) %>% 
  dplyr::summarise(basal_area = sum(area_stem)) %>%
  dplyr::group_by(env, species_id,  env_mean, env_sd, time_slice) %>%
  dplyr::summarise(mean_basal_area = mean(basal_area) * 100)

sum_basal_data <- subset(sum_basal_data, (env_mean %in% c(75, 85)))

p <- ggplot(sum_basal_data, aes(x=env_sd, 
                                y = mean_basal_area, 
                                color = as.factor(species_id), 
                                linetype=as.factor(env_mean))) + 
  geom_line() + 
  geom_point() + 
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


### sd versus height max

height_data <- hist_data_100 %>%
  dplyr::group_by(env, run_rep, env_rep, species_id, env_mean, env_sd, time_slice) %>% 
  dplyr::summarise(basal_area = max(height)) %>%
  dplyr::group_by(env, species_id,  env_mean, env_sd, time_slice) %>%
  dplyr::summarise(max_heihgt = mean(basal_area))


height_data <- subset(height_data, (env_mean %in% c(75, 85)))

p <- ggplot(height_data, aes(x=env_sd, 
                                y = max_heihgt, 
                                color = as.factor(species_id), 
                                linetype=as.factor(env_mean))) + 
  geom_line() + 
  geom_point() + 
  scale_color_manual("Species Id:",
                     values=c(foxes_palettes$main[c(2, 1, 4, 3)]),
                     labels=c("Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe")) +
  scale_linetype_discrete("Environment Mean (yr):",
                          labels = c("0.75", "0.85")) +
ylab("Final Max Height (m)") +
  xlab("Standard Deviation of Stress (d)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        legend.position = "right",
        plot.margin = margin(1,1,1,1, "cm"),
        text=element_text(size=12, color = "#083855", family="Lato Light"),
        axis.text=element_text(size=8, color = "#083855", family="Lato Light"),
        strip.background = element_rect(fill = "white"))
p



file_figure <- tempfile(paste("height_max_sd", sep = "_"), tmpdir = figure_dir, fileext = ".png")
ggsave(file_figure, plot = p, device = NULL, path = NULL,
       scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
       units =  "mm")


p <- ggplot(height_data, aes(x=env_mean, 
                             y = max_heihgt, 
                             color = as.factor(species_id), 
                             linetype=as.factor(env_sd))) + 
  geom_line() + 
  geom_point() + 
  scale_color_manual("Species Id:",
                     values=c(foxes_palettes$main[c(2, 1, 4, 3)]),
                     labels=c("Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe")) +
  scale_linetype_discrete("Environment SD (d):",
                          labels = c("0", "15", "30", "60")) +
  ylab("Final Max Height (m)") +
  xlab("Mean of Stress (yr)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        legend.position = "right",
        plot.margin = margin(1,1,1,1, "cm"),
        text=element_text(size=12, color = "#083855", family="Lato Light"),
        axis.text=element_text(size=8, color = "#083855", family="Lato Light"),
        strip.background = element_rect(fill = "white"))
p


file_figure <- tempfile(paste("height_max_mean", sep = "_"), tmpdir = figure_dir, fileext = ".png")
ggsave(file_figure, plot = p, device = NULL, path = NULL,
       scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
       units =  "mm")



### sd versus lai

lai_data <- hist_data_100 %>%
  dplyr::group_by(env, run_rep, env_rep, species_id, env_mean, env_sd, time_slice) %>% 
  dplyr::summarise(basal_area = sum(area_leaf)) %>%
  dplyr::group_by(env, species_id,  env_mean, env_sd, time_slice) %>%
  dplyr::summarise(mean_lai = mean(basal_area/100))

lai_data <- subset(lai_data, (env_mean %in% c(75, 85)))

p <- ggplot(lai_data, aes(x=env_sd, 
                                y = mean_lai, 
                                color = as.factor(species_id), 
                                linetype=as.factor(env_mean))) + 
  geom_line() + 
  geom_point() + 
  scale_color_manual("Species Id:",
                     values=c(foxes_palettes$main[c(2, 1, 4, 3)]),
                     labels=c("Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe")) +
  scale_linetype_discrete("Environment Mean (yr):",
                          labels = c("0.75", "0.85")) +
  ylab("Final LAI (m2/m2)") +
  xlab("Standard Deviation of Stress (d)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        legend.position = "right",
        plot.margin = margin(1,1,1,1, "cm"),
        text=element_text(size=12, color = "#083855", family="Lato Light"),
        axis.text=element_text(size=8, color = "#083855", family="Lato Light"),
        strip.background = element_rect(fill = "white"))
p



file_figure <- tempfile(paste("lai_sd", sep = "_"), tmpdir = figure_dir, fileext = ".png")
ggsave(file_figure, plot = p, device = NULL, path = NULL,
       scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
       units =  "mm")


p <- ggplot(lai_data, aes(x=env_mean, 
                                y = mean_lai, 
                                color = as.factor(species_id), 
                                linetype=as.factor(env_sd))) + 
  geom_line() + 
  geom_point() + 
  scale_color_manual("Species Id:",
                     values=c(foxes_palettes$main[c(2, 1, 4, 3)]),
                     labels=c("Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe")) +
  scale_linetype_discrete("Environment SD (d):",
                          labels = c("0", "15", "30", "60")) +
  ylab("Final LAI (m2/m2)") +
  xlab("Mean of Stress (yr)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        legend.position = "right",
        plot.margin = margin(1,1,1,1, "cm"),
        text=element_text(size=12, color = "#083855", family="Lato Light"),
        axis.text=element_text(size=8, color = "#083855", family="Lato Light"),
        strip.background = element_rect(fill = "white"))
p



file_figure <- tempfile(paste("lai_mean", sep = "_"), tmpdir = figure_dir, fileext = ".png")
ggsave(file_figure, plot = p, device = NULL, path = NULL,
       scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
       units =  "mm")


#### storage


mass_storage_data <- hist_data_100 %>%
  dplyr::group_by(env, run_rep, env_rep, species_id, env_mean, env_sd, time_slice) %>% 
  dplyr::summarise(basal_area = sum(mass_storage)) %>%
  dplyr::group_by(env, species_id,  env_mean, env_sd, time_slice) %>%
  dplyr::summarise(mean_storage_sum = mean(basal_area)/100)

mass_storage_data <- subset(mass_storage_data, (env_mean %in% c(75, 85)))

p <- ggplot(mass_storage_data, aes(x=env_sd, 
                          y = mean_storage_sum, 
                          color = as.factor(species_id), 
                          linetype=as.factor(env_mean))) + 
  geom_line() + 
  geom_point() + 
  scale_color_manual("Species Id:",
                     values=c(foxes_palettes$main[c(2, 1, 4, 3)]),
                     labels=c("Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe")) +
  scale_linetype_discrete("Environment Mean (yr):",
                          labels = c("0.75", "0.85")) +
  ylab("Final Mass Storage per m2 (kg/m2)") +
  xlab("Standard Deviation of Stress (d)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        legend.position = "right",
        plot.margin = margin(1,1,1,1, "cm"),
        text=element_text(size=12, color = "#083855", family="Lato Light"),
        axis.text=element_text(size=8, color = "#083855", family="Lato Light"),
        strip.background = element_rect(fill = "white"))
p



file_figure <- tempfile(paste("mass_storage_sd", sep = "_"), tmpdir = figure_dir, fileext = ".png")
ggsave(file_figure, plot = p, device = NULL, path = NULL,
       scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
       units =  "mm")


p <- ggplot(mass_storage_data, aes(x=env_mean, 
                          y = mean_storage_sum, 
                          color = as.factor(species_id), 
                          linetype=as.factor(env_sd))) + 
  geom_line() + 
  geom_point() + 
  scale_color_manual("Species Id:",
                     values=c(foxes_palettes$main[c(2, 1, 4, 3)]),
                     labels=c("Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe")) +
  scale_linetype_discrete("Environment SD (d):",
                          labels = c("0", "15", "30", "60")) +
  ylab("Final LAI (m2/m2)") +
  xlab("Mean of Stress (yr)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        legend.position = "right",
        plot.margin = margin(1,1,1,1, "cm"),
        text=element_text(size=12, color = "#083855", family="Lato Light"),
        axis.text=element_text(size=8, color = "#083855", family="Lato Light"),
        strip.background = element_rect(fill = "white"))
p



file_figure <- tempfile(paste("mean_storage_mean", sep = "_"), tmpdir = figure_dir, fileext = ".png")
ggsave(file_figure, plot = p, device = NULL, path = NULL,
       scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
       units =  "mm")




