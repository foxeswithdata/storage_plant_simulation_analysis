plot_basic_figures <- function(files, property, y_axis_label, y_axis_label_sum, figure_prefix, figure_out_dir, coeff = 1){
  tree_data <- process_data(files, get_dist_tree, TRUE, property)
  tree_data$species_id <- factor(tree_data$species_id, levels = c(1,2,3,4), labels = species_names)

  p <- ggplot(tree_data, aes(x = time, y = median, ymin = quantile_025,
                             ymax = quantile_075, colour = as.factor(species_id), fill = species_id)) +
    geom_line() +
    geom_ribbon(alpha = 0.15, linetype = "blank") +
    scale_color_manual("Allocation \nStrategy:",
                       values=foxes_palettes$main[c(2, 1, 4, 3)],
                       guide = guide_legend(override.aes = list(fill=foxes_palettes$main[c(2, 1, 4, 3)],
                                                                alpha = 0.15))) +
    scale_x_continuous("Time (yrs)") +
    scale_y_continuous(paste(y_axis_label, "per plant")) +
    facet_wrap(.~species_id, nrow = 2, ncol = 2) +
    guides(fill = FALSE) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          legend.position = "right",
          plot.margin = margin(1,1,1,1, "cm"),
          text=element_text(size=12, color = "#083855", family="Lato Light"),
          axis.text=element_text(size=8, color = "#083855", family="Lato Light"),
          strip.background = element_rect(fill = "white"))
  p
  
  file_figure <- tempfile(paste(figure_prefix, "median_igr", sep = "_"), tmpdir = figure_out_dir, fileext = ".png")
  ggsave(file_figure, plot = p, device = NULL, path = NULL,
         scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
         units =  "mm")
  
  p <- ggplot(tree_data, aes(x = time, y = mean, ymin = quantile_025,
                             ymax = quantile_075, colour = as.factor(species_id), fill = species_id)) +
    geom_line() +
    geom_ribbon(alpha = 0.15, linetype = "blank") +
    scale_color_manual("Allocation \nStrategy:",
                       values=foxes_palettes$main[c(2, 1, 4, 3)],
                       guide = guide_legend(override.aes = list(fill=foxes_palettes$main[c(2, 1, 4, 3)],
                                                                alpha = 0.15))) +
    scale_x_continuous("Time (yrs)") +
    scale_y_continuous(paste(y_axis_label, "per plant")) +
    facet_wrap(.~species_id, nrow = 2, ncol = 2) +
    guides(fill = FALSE) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          legend.position = "right",
          plot.margin = margin(1,1,1,1, "cm"),
          text=element_text(size=12, color = "#083855", family="Lato Light"),
          axis.text=element_text(size=8, color = "#083855", family="Lato Light"),
          strip.background = element_rect(fill = "white"))
  p
  
  file_figure <- tempfile(paste(figure_prefix, "mean_igr", sep = "_"), tmpdir = figure_out_dir, fileext = ".png")
  ggsave(file_figure, plot = p, device = NULL, path = NULL,
         scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
         units =  "mm")
  
  p <- ggplot(tree_data, aes(x = time, y = max, colour = as.factor(species_id))) +
    geom_line() +
    scale_color_manual("Allocation \nStrategy:",
                       values=foxes_palettes$main[c(2, 1, 4, 3)]) +
    scale_x_continuous("Time (yrs)") +
    scale_y_continuous(paste(y_axis_label, "per plant")) +
    facet_wrap(.~species_id, nrow = 2, ncol = 2) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          legend.position = "right",
          plot.margin = margin(1,1,1,1, "cm"),
          text=element_text(size=12, color = "#083855", family="Lato Light"),
          axis.text=element_text(size=8, color = "#083855", family="Lato Light"),
          strip.background = element_rect(fill = "white"))
  p
  
  file_figure <- tempfile(paste(figure_prefix, "max_multiple_plots", sep = "_"), tmpdir = figure_out_dir, fileext = ".png")
  ggsave(file_figure, plot = p, device = NULL, path = NULL,
         scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
         units =  "mm")
  
  p <- ggplot(tree_data, aes(x = time, y = max, colour = as.factor(species_id))) +
    geom_line() +
    scale_color_manual("Allocation \nStrategy:",
                       values=foxes_palettes$main[c(2, 1, 4, 3)]) +
    scale_x_continuous("Time (yrs)") +
    scale_y_continuous(paste(y_axis_label, "per plant")) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          legend.position = "right",
          plot.margin = margin(1,1,1,1, "cm"),
          text=element_text(size=12, color = "#083855", family="Lato Light"),
          axis.text=element_text(size=8, color = "#083855", family="Lato Light"),
          strip.background = element_rect(fill = "white"))
  p
  
  file_figure <- tempfile(paste(figure_prefix, "max", sep = "_"), tmpdir = figure_out_dir, fileext = ".png")
  ggsave(file_figure, plot = p, device = NULL, path = NULL,
         scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
         units =  "mm")
  
  p <- ggplot(tree_data, aes(x = time, y = sum, colour = species_id)) +
    geom_line() +
    scale_color_manual("Allocation \nStrategy:",
                       values=foxes_palettes$main[c(2, 1, 4, 3)]) +
    scale_x_continuous("Time (yrs)") +
    scale_y_continuous(paste(y_axis_label, "per plant")) +
    facet_wrap(.~species_id, nrow = 2, ncol = 2) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          legend.position = "right",
          plot.margin = margin(1,1,1,1, "cm"),
          text=element_text(size=12, color = "#083855", family="Lato Light"),
          axis.text=element_text(size=8, color = "#083855", family="Lato Light"),
          strip.background = element_rect(fill = "white"))
  p
  
  file_figure <- tempfile(paste(figure_prefix, "sum", sep = "_"), tmpdir = figure_out_dir, fileext = ".png")
  ggsave(file_figure, plot = p, device = NULL, path = NULL,
         scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
         units =  "mm")
  
  
  p <- ggplot(tree_data, aes(x = time, y = sum * coeff, colour = species_id)) +
    geom_line() +
    scale_color_manual("Allocation \nStrategy:",
                       values=foxes_palettes$main[c(2, 1, 4, 3)]) +
    scale_x_continuous("Time (yrs)") +
    scale_y_continuous(y_axis_label_sum) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          legend.position = "right",
          plot.margin = margin(1,1,1,1, "cm"),
          text=element_text(size=12, color = "#083855", family="Lato Light"),
          axis.text=element_text(size=8, color = "#083855", family="Lato Light"),
          strip.background = element_rect(fill = "white"))
  p
  
  file_figure <- tempfile(paste(figure_prefix, "sum_signle_plot", sep = "_"), tmpdir = figure_out_dir, fileext = ".png")
  ggsave(file_figure, plot = p, device = NULL, path = NULL,
         scale = 1, width = 190, height = 138, dpi = 300, limitsize = TRUE,
         units =  "mm")
  
  rm(tree_data)
}