rolling_average_basal_area <- function(data, window_length){
  time_seq <- seq(from = window_length/2, to=(max(data$time) - window_length))
  
  value_
  
  for(t in time_seq){
    
  }
}

process_data <- function(file_list, func, live_only = TRUE, value = NULL){
  out <- lapply(file_list, function(x){
    parts <- unlist(str_split(x, "/"))
    envrep <- unlist(str_split(parts[grep("envrep", parts)], "_"))
    envrep <- envrep[2]
    runrep <- unlist(str_split(parts[grep("runrep", parts)], "_"))
    runrep <- runrep[2]
    data_trees <- readRDS(file = x)
    data_trees$env_rep <- envrep
    data_trees$run_rep <- runrep
    if(live_only){
      data_trees <- get_trees_live(data_trees)  
    }
    if(!is.null(value)){
      out <- func(data_trees, value = value)  
    }
    else{
      out <- func(data_trees)
    }
    rm(data_trees)
    return(out)
  })
  out_all <- plyr::rbind.fill(out)
  return(out_all)
}

# make_rolling_average <- function(data_t, values){
#   out <- lapply(unique(data_t$species_id), function(sp_id))
# }


get_trees_live <- function(data_t){
  return(subset(data_t, is_alive==TRUE))
}

get_dist_tree_height <- function(data_t){
  min_tree <- data_t %>% group_by(species_id, time, env_rep, run_rep) %>%
    dplyr::summarise(min_height = min(height, na.rm = TRUE),
              mean_height = mean(height, na.rm = TRUE),
              median_height = median(height, na.rm = TRUE),
              quantile_005_height = quantile(height, probs = 0.05),
              quantile_025_height = quantile(height, probs = 0.25),
              quantile_075_height = quantile(height, probs = 0.75),
              quantile_095_height = quantile(height, probs = 0.95),
              max_height = max(height, na.rm = TRUE))
  return(min_tree)
}

get_dist_live_mass <- function(data_t){
  min_tree <- data_t %>% group_by(species_id, time, env_rep, run_rep) %>%
    dplyr::summarise(min_basal_area = min(basal_area, na.rm = TRUE),
                     mean_basal_area = mean(basal_area, na.rm = TRUE),
                     median_basal_area = median(basal_area, na.rm = TRUE),
                     quantile_005_basal_area = quantile(basal_area, probs = 0.05),
                     quantile_025_basal_area = quantile(basal_area, probs = 0.25),
                     quantile_075_basal_area = quantile(basal_area, probs = 0.75),
                     quantile_095_basal_area = quantile(basal_area, probs = 0.95),
                     max_basal_area = max(basal_area, na.rm = TRUE),
                     sum_basal_area = sum(basal_area, na.rm = TRUE))
  return(min_tree)
}

get_dist_tree_leaf_area <- function(data_t){
  min_tree <- data_t %>% group_by(species_id, time, env_rep, run_rep) %>%
    dplyr::summarise(min_leaf_area = min(leaf_area, na.rm = TRUE),
                     mean_leaf_area = mean(leaf_area, na.rm = TRUE),
                     median_leaf_area = median(leaf_area, na.rm = TRUE),
                     quantile_005_leaf_area = quantile(leaf_area, probs = 0.05),
                     quantile_025_leaf_area = quantile(leaf_area, probs = 0.25),
                     quantile_075_leaf_area = quantile(leaf_area, probs = 0.75),
                     quantile_095_leaf_area = quantile(leaf_area, probs = 0.95),
                     max_leaf_area = max(leaf_area, na.rm = TRUE),
                     sum_leaf_area = sum(leaf_area, na.rm = TRUE))
  return(min_tree)
}

get_dist_tree <- function(data_t, value){
  out <- data_t %>% group_by(species_id, time, env_rep, run_rep) %>%
    dplyr::summarise(min = min(eval(rlang::sym(value)), na.rm = TRUE),
                     mean = mean(eval(rlang::sym(value)), na.rm = TRUE),
                     median = median(eval(rlang::sym(value)), na.rm = TRUE),
                     quantile_005 = quantile(eval(rlang::sym(value)), probs = 0.05),
                     quantile_025 = quantile(eval(rlang::sym(value)), probs = 0.25),
                     quantile_075 = quantile(eval(rlang::sym(value)), probs = 0.75),
                     quantile_095 = quantile(eval(rlang::sym(value)), probs = 0.95),
                     max = max(eval(rlang::sym(value)), na.rm = TRUE),
                     sum = sum(eval(rlang::sym(value)), na.rm = TRUE))
  return(out)
}

get_multiple_dist <- function(data_t, values, new_name_value, new_name_type){
  out <- data_t %>% group_by(species_id, time, tree_id, env_rep, run_rep) %>%
    pivot_longer(cols = values, values_to = new_name_value, names_to = new_name_type) %>%
    group_by(species_id, time, eval(rlang::sym(new_name_type)))%>%
    dplyr::summarise(min = min(eval(rlang::sym(new_name_value)), na.rm = TRUE),
                     mean = mean(eval(rlang::sym(new_name_value)), na.rm = TRUE),
                     median = median(eval(rlang::sym(new_name_value)), na.rm = TRUE),
                     quantile_005 = quantile(eval(rlang::sym(new_name_value)), probs = 0.05),
                     quantile_025 = quantile(eval(rlang::sym(new_name_value)), probs = 0.25),
                     quantile_075 = quantile(eval(rlang::sym(new_name_value)), probs = 0.75),
                     quantile_095 = quantile(eval(rlang::sym(new_name_value)), probs = 0.95),
                     max = max(eval(rlang::sym(new_name_value)), na.rm = TRUE),
                     sum = sum(eval(rlang::sym(new_name_value)), na.rm = TRUE))
  return(out)
}

get_num_tree_height <- function(data_t){
  num_trees <- data_t %>% group_by(species_id, time, env_rep, run_rep) %>%
    dplyr::summarise(num_tree = n())
  return(num_trees)
}

subset_live_dead <- function(data_t){
  trees <- select(data_t, species_id, tree_id, is_alive, time, env_rep, run_rep)
  return(trees)
}


process_tree_data <- function(file){
  path_parts <- unlist(str_split(file, "/"))
  
  run_rep = unlist(str_split(path_parts[(length(path_parts)-1)], "_"))
  run_rep = run_rep[length(run_rep)]
  env_rep = unlist(str_split(path_parts[(length(path_parts)-2)], "_"))
  env_rep = env_rep[length(env_rep)]
  
  file_name <- path_parts[length(path_parts)]
  
  file_parts <- unlist(str_split(file_name, "_"))
  
  from = floor(as.numeric(file_parts[4]))
  
  to_parts = unlist(str_split(file_parts[6], "-"))
  to = floor(as.numeric(to_parts[1]))
  
  return(data.frame(file = file,
                    file_name = file_name,
                    from = from,
                    to = to,
                    run_rep = run_rep,
                    env_rep = env_rep,
                    stringsAsFactors = FALSE))
}

