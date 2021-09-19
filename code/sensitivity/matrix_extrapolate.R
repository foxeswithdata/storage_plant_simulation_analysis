matrix_extrapolate <- function(data, xby, yby){
  x_vals <- as.numeric(rownames(data))
  y_vals <- as.numeric(colnames(data))
  
  # if(any(is.na(x_vals))){
    # warning("missing row values")
  # }
  # else if(!numeric(x_vals)){
  #   warning("row names must be numeric")
  # }
  # 
  # if(any(is.na(y_vals))){
  #   warning("missing column values")
  # }
  # else if(!numeric(y_vals)){
  #   warning("column names must be numeric")
  # }
  # 
  
  # x_vals_diffs <- min(diff(x_vals))
  x_vals_new <- seq(from=min(x_vals), to=max(x_vals), by = xby)
  # found = FALSE
  # while(!found){
  #   if(!is.subset(x_vals, x_vals_new)){
  #     new_x <- unique(sort(union(x_vals, x_vals_new)))
  #     x_vals_diffs <- min(diff(new_x))
  #     print(x_vals_diffs)
  #     x_vals_new <- seq(from=min(x_vals), to=max(x_vals), by = x_vals_diffs)
  #   }
  #   else{
  #     found == TRUE
  #   }
  # }
  
  y_vals_diffs <- min(diff(y_vals))
  y_vals_new <- seq(from=min(y_vals), to=max(y_vals), by = yby)
  
  data_new_xy <- matrix(nrow=length(x_vals_new), ncol=length(y_vals_new))
  data_new_yx <- matrix(nrow=length(x_vals_new), ncol=length(y_vals_new))
  colnames(data_new_xy) <- y_vals_new
  rownames(data_new_xy) <- x_vals_new
  colnames(data_new_yx) <- y_vals_new
  rownames(data_new_yx) <- x_vals_new
  
  ## Fill in empty values
  
  for(i in 1:length(x_vals)){
    for(j in 1:length(y_vals)){
      i_new <- which(x_vals_new == x_vals[i])
      j_new <- which(y_vals_new == y_vals[j])
      data_new_xy[i_new, j_new] = data[i,j]
      data_new_yx[i_new, j_new] = data[i,j]
    }
  }
  
  ## Do a x then y spleen
  
  for(i in 1:length(y_vals)){
    new_vals = spline(x_vals, data[,i], xout = x_vals_new)
    i_new <- which(y_vals_new == y_vals[i])
    data_new_xy[,i_new] = new_vals$y
  }
  
  subscripts <- which(!(is.na(data_new_xy[1,])))
  
  for(i in 1:length(x_vals_new)){
    new_vals = spline(names(subscripts), data_new_xy[i,subscripts], xout = y_vals_new)
    data_new_xy[i,] = new_vals$y
  }
  
  
  ## Do a y then x spleen
  
  for(i in 1:length(x_vals)){
    new_vals = spline(y_vals, data[i,], xout = y_vals_new)
    i_new <- which(x_vals_new == x_vals[i])
    data_new_yx[i_new,] = new_vals$y
  }
  
  subscripts <- which(!(is.na(data_new_yx[,1])))
  
  for(i in 1:length(y_vals_new)){
    new_vals = spline(names(subscripts), data_new_yx[subscripts,i], xout = x_vals_new)
    data_new_yx[,i] = new_vals$y
  }
  
  ## Average out the two spleens
  
  data_new = (data_new_xy + data_new_yx)/2
  
  ## Correct for 0 and 0.75
  
  data_new[which(data_new < 0)] = 0
  data_new[which(data_new > 0.75)] = 0.75
  
  
  return(data_new)
}


convert_to_df <- function(data){
  data_conv <- expand.grid(X=as.numeric(rownames(data)), Y=as.numeric(colnames(data)))
  data_conv$Z <- unmatrix(data)
  return(data_conv)
}

