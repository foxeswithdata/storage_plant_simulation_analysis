plotly_3d_render <- function(data){
  
  axx <- list(
    title = "Initial Height [m]"
  )
  axy <- list(
    title = "b_s1 Values [kgCkg^(-1)C]"
  )
  axz <- list(
    title = "Optimal time switch (ts [d]) values"
  )
  
  x_list = as.numeric(rownames(data))
  y_list = as.numeric(colnames(data))
  
  fig <- plot_ly(z = ~data, x = x_list, 
                 y = y_list)
  fig <- fig %>% add_surface()
  fig <- fig %>% layout(scene = list(xaxis=axx,yaxis=axy,zaxis=axz))
  
  return(fig)
}



ggplot_heatmap <- function(data){
  data_conv <- expand.grid(X=as.numeric(rownames(data)), Y=as.numeric(colnames(data)))
  data_conv$Z <- unmatrix(data)
  
  p <- ggplot(data_conv, aes(X, Y, fill= Z)) + 
    geom_tile() +
    scale_x_continuous("Initial Plant Height [m]") + 
    scale_y_continuous(expression(paste("Initial Storage Mass Proportion , ", beta[s], ' , ', kgCkg^-1, C))) + 
    scale_fill_viridis("Time Switch \nValue [d]", discrete=FALSE) +
    theme_bw()
  
  return(p)
}


