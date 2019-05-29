#' Compute difference between degree sequences on a graph's list
#' 
#' @param graphs List of graphs (two or more).
#' @param datasetName Name of the dataset (to store temporal values).
#' @return Array with values.
computeDegreeChangedVertices <- function(graphs, datasetName) {
  # define array to store score values
  values <- array(data=NA, dim=c(length(graphs))
                  , dimnames=list(1:length(graphs)));
  
  # original graph
  g0 <- graphs[[1]];
  
  # original network's degree sequence
  d0 <- degree(g0, mode="all", normalized=FALSE);
  
  # Compute metrics on each graph
  for(i in 1:length(graphs)) {
    g <- graphs[[i]];
    
    # compute network's degree sequence
    d <- degree(g, mode="all", normalized=FALSE);
    
    # remove new fake vertices
    if(length(d) > length(d0)) {
      d <- d[1:length(d0)];
    }
    
    # compute number of vertices
    values[i] <- sum(abs(d0 - d) > 0);
  }
  
  return(values);
}
