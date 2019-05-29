#' Compute the number of vertices which change their 1-neighbourhood during perturbation process
#' 
#' @param graphs List of graphs (two or more).
#' @param datasetName Name of the dataset (to store temporal values).
#' @return Array with values.
computeNeighChangesVertices <- function(graphs, datasetName) {
  # define array to store score values
  values <- array(data=NA, dim=c(length(graphs))
                  , dimnames=list(1:length(graphs)));
  
  # original graph
  g0 <- graphs[[1]];
  
  # Compute metrics on each graph
  for(i in 1:length(graphs)) {
    g <- graphs[[i]];
    
    # compute number of vertices
    values[i] <- compareGraphs(g0, g);
  }
  
  return(values);
}

areEquals <- function(l1, l2) {
  if(length(l1) == length(l2)) {
    eq <- (l1 == l2);
    if(sum(eq==FALSE) == 0) {
      return(TRUE);
    }
  }
  
  return(FALSE);
}

compareGraphs <- function(g0, g) {
  # vertex set
  vertexSet <- V(g0);
  numEquals <- 0;
  numNoEquals <- 0;
  
  # check whether the neighbourhood has changed
  for(vert in vertexSet) {
    n0 <- neighbors(g0, vert, mode=1);
    n <- neighbors(g, vert, mode=1);
    
    if(areEquals(n0, n)) {
      # equal
      numEquals <- numEquals + 1;
    } else {
      # no equal
      numNoEquals <- numNoEquals + 1;
    }
  }
  
  logdebug("No equals = %s, equals = %s, total = %s", numNoEquals, numEquals, numNoEquals + numEquals);
  
  return(numNoEquals);
}
