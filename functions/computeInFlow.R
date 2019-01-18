#' Compute SIL-InFlow metrics on a graph's list
#' 
#' @param graphs List of graphs (two or more).
#' @param datasetName Name of the dataset (to store temporal values).
#' @return Array with metrics' score values.
createInFlow <- function(graphs, datasetName) {
  # Create ans save clusters for each graph
  for(i in 1:length(graphs)) {
    g <- graphs[[i]];
    logdebug("*** Processing graph:");
    summary(g);
    
    # compute distances from every vertex to all other vertices
    dists <- distances(g, v=V(g), to=V(g), mode="all", weights=NULL);
    
    # check if any vertex is isolated (distance == Inf)
    if(sum(is.infinite(dists)) > 0) {
      logwarn("createInFlow:: setting distance = 0 for isolated vertices! (Inf value).");
      dists[is.infinite(dists)] <- 0;
    }
    
    # get maximum distance from each vertex to all other vertices
    maxDists <- apply(dists, 1, max);
    
    # Save values
    filename <- paste(subdirInFlow, datasetName, "-", i, ".csv", sep="");
    write.table(maxDists, file=filename, quote=FALSE, sep=";", dec=".", row.names=FALSE, col.names=FALSE);
  }
  
  return(TRUE);
}

#' Compute SIL-InFlow metrics on a graph's list
#' 
#' @param graphs List of graphs (two or more).
#' @param datasetName Name of the dataset (to store temporal values).
#' @return Array with precision score values.
computeInFlow <- function(graphs, datasetName) {
  # define array to store score values
  inflow <- array(data=NA, dim=length(graphs));
  
  # load RRTI for original graph
  filename <- paste(subdirInFlow, datasetName, "-1.csv", sep="");
  
  if(file.exists(filename)) {
    valuesG0 <- read.csv(file=filename, header=FALSE, sep=";", dec=".");
  } else {
    logerror("computeInFlow:: CSV export file not found! [%s]", filename);
    stop();
  }
  
  # compute score for all graphs
  for(i in 1:length(graphs)) {
    filename <- paste(subdirInFlow, datasetName, "-", i, ".csv", sep="");
    
    if(file.exists(filename)) {
      values <- read.csv(file=filename, header=FALSE, sep=";", dec=".");
    } else {
      logerror("computeRRTI:: CSV export file not found! [%s]", filename);
      stop();
    }
    
    # compute InFlow
    inflow[i] <- sum(abs(valuesG0 - values)) / nrow(valuesG0);
  }
  
  # save
  filename <- paste(subdirRes, datasetName, "-InFlow.RData", sep="");
  save(inflow, file=filename);
  
  return(inflow);
}
