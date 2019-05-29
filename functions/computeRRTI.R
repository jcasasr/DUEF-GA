#' Compute SIL-RRTI metrics on a graph's list
#' 
#' @param graphs List of graphs (two or more).
#' @param datasetName Name of the dataset (to store temporal values).
#' @return Array with metrics' score values.
createRRTI <- function(graphs, datasetName, repetition) {
  # Create ans save clusters for each graph
  for(i in 1:length(graphs)) {
    g <- graphs[[i]];
    logdebug("*** Processing graph:");
    summary(g);
    
    # compute Page Rank
    pr <- page_rank(g, algo="prpack", vids = V(g), directed=FALSE, damping=0.85);
    
    # Save values
    filename <- paste(subdirRRTI, datasetName, "-", repetition, "-", i, ".csv", sep="");
    write.table(pr$vector, file=filename, quote=FALSE, sep=";", dec=".", row.names=FALSE, col.names=FALSE);
  }
  
  return(TRUE);
}

#' Compute SIL-RRTI metrics on a graph's list
#' 
#' @param graphs List of graphs (two or more).
#' @param datasetName Name of the dataset (to store temporal values).
#' @return Array with precision score values.
computeRRTI <- function(graphs, datasetName, repetition, percen=0.2) {
  # define array to store score values
  rrti <- array(data=NA, dim=length(graphs));
  
  # load RRTI for original graph
  filename <- paste(subdirRRTI, datasetName, "-", repetition, "-1.csv", sep="");
  
  if(file.exists(filename)) {
    valuesG0 <- read.csv(file=filename, header=FALSE, sep=";", dec=".");
  } else {
    logerror("computeRRTI:: CSV export file not found! [%s]", filename);
    stop();
  }
  
  # get the n-th most important vertices
  max <- round(vcount(g) * percen, digits=0);
  tmpG0 <- order(valuesG0, decreasing=TRUE)[1:max]
  
  # compute score for all graphs
  for(i in 1:length(graphs)) {
    g <- graphs[[i]];
    
    filename <- paste(subdirRRTI, datasetName, "-", repetition, "-", i, ".csv", sep="");
    
    if(file.exists(filename)) {
      values <- read.csv(file=filename, header=FALSE, sep=";", dec=".");
    } else {
      logerror("computeRRTI:: CSV export file not found! [%s]", filename);
      stop();
    }
    
    # compute RRTI
    tmp <- order(values, decreasing=TRUE)[1:max];
    rrti[i] <- length(intersect(tmpG0, tmp)) / length(tmpG0);
  }
  
  # save
  filename <- paste(subdirRes, datasetName, "-RRTI.RData", sep="");
  save(rrti, file=filename);
  
  return(rrti);
}
