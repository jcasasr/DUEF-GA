#' Compute GIL metrics on a graph's list
#' 
#' @param graphs List of graphs (two or more).
#' @param datasetName Name of the dataset (to store temporal values).
#' @return Array with metrics' score values.
computeGIL <- function(graphs, datasetName) {
  # define array to store score values
  values <- array(data=NA, dim=c(length(graphs), length(metricSet))
                  , dimnames=list(1:length(graphs), metricSet));
  
  # original graph (needed for edge intersection)
  g0 <- graphs[[1]];
  
  # Compute metrics on each graph
  for(i in 1:length(graphs)) {
    g <- graphs[[i]];
    logdebug("*** Processing graph:");
    summary(g);
    
    for(metric in metricSet) {
      logdebug("+++ Processing metric: %s", metric);
          
      values[i, metric] <- getMetricValue(g, g0, metric, datasetName, i);
    }
  }
  
  # save results
  save(values, file=paste(subdirRes, datasetName, "-metrics.RData", sep=""));

  return(values);
}

#' Compute GIL metrics on a graph's list
#' 
#' @param g A graph object.
#' @param g0 A graph objec (original graph).
#' @param metric A valid metric (see metricSet from 'config.R' file).
#' @param datasetName Name of the dataset (to store temporal values).
#' @param i Index of graph in graph's list.
#' @return Metric's score value or values.
getMetricValue <- function(g, g0, metric, datasetName, i) {
  
  # filename to store metric's score values
  filenameMetric <- paste(subdirGIL, datasetName, "-", i, "-", metric, ".RData", sep="");
  
  # filename to load original graph's values
  filenameMetricG0 <- paste(subdirGIL, datasetName, "-ORI-", metric, ".RData", sep="");
  
  if(file.exists(filenameMetric)) {
    # metric exists, load file
    logdebug("+++ Loading %s...", metric);
    
    load(filenameMetric);
    
  } else {
    # metric does not exist, compute it
    logdebug("+++ Computing %s...", metric);
    
    if(metric == 'lambda1') {
      # lambda_1
      value <- getLambda1(g);
    } else if(metric == 'mu2') {
      # mu_2
      value <- getMu2(g);
    } else if(metric == 'AD') {
      # Average distance
      value <- average.path.length(g, directed=FALSE);
    } else if(metric == 'D') {
      # Diameter
      value <- diameter(g, directed=FALSE);
    } else if(metric == 'T') {
      # Transitivity
      value <- transitivity(g, type="global", isolates=NaN);
    } else if(metric == 'EI') {
      # Edge intersection
      value <- edgeIntersection(g0, g);
    } else if(metric == 'SC') {
      # Subgraph centrality
      value <- getSubgraphCentrality(g);
    } else if(metric == 'h') {
      # Harmonic
      value <- getHarmonic(g);
    } else if(metric == 'Q') {
      # Modularity
      value <- modularity(g, communities);
    } else if(metric == 'Core') {
      # k-core
      value <- getCoreness(g, filenameMetricG0, i)
    } else if(metric == 'BC') {
      # Betweenness centrality
      value <- getBetweennessRMS(g, filenameMetricG0, i);
    } else if(metric == 'CC') {
      # Closeness centrality
      value <- getClosenessRMS(g, filenameMetricG0, i);
    } else if(metric == 'DC') {
      # Degree centrality
      value <- getDegreeRMS(g, filenameMetricG0, i);
    }
    
    # save data
    save(value, file=filenameMetric);
  }
  
  return(value);
}
