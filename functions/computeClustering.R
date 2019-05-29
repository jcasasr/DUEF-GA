#' Compute SIL-clustering metrics on a graph's list
#' 
#' @param graphs List of graphs (two or more).
#' @param datasetName Name of the dataset (to store temporal values).
#' @return Array with metrics' score values.
createClusters <- function(graphs, datasetName, repetition) {
  # Create ans save clusters for each graph
  for(i in 1:length(graphs)) {
    g <- graphs[[i]];
    logdebug("*** Processing graph:");
    summary(g);
    
    for(clustering in clusteringSet) {
      logdebug("+++ Computing clustering algorithm: %s", clustering);
      
      computeAndSave(g, clustering, datasetName, repetition, i);
    }
  }
  
  return(TRUE);
}

# Computes the community structure and save the results in .txt file
# 
computeAndSave <- function(g, clustering, datasetName, repetition, i) {
  # file
  filename = paste(subdirClust, datasetName, "-", repetition, "-", i, "-", clustering, ".txt", sep="");
  
  if(!file.exists(filename)) {
    # clustering
    if(clustering == "infomap") {
      communities <- infomap.community(g);
      
    } else if(clustering == "multilevel") {
      communities <- multilevel.community(g);
      
    } else if(clustering == "edge.betweenness") {
      communities <- edge.betweenness.community(g);
      
    } else if(clustering == "fastgreedy") {
      communities <- fastgreedy.community(g);
      
    } else if(clustering == "walktrap") {
      # delete isolated vertices (unconnected graphs are not allowed)
      g <- DeleteIsolatedVertices(g);
      communities <- walktrap.community(g);
      
    } else {
      logerror("Error! Unknown clustering %s", clustering);
      stop();
    }
    
    # write to file
    CommunitiesWriteToFile(communities, filename);
    
    logdebug("%s clustering... done!", clustering);
    
  } else {
    # File exists
    logdebug("File '%s' already exists!", filename);
  }
}

# write communities to file
#
CommunitiesWriteToFile <- function(communities, filename) {
  sink(filename);
  for (i in 1:max(communities$membership)) {
    community <- paste(which(communities$membership==i));
    cat(community);
    cat("\n");
  }
  sink();
}

# Delete isolated vertices from graph
#
DeleteIsolatedVertices <- function(g) {
  g <- g - vertices(degree(g)==0);
  
  return(g);
}
