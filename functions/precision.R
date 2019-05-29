#' Compute SIL-clustering metrics on a graph's list
#' 
#' @param graphs List of graphs (two or more).
#' @param datasetName Name of the dataset (to store temporal values).
#' @return Array with precision score values.
computePrecision <- function(graphs, datasetName, repetition) {
  # define array to store score values
  p <- array(data=NA, dim=c(length(graphs), length(clusteringSet))
                  , dimnames=list(1:length(graphs), clusteringSet));
  
  # Create ans save clusters for each graph
  for(clustering in clusteringSet) {
    logdebug("+++ Computing clustering algorithm: %s", clustering);
    
    # load default communities values (i.e. original graph)
    filename <- paste(subdirClust, datasetName, "-", repetition, "-1-", clustering, ".txt", sep="");
    
    if(file.exists(filename)) {
      com0 <- readCommunityFile(filename);
    } else {
      logerror("computePrecision:: Communities export file not found! [%s]", filename);
      stop();
    }
    
    # compute precision for all graphs
    for(i in 1:length(graphs)) {
      g <- graphs[[i]];
      
      filename <- paste(subdirClust, datasetName, "-", repetition, "-", i, "-", clustering, ".txt", sep="");
      
      if(file.exists(filename)) {
        com1 <- readCommunityFile(filename);
      } else {
        logerror("computePrecision:: Communities export file not found! [%s]", filename);
        stop();
      }
      
      # compute precision
      p[i, clustering] <- getPrecisionScore(com0, com1);
    }
  }
  
  # save
  filename <- paste(subdirRes, datasetName, "-precision.RData", sep="");
  save(p, file=filename);
  
  return(p);
}

#' Read community set from a txt file
#' 
#' @param filename A valid file name.
#' @return A list of communities.
readCommunityFile <- function(filename) {
  # open file
  conn <- file(filename, open="r");
  linn <- readLines(conn);
  # create empty vector
  v <- vector(length=length(linn));
  # fill it
  for (i in 1:length(linn)){
    #print(linn[i]);
    v[i] <- list(strsplit(linn[i], " ")[[1]]);
  }
  close(conn);
  
  return(v);
}

#' Compute the Jaccard precision index
#' 
#' @param l1 A list of communities.
#' @param l1 A list of communities.
#' @return A real value in range [0,1].
getPrecisionScore <- function(l1, l2) {
  logdebug("Computing precision #l1 = %d and #l2 = %d", length(l1), length(l2));
  
  # pre-process step (dimensionality reduction)
  if(useDimensionalityReduction) {
    l1 <- dimensionalityReduction(l1, maxDim);
    l2 <- dimensionalityReduction(l2, maxDim);
    
    logdebug("Computing precision #l1 = %d and #l2 = %d", length(l1), length(l2));
  }
  
  # create table
  t <- array(data=NA, dim=c(length(l1), length(l2)));
  
  # inform table
  #logdebug("Creating table...");
  for(i in 1:length(l1)) {
    for(j in 1:length(l2)) {
      t[i, j] <- length(intersect(l1[[i]], l2[[j]])); 
    }
  }
  
  # search max coincidence
  #logdebug("Matching pairs...");
  p <- 0;
  while(max(t)>=0) {
    maxs <- which(t==max(t), arr.ind=TRUE);
    
    if(is.array(max)) {
      # hi ha varis maxims, seleccionar el mes adequat
      max_i <- max[1,1];
      max_j <- max[1,2];
      max_val <- max(t[max_i, -max_j]);
      for(r in 1:nrow(max)) {
        tmp_i <- max[1,1];
        tmp_j <- max[1,2];
        tmp_val <- max(t[tmp_i, -tmp_j]);
        if(tmp_val > max_val) {
          max_i <- tmp_i;
          max_j <- tmp_j;
          max_val <- tmp_val;
        }
      }
    } else {
      # nomes hi ha un maxim
      max_i <- maxs[1,1];
      max_j <- maxs[1,2];
    }
    # sum
    p <- p + t[max_i, max_j];
    
    #logdebug("Matching %d-%d = %d", max_i, max_j, t[max_i, max_j]);
    
    # remove
    t[max_i, ] <- -1;
    t[, max_j] <- -1;
  }
  
  # compute precision value
  total <- 0;
  for(i in 1:length(l1)) {
    total <- total + length(l1[[i]]);
  }
  
  precision <- p / total;
  logdebug("Precision value = %f [%s / %s]", precision, p, total);
  
  return(precision);
}

#' Remove small communities to reduce computing time (see config.R to adjust this value)
#' 
#' @param l A list of communities.
#' @param maxDim Maximum dimensionality.
#' @return A list of communities.
dimensionalityReduction <- function(l, maxDim) {
  # params
  ind <- 1;
  totalRemoved <- 0;
  lenOri <- length(l);
  
  while(length(l) > maxDim) {
    rem <- c();
    
    # select items to remove
    for(i in 1:length(l)) {
      if(length(l[[i]]) < ind) {
        rem <- c(rem, i);
      }
    }
    
    # decreasing order
    rem <- sort(rem, decreasing=TRUE);
    
    # remove them
    for(r in rem) {
      l[[r]] <- NULL;
    }
    
    #logdebug("%s sets of size: %s were removed!", length(rem), ind);
    totalRemoved <- totalRemoved + length(rem);
    
    # increase "ind"
    ind <- ind + 1;
  }
  
  logdebug("Dimensionality reduction (maxDim=%s): %s -> %s (%s vertices were removed (sets < %s elements)!)", maxDim, lenOri, length(l), totalRemoved, ind);
  
  return (l);
}
