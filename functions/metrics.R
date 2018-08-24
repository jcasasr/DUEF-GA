# Get average degree
getAverageDegree <- function(g) {
  return(2 * ecount(g) / vcount(g));
}

# Get the subgraph centrality
getSubgraphCentrality <- function(g) {
  return(sum(subgraph.centrality(g)) / vcount(g));
}

# Get the largest eigenvalue of the adjacency matrix
getLambda1 <- function(g) {
  e <- graph.eigen(g, which=list(pos="LM", howmany=1));
  return(e$values[1]);
}

# Get the second smallest eigenvalue of the Laplacian Matrix
getMu2 <- function(g, index=2) {
  L <- graph.laplacian(g);
  e <- eigen(L);
  eigenvalues <- sort(e$values, decreasing=FALSE);
  
  return(eigenvalues[index]);
}

# Get the second smallest eigenvalue of the Laplacian Matrix
# Alternative function
getMu2_2 <- function(g, index=2) {
  # remove not connected components
  # ARPACK might be unstable for graphs with multiple components
  g <- removeNotConnectedComponents(g);
  
  logdebug("G is connected = %s", is.connected(g));
  
  L <- graph.laplacian(g, sparse=TRUE);
  func <- function(x, extra=NULL) { as.vector(L %*% x) }
  vals <- arpack(func, sym=TRUE, options=list(n=vcount(g), nev=index, ncv=8, maxiter=10000, which="SM"))$values;
  
  return(vals[index]);
}

removeNotConnectedComponents <- function(g) {
  clu <- decompose.graph(g);
  val <- -1;
  ind <- -1;
  for(i in 1:length(clu)) {
    if(vcount(clu[[i]]) > val) {
      val <- vcount(clu[[i]]);
      ind <- i;
    }
  }
  
  return(clu[[ind]]);
}

# Get the harmonic value
# ChesterEtAl:2012
# TODO
getHarmonic <- function(g) {
  n <- vcount(g);
  sp <- shortest.paths(g);
  
  value <- 0;
  for(i in 1:length(sp[,1])) {
    for(j in 1:length(sp[i,])) {
      if(sp[i,j] > 0) {
        value <- value + (1/sp[i,j]);
      }
    }
  }
  term <- (1/(n*(n-1)));
  res <- (1 / (term * value));
  
  return(res);
}

# Edge intersection
edgeIntersection <- function(g1, g2) {
  logdebug("edgeIntersection: Starting...");
  numNodesG1 <- length(V(g1));
  numNodesG2 <- length(V(g2));
  
  if(numNodesG1 != numNodesG2) {
    logerror("edgeIntersection: ERROR: Different number of nodes G1=%d and G2=%d",numNodesG1, numNodesG2);
    
    return(NA);
  } else {
    numEdgesG1 <- length(E(g1));
    numEdgesG2 <- length(E(g2));
    total <- max(numEdgesG1,numEdgesG2);
    inter <- 0;
    
    # use matrix
    mG1 <- get.adjacency(g1);
    mG2 <- get.adjacency(g2);
    
    # count number of equal edges
    if(is.directed(g1) && is.directed(g2)) {
      stop("NOT YET IMPLEMENTED!");
      
    } else {
      mA <- mG1 * mG2;
      inter <- sum(mA[mA>0]);
      # undirected
      inter <- inter/2;
    }
    
    # score value
    inter <- (inter/total);
    
    loginfo("edge_intersection: datasets G1=%d and G2=%d", numEdgesG1, numEdgesG2);
    loginfo("edge_intersection: Edge intersection(G1,G2) = %.4f", inter);
    
    return(inter);
  }
}

getBetweennessRMS <- function(g, filenameMetricG0, i) {
  
  # we are processing the original graph
  if(i == 1) {
    value <- betweenness(g, directed=FALSE, normalized=TRUE);
    
    # save data
    save(value, file=filenameMetricG0);
  }
  
  # load original graph's metric values
  if(file.exists(filenameMetricG0)) {
    load(filenameMetricG0);
    
    return(computeDiff(value, betweenness(g, directed=FALSE, normalized=TRUE)));
  } else {
    # Metric score values for the original graph are not computed!
    logerror("getBetweennessRMS:: Unable to load original graph's score values! [file=%s]", filenameMetricG0);
  }
  
  return(NaN);
}

getClosenessRMS <- function(g, filenameMetricG0, i) {
  
  # we are processing the original graph
  if(i == 1) {
    value <- closeness(g, mode="all", normalized=TRUE);
    
    # save data
    save(value, file=filenameMetricG0);
  }
  
  # load original graph's metric values
  if(file.exists(filenameMetricG0)) {
    load(filenameMetricG0);
    
    return(computeDiff(value, closeness(g, mode="all", normalized=TRUE)));
  } else {
    # Metric score values for the original graph are not computed!
    logerror("getClosenessRMS:: Unable to load original graph's score values! [file=%s]", filenameMetricG0);
  }
  
  return(NaN);
}

getDegreeRMS <- function(g, filenameMetricG0, i) {
  
  # we are processing the original graph
  if(i == 1) {
    value <- degree(g, mode="all", normalized=TRUE);
    
    # save data
    save(value, file=filenameMetricG0);
  }
  
  # load original graph's metric values
  if(file.exists(filenameMetricG0)) {
    load(filenameMetricG0);
    
    return(computeDiff(value, degree(g, mode="all", normalized=TRUE)));
  } else {
    # Metric score values for the original graph are not computed!
    logerror("getDegreeRMS:: Unable to load original graph's score values! [file=%s]", filenameMetricG0);
  }
  
  return(NaN);
}

getCoreness <- function(g, filenameMetricG0, i) {
  
  # we are processing the original graph
  if(i == 1) {
    value <- graph.coreness(g);
    
    # save data
    save(value, file=filenameMetricG0);
  }
  
  # load original graph's metric values
  if(file.exists(filenameMetricG0)) {
    load(filenameMetricG0);
    
    return(sum(value == graph.coreness(g)) / vcount(g));
  } else {
    # Metric score values for the original graph are not computed!
    logerror("getCoreness:: Unable to load original graph's score values! [file=%s]", filenameMetricG0);
  }
  
  return(NaN);
}
