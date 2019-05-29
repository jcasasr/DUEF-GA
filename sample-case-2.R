### load libraries, sources and configuration options
source('config.R');

### vars
datasetName <- "sample-case-2";
repetition <- 1;
xs <- c(1:10);

### load graphs
for(i in 1:10) {
  g <- loadGraph(paste("./graphs/URV-email/graphs/URV-email-", repetition, "-", i, ".gml", sep=""), type="gml");
  if(i==1) {
    graphs <- list(g);
  } else {
    graphs <- c(graphs, list(g)); 
  }
}

### Computations

### Utility measures

# compute GIL metrics
gil <- computeGIL(graphs, datasetName, repetition);

# compute SIL-clustering communities
createClusters(graphs, datasetName, repetition);
p <- computePrecision(graphs, datasetName, repetition);

# compute SIL-RRTI communities
createRRTI(graphs, datasetName, repetition);
rrti <- computeRRTI(graphs, datasetName, repetition);

# compute SIL-InFlow communities
createInFlow(graphs, datasetName, repetition);
inflow <- computeInFlow(graphs, datasetName, repetition);

### Privacy measures

# compute DCV
dcv <- computeDegreeChangedVertices(graphs, datasetName);

# compute 1-Neigh
neigh <- computeNeighChangesVertices(graphs, datasetName);

### plots and visualization

# GIL of the whole serie
plotSeries(xs, list(gil[,1]), legendNames=c("UMGA"), legendPosition="topright", xlab="K value", ylab="Average degree", filename="s2-AD");
plotSeries(xs, list(gil[,2]), legendNames=c("UMGA"), legendPosition="topright", xlab="K value", ylab="Edge intersection", filename="s2-EI");
plotSeries(xs, list(gil[,3]), legendNames=c("UMGA"), legendPosition="topright", xlab="K value", ylab="Diameter", filename="s2-D");
plotSeries(xs, list(gil[,4]), legendNames=c("UMGA"), legendPosition="bottomright", xlab="K value", ylab="Betweenness centrality", filename="s2-BC");
plotSeries(xs, list(gil[,5]), legendNames=c("UMGA"), legendPosition="topleft", xlab="K value", ylab="Closeness centrality", filename="s2-CC");
plotSeries(xs, list(gil[,6]), legendNames=c("UMGA"), legendPosition="topright", xlab="K value", ylab="Degree centrality", filename="s2-DC");

# Precision
plotSeries(xs, list(p[,1]), legendNames=c("UMGA"), legendPosition="topright", xlab="K value", ylab="Precision - Infomap", filename="s2-IM");
plotSeries(xs, list(p[,2]), legendNames=c("UMGA"), legendPosition="topright", xlab="K value", ylab="Precision - Multilevel", filename="s2-ML");
plotSeries(xs, list(p[,3]), legendNames=c("UMGA"), legendPosition="topright", xlab="K value", ylab="Precision - Fastgreedy", filename="s2-FG");
plotSeries(xs, list(p[,4]), legendNames=c("UMGA"), legendPosition="topright", xlab="K value", ylab="Precision - Walktrap", filename="s2-WT");

# RRTI
plotSeries(xs, list(t(rrti)), legendNames=c("UMGA"), legendPosition="bottomleft", xlab="K value", ylab="RRTI", filename="s2-RRTI");

# InFlow
plotSeries(xs, list(t(inflow)), legendNames=c("UMGA"), legendPosition="topleft", xlab="K value", ylab="FRV", filename="s2-FRV");

# DCV
dcv <- dcv / vcount(g);
plotSeries(xs, list(t(dcv)), legendNames=c("UMGA"), legendPosition="topleft", xlab="K value", ylab="DCV", filename="s2-DCV");

# 1-Neigh
neigh <- neigh / vcount(g);
plotSeries(xs, list(t(neigh)), legendNames=c("UMGA"), legendPosition="topleft", xlab="K value", ylab="1-Neighbourhood", filename="s2-Neigh");

# compute and plot the candidate set size
candSize <- computeH1Analysis(graphs, datasetName, add=TRUE, del=TRUE);
