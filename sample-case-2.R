### load libraries, sources and configuration options
source('config.R');

### vars
datasetName <- "sample-case-2";
xs <- c(0:25);

### load graphs
g <- loadGraph("./graphs/polbooks.gml", type="gml");

graphs <- list(g);
for(i in 1:25) {
  g1 <- loadGraph(paste("./graphs/polbooks-rp/polbooks-1-", i, ".gml", sep=""), type="gml");
  graphs <- c(graphs, list(g1));
}

### Computations

# compute GIL metrics
gil <- computeGIL(graphs, datasetName);

# compute SIL-clustering communities
createClusters(graphs, datasetName);
p <- computePrecision(graphs, datasetName);

# compute SIL-RRTI communities
createRRTI(graphs, datasetName);
rrti <- computeRRTI(graphs, datasetName);

# compute SIL-InFlow communities
createInFlow(graphs, datasetName);
inflow <- computeInFlow(graphs, datasetName);

### plots and visualization

# Average degree of the whole serie
plotSeries(xs, list(gil[,1]), legendNames=c("Method A"), legendPosition="topright", xlab="% Anonymization", ylab="Average Degree");

# Precision
plotSeries(xs, list(p[,1]), legendNames=c("Method A"), legendPosition="topright", xlab="% Anonymization", ylab="Precision");

# RRTI
plotSeries(xs, list(t(rrti)), legendNames=c("Method A"), legendPosition="topright", xlab="% Anonymization", ylab="% RRTI");

# InFlow
plotSeries(xs, list(t(inflow)), legendNames=c("Method A"), legendPosition="topright", xlab="% Anonymization", ylab="Information flow");
