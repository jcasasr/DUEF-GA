### load libraries, sources and configuration options
source('config.R');

### vars
datasetName <- "sample-case-2";
xs <- c(1:10);

### load graphs
for(i in 1:10) {
  g <- loadGraph(paste("./graphs/polblogs-E-R/polblogs-k", i, "-E-R.gml", sep=""), type="gml");
  if(i==1) {
    graphs <- list(g);
  } else {
    graphs <- c(graphs, list(g)); 
  }
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

# GIL of the whole serie
plotSeries(xs, list(gil[,1]), legendNames=c("Method A"), legendPosition="topright", xlab="% Anonymization", ylab="Average Degree");
plotSeries(xs, list(gil[,2]), legendNames=c("Method A"), legendPosition="topright", xlab="% Anonymization", ylab="EI");
plotSeries(xs, list(gil[,3]), legendNames=c("Method A"), legendPosition="topright", xlab="% Anonymization", ylab="D");
plotSeries(xs, list(gil[,4]), legendNames=c("Method A"), legendPosition="bottomright", xlab="% Anonymization", ylab="Betweenness Centrality");
plotSeries(xs, list(gil[,5]), legendNames=c("Method A"), legendPosition="topright", xlab="% Anonymization", ylab="CC");
plotSeries(xs, list(gil[,6]), legendNames=c("Method A"), legendPosition="topright", xlab="% Anonymization", ylab="DC");

# Precision
plotSeries(xs, list(p[,1]), legendNames=c("Method A"), legendPosition="bottomleft", xlab="% Anonymization", ylab="Precision - Infomap");
plotSeries(xs, list(p[,2]), legendNames=c("Method A"), legendPosition="topright", xlab="% Anonymization", ylab="Precision - ML");
plotSeries(xs, list(p[,3]), legendNames=c("Method A"), legendPosition="topright", xlab="K value", ylab="Precision - Fastgreedy");
plotSeries(xs, list(p[,4]), legendNames=c("Method A"), legendPosition="topright", xlab="% Anonymization", ylab="Precision - WT");

# RRTI
plotSeries(xs, list(t(rrti)), legendNames=c("Method A"), legendPosition="topright", xlab="% Anonymization", ylab="% RRTI");

# InFlow
plotSeries(xs, list(t(inflow)), legendNames=c("Method A"), legendPosition="topright", xlab="% Anonymization", ylab="Information flow");
