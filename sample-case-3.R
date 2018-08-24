### load libraries, sources and configuration options
source('config.R');

### vars
datasetName1 <- "sample-case-3_1";
datasetName2 <- "sample-case-3_2";
xs <- c(0:25);

### load graphs
g <- loadGraph("./graphs/polbooks.gml", type="gml");

graphs1 <- list(g);
for(i in 1:25) {
  g1 <- loadGraph(paste("./graphs/polbooks-rp/polbooks-1-", i, ".gml", sep=""), type="gml");
  graphs1 <- c(graphs1, list(g1));
}

graphs2 <- list(g);
for(i in 1:25) {
  g1 <- loadGraph(paste("./graphs/polbooks-nc/polbooks-1-", i, ".gml", sep=""), type="gml");
  graphs2 <- c(graphs2, list(g1));
}

### Computations

# compute GIL metrics
metricSet <- c("AD", "EI", "D", "BC", "CC", "DC");
res1 <- computeGIL(graphs1, datasetName1);
res2 <- computeGIL(graphs2, datasetName2);

# compute SIL-clustering communities
createClusters(graphs1, datasetName1);
p1 <- computePrecision(graphs1, datasetName1);

createClusters(graphs2, datasetName2);
p2 <- computePrecision(graphs2, datasetName2);

# compute SIL-RRTI communities
createRRTI(graphs1, datasetName1);
rrti1 <- computeRRTI(graphs1, datasetName1);

createRRTI(graphs2, datasetName2);
rrti2 <- computeRRTI(graphs2, datasetName2);

# compute SIL-InFlow communities
createInFlow(graphs1, datasetName1);
inflow1 <- computeInFlow(graphs1, datasetName1);

createInFlow(graphs2, datasetName2);
inflow2 <- computeInFlow(graphs2, datasetName2);

### plots and visualization

# Average degree of the whole serie
plotSeries(xs, list(res1[,1], res2[,1]), legendNames=c("Method A", "Method B"), legendPosition="topright", xlab="% Anonymization", ylab="Average Degree");

# Precision
plotSeries(xs, list(p1[,1], p2[,1]), legendNames=c("Method A", "Method B"), legendPosition="topright", xlab="% Anonymization", ylab="Precision");

# RRTI
plotSeries(xs, list(t(rrti1), t(rrti2)), legendNames=c("Method A", "Method B"), legendPosition="topright", xlab="% Anonymization", ylab="% RRTI");

# InFlow
plotSeries(xs, list(t(inflow1), t(inflow2)), legendNames=c("Method A", "Method B"), legendPosition="topright", xlab="% Anonymization", ylab="Information flow");
