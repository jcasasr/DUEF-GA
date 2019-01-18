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
plotSeries(xs, list(res1[,2], res2[,2]), legendNames=c("Method A", "Method B"), legendPosition="topright", xlab="% Anonymization", ylab="EI");
plotSeries(xs, list(res1[,3], res2[,3]), legendNames=c("Method A", "Method B"), legendPosition="topright", xlab="% Anonymization", ylab="D");
plotSeries(xs, list(res1[,4], res2[,4]), legendNames=c("Method A", "Method B"), legendPosition="bottomright", xlab="% Anonymization", ylab="Betweenness Centrality");
plotSeries(xs, list(res1[,5], res2[,5]), legendNames=c("Method A", "Method B"), legendPosition="topright", xlab="% Anonymization", ylab="CC");
plotSeries(xs, list(res1[,6], res2[,6]), legendNames=c("Method A", "Method B"), legendPosition="topright", xlab="% Anonymization", ylab="DC");

# Precision
plotSeries(xs, list(p1[,1], p2[,1]), legendNames=c("Method A", "Method B"), legendPosition="topright", xlab="% Anonymization", ylab="Precision - Infomap");
plotSeries(xs, list(p1[,2], p2[,2]), legendNames=c("Method A", "Method B"), legendPosition="topright", xlab="% Anonymization", ylab="Precision - MultiLevel");
plotSeries(xs, list(p1[,3], p2[,3]), legendNames=c("Method A", "Method B"), legendPosition="topright", xlab="% Anonymization", ylab="Precision - FastGreegy");
plotSeries(xs, list(p1[,4], p2[,4]), legendNames=c("Method A", "Method B"), legendPosition="topright", xlab="% Anonymization", ylab="Precision - WalkTrap");

# RRTI
plotSeries(xs, list(t(rrti1), t(rrti2)), legendNames=c("Method A", "Method B"), legendPosition="topright", xlab="% Anonymization", ylab="% RRTI");

# InFlow
plotSeries(xs, list(t(inflow1), t(inflow2)), legendNames=c("Method A", "Method B"), legendPosition="topright", xlab="% Anonymization", ylab="Information flow");
