### load libraries, sources and configuration options
source('config.R');

### load graphs
g <- loadGraph("./graphs/polbooks.gml", type="gml");
rp10 <- loadGraph("./graphs/polbooks-rp/polbooks-1-10.gml", type="gml");
graphs <- list(g, rp10);

### vars
datasetName <- "sample-case-1";

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
