### load libraries, sources and configuration options
source('config.R');

### vars
datasetName <- "sample-case-1";
num_repetitions <- 10;

# create empty vars
gil <- array(data=NA, dim=c(num_repetitions, 2, length(metricSet))
                , dimnames=list(1:num_repetitions, 1:2, metricSet));
p <- array(data=NA, dim=c(num_repetitions, 2, length(clusteringSet))
             , dimnames=list(1:num_repetitions, 1:2, clusteringSet));
rrti <- array(data=NA, dim=c(num_repetitions, 2, length(rrtiSet))
             , dimnames=list(1:num_repetitions, 1:2, rrtiSet));
inflow <- array(data=NA, dim=c(num_repetitions, 2, length(inflowSet))
             , dimnames=list(1:num_repetitions, 1:2, inflowSet));
dcv <- array(data=NA, dim=c(num_repetitions, 2)
             , dimnames=list(1:num_repetitions, 1:2));
neigh <- array(data=NA, dim=c(num_repetitions, 2)
             , dimnames=list(1:num_repetitions, 1:2));
candSize <- array(data=NA, dim=c(num_repetitions, 2)
                  , dimnames=list(1:num_repetitions, 1:2));

# compute metrics
for(repetition in 1:num_repetitions) {
  loginfo("*** Starting iteration %s / %s ***", repetition, num_repetitions)
  
  ### load graphs
  g <- loadGraph("./graphs/infectious/graphs/infectious.gml", type="gml");
  rp10 <- loadGraph(paste("./graphs/infectious/graphs/Add/infectious-", repetition, "-10.gml", sep=""), type="gml");
  graphs <- list(g, rp10);
  
  ### Utility measures
  
  # compute GIL metrics
  gil[repetition, , ] <- computeGIL(graphs, datasetName, repetition);
  
  # compute SIL-clustering communities
  createClusters(graphs, datasetName, repetition);
  p[repetition, , ] <- computePrecision(graphs, datasetName, repetition);
  
  # compute SIL-RRTI communities
  createRRTI(graphs, datasetName, repetition);
  rrti[repetition, , ] <- computeRRTI(graphs, datasetName, repetition);
  
  # compute SIL-InFlow communities
  createInFlow(graphs, datasetName, repetition);
  inflow[repetition, , ] <- computeInFlow(graphs, datasetName, repetition);
  
  ### Privacy measures
  
  # degree changed vertices
  dcv[repetition, ] <- computeDegreeChangedVertices(graphs, datasetName);
  
  # 1-neighbourhood changed
  neigh[repetition, ] <- computeNeighChangesVertices(graphs, datasetName);
}
  
# print GIL
for(m in metricSet) {
  loginfo("Metric %s : Original value = %0.4f and perturbed = %0.4f (CI +- %0.4f)", m, gil[1, 1, m], mean(gil[, 2, m]), getCI(gil[, 2, m]))
}

# print Clustering
for(m in clusteringSet) {
  loginfo("Algorithm %s : Original value = %0.4f and perturbed = %0.4f (CI +- %0.4f)", m, p[1, 1, m], mean(p[, 2, m]), getCI(p[, 2, m]))
}

# print RRTI
for(m in rrtiSet) {
  loginfo("Algorithm %s : Original value = %0.4f and perturbed = %0.4f (CI +- %0.4f)", m, rrti[1, 1, m], mean(rrti[, 2, m]), getCI(rrti[, 2, m]))
}

# print inflow
for(m in inflowSet) {
  loginfo("Algorithm %s : Original value = %0.4f and perturbed = %0.4f (CI +- %0.4f)", m, inflow[1, 1, m], mean(inflow[, 2, m]), getCI(inflow[, 2, m]))
}

# print DCV
loginfo("DCV : Original value = %0.4f and perturbed = %0.4f (CI +- %0.4f)", dcv[1, 1], mean(dcv[, 2]), getCI(dcv[, 2]))

# print DCV
loginfo("1-Neigh : Original value = %0.4f and perturbed = %0.4f (CI +- %0.4f)", neigh[1, 1], mean(neigh[, 2]), getCI(neigh[, 2]))

# compute and plot the candidate set size
candSize <- computeH1Analysis(graphs, datasetName, add=TRUE, del=FALSE);
