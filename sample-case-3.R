### load libraries, sources and configuration options
source('config.R');

### vars
datasetName1 <- "sample-case-3_1";
datasetName2 <- "sample-case-3_2";
metricSet <- c("AD", "EI", "D", "BC", "CC", "DC");
metricSet_labels <- c("Average distance", "Edge intersection", "Diameter", "Betweenness centrality", "Closeness centrality", "Degree centrality");
num_repetitions <- 10;
p_anon <- 10;
xs <- c(0:p_anon);

# create empty vars
gil1 <- array(data=NA, dim=c(num_repetitions, length(xs), length(metricSet))
             , dimnames=list(1:num_repetitions, as.character(xs), metricSet));
gil2 <- gil1;
p1 <- array(data=NA, dim=c(num_repetitions, length(xs), length(clusteringSet))
           , dimnames=list(1:num_repetitions, as.character(xs), clusteringSet));
p2 <- p1;
rrti1 <- array(data=NA, dim=c(num_repetitions, length(xs), length(rrtiSet))
              , dimnames=list(1:num_repetitions, as.character(xs), rrtiSet));
rrti2 <- rrti1;
inflow1 <- array(data=NA, dim=c(num_repetitions, length(xs), length(inflowSet))
                , dimnames=list(1:num_repetitions, as.character(xs), inflowSet));
inflow2 <- inflow1;
dcv1 <- array(data=NA, dim=c(num_repetitions, length(xs))
                 , dimnames=list(1:num_repetitions, as.character(xs)));
dcv2 <- dcv1;
neigh1 <- array(data=NA, dim=c(num_repetitions, length(xs))
              , dimnames=list(1:num_repetitions, as.character(xs)));
neigh2 <- neigh1;

# compute metrics
for(repetition in 1:num_repetitions) {
  loginfo("*** Starting iteration %s / %s ***", repetition, num_repetitions)
  
  ### load graphs : sequence #1
  g <- loadGraph(paste("./graphs/hamsterster/graphs/Add/hamsterster-", repetition, "-0.gml", sep=""), type="gml");
  graphs1 <- list(g);
  for(i in 1:p_anon) {
    g1 <- loadGraph(paste("./graphs/hamsterster/graphs/Add/hamsterster-", repetition, "-", i, ".gml", sep=""), type="gml");
    graphs1 <- c(graphs1, list(g1));
  }
  
  ### load graphs : sequence #1
  g <- loadGraph(paste("./graphs/hamsterster/graphs/Del/hamsterster-", repetition, "-0.gml", sep=""), type="gml");
  graphs2 <- list(g);
  for(i in 1:p_anon) {
    g2 <- loadGraph(paste("./graphs/hamsterster/graphs/Del/hamsterster-", repetition, "-", i, ".gml", sep=""), type="gml");
    graphs2 <- c(graphs2, list(g2));
  }
  
  ### Computations
  
  ### Utility measures
  
  # compute GIL metrics
  gil1[repetition, , ] <- computeGIL(graphs1, datasetName1, repetition);
  gil2[repetition, , ] <- computeGIL(graphs2, datasetName2, repetition);
  
  # compute SIL-clustering communities
  createClusters(graphs1, datasetName1, repetition);
  p1[repetition, , ] <- computePrecision(graphs1, datasetName1, repetition);
  
  createClusters(graphs2, datasetName2, repetition);
  p2[repetition, , ] <- computePrecision(graphs2, datasetName2, repetition);
  
  # compute SIL-RRTI communities
  createRRTI(graphs1, datasetName1, repetition);
  rrti1[repetition, , ] <- computeRRTI(graphs1, datasetName1, repetition);
  
  createRRTI(graphs2, datasetName2, repetition);
  rrti2[repetition, , ] <- computeRRTI(graphs2, datasetName2, repetition);
  
  # compute SIL-InFlow communities
  createInFlow(graphs1, datasetName1, repetition);
  inflow1[repetition, , ] <- computeInFlow(graphs1, datasetName1, repetition);
  
  createInFlow(graphs2, datasetName2, repetition);
  inflow2[repetition, , ] <- computeInFlow(graphs2, datasetName2, repetition);
  
  ### Privacy measures
  
  # compute DCV
  dcv1[repetition, ] <- computeDegreeChangedVertices(graphs1, datasetName1);
  dcv2[repetition, ] <- computeDegreeChangedVertices(graphs2, datasetName2);
  
  # compute 1-Neigh
  neigh1[repetition, ] <- computeNeighChangesVertices(graphs1, datasetName1);
  neigh2[repetition, ] <- computeNeighChangesVertices(graphs2, datasetName2);
}

### plots and visualization

# GIL
for(i in 1:length(metricSet)) {
  m <- metricSet[i]
  m_label <- metricSet_labels[i]
  # mean and SD/CI for each set
  v_1 <- colMeans(gil1[, , m]);
  e_1 <- apply(gil1[, , m], 2, getCI);
  v_2 <- colMeans(gil2[, , m]);
  e_2 <- apply(gil2[, , m], 2, getCI);
  plotSeriesRand(xs, list(v_1, v_2), list(e_1, e_2), legendNames=c("Add", "Del"), legendPosition="topright", xlab="Anonymization (%)", ylab=m_label, filename=paste("s3-", m, sep=""));
}

# Precision
for(m in clusteringSet) {
  # mean and SD/CI for each set
  v_1 <- colMeans(p1[, , m]);
  e_1 <- apply(p1[, , m], 2, getCI);
  v_2 <- colMeans(p2[, , m]);
  e_2 <- apply(p2[, , m], 2, getCI);
  plotSeriesRand(xs, list(v_1, v_2), list(e_1, e_2), legendNames=c("Add", "Del"), legendPosition="topright", xlab="Anonymization (%)", ylab=paste("Precision - ", m, sep=""), filename=paste("s3-", m, sep=""));
}

# RRTI
for(m in rrtiSet) {
  # mean and SD/CI for each set
  v_1 <- colMeans(rrti1[, , m]);
  e_1 <- apply(rrti1[, , m], 2, getCI);
  v_2 <- colMeans(rrti2[, , m]);
  e_2 <- apply(rrti2[, , m], 2, getCI);
  plotSeriesRand(xs, list(v_1, v_2), list(e_1, e_2), legendNames=c("Add", "Del"), legendPosition="topright", xlab="Anonymization (%)", ylab="RRTI", filename="s3-RRTI");
}

# InFlow
for(m in inflowSet) {
  # mean and SD/CI for each set
  v_1 <- colMeans(inflow1[, , m]);
  e_1 <- apply(inflow1[, , m], 2, getCI);
  v_2 <- colMeans(inflow2[, , m]);
  e_2 <- apply(inflow2[, , m], 2, getCI);
  plotSeriesRand(xs, list(v_1, v_2), list(e_1, e_2), legendNames=c("Add", "Del"), legendPosition="topleft", xlab="Anonymization (%)", ylab="FRV", filename="s3-FRV");
}

# DCV
# mean and SD/CI for each set
dcv1 <- dcv1 / vcount(g);
dcv2 <- dcv2 / vcount(g);

v_1 <- colMeans(dcv1[, ]);
e_1 <- apply(dcv1[, ], 2, getCI);
v_2 <- colMeans(dcv2[, ]);
e_2 <- apply(dcv2[, ], 2, getCI);
plotSeriesRand(xs, list(v_1, v_2), list(e_1, e_2), legendNames=c("Add", "Del"), legendPosition="topleft", xlab="Anonymization (%)", ylab="DCV", filename="s3-DCV");

# 1-Neigh
# mean and SD/CI for each set
neigh1 <- neigh1 / vcount(g);
neigh2 <- neigh2 / vcount(g);

v_1 <- colMeans(neigh1[, ]);
e_1 <- apply(neigh1[, ], 2, getCI);
v_2 <- colMeans(neigh2[, ]);
e_2 <- apply(neigh2[, ], 2, getCI);
plotSeriesRand(xs, list(v_1, v_2), list(e_1, e_2), legendNames=c("Add", "Del"), legendPosition="topleft", xlab="Anonymization (%)", ylab="1-Neighbourhood", filename="s3-Neigh");

# compute and plot the candidate set size
candSize1 <- computeH1Analysis(graphs1, datasetName1, add=TRUE, del=TRUE);
candSize2 <- computeH1Analysis(graphs2, datasetName2, add=TRUE, del=TRUE);
