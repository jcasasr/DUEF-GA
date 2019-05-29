# libraries
library(igraph);

# sources
source('functions/load.R');
source('functions/computeGIL.R');
source('functions/metrics.R');
source('functions/computeClustering.R');
source('functions/precision.R');
source('functions/computeRRTI.R');
source('functions/computeInFlow.R');
source('functions/statistics.R');
source('functions/degreeChangedVertices.R');
source('functions/neighChangedVertices.R');
source('functions/H1.R');
# utils
source('utils/utils.R');
source('utils/utils-filesystem.R');
source('utils/utils-matrix.R');
# log 
source('utils/log.R');
# plot
source('functions/plot.R');

##################
# Params         #
##################

# GIL
# Set: "AD", "h", "T", "SC", "EI", "D", "Core", "BC", "CC", "DC", "lambda1", "mu2"
metricSet <- c("AD", "EI", "D", "BC", "CC", "DC");

# SIL clustering (community detection algorithms)
# Set: "infomap", "multilevel", "fastgreedy", "walktrap"
clusteringSet <- c("infomap", "multilevel", "fastgreedy", "walktrap");
useDimensionalityReduction <- TRUE;
maxDim <- 3000;

# SIL RRTI
rrtiSet <- c("PageRank");

# SIL InFlow
inflowSet <- c("FRV");

##################
# Paths and URLs #
##################

# define subdirs to store results and temporal files
subdirRes <- "./results/";
createPath(subdirRes)
subdirTmp <- "./tmp/";
createPath(subdirTmp)
subdirGIL <- paste(subdirTmp, "GIL/", sep="");
createPath(subdirGIL)
subdirClust <- paste(subdirTmp, "Clust/", sep="");
createPath(subdirClust)
subdirRRTI <- paste(subdirTmp, "RRTI/", sep="");
createPath(subdirRRTI)
subdirInFlow <- paste(subdirTmp, "InFlow/", sep="");
createPath(subdirInFlow)
