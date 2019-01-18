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
# utils
source('utils/utils.R');
source('utils/utils-filesystem.R');
source('utils/utils-matrix.R');
source('utils/utils-statistics.R');
# log 
source('log/log.R');
# plot
source('plot/plot.R');

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

##################
# Paths and URLs #
##################

# define subdirs to store results and temporal files
subdirRes <- "./results/";
subdirTmp <- "./tmp/";
subdirGIL <- paste(subdirTmp, "GIL/", sep="");
subdirClust <- paste(subdirTmp, "Clust/", sep="");
subdirRRTI <- paste(subdirTmp, "RRTI/", sep="");
subdirInFlow <- paste(subdirTmp, "InFlow/", sep="");
