getIndexOfMaxValue <- function(values) {
  maxValue <- max(values);
  ind <- which(values==maxValue);
  
  if(length(ind)>1) {
    return(ind[1]);
  } else {
    return(ind);
  }
}

getIndexOfMinValue <- function(values) {
  minValue <- min(values);
  ind <- which(values==minValue);
  
  if(length(ind)>1) {
    return(ind[1]);
  } else {
    return(ind);
  }
}

getIndexOfMinValueGreaterThanZero <- function(values) {
  minValue <- min(values[values>0]);
  ind <- which(values==minValue);
  
  if(length(ind)>1) {
    return(ind[1]);
  } else {
    return(ind);
  }
}

# Compute the RMS of 2 vectors
computeDiff <- function(v1, v2) {
  n1 <- length(v1);
  n2 <- length(v2);
  
  if(n1 != n2) {
    logerror("getDifferenceValue:: Vectors of difference length %s != %s", n1, n2);
    stop();
  }
  
  return(sqrt(sum((v1 - v2)^2) / n1));
}

# DEPRECATED
# TODO: duplicate function. Delete
getDifferenceValue <- function(v1, v2) {
  return(computeDiff(v1, v2));
}

# compute the correlation between two sets
#
getCorr <- function(data1, data2) {
  # available methods: pearson | kendall | spearman
  method <- "pearson";
  corr <- cor(data1, data2, use="pairwise.complete.obs", method=method);
  
  return(corr);
}

# Compute the total error
#
getErrorTotal <- function(values) {
  return(sum(abs(values-values[1])));
}

# Compute the mean error
#
getAverageError <- function(values, numDec=5) {
  return(round(mean(abs(values-values[1])), numDec));
}