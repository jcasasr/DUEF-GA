# compute the probability density distribution
# @type: equal, 1overX, 1overX2
getProbabilityDistribution <- function(values, type) {
  if(type=="equal") {
    p <- rep(1/length(values), times=length(values));
  } else if(type=="1overX") {
    p <- 1 / values;
  } else if(type=="1overX2") {
    p <- 1 / (values^2);
  } else {
    logerror("getProbabilityDistribution:: Unknown function '%s'", type);
  }
  # remove 'Inf' values
  p[!is.finite(p)] <- NA;
  p[is.na(p)] <- max(p, na.rm=TRUE);
  
  # normalize: sum(p)=1
  p <- p / sum(p); 
  
  return(p);
}