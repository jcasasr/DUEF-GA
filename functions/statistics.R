# get X-th percent of confidence interval (CI)
getCI <- function(x, percen=0.95) {
  if(percen==0.95) {
    ct <- 1.96;
  } else {
    logerror("Error, %s is not yet implemented!", percen);
    stop();
  }
  
  # error
  err <- ct * sd(x) / sqrt(length(x));
  
  return(err);
}