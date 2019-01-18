findValueInMatrix <- function(M, value) {
  found <- FALSE;
  
  while(!found) {
    i <- sample(1:nrow(M), 1);
    j <- sample(1:ncol(M), 1);
    
    if(M[i,j]==value & i!=j) {
      found <- TRUE;
    }
  }
  
  return(c(i,j));
}

# edges which are in the original graph but NOT in the modified one
getEdgeDifferenceNum <- function(A1, A2) {
  # verification
  if((nrow(A1) != nrow(A2)) | (ncol(A1) != ncol(A2))) {
    logerror("getEdgeDifferenceNum:: A1 and A2 do not have the same number of rows and/or cols!");
    stop();
  }
  
  # compute ED
  ed <- sum(A1>0 & A2==0);
  
  # each edge is counted twice
  ed <- round(ed/2);
  
  return(ed);
}
