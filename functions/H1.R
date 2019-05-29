#' Compute Cand_H1 analysis on a graph's list
#' 
#' @param graphs List of graphs (two or more).
#' @param datasetName Name of the dataset (to store temporal values).
#' @return Array with values.
computeH1Analysis <- function(graphs, datasetName, add=TRUE, del=TRUE) {
  # define anonSet
  anonSet <- array(data=NA, dim=c(length(graphs))
                   , dimnames=list(1:(length(graphs))));
  
  # original graph
  g0 <- graphs[[1]];
  
  # Compute metrics on each graph
  for(i in 1:length(graphs)) {
    g <- graphs[[i]];
    
    # compute edge intersection
    anonSet[i] <- (1 - edgeIntersection(g0, g)) * 100;
  }
  
  loginfo("AnonSet is %s", anonSet);
  
  # compute candidate set size on H1
  candSize <- computeH1Analysis_tmp(g0, anonSet, add, del);
  
  # plot
  plotH1(candSize, anonSet, g0, datasetName);
  
  return(candSize);
}

######
# Return the candidate set size
computeH1Analysis_tmp <- function(g0, anonSet, add=TRUE, del=TRUE) {
  V <- V(g0);
  deg <- degree(g0, mode="all");
  m <- ecount(g0);
  n <- vcount(g0);
  
  loginfo("+++ Computing H1 analysis (add=%s and del=%s)...", add, del);

  # number of ED in every step
  anonNumEdgesSet <- round(anonSet * ecount(g0) / 100);

  # empty variable
  cand <- array(data=NA, dim=c(length(anonSet), vcount(g0)));
  
  # edges and non-existent edges
  # |E| = m
  numEdges <- m;
  # |E^c| = (n * (n-1) / 2) - m
  NumNonExistentEdges <- ((n * (n-1) / 2) - m);

  for(a in 1:length(anonSet)) {
    w <- anonNumEdgesSet[a];
    
    loginfo("*** Anonymization percentage = %s [%s of %s]", w, a, length(anonSet));
    
    for(x in V) {
      logdebug("Processing vertex %d [deg=%d] with w=%d", x, deg[x], w);
      
      qtt <- 0;
      for(y in V) {
        # CAS PROMIG
        # grau actual - nombre arestes que poden eliminar * prob. eliminar cada aresta
        if(del) {
          degmin <- round(deg[y] - (deg[y] * (w / numEdges)));
        } else {
          degmin <- deg[y];
        }
        # grau actual + nombre de possibles arestes (totes les no existents) * prob. crear cada aresta
        if(add) {
          degmax <- round(deg[y] + ((n - 1 - deg[y]) * (w / NumNonExistentEdges)));
        } else {
          degmax <- deg[y];
        }
        
        if((degmin <= deg[x]) & (deg[x]) <= degmax) {
          logdebug("Vertex %d [deg=%d (min=%d -- max=%d) edges] should be a candidate in G_%d", y, deg[y], degmin, degmax, a);
          qtt <- qtt + 1;
        }
      }
      cand[a, x] <- qtt;
      logdebug("Cand_H1(%d) = %d", x, qtt);
    }
  }

  logdebug("Computing the candidate group for each vertex...");
  
  candGroup <- array(data=NA, dim=c(length(anonSet), vcount(g0)));
  
  for(a in 1:nrow(cand)) {
    for(x in 1:ncol(cand)) {
      candGroup[a, x] <- getVal(cand[a, x]);
    }
  }

  ######
  # compute the size of candidate set
  logdebug("Computing the size of the candidate set...");
  
  candSize <- array(data=0, dim=c(length(anonSet), 5));
  
  for(a in 1:nrow(cand)) {
    for(v in 1:5) {
      candSize[a, v] <- sum(candGroup[a, ]==v);
    }
  }
  
  return(candSize);
}

######
# compute the candidate set size
getVal <- function(val) {
  if(val == 1) {
    return(1);
  } else if(2 <= val & val <= 4) {
    return(2);
  } else if(5 <= val & val <= 10) {
    return(3);
  } else if(11 <= val & val <= 20) {
    return(4);
  } else if(21 <= val) {
    return(5);
  }
}

########
# Plot #
########
plotH1 <- function(candSize, anonSet, g0, datasetName) {
  # normalize [0,1]
  candSize <- candSize / vcount(g0);
  
  # output filename
  filename <- paste("./plots/", datasetName, "-H1", sep="");
  
  #############
  # PLOT
  colors <- c("gray20", "gray40", "gray60", "gray80", "white");
  lty <- c(1, 2, 3, 4, 5);
  lwd <- 3;
  ymin <- 0;
  ymax <- 1;
  
  #x <- 0:(length(anonSet)-1);
  x <- anonSet
  
  # EPS
  setEPS();
  postscript(paste(filename, ".eps", sep=""), width=8.0, height=8.0, pointsize=24);
  par(mar=c(4, 4, 1, 0));
  plot(x=x, y=candSize[, 1], type="l", lty=lty[1], lwd=lwd, col=colors[1], ylim=c(ymin, ymax), xlab="Edge perturbation (%)", ylab="Vertex proportion", cex.lab=1.2);
  xx <- c(x, rev(x));
  yy <- c(rep(0, nrow(candSize)), rev(candSize[, 1]));
  polygon(x=xx, y=yy, col=colors[1], border=NA);
  for(i in 2:5) {
    lines(x=0:(length(anonSet)-1) , y=rowSums(candSize[, 1:i]), type="l", lty=lty[i], lwd=lwd, col=colors[i]);
    
    xx <- c(x, rev(x));
    if(i > 2) {
      yy <- c(rowSums(candSize[, 1:(i-1)]), rev(rowSums(candSize[, 1:i])));
    } else {
      yy <- c(candSize[, 1], rev(rowSums(candSize[, 1:i])));
    }
    polygon(x=xx, y=yy, col=colors[i], border=NA);
  }
  dev.off();
  
  loginfo("Plots saved at '%s'", filename);
}

