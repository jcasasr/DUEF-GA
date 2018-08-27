#' Plot series of values
#' 
#' @param xs Array of x's values.
#' @param ys List of array's values (each array has to be the same length as xs).
#' @param legendNames Legend names (list).
#' @param legendPosition Legend position (default: topright).
#' @param xlab String.
#' @param ylab String.
#' @param filename Valid path to save figure as a EPS file. If NULL, do not save the figure, just plot it! 
#' @return Array with metrics' score values.
plotSeries <- function(xs, ys, legendNames=NULL, legendPosition="topright", xlab=NULL, ylab=NULL, filename=NULL) {
  # params
  series <- length(ys);
  yrange <- c(min(unlist(ys)), max(unlist(ys)));
  
  # colors and lines
  colors <- rainbow(series);
  colors <- c("red", "blue")
  lty <- 1:series; # line type
  lwd <- 3; # line width
  pch <- 1:series; # point type
  step <- as.integer(length(xs) / 5);
  
  if(!is.null(filename)) {
    # write plot to file
    setEPS();
    postscript(paste(filename, ".eps", sep=""), width=8.0, height=8.0, pointsize=24);
  }
  
  # reduce margins to improve visibility
  par(mar=c(4, 4, 1, 0));
  
  # plot lines
  for(i in 1:series) {
    if(i==1) {
      plot(x=xs, y=ys[[i]], type="l", col=colors[i], lty=lty[i], lwd=lwd, ylim=yrange, xlab=xlab, ylab=ylab, cex.lab=1.2);
    } else {
      lines(x=xs, y=ys[[i]], type="l", col=colors[i], lty=lty[i], lwd=lwd);
    }
    sel <- seq(1, length(ys[[i]]), by=step);
    lines(x=xs[sel], y=ys[[i]][sel], type="p", col=colors[i], lty=lty[i], lwd=lwd, pch=pch[i], cex=1.0);
  }
  
  # plot legend
  legend(x=legendPosition, legend=legendNames, ncol=1, bty="n", col=colors, lty=lty, lwd=lwd, pch=pch);
  
  if(!is.null(filename)) {
    dev.off();
  }
}
