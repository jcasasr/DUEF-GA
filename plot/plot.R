plotSeries <- function(xs, ys, legendNames=NULL, legendPosition="topright", xlab=NULL, ylab=NULL) {
  # params
  series <- length(ys);
  yrange <- c(min(unlist(ys)), max(unlist(ys)));
  
  # colors and lines
  colors <- rainbow(series);
  lty <- 1:series; # line type
  lwd <- 3; # line width
  
  # reduce margins to improve visibility
  par(mar=c(4, 4, 1, 0));
  
  # plot lines
  for(i in 1:series) {
    if(i==1) {
      plot(x=xs, y=ys[[i]], type="l", col=colors[i], lty=lty[i], lwd=lwd, ylim=yrange, xlab=xlab, ylab=ylab, cex.lab=1.2);
    } else {
      lines(x=xs, y=ys[[i]], type="l", col=colors[i], lty=lty[i], lwd=lwd);
    }
  }
  
  # plot legend
  legend(x=legendPosition, legend=legendNames, ncol=1, bty="n", col=colors, lty=lty, lwd=lwd);
}
