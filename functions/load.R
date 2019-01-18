###########################
# Load and clean datasets #
###########################

#' Load and simplify dataset
#' 
#' @param path Path.
#' @param type File type ("edgelist", "pajek", "ncol", "lgl", "graphml", "dimacs", "graphdb", "gml", "dl").
#' @return A graph object.
loadGraph <- function(path, type) {
  g <- read.graph(path, format=type);
  g <- simplify(g, remove.loops=TRUE, remove.multiple=TRUE);
  loginfo("Network '%s' loaded!", path);
  summary(g);
  
  return(g);
}

#' Load, convert to UG, simplify and remove isolated vertices from source dataset
#' 
#' @param path Path.
#' @param type File type ("edgelist", "pajek", "ncol", "lgl", "graphml", "dimacs", "graphdb", "gml", "dl").
#' @return A graph object.
loadAndCorrectGraph <- function(path, type) {
  # load
  g <- read.graph(path, format=type);
  show("*** Original dataset:");
  summary(g);
  
  # to undirected
  g <- as.undirected(g);
  # simplify
  g <- simplify(g, remove.loops=TRUE, remove.multiple=TRUE);
  # remove isolated nodes
  g <- delete.vertices(g, which(degree(g) < 1));
  
  show("*** Cleaned dataset:");
  summary(g);
  
  return(g);
}
