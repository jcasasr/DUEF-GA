# check libraries
libs <- c("igraph");

for(lib in libs) {
  if(!(lib %in% rownames(installed.packages()))) {
    install.packages(lib);
  }
}

# load "config.R" to get path definitions
source('config.R');

# create all directories (defined in config.R)
createPath(subdirRes);
createPath(subdirTmp);
createPath(subdirGIL);
createPath(subdirClust);
createPath(subdirRRTI);
createPath(subdirInFlow);
