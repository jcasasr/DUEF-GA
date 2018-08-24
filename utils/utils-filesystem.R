#
createPath <- function(path) {
  parts <- strsplit(path, "/")[[1]];
  
  # check subdir
  for(i in 1:length(parts)) {
    # select string 1:i
    f <- paste(parts[1:i], collapse="/");
    # check if it exists
    if(!file.exists(f)) {
      # if not, create
      logdebug("creating path: %s", f);
      dir.create(f, showWarnings=FALSE);
    }
  }
}