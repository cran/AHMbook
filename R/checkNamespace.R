checkNamespace <- function(pkg){
  if(!requireNamespace(pkg, quietly = TRUE)){
    stop(paste("Install", pkg, "package to use this function", call.=FALSE))
  }
  TRUE
}
