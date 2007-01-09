
# assignment problem 
#
# see Marketing Models pp. 404 Ex83
#
# ceeboo 2005
# modified by theussl 2007-01


solveAP <- function(x){
  if(!require(clue)) stop("Package CLUE not available")
  out <- list()
  assignment <- solve_LSAP(x,maximum=TRUE)
  out$maximum <- sum(diag(x[,assignment]))
  out$matrix <- x
  out$assignment <- as.vector(assignment)
  class(out) <- "solveAP"
  out
}

print.solveAP <- function(x){
  if(!inherits(x,"solveAP")) stop("class of 'x' is not 'solveAP'")
  cn <- colnames(x$matrix)
  rn <- rownames(x$matrix)
  if(is.null(cn)) cn <- c(1:ncol(x$matrix))
  if(is.null(rn)) rn <- c(1:nrow(x$matrix))
  writeLines(paste("Optimal Assignment with maximum of",x$maximum,":"))
  writeLines(paste(rn, cn[x$assignment], sep = " => ", collapse = ", "))
  invisible(x)
}

###
