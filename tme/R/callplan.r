
# callplan example
#
# ceeboo 2005

# evaluation function for the simplified callplan 
# model (compare the ME tutorial)
#
# note that the resource constraint is 8 hour days
# and that the decision variable is the number of 
# calls (integer)


## FIXME: eval callplan takes a callplan as an object
eval.callplan <- function(x, r, e,h,m,s,p, details=FALSE) {
    x <- x / e
    k <- if (is.null(k <- names(x))) seq(x) else k
    y <- sapply(k, function(k)
        response(as.vector(x[k]),unlist(p[[k]]),model="adbudg")) ## FIXME: other response models??
    p <- sum(y * m * s)
    z <- r - sum(x * e * h) / 8
    if (z < 0)                  # constraint violated ?
       p <- z / p
    if (details) {
       if (z < 0)
          warning("infeasible solution")
       list(profit=p,slack=z,effort=x*e,sales=y*s)
    }
    else
       p
}

# work in progress

as.callplan <- function(x){
  UseMethod("as.callplan")
}

as.callplan.data.frame <- function(x){
  if(!inherits(x,"data.frame")) stop("'x' is not of class 'data.frame'")
  out <- as.list(x[1:4])
  names(out) <- c("e","h","s","m")
  rn <- rownames(CPEX)
  if(is.null(rn)) rn <- LETTERS[1:length(out$e)]
  names(out$e) <- rn
  out$p <- .adbudg.calibrate.data.frame(x[5:8])
  class(out) <- "callplan"
  out
}

## callplan methods

is.callplan <- function(x){
  if(!inherits(x,"callplan")) FALSE
  else TRUE
}

## FIXME: better print method
print.callplan <- function(x, ...){
  if(!inherits(x,"callplan")) stop("'x' must be of class 'callplan'")
  nr <- length(x$e)       ## THIS IS NOT A GOOD WAY
  writeLines(paste("A callplan for ",nr,"terretories"))
  invisible(x)
}

# this is really braindead!

.adbudg.calibrate <- function(yN,yH,yD,yS,w=1/3) {
    d  <- (yS-1)/(1-yN)
    c  <-    w  * log(d*(yH-yN)/(yS-yH))/log(0.5) +
          (1-w) * log(d*(yD-yN)/(yS-yD))/log(1.5)
    list(a=yS,b=yN,c=c,d=d)
}

.adbudg.calibrate.data.frame <- function(x) {
    if (!is.data.frame(x))
       stop("'x' not a data frame")
    n <- dim(x)[1]
    z <- vector("list",n)
    for (i in seq(n)) {
        zz <- as.list(x[i,])
        names(zz) <- NULL
        z[[i]] <- do.call(.adbudg.calibrate,zz)
    }
    names(z) <- rownames(x)
    z
}

