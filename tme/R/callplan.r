
# callplan example
#
# ceeboo 2005

# evaluation function for the simplified callplan 
# model (compare the ME tutorial)
#
# note that the resource constraint is 8 hour days
# and that the decision variable is the number of 
# calls (integer)

eval.callplan <- function(x, r, e,h,m,s,p, details=FALSE) {
    x <- x / e
    k <- if (is.null(k <- names(x))) seq(x) else k
    y <- sapply(k, function(k)
        do.call(adbudg, c(as.vector(x[k]),p[[k]])))
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
  UseMethod(".as.callplan")
}

.as.callplan.data.frame <- function(x){
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
  TRUE
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



# evaluate current plan

#x <- do.call(eval.callplan,c(list(cpm$e), r=2, cpm, 
#             details=TRUE))
#print(x)

#gcp <- gom(eval.callplan, evalArgs=c(r=2, cpm), evalVector=TRUE, 
#           bounds=data.frame(Lower=rep(0,length(cpm$h)),
#                             Upper=rep(8,length(cpm$h)),
#                             row.names=names(cpm$p)),
#           numVar=1:2, intVar=1:2, trace=TRUE)

#xx <- do.call(eval.callplan,c(list(gcp$bestVar), r=2, cpm, details=TRUE))
#print(xx)

#op <- par(mfrow = c(1,2), pty="s")
#barplot(cpm$e, ylab="Calls", 
#        main=paste("Current:",formatC(x$profit,digits=3)))
#barplot(gcp$bestVar, ylab="Calls", 
#        main=paste("Recommended:",formatC(gcp$bestFit,digits=3)))
#par(op)

## printmethods plotmethods!!

###
