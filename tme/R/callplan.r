
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

### library(cbmisc) copy genetic algorithm

### source("optmisc.r") implement adbudg


# assemble/calibrate the callplan model

# TODO create a callplan class
# example for vignette
#cpm <- as.list(CPEX[1:4])
#names(cpm) <- c("e","h","s","m")
#names(cpm$e) <- rownames(CPEX)

#cpm$p <- adbudg.calibrate.data.frame(CPEX[5:8])

# evaluate current plan

#x <- do.call(eval.callplan,c(list(cpm$e), r=2, cpm, 
             details=TRUE))
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
