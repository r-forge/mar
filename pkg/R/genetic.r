#
# goMixed - Mixed Type Genetic Optimizer
#
# C.R. Houck, J.A. Joines, and M.G. Kay. (1995) A Genetic Algorithm
# for Function Optimization: A Matlab Implementation.
#
# fixme: add interfaces for typical optimization problems
#
# (C) ceeboo 2003, 2005

gom <- function(evalFun, evalArgs=NULL, evalVector=FALSE, 
                bounds, numVar, intVar=NULL, 
                popSize=100, maxGen=100, xoverOps, mutationOps, 
                selectionOp, verbose=TRUE, trace=FALSE) {

### crossover functions #######################################################

    nomXover <- function(i1, i2) {          # swap nominal variables only
        c2 <- popVar[i1,]
        c1 <- popVar[i2,]
        c1[numVar] <- popVar[i1,numVar]
        c2[numVar] <- popVar[i2,numVar]
        c <- rbind(c1,c2)
        c
    }

    simpleXover <- function(i1, i2) {
        c <- 1:sample(nVars, 1)             # select cutpoint
        c <- rbind(c(popVar[i1, c], 
                     popVar[i2,-c]),        # create children
                   c(popVar[i2, c],
                     popVar[i1,-c]))
        c
    }

    arithXover <- function(i1, i2) {
	    w <- runif(1)				        # select mix
        c1 <- popVar[i1,]
        c2 <- popVar[i2,]
        c1[numVar] <- c1[numVar] *    w +
                      c2[numVar] * (1-w)
        c2[numVar] <- c2[numVar] *    w +
                      c1[numVar] * (1-w)
        c <- rbind(c1,c2)
        c[,intVar] <- round(c[,intVar])
        c
    }

    heuristicXover <- function(i1, i2, retry=3) {
	    if (popFit[i1] > popFit[i2]) {
           c1 <- popVar[i1,]
           c2 <- popVar[i2,]
        }
        else {
           c1 <- popVar[i2,]
           c2 <- popVar[i1,]
        }
	    for (i in 1:retry) {
            w <- runif(1)
            z <- c1[numVar] * (1+w) - c2[numVar] * w
            if (all(bounds[numVar,1] <= z) && all(z <= bounds[numVar,2])) {
               c2[numVar] <- z
               c2[intVar] <- round(c2[intVar])
               break
            }
        }
        c <- rbind(c1,c2)
        c
    }

### mutation functions ########################################################

    uniformMutation <- function(c) {
        w <- runif(1)				        # select mix
        p <- sample(nVars, 1)		        # select variable
        c[p] <- bounds[p,1] * (1-w) + 
                bounds[p,2] *    w
        c[intVar] <- round(c[intVar])
        c
    }

    delta <- function(n, shape=3)			# change function
        runif(n)*(1-gen/maxGen)^shape

    nonUniformMutation <- function(c, ...) {
        p <- sample(numVar, 1)	            # select variable
        w <- delta(1, ...)				    # change
        c[p] <- c[p] * (1-w) + 
                bounds[p, sample(2,1)] * w	# select bound
        c[intVar] <- round(c[intVar])
        c
    }

    multiNonUniformMutation <- function(c, ...) {
        w <- delta(length(numVar), ...)
        p <- sample(2, length(numVar), rep=TRUE)
        c[numVar] <- c[numVar] * (1-w) + 
            (bounds[numVar,])[matrix(c(seq(numVar),p), ncol=2)] * w
        c[intVar] <- round(c[intVar])
        c
    }

    boundaryMutation <- function(c) {
        p <- sample(numVar, 1)		        # select variable
        c[p] <- bounds[p, sample(2, 1)]	    # select bound
        c
    }

    binaryMutation <- function(c, p=0.005) {	    # flip bits
        c <- xor(c, rbinom(nVars, 1, p))
        c
    }

### selection functions #######################################################

    normGeomSelect <- function(p=0.08) {
        p <- (p/(1-(1-p)^popSize) * 
                   (1-p)^((popSize-1):0))		# probability of selection
        s <- sample(order(popFit), 
                    size=popSize,
                    replace=TRUE,
                    prob=p)
        s
    }

    rouletteSelect <- function() {
        p <- popFit / sum(popFit)
        s <- sample(popSize, 
                    size=popSize, 
                    replace=TRUE,
                    prob=p)
        s
    }

    tournSelect <- function(n=2) {
        p <- sample(popSize, 
                    size=popSize*n, 
                    replace=TRUE)
        s <- max.col(matrix(popFit[p], ncol=n))
        s <- matrix(p, ncol=n)[matrix(c(1:popSize, s), ncol=2)]
        s
    }
    
### utilities #################################################################

    setEnv <- function(ops, n=sys.parent()) {
        for (op in ops)
            if (is.function(op[[1]]))
               environment(op[[1]]) <- sys.frame(n)
        ops
    }

    evalFit <- function(x)
        if (evalVector)
           do.call(evalFun,c(list(x),evalArgs))
        else
	       do.call(evalFun,c(as.list(x),evalArgs))
    
### defaults ##################################################################

    if (missing(xoverOps))
       xoverOps <- list(list("arithXover",    popSize * 0.02),
                        list("heuristicXover",popSize * 0.02),
                        list("simpleXover",   popSize * 0.02))
    else
       xoverOps <- setEnv(xoverOps)

    if (missing(mutationOps))
       mutationOps <- list(list("boundaryMutation",       popSize * 0.04),
                           list("multiNonUniformMutation",popSize * 0.06),
                           list("nonUniformMutation",     popSize * 0.04),
                           list("uniformMutation",        popSize * 0.04))
    else
       mutationOps <-  setEnv(mutationOps)
       
    if (missing(selectionOp))
       selectionOp <- list("normGeomSelect")
    else
       selectionOp <- setEnv(list(selectionOp))[[1]]

    nVars <- dim(bounds)[1]
    
    if (missing(numVar))
       numVar <- 1:nVars
    else
       numVar <- unique(numVar)
            
    if (!is.null(intVar))
       unique(intVar)
       
    # initialize

    w <- runif(popSize)
    popVar <- matrix(bounds[,1] * rep((1-w), each=nVars) +
                     bounds[,2] * rep(   w,  each=nVars),
                     ncol=nVars,
                     byrow=TRUE)
    colnames(popVar) <- rownames(bounds)
    popVar[,intVar]  <- round(popVar[,intVar])

    popFit <- rep(0,popSize)
    n <- popSize
    while (n) {
	    popFit[n] <- evalFit(popVar[n,])
        n <- n-1
    }
    gen <- 0
    bestFit <- -Inf
    bestPop <- NULL

    fitTrace <- NULL

    while (gen <= maxGen) {
	    i <- which.max(popFit)
	    if (abs(popFit[i] - bestFit) > 1e-6) { 
           bestFit <- popFit[i]
           bestVar <- popVar[i,]
           bestPop <- rbind(bestPop, c(bestFit, bestVar))
           if (verbose) 
	          cat("\n",gen,bestFit,"\n")   
	    } 
        else
           if (verbose) 
	          cat(" ",gen)

	    if (trace)
           fitTrace <- rbind(fitTrace, c(max(popFit),mean(popFit),sd(popFit)))

        s <- do.call(selectionOp[[1]], selectionOp[-1])
        
        popFit <- popFit[s]
        popVar <- as.matrix(popVar[s,])

	    for (op in xoverOps) {
            if (0 < op[[2]] && op[[2]] < 1)
               n <- rbinom(1, popSize, op[[2]])
            else
               n <- op[[2]]
	        while (n) {
		        i1 <- sample(popSize, 1)
		        i2 <- sample(popSize, 1)
                
		        c  <- do.call(op[[1]], c(list(i1, i2), op[-c(1,2)]))
	    
		        if (all(c[1,] == popVar[i1,]))		# check first child 
		           f1 <- popFit[i1]
		        else 
		        if (all(c[1,] == popVar[i2,])) 
                   f1 <- popFit[i2]
		        else 
                   f1 <- evalFit(c[1,])

		        if (all(c[2,] == popVar[i1,]))		# check second child
	               f2 <- popFit[i1]
		        else 
		        if (all(c[2,] == popVar[i2,])) 
                   f2 <- popFit[i2]
		        else
                   f2 <- evalFit(c[2,])

		        popVar[c(i1,i2),] <- c
		        popFit[c(i1,i2)]  <- c(f1,f2)
		        n <- n-1
	        }     
	    }
	    for (op in mutationOps) {
            if (0 < op[[2]] && op[[2]] < 1)
               n <- rbinom(1, popSize, op[[2]])
            else
               n <- op[[2]]
	        while (n) {
		        i <- sample(popSize, 1)
		        c <- do.call(op[[1]], c(list(popVar[i,]), op[-c(1,2)]))
		        if (!all(c == popVar[i,])) 
		           popFit[i] <- evalFit(c)

		        popVar[i,] <- c
		        n <- n-1
	        }
	    }
	    i <- which.min(popFit)
	    popFit[i]  <- bestFit
	    popVar[i,] <- bestVar 

	    gen <- gen + 1
    }
    if (verbose)
       cat("\n\n")
    bestPop <- as.data.frame(bestPop)
    names(bestPop) <- c("fit", rownames(bounds))
    if (trace) {
       fitTrace <- as.data.frame(fitTrace)
       names(fitTrace) <- c("max","mean","std")
    }
    obj <- list(bestFit=bestFit, bestVar=bestVar, bestPop=bestPop,
                fitTrace=fitTrace)
    class(obj) <- "gom"
    obj
}

###  

print.gom <- function(x, print.trace=FALSE, ...) {
    cat("bestFit: ",x$bestFit,"\n")
    cat("bestVar:\n")
    print(x$bestVar)
    if (print.trace && !is.null(x$fitTrace)) {
       cat("fitTrace:\n")
       print(x$fitTrace)
    }
    invisible(x)
}

###############################################################################
