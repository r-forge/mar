
# metric conjoint analysis
#
# fixem: 1) direction of scale?
#        2) scale models?
#
# we definitely need to clean this up!
#
# ceeboo 2005, 2006
# theussl 2006

# note that SPSS uses zero-sum coding,
# hence provide option contr.sum.
#
# fixme: provide for ideal point scales e.g. we 
#        may use a scale type attribute on the
#        design variables (and functions to set
#        them

## where the work is done

conjoint <- function(x, design, contr.sum=FALSE, ...) {
    if (!is.data.frame(x))
       stop("'x' not a data frame")
    if (!is.data.frame(design))
       stop("'design' not a data frame")
    if (dim(x)[2] != dim(design)[1])
       stop("'x' and 'design' do not conform")
  
    if (contr.sum) {
       rn <- rownames(design)
       design <- as.data.frame(lapply(design, function(x) {
            if (is.factor(x)) 
               contrasts(x) <- "contr.sum"
            #else
            #   stop("not a factor in 'design'")
            x}))
       rownames(design) <- rn
    }

    obj <- vector("list",dim(x)[1])
    for (k in seq(dim(x)[1]))
        obj[k] <- list(lm(y~.,data.frame(y=as.vector(t(x[k,])), design), ...))
    
    names(obj) <- rownames(x)
    obj <- list(models=obj, design=design)
    class(obj) <- "conjoint"
    obj
}

##############################################
## conjoint methods


is.conjoint <- function(x) inherits(x, "conjoint")

print.conjoint <- function(x, ...) {
  if(!inherits(x, "conjoint"))
    stop("'x' must inherit from class \"conjoint\"")
  
    cat("an object of class 'conjoint' with",
        length(x$models),"cases\n")
    invisible(x)
}

fitted.conjoint <- function(object, ...)
    t(sapply(object$models, function(z) fitted(z, ...)))

residuals.conjoint <- function(object, ...)
    t(sapply(object$models, function(z) residuals(z, ...)))

predict.conjoint <- function(object, newdata, ...) {
    if (missing(newdata))
       return(fitted(object))
    obj <- t(sapply(object$models,function(z,n)predict(z,n), newdata))
    colnames(obj) <- rownames(newdata)
    rownames(obj) <- names(object$models)
    obj
}

coef.conjoint <- function(object, ...)
    t(sapply(object$models, function(z) coef(z, ...)))


# fixme: better naming of methods?

choice.conjoint <- function(object, method=c("hard","exp","power"), p=1,
                            expand=FALSE, ...) {
    fun <- switch(match.arg(method),
                  hard = function(x, ...)
                    if (length(c <- which(max(x)==x)) > 1)
                       sample(c,1) 
                    else 
                       c,
                  exp  = function(x, ...) {
                    x <- exp(x)
                    x / sum(x)
                  },
                  power= function(x, p, ...) {
                    x <- x^p
                    x / sum(x)
                  })
    x <- if (expand)
            predict(object, expand.design(object$design))
         else
            fitted(object)
    z <- apply(x, 1, fun, p)
    if (is.matrix(z)) {
       z <- t(z)
       class(z) <- "choice"
    }
    else {
       n <- names(z)
       z <- factor(z)
       if (!is.null(nn <- colnames(x)))
           levels(z) <- nn[as.integer(levels(z))]          
       names(z) <- n
       class(z) <- c("choice","factor")
    }
    z
}


# compute the relative importance (aggregable)

importance <- function(object, ...) {
    z <- t(sapply(object$models, function(z) {
        if (attr(terms(z),"intercept") != 1)
           stop("missing intercept")
        x <- model.matrix(z)[,-1]
        x <- x * rep(coef(z)[-1],each=dim(x)[1])
        x <- apply(x,1,function(x) tapply(x,z$assign[-1],sum))
        x <- apply(x,1,max) - apply(x,1,min)
        x / sum(x)
    }))
    colnames(z) <- colnames(object$models[[1]]$model)[-1]
    z
}

# compute normalized part-worths, i.e we
# expand the models and scale to [0,1] 
# (aggregable)

utility <- function(object, unlist=FALSE, ...) {
    x <- lapply(object$models, function(z) {
        if (attr(terms(z),"intercept") != 1)
           stop("missing intercept")
        x <- model.matrix(z)[,-1]
        x <- x * rep(coef(z)[-1],each=dim(x)[1])
        x <- apply(x,1,function(x) tapply(x,z$assign[-1],sum))
        x <- x - apply(x,1,min)
        x <- x / sum(apply(x,1,max))
        x <- as.data.frame(t(x))
        x <- lapply(seq(length(x)),function(k)
            tapply(x[[k]],z$model[[k+1]],function(x)x[1]))
        names(x) <- names(z$model)[-1]
        x
    })
    if (unlist) {
       z <- unlist(lapply(seq(length(x[[1]])),function(z)
           rep(z,length(x[[1]][[z]]))))
       x <- t(sapply(x,function(x) unlist(x)))
       attr(x,"assign") <- z
       attr(x,"xlevels") <- object$models[[1]]$xlevels
    }
    x
}

## given normalized utilities we can compute
## importance like so

importance.utility <- function(x, ...)
    t(sapply(x, function(x) sapply(x, function(x) diff(range(x)))))


# plot part-worth profiles (exercise: we need to know
# the type of attribute [factor or numeric variable]
# for choosing the type of plot ...)

pl.utility <- function(x, nrow=1, lines=FALSE) {
    op <- par(mfrow = c(nrow,ceiling(length(x)/nrow)))
    on.exit(par(op))
    for (n in names(x))
        if (lines) {
           z <- x[[n]]
           plot(z, ylim=c(0,1), axes=FALSE, ylab="", xlab="", main=n)
           lines(z, col="red", lty=2)
           axis(1,seq(z),labels=names(z))
           axis(2)
        }
        else
           barplot(x[[n]], ylim=c(0,1), ylab="", xlab="", main=n)
    invisible(x)
}



# this is already in profit.r !!!

##############################################
## choice generic function


choice <- function(object, ...) 
    UseMethod("choice")


##############################################
## choice methods



print.choice <- function(x, ...) {
    if (is.factor(x)) {
       z <- as.vector(x)
       names(z) <- names(x)
       print(z, quote=FALSE, ...)
    }
    else
       print(unclass(x), ...)
    invisible(x)
}

summary.choice <- function(object, ...) {
    if (is.factor(object)) {
       t <- table(object, deparse.level=0)
       x <- data.frame(Freq=as.vector(t), Share=as.vector(t)/sum(t))
       rownames(x) <- names(t)
    }
    else {
       if (!is.matrix(object))
          stop("type not implemented")
       x <- data.frame(Share=colSums(object)/dim(object)[1])
    }
    x
}

# fixme: profile naming?

expand.design <- function(x, diff=FALSE) { 
    z <- expand.grid(lapply(x,function(x)
        if (!is.factor(x))
           stop("not a factor in 'x'")
        else 
           levels(x)))
    if (diff) {                             # inefficent
       k <- NULL
       for (i in seq(dim(x)[1]))
           for (j in seq(dim(z)[1]))
               if (all(x[i,]==z[j,])) {
                  k <- c(k,j)
                  break
               }
       if (length(k)==dim(z)[1])
          z <- NULL
       else
          z <- z[-k,]
    }
    z
}

# dummy code (factor) variables

as.indicator <- function(x, sep="", ...) {
    if (!is.list(x))
       stop("'x' not a list")
    obj <- NULL
    for (k in seq(length(x))) {
        if (!is.factor(z <- x[[k]]))
           stop("not a factor in 'x'")
        zz <- matrix(as.integer(outer(as.integer(z),rep(1,nlevels(z))) ==
                     outer(rep(1,length(z)),seq(nlevels(z)))),
                     ncol=nlevels(z))
        cn <- colnames(obj)
        obj <- cbind(obj,zz)
        colnames(obj) <- c(cn,paste(names(x)[k],levels(z),sep=sep))
    }
    rownames(obj) <- rownames(x)
    obj
}

## compute the stimulus utilities using 
## (normalized) part-worths and design

score <- function(x, design, ...) {
    d <- as.indicator(design, ...)
    t(sapply(x, function(x) {
        x <- d * rep(unlist(x), each=dim(d)[1])
        apply(x, 1, sum)
    }))
}

###
