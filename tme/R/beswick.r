
# Beswick's approach of simultaneously 
# determining an optimal salesforce size 
# and sales effort allocation
#
# see Hruschka pp. 272-275
#
# ceeboo 2005

# note that g and c are the region (account, 
# product) and shape parameters of the sales 
# response functions.

### not ready to release

beswick <- function(g, c=0.2, margin=0.1, cost=1, cost.fixed=0, 
                    max.sales=Inf, max.cost=10, verbose=FALSE) {

    g <- sort(g, decreasing = FALSE)
    if (0 > g[1]) 
       stop("'g' invalid values")

    # recursions of the dynamic programming problem
    
    forward <- function(g,a=1,b,c) {
        if (length(g) < 1)
           return(c(a,b))
        a1 <- (g[1]/b)^(1/(1-c))
        a1 <- a1/(1+a1)
        b1 <- g[1]*a1^c+b*(1-a1)^c
        rbind(c(a,b),forward(g[-1],a1,b1,c))
    }

    backward <- function(a,b,v,c) {
        if ((l<-length(a))!=length(b))
           stop("'a' and 'b' do not conform")
        v1 <- a[l]*v
        s  <- b[l]*v^c
        if (l == 1)
           return(matrix(c(s,v1),nrow=1))
        z <- backward(a[-l],b[-l],v-v1,c)
        rbind(z,c(s-z[dim(z)[1],1],v1))
    }

    if (verbose)
       cat("\n",formatC("#",width=2),formatC("sales",width=12),
                formatC("profit",width=12),"\n")
    
    f <- forward(g[-1],b=g[1],c=c)

    print(f)

    n <- 0
    p <- 0
    b <- NULL
    while (n <= max.cost / cost) {
        # fixme: optimal efforts are multiples 
        #        of the salesforce size ...
        zb <- backward(f[,1],f[,2],v=n*100,c=c)
        z  <- sum(zb[,1])
        zp <- z * margin - n * cost - cost.fixed
        if (verbose)
           cat(formatC(n,width=3),formatC(z,width=12), 
               formatC(zp,width=12),"\n")
        if (n > 0 && (z > max.sales || zp < p))
           break
        b <- zb
        p <- zp
        n <- n + 1
    }
    n <- n - 1
    rownames(b) <- names(g)
    list(profit=p, size=n, sales=b[,1],effort=b[,2]/n)
}

# test

#g <- c(75.36, 28.76, 36.21, 43.28, 56.65, 69.27, 63.04)
#names(g) <- LETTERS[seq(g)]

#bm <- beswick(g, verbose=TRUE) 
#print(bm)

###
