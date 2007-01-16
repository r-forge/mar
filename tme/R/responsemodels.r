## Response Models
## Marketing Engineering p.33-37
## implemented by theussl


## FIXME constraints (see Marketing Models Apendix C)

response <- function(x,pars,model="linear"){
  models <- c("linear",
              "power_series",
              "fractional_root",
              "semilog",
              "exponential",
              "modexp",
              "logistic",
              "gompertz",
              "adbudg")
  if(is.null(model)) modelNr <- 1
  else modelNr <- pmatch(tolower(model), tolower(models))
  if(is.na(modelNr)) stop (paste("Unknown model:",sQuote(model)))

  lenpars <- length(pars)
  
  y <- switch(modelNr,
              
              "1" = {if(lenpars<2) stop("not enough parameters!")
                     if(lenpars>2) warning("too many parameters, using only 2 of them!")
                     pars[1] + pars[2]*x},
              "2" = .powerseriesmodel(x,pars),
              "3" = {if(lenpars<3) stop("not enough parameters!")
                     if(lenpars>3) warning("too many parameters, using only 3 of them!")
                    pars[1] + pars[2] * x^pars[3]},
              "4" = {if(lenpars<2) stop("not enough parameters!")
                     if(lenpars>2) warning("too many parameters, using only 2 of them!")
                    pars[1] + pars[2] * log(x)},
              
              "5" = {if(lenpars<2) stop("not enough parameters!")
                     if(lenpars>2) warning("too many parameters, using only 2 of them!")
                    pars[1] * exp(pars[2]*x)},
              
              "6" = {if(lenpars<3) stop("not enough parameters!")
                     if(lenpars>3) warning("too many parameters, using only 3 of them!")
                    pars[1] * (1 - exp(-pars[2]*x)) + pars[3]},
              "7" = {if(lenpars<4) stop("not enough parameters!")
                     if(lenpars>4) warning("too many parameters, using only 2 of them!")
                    pars[1] / (1 + exp(-(pars[2]+pars[3]*x))) + pars[4]},
              "8" = {if(lenpars<4) stop("not enough parameters!")
                     if(lenpars>4) warning("too many parameters, using only 2 of them!")
                     pars[1] * pars[2]^(pars[3]*x) + pars[4]},
              "9" = {if(lenpars<4) stop("not enough parameters!")
                     if(lenpars>4) warning("too many parameters, using only 2 of them!")
                     pars[2] + (pars[1] - pars[2]) * x^pars[3]/(pars[4] + x^pars[3])}
              )
  y
}

.powerseriesmodel <- function(x,coef){
  len <- length(coef)
  if(len==1)
    return(rep(coef,length(x)))
  out <- c(coef[len]*x^(len-1),.powerseriesmodel(x,coef[-len]))
  apply(matrix(out,nrow=length(x)),1,sum)
}

adbudg.inv <- function(y, a,b,c,d)
    (d*(y-b)/(a-y))^(1/c)


