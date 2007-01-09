## Response Models
## Marketing Engineering p.33-37
## implemented by theussl

response <- function(x,model="linear", ...){
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

  y <- switch(modelNr,
         1 = a + b*x,
         2 = 
         
              



  

.powerseriesmodel <- function(x,coef){
  len <- length(coef)
  if(len==1)
    return(coef)
  out <- c(coef[len]*x^(len-1),power.series.model(x,coef[-len]))
  sum(out)
}

fractional.root <- function(x,a,b,c)
  a + b * x^c

semilog.model <- function(x,a,b)
  a + b * log(x)

exponential.model <- function(x,a,b)
  a * exp(b*x)

modexp.model <- function(x,a,b,c)
  a * (1 - exp(-b*x)) + c

logistic.model <- function(x,a,b,c,d)
  a / (1 + exp(-(b+c*x))) + d

gompertz.model <- function(x,a,b,c,d)
  a * b^(c*x) + d

adbudg.model <- function(x,a,b,c,d)
  b + (a - b) * x^c/(d + x^c)

