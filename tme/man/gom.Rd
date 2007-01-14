\name{gom}
\alias{gom}
\title{Genetic Optimization with Mixed-Type Variables}
\description{
  General-purpose genetic optimization environment for mixed-type (real and 
  integer) and binary variables.
}
\usage{
gom(evalFun, evalArgs = NULL, evalVector = FALSE, bounds, numVar, intVar = NULL, popSize = 100, maxGen = 100, xoverOps, mutationOps, selectionOp, verbose = TRUE, trace = FALSE)
}
\arguments{
  \item{evalFun}{objective (fitness) function}
  \item{evalArgs}{a list of named parameters to the objective function}
  \item{evalVector}{logical indicating if a member should be supplied as a 
		    vector to the objective function}
  \item{bounds}{a two-column matrix or data frame of the lower and upper 
    bounds of the variables}
  \item{numVar}{a vector naming or indexing the numeric (real and integer) 
    variables}
  \item{intVar}{a vector naming or indexing the integer (ordinal and nominal) 
    variables}
  \item{popSize}{the population size}
  \item{maxGen}{the number of generations (epochs)}
  \item{xoverOps}{a list of crossover operator specifications}
  \item{mutationOps}{a list of mutation operator specifications}
  \item{selectionOp}{a selection operator specification}
  \item{verbose}{option to show progress information}
  \item{trace}{option to collect population statistics}
}
\details{

  This function maximizes a supplied evaluation function using a population
  of solutions which are transformed by genetic operators and are sampled by 
  their fitness values. 

  The following crossover operators are available: 
  
  \describe{
    \item{\code{simpleXover}}{Selects a cut point with uniform probability and 
	exchanges the values of the following parent variables.}
    \item{\code{nomXover}}{The values of the nominal parent variables are 
	exchanged.}
    \item{\code{arithXover}}{Computes a random linear interpolation of the 
	(numeric) parent variables.}
    \item{\code{heuristicXover}}{Computes a random linear extrapolation of the 
	(numeric) parent variables in the direction of the better parent.
	\code{retry=3} specifies the number of trials.}
  }

  The following mutation operators are available:

  \describe{
    \item{\code{uniformMutation}}{Selects a variable with equal probability 
	and sets it to a random linear interpolation of its bounds.}
    \item{\code{nonUniformMutation}}{Selects a (numeric) variable with equal 
	probability and sets it to a random linear interpolation of that 
	variable and one of its bounds. the latter are chosen with equal 
	probability.}
    \item{\code{multiNonUniformMutation}}{Similar as above except that all 
	(numeric) variables are selected.}
    \item{\code{boundaryMutation}}{Selects a (numeric) variable with equal 
	probability and set it to one of its bounds.}
    \item{\code{binaryMutation}}{Flips the value of a binary variable with 
	probability \code{p=0.005}.}
  }

  The following select operators are available:

  \describe{
    \item{\code{normGeomSelect}}{Maps the order of the fitness values to a 
	geometrically declining selection density function with parameter 
	\code{p=0.08}.}
    \item{\code{rouletteSelect}}{Uses the relative fitness values as selection 
	density function.}
    \item{\code{tournSelect}}{Selects the best among \code{n=2} members drawn  
	with replacement from the population.}
  }
  
  An element of the list specifying the crossover or mutation operators is 
  a list with an identifier string, the number of times the operator is to 
  be applied or a probability, and further operator-specific parameters 
  (see above). The number of times a probabilistic operator is applied is
  drawn from the binomial distribution. The specification of selection 
  operators is similar: identifier plus operator-specific parameters.
  (see above).

  Note that functions that are passed in for operators have their environment 
  set to the environment of the optimizer, so that they can use its internal 
  variables. Use with caution!

  The arguments specifying the type of variables can be either a vectors
  of identifier strings, or positional indexes. In the first case the 
  row names must be set on \code{bounds} for referencing. By default all 
  variables are numeric.
  
  The operators enforce the integer constraint by rounding. A variable that 
  is referenced by \code{intVar} but not by \code{numVar} is treated as 
  nominal, i.e. as integer but excluded from the application of operators 
  that assume a variable is ordered. In the case nominal variables are used
  inclusion of the \code{nomXover} operator may be considered for balancing 
  the operator mix.
  
  The evaluation function is protected from the optimization environment, 
  i.e. it is evaluated in the environment where it was defined. Parameters of 
  this function can be passed in via \code{evalArgs}. This may be useful in
  (heuristic) approaches that try to decompose a problem.

  By default a population member is passed into the evaluation function
  by separate arguments, i.e.~one for each chromosome (see the example).
  With the \code{evalVector} option this behavior can be altered such
  that the member is passed in as a vector in the first position.
}
\value{
  A list with the following elements:

  \item{bestFit}{the fitness value of the best solution.}
  \item{bestVar}{the variable values of the best solution.}
  \item{bestPop}{a data.frame containing the best solutions found so far.}
  \item{fitTrace}{a data.frame containing fitness statistics for each 
    generation (maximum, average, and standard deviation of the fitness).}
}
\references{
  C.R. Houck, J.A. Joines, and M.G. Kay. (1995) A Genetic 
  Algorithm for Function Optimization: A Matlab Implementation.
}
\author{Christian Buchta}
%\note{}
\seealso{package \code{genalg} for genetic optimization with float chromosomes.}
\examples{
### 
fun <- function(x1, x2, y=0.5)
    x1 * (1-x1) + x2 * (1-x2) + y
bounds <- data.frame(lower=c(0,0),upper=c(1,1))
rownames(bounds) <- c("x1","x2")
go <- gom(evalFun=fun, bounds=bounds, trace=TRUE)
print(go)
}
\keyword{optimize} 
\keyword{multivariate}
