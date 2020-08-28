\name{IntMPP}
\alias{IntMPP}
\alias{funMPPGen}
\alias{firstt}
\alias{NumI}


\title{Simulated intervals  in a vector of point processes}

\description{This function calculates a  point estimation and a confidence interval
for a  given parameter related to a  vector of point processes using a Monte Carlo  
(or parametric bootstrap) approach. The  estimator of the parameter must be a function 
of the occurrence points of   the (possibly dependent)  marginal  processes of the vector of processes.

It calls the auxiliary function \code{funMPPGen} (not intended
for the users), see  Details.}


\usage{IntMPP(funMPP.name, funMPP.args, fun.name, fun.args = NULL, nsim=1000, clevel = 0.95, 
    cores = 1, fixed.seed = NULL) }

\arguments{

  \item{funMPP.name}{Name of the function defining the  distribution of the vector of point processes.}

  \item{funMPP.args}{Additional arguments for the function \code{funMPP.name}.}

  \item{fun.name}{Name of the function to calculate the estimation of the parameters. 
	The  first argument of this function must be  a list called posNH.}

  \item{fun.args}{A list whose elements are the additional arguments for the function \code{fun.name}.}

  \item{nsim}{Number of simulations to be carried out.}

  \item{clevel}{Confidence level of the interval. A value  in (0,1).}

  \item{cores}{Optional.  Number of cores of the computer to be used in the calculations. Default: one core is used.}

  \item{fixed.seed}{An integer or NULL. If it is an integer, that is the value  used to set the seed 
in  random  generation processes. It it is NULL, a random seed is used.}

}


\details{This function  calculates a  point estimation and a confidence interval of a  
 parameter related to a  vector of point processes. It calls the auxiliary function \code{funMPPGen}, which generates  a  sample of  vectors of processes  using a
parametric model. The parameter of interest  is estimated using  each process in that sample, so that a sample of values 
of the estimator is obtained. The mean of that sample is the point estimator, and the adequate sample percentiles give 
the lower and upper bounds of the confidence interval. 

The parametric model is specified by the arguments \code{funMPP.name} and 
\code{funMPP.args}.  Functions \code{\link{DepNHCPSP}}, \code{\link{DepNHNeyScot}}, \code{\link{DepNHPPqueue}} and 
\code{\link{DepNHPPMarked}} can be used  as input of the argument \code{funMPP.name} to generate
the corresponding vector of processes. 

The  considered estimator must be a function of the occurrence points of the vector of processes and  
any additional arguments, provided by argument  \code{fun.args}, which must be a list. The first argument of the
function \code{fun.name} must be  a list called \code{posNH} whose elements are  numeric vectors containing the
occurrence points of  each point process in the vector. For example,  the first element of the output list of 
\code{\link{DepNHCPSP}}  can be used as first argument of \code{fun.name}.
}


\value{ A list with elements:
  \item{valmed}{Point estimation (mean value) of the parameter.}
  \item{valinf}{Lower bound of the generated interval.}
  \item{valsup}{Upper bound of the generated interval.}
  \item{nsim}{Input argument.}
  \item{fixed.seed}{Input argument.}	
}




\examples{

# Calculation of the point estimation and  95% intervals based on 1000 simulations 
#of the   number of accurrences in each marginal process of a bivariate  Neyman-Scot process
# in  the time interval [100,200]
#NumI calculates the   number of occurrences in interval I in each element of  the list posNH

set.seed(123)
lambdai<-runif(1000,0.01,0.02)

aux<-IntMPP(funMPP.name="DepNHNeyScot", funMPP.args=list(lambdaParent=lambdai,d=2, 
  lambdaNumP=c(2,1), dplot=FALSE), fun.name="NumI", fun.args = list(I=c(100,200)), 
  fixed.seed = 125) 


# Calculation of the point estimation and a 95% interval based on 1000 simulations 
#of the  first occurrence time in  a multivariate  CPSP with d=3
#firstt calculates the  minimim occurrence time of  all the elements in the list posNH

#set.seed(124)
#lambdaij<-runif(1000,0.005,0.02)
#set.seed(125)
#lambdaijk<-runif(1000,0.001,0.02)
#lambdaiM<-cbind(lambdai,lambdai, lambdai, lambdaij, lambdaij, lambdaij, lambdaijk)
#aux<-IntMPP(funMPP.name="DepNHCPSP",funMPP.args=list(lambdaiM=lambdaiM,d=3,dplot=FALSE), 
#  fun.name="firstt", fixed.seed = 125) 


}

