\name{DutilleulPlot}
\alias{DutilleulPlot}
\alias{fn3}

\title{A graphical test to assess independence between two point processes}

\description{This function  applies the  Diggle's randomization testing procedure  
extended  by Dutilleul(2011), and performs a plot  which  graphically  assesses 
the independence between two point proceses. 
It is implemented for  homogenous and non homogenous Poisson processes.}

\usage{DutilleulPlot(posx, posy, lambday, nsim = 1000, lenve = c(0.025, 0.975), ...)}


\arguments{
  \item{posx}{Numeric vector.  Occurrence times of the  points in the first  point process.}

  \item{posy}{Numeric vector.  Occurrence times of the  points in the second  point process.}

  \item{lambday}{Numeric vector. Intensity vector  of the  second point process. If the process is homogeneous, 
a vector of length  \eqn{T},  with equal values must be provided; see Details.}


  \item{nsim}{Optional. Positive integer. Number of simulations  to calculate the confidence band.}

  \item{lenve}{Optional.  Numeric vector. The order of the lower and the upper percentiles  to build 
	the confidence band.}

  \item{...}{Further arguments to  be passed to  the function \code{\link{plot}}.}
}


\details{This  graphical approach is  based on the comparison of the 
cumulative relative frequency  of the nearest neighbour distances 
between the points in  the two  observed processes, with their counterpart in  two  independent processes  with the same 
marginal distributions, which are obtained by simulation.

The function plots the cumulative relative frequency of the observed processes and a 
confidence band calculated from \emph{nsim} simulated independent processes.

The length of the observed period  \eqn{T} is determined by the length of the argument  \code{lambday}.
}

\value{A list with the elements:
 \item{quantobs}{Vector of the observed percentiles of the nearest neighbour distances.}
 \item{enve1}{Vector of the lower bounds of the confidence band.}
 \item{enve2}{ Vector of the upper bounds of the confidence band.}
}

\references{
Dutilleul, P. (2011), \emph{Spatio-temporal heterogeneity: Concepts and analyses}, Cambridge University Press.
}


\seealso{\code{\link{TestIndNH}}, \code{\link{CondTest}},\code{\link{nearestdist}}}

\examples{

#Two independent NHPPs
set.seed(123)
lambdax<-runif(200, 0.01,0.1)
set.seed(124)
lambday<-runif(200, 0.015,0.15)
posx<-simNHPc(lambdax,fixed.seed=123)$posNH
posy<-simNHPc(lambday, fixed.seed=123)$posNH

aux<-DutilleulPlot(posx, posy, lambday,  nsim = 100)


#Two dependent Neyman Scott processes
#set.seed(123)
#lambdaParent<-runif(200)/10
#DepPro<-DepNHNeyScot(lambdaParent=lambdaParent, d=2, lambdaNumP = 3, 
#	dist = "normal", sigmaC = 3,fixed.seed=123)
#posx<-DepPro$PP1
#posy<-DepPro$PP2
#aux<-DutilleulPlot(posx, posy, lambday, nsim = 100)
}
