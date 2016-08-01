\name{DutilleulPlot.fun}
\alias{DutilleulPlot.fun}
\alias{fn3}

\title{Performs  a Diggle's randomization testing procedure to check independence between two point processes}

\description{ This function  applies the  Diggle's randomization testing procedure  
extended  by Dutilleul(2011) and performs a plot  which  checks graphically the independence of two point proceses. 
It is implemented for  homogenous and non homogenous processes.}

\usage{DutilleulPlot.fun(posx, posy, lambday, nsim = 1000, lenve = c(0.025, 0.975), ...)}


\arguments{
  \item{posx}{Numeric vector. Position of the occurrence points of the first  point process.}

  \item{posy}{Numeric vector. Position of the occurrence points of the second  point process.}

  \item{lambday}{Numeric vector. Intensity values  of the  second point process}


  \item{nsim}{Optional. Positive integer. Number of simulations performed to obtain the confidence bands.}

  \item{lenve}{Optional.  Numeric vector. Lower and the upper percentiles which determine the  limits of 
	the confidence band.}

  \item{...}{Further arguments to  be passed to \code{\link{plot}}.}
}


\details{This is a procedure to  check graphically the independence of two point proceses. 
It is  based on the comparison of the 
cumulative relative frequency  of the nearest neighbour distances 
between the points in  the two  observed processes, with their counterpart in  two  independent processes  with the same 
intensities. The procedure consists on plotting the cumulative relative frequency of the observed processes and a 
confidence band calculated from \emph{nsim} simulated independent processes.
}

\value{ A list with the elements used in the plot
 \item{quantobs}{Vector of  observed percentiles of the nearest neighbour distances.}
 \item{enve1}{Vector of  lower limits of the confidence band.}
 \item{enve2}{ Vector of  upper limits of the confidence band.}
}

\references{
Dutilleul, P. (2011), \emph{Spatio-temporal heterogeneity: Concepts and analyses}, Cambridge University Press.
}


\seealso{\code{\link{TestIndNH.fun}}, \code{\link{CondTest.fun}},\code{\link{nearestdist.fun}}}

\examples{

#Two independent NHPPs
set.seed(123)
lambdax<-runif(200, 0.01,0.1)
set.seed(124)
lambday<-runif(200, 0.015,0.15)
posx<-simNHPc.fun(lambdax,fixed.seed=123)$posNH
posy<-simNHPc.fun(lambday, fixed.seed=123)$posNH

aux<-DutilleulPlot.fun(posx, posy, lambday,  nsim = 100)


#Two dependent NSPs
#set.seed(123)
#lambdaParent<-runif(200)/10
#DepPro<-DepNHNeyScot.fun(lambdaParent=lambdaParent, d=2, lambdaNumP = 3, 
#	dist = "normal", sigmaC = 3,fixed.seed=123)
#posx<-DepPro$PP1
#posy<-DepPro$PP2
#aux<-DutilleulPlot.fun(posx, posy, lambday, nsim = 100)
}
