\name{DepCPSPNHK}
\alias{DepCPSPNHK}
\alias{DepCPSPKenv}

\title{Estimating cross K-function  and envelopes for marginal processes of a CPSP}

\description{This function estimates the  cross K-function  between 
two (homogenous or nonhomogeneous)  point processes in time, \eqn{N_{x}} and \eqn{N_{y}}.  
It  is evaluated in a grid of distances \eqn{r} and plotted. An envelope built  by simulation under the  hypothesis
that the processes are the marginal processes of a bivariate CPSP is also plotted.

It calls the auxiliary function DepCPSPKenv, not intended for users.}

\usage{DepCPSPNHK(posx, posy, lambdaix, lambdaiy, lambdaixy, r=NULL, typeEst=1, 
                  nsim=1000, conf=0.95,tit=NULL, cores=1,fixed.seed=NULL,...)}

\arguments{

  \item{posx}{Numeric vector.  Occurrence times of the  points in the first point process \eqn{N_{x}} .}

  \item{posy}{Numeric vector.  Occurrence times of the  points in the second point process \eqn{N_{y}} .}

  \item{lambdaix}{Numeric vector. Intensity values of \eqn{N_{(x)}}.}

  \item{lambdaiy}{Numeric vector. Intensity values of \eqn{N_{(y)}}.}

  \item{lambdaixy}{Numeric vector. Intensity values of \eqn{N_{(xy)}}.}


  \item{r}{Optional. Numeric vector.  Grid values where  the K-function  must be evaluated. If it  is NULL, 
a default vector is used; see Details.}

   \item{typeEst}{Optional. Two possible values: 1 or 2. They determines which one of the two  available estimators  
	of the function \eqn{K_{ij}} has to be used; see Details. }

  \item{nsim}{Optional. Numeric value.  Number of simulations  to obtain the envelope.}

  \item{conf}{Optional. Numeric value  in (0,1). Confidence level of the envelope for the K-function.}


  \item{tit}{Optional.  Title to be used in the plot of the K-function. }

  \item{cores}{Optional. Number of cores of the computer to be used in the calculations. }

  \item{fixed.seed}{An integer or NULL. If it is an integer, that is the value  used to set the seed 
in  random  generation processes. If it is NULL, a random seed is used.}

  \item{...}{Further arguments to  be passed to  the function \code{\link{plot}}.}
}

\details{

This function estimates the cross K function  between two (homogenous or nonhomogeneous)  point processes 
in time, \eqn{N_{x}} and \eqn{N_{y}}. Two different estimators
are available, see  \code{\link{NHK}} for  more details.

An envelope for the cross K function is built  under the hypothesis that the  processes are the marginal processes of a  bivariate CPSP with intensities of the indicator processes
\code{lambdaxi}, \code{lambdayi} and \code{lambdaxyi}. The envelope is based on the simulation of CPSPs,
gnerated by function \code{\link{DepNHCPSP}}.


If argument \code{r} is NULL,   the following r-grid is used to evaluate the function:

		 r1<-max(20, floor(T/20))

		 r<-seq(1,r1,by=2)

		 if (length(r)>200) r<-seq(1,r1,length.out=200)

where \code{T} is the length of the observed period.
}

\value{A list with elements:

 \item{r}{Vector of values \eqn{r} where the cross K-function is estimated.}
 \item{NHKr}{Estimated values of  \eqn{K_{ij}(r)}.}
 \item{KenvL}{Lower bounds of the envelope of \eqn{K_{ij}(r)}.}
 \item{KenvU}{Upper bounds of the envelope of \eqn{K_{ij}(r)}.}
 \item{T}{Length of the observed period of the processes.}

}

\seealso{\code{\link{NHK}}, \code{\link{DepNHCPSP}} }


\examples{


set.seed(123)
lambdai1<-runif(500,0.005,0.01)
set.seed(124)
lambdai2<-runif(500,0.005,0.01)
set.seed(125)
lambdai12<-runif(500,0.005,0.02)

#Observed process: independent Poisson processes
posx<-simNHPc(lambda = (lambdai1+lambdai12), fixed.seed = 13)$posNH
posy<-simNHPc(lambda = (lambdai2+lambdai12), fixed.seed = 14)$posNH
aux<-DepCPSPNHK(posx=posx, posy=posy, lambdaix=lambdai1, lambdaiy=lambdai2, 
	       lambdaixy=lambdai12, fixed.seed=123, r=c(1:10), nsim=500)

#Observed processes: dependent marginal processes of a CPSP
#pos<-DepNHCPSP(lambdaiM=cbind(lambdai1, lambdai2, lambdai12), d=2,
#	fixed.seed=123, dplot=F)$posNH
#aux<-DepCPSPNHK(posx=pos$N1, posy=pos$N2, lambdaix=lambdai1, 
#	lambdaiy=lambdai2, lambdaixy=lambdai12, fixed.seed=125, r=c(1:10))
}
