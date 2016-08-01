\name{NHK.fun}
\alias{NHK.fun}
\alias{NHKaux}
\alias{NHKaux2}
\alias{NHKaux3}
\alias{Kenv}

\title{It estimates the cross K-function  for two sets of point processes}

\description{This function estimates the  cross K-function  between 
two sets, \eqn{C} and \eqn{D},  of (homogenous or nonhomogeneous) time point processes.  
It  is evaluated in a grid of distances \eqn{r},  and it can be optionally plotted.  An independence test
based on K-function is also available.

It calls the auxiliary functions NHKaux.fun, NHKaux2.fun, NHKaux3.fun and Kenv, not intended for users.}

\usage{
NHK.fun(lambdaC, lambdaD, T=NULL, posC, typeC=1, posD, typeD=1, r=NULL, 
	 test=TRUE, nTrans=100, conf=0.95, rTest=20, dplot=FALSE, 
	tit="K-function", cores=1, fixed.seed=NULL)
}

\arguments{
\item{lambdaC}{A matrix. Each column is the intensity vector of one of the point processes in 
\eqn{C}. If there is only one process in \eqn{C}, it can be a vector or even a numeric value
if the process is homogeneous.}

  \item{lambdaD}{A matrix. Each column is the intensity vector of one of the point process in 
\eqn{D}. If there is only one process in \eqn{D}, it can be a vector oe even a numeric value
if the process is homogeneous.}

  \item{T}{Numeric value. Length of the observation period. It only must be specified
 if all the processes are homogeneous, that is if the number of rows in \code{lambdaC} and \code{lambdaD} is 1.}


  \item{posC}{Numeric vector.  Time position of the  points in all the point processes in \eqn{C}.}

  \item{typeC}{Numeric vector with the same length as \code{posC}.  Code of the point process  
in \eqn{C}  where the points in \code{posC}  have occurred. See Details. }


  \item{posD}{Numeric vector.  Time position of the  points in all the point processes in \eqn{D}.}

  \item{typeD}{Numeric vector with the same length as \code{posD}.  Code of the point process  
in \eqn{D}  where the points in \code{posD}  have occurred.  }

  \item{r}{Optional. Numeric vector. Values where K-function  must be evaluated. If it  is NULL, a default vector is used, 
see Details}


  \item{test}{Optional. Logical flag. If it is TRUE, a  test to check the independence
  and a 95\%  envelope   for the K-function are calculated.}

  \item{nTrans}{Optional. Numeric value. Only used if \code{test=TRUE}. Number of translations to
be performed in the test and envelope calculation.}

  \item{conf}{Optional. Numeric value  in (0,1). Confidence level of the envelope for the K-function.}

  \item{rTest}{Optional. Numeric value. Maximum value of \eqn{r} used to calculate the 
independence test statistc, see Details.}


  \item{dplot}{Optional. A logical flag. If it is true a plot of the K-function is shown.}

  \item{tit}{Optional.  Title to be used in the plot of the K-function. }

  \item{cores}{Optional. Number of cores of the computer to be used in the calculations. }

  \item{fixed.seed}{An integer or NULL. If it is an integer, that is the value  used to set the seed 
in  random  generation processes. It it is NULL, a random seed is used.}

}

\details{
This function estimates the cross K-function  between 
two sets,  \eqn{C} and \eqn{D}, of (homogenous or nonhomogeneous)  time point processes, using  a version of the spatial estimator 
suggested by Moller and Waagepetersen (2007), p662, adapted to cross time processes; the  edge correction suggested in that
paper is also used.
In the case of nonhomogeneous processes, the estimator is only defined for  second order intensity-reweighted 
stationary point processes. Homogenous  processes  are  generated if the intensity vector  \code{lambda} is  constant 
(that is if all the values are equal) or its length is 1.

K-function measures the degree of dependence between two point processes (or  two sets of 
point processes) and counts the expected number of points  in any 
of the processes in \eqn{D},  within a given distance of a point in any of the processes in \eqn{C}, 
adjusted for time varying intensity in the case of nonhomogenous processes. 
 
The cross K-function  of independent  Poisson processes (conditionally on the  marginal structure of the processes)
 is the length of the considered intervals,  \eqn{K_{CD}(r)=2r+1}. Then, values \eqn{K_{CD}(r)>2r+1} indicate attraction between the 
 processes, while values lower than \eqn{2r+1} indicate repulsion.


If argument \code{r} is NULL, it is calculated as

		 r1<-max(20, floor(T/20))

		 r<-seq(1,r1,by=2)

		 if (length(r)>200) r<-seq(1,r1,length.out=200)




Testing independence

In order to test the independence hypothesis using this function, a Lotwick-Silverman type test is available. This 
test provides a nonparametric way to test independence when the study area is rectangular. The key 
idea is to wrap  the processes onto a torus by identifying opposite sides, keeping the processes in set \eqn{C} fixed and
 translating the processes in set \eqn{D} randomly over the torus. In nonhomogenous processes, random translation
 may change their distribution,  and to compensate, the intensity must also be translated. 
The considered translations  keep the distribution of the
processes in \eqn{D}  but break any dependence between them, without the 
need of parametric models to describe the univariate marginal patterns.  Critical values are estimated
by  bootstrap methods.  Using this approach,  not only the p-value of a test to check independence but also
an envelope for the \eqn{K(r)} values is calculated. The test is based on the statistic 
\eqn{max_r(K(r)/(2r+1))}.


One disadvantage of  the proposed test is that it may be quite sensitive to
the values of \eqn{r}.  In addition, in time processes, dependence often
appears between close observations,  and  with high \eqn{r} values it is more difficult that the
K-function is able to discriminate  between dependent and independent processes.
By this reason, the argument \code{rTest} allows us to fix a maximum value of \eqn{r}
and only K(r) estimations for \eqn{r<rTest}  will be used to
calculate the  test statistic. The value \code{rTest} is  drawn in the plot of the K-function
 as a vertical grey line, in order to help us to identify an adequate  value.

The occurrence points in all the processes in \eqn{C} must be part of the input.
Since the number of points  will be possibly different in each  process, a matrix  cannot be used. Instead
 two vectors  with the same length are used: the first one contains  the occrrence points in all 
the processes while  the second one indicates the point process  
in \eqn{C}  where the point in the same row in \code{posC}  has occurred. The codes used in 
\code{typeC} are the column number  where the intensity of that process  is in matrix \code{lambdaC}.  
The same for set \eqn{D}.


}

\value{ A list with elements


 \item{r}{Vector of values \eqn{r} where  K-function is evaluated.}
 \item{NHKr}{Estimated values of function K(r).}
 \item{KenvL}{Lower limits of the envelope for K(r).}
 \item{KenvU}{Upper limits of the envelope for K(r).}
 \item{KStatOb}{Observed value of the statistic used to test the independence assumption.}
 \item{KStatTr}{Sample of the values of the  test statistic obtained by random translation.}
 \item{pv}{P-value of the independence test.}
 \item{T}{Length of the observation period of the process.}

}


\references{
Cronie, O. and van Lieshout, M.N.M. (2015). Summary statistics for inhomogeneous marked point
processes. Ann Inst Stat Math. DOI 10.1007/s10463-015-0515-z

Lotwick, H.W. and Silverman, B.W. (1982). Methods for analysing Spatial processes of several types of points. 
\emph{J.R. Statist. Soc. B}, 44(3), pp. 406-13

Moller, J. and Waagepetersen, R.P. (2007). Modern statistics for spatial point processes. 
Scandinavian Journal of Statistics, 34(4), 643 684

}



\seealso{\code{\link{NHD.fun}}, \code{\link{NHJ.fun}}, \code{\link{NHF.fun}} }



\examples{
set.seed(122)
lambda1<-runif(100, 0.05, 0.1)
set.seed(121)
lambda2<-runif(100, 0.01, 0.2)
pos1<-simNHPc.fun(lambda=lambda1, fixed.seed=123)$posNH 
pos2<-simNHPc.fun(lambda=lambda2, fixed.seed=123)$posNH

aux<-NHK.fun(lambdaC=lambda1, lambdaD=lambda2, posC=pos1, posD=pos2, rTest=7,
	 dplot=TRUE,tit="K(r).", nTrans=75, cores=1)
aux$pv




#Sets with two processes
#pos3<-simNHPc.fun(lambda=lambda1, fixed.seed=321)$posNH
#pos4<-simNHPc.fun(lambda=lambda2, fixed.seed=321)$posNH
#aux<-NHK.fun(lambdaC=cbind(lambda1,lambda2), lambdaD=cbind(lambda1,lambda2), posC=c(pos1,pos2), 
#	typeC=c(rep(1, length(pos1)), rep(2, length(pos2))), posD=c(pos3, pos4), 
#	typeD=c(rep(1, length(pos3)), rep(2, length(pos4))), dplot=TRUE)
#aux$pv
}
