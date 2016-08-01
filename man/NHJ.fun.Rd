\name{NHJ.fun}
\alias{NHJ.fun}
\alias{NHJaux}
\alias{NHDFaux}
\alias{Jenv}



\title{It estimates the cross J-function  for two sets of point processes}

\description{This function estimates the cross J-function between 
two sets, \eqn{C} and \eqn{D}, of (homogenous or nonhomogeneous) point processes. 
The J-function  is evaluated in a grid of  values \eqn{r},  and it
 can be optionally plotted. An independence test
based on J-function is also available.


It calls the auxiliary functions NHJaux.fun and Jenv, not intended for users.}

\usage{
NHJ.fun(lambdaC, lambdaD, T=NULL,Ptype='inhom', posC, typeC=1, posD,  typeD=1, r = NULL,
	L = NULL,  test=FALSE, nTrans=100, rTest=20, conf=0.95, dplot = NULL,
	tit = rep("J-function", 3), mfrow = c(1, 1), cores=1, fixed.seed=NULL)}



\arguments{
\item{lambdaC}{A matrix. Each column is the intensity vector of one of the point processes in 
\eqn{C}. If there is only one process in \eqn{C}, it can be a vector or even a numeric value
if the process is homogeneous.}

  \item{lambdaD}{A matrix. Each column is the intensity vector of one of the point process in 
\eqn{D}. If there is only one process in \eqn{D}, it can be a vector oe even a numeric value
if the process is homogeneous.}

  \item{T}{Numeric value. Length of the observation period. It only must be specified
 if all the processes are homogeneous, that is if the number of rows in \code{lambdaC} and \code{lambdaD} is 1.}

  \item{Ptype}{Optional. Label: 'hom' or 'inhom'.  The first one indicates that 
all the point processes in sets \eqn{C} and \eqn{D} are homogeneous. In that case,  columns of arguments
\code{lambdaC} and \code{lambdaD} can be a  number or a constant vector.}


  \item{posC}{Numeric vector.  Time position of the  points in all the point processes in \eqn{C}.}

  \item{typeC}{Numeric vector with the same length as \code{posC}.  Code of the point process  
in \eqn{C}  where the points in \code{posC}  have occurred. See Details}


  \item{posD}{Numeric vector.  Time position of the  points in all the point processes in \eqn{D}.}

  \item{typeD}{Numeric vector with the same length as \code{posD}.  Code of the point process  
in \eqn{D}  where the points in \code{posD}  have occurred.}

  \item{r}{Optional. Numeric vector. Values where J-function  must be evaluated. If it  is NULL, a default 
vector is used, see Details}


  \item{L}{Optional. Numeric vector. Net of values in the observation period used to calculate 
the J-function. If it  is NULL, a default vector is used, see Details.}


  \item{test}{Optional. Logical flag. If it is TRUE, a  test to check the independence
  and a 95\%  envelope   for the J-function are calculated.}

  \item{nTrans}{Optional. Numeric value. Only used if \code{test=TRUE}. Number of translations to
be performed in the test and envelope calculation.}


  \item{rTest}{Optional. Numeric value. Maximum value of \eqn{r} used to calculate the 
independence test statistc, see Details.}



  \item{conf}{Optional. Numeric value  in (0,1). Confidence level of the envelope for the K-function.}

  \item{dplot}{Optional. Label 'JDF' or 'J'. If it is  equal to'JDF', plots of J, D and F-functions
 are displayed. If it is 'J', only  J-function is plotted.}

  \item{tit}{Optional.  A vector with  one or  three titles to be used in the plots of J, D and F-functions. }
 
 \item{mfrow}{Optional. Argument to be passed to \code{\link{par}} for the plot of the J-function.}

  \item{cores}{Optional. Number of cores of the computer to be used in the calculations. }

  \item{fixed.seed}{An integer or NULL. If it is an integer, that is the value  used to set the seed 
in  random  generation processes. It it is NULL, a random seed is used.}
}

\details{
This function estimates the cross J-function  between 
two sets,  \eqn{C} and \eqn{D}, of  homogenoeus or nonhomogeneous  time point processes, 
using  a version of the spatial estimator 
suggested by Cronie and van Lieshout (2015) adapted to time processes.  
In the case of nonhomogeneous processes, the estimator is only defined for  second order intensity-reweighted 
stationary point processes.
J-function measures the interpoint dependence between points in any 
of the processes in \eqn{D},  and points in any of the processes in \eqn{C}, adjusted for time varying intensity in the case 
of nonhomogenous processes. 


The cross J-function is defined as \eqn{J_{CD}(r)=(1-D_{CD}(r))/(1-F_D(r))}, if \eqn{F_D(r)<1} and it is not calculated
otherwise.  It compares D(r), the distribution 
function of the distances from a  point  in any of the processes in set \eqn{C} to the nearest
point in any of the processes in set \eqn{D},  to F(r),  the distribution 
function of the distances from a fixed point   in the space to the nearest
point in any of the processes in set \eqn{D}.


If the processes in \eqn{C}  are independent of the processes in \eqn{D} (conditionally on the marginal structure 
of the processes) J-funtion is equal to 1, since D(r)=F(r). Hence, deviations of J(r) estimations from 1, 
suggest dependence betweent  the two sets of processes.

If argument \code{r} is NULL, it is calculated as

		 r1<-max(20, floor(T/20))

		 r<-seq(1,r1,by=2)

		 if (length(r)>200) r<-seq(1,r1,length.out=200)

If argument \code{L} is NULL, it is calculated as

		L<-seq(1, T, by=2)

		if (length(L)>200) L<-seq(1,T,by = round((T - 1)/199))


Testing independence

In order to test the independence hypothesis using this function, a Lotwick-Silverman type test is available. This 
test provides a nonparametric way to test independence when the study area is rectangular. The key 
idea is to wrap  the processes onto a torus by identifying opposite sides, keeping the processes in set \eqn{C} fixed and
 translating the processes in set \eqn{D} randomly over the torus. In nonhomogenous processes, random translation
 may change their distribution,  and to compensate, the intensity must also be translated. 
The considered translations  keep the distribution of the
processes in \eqn{D}  but break any dependence between them, without the 
need of parametric models to describe the univariate marginal patterns.  Critical values are estimated
by  bootstrap methods.  Using this approach,  not only the p-value of a test to check independence but 
also an envelope for \eqn{J(r)} values is calculated. The test is based on the statistic 
\eqn{max_r(abs(J(r)-1))}.



One disadvantage of  the proposed test is that it may be quite sensitive to
the values of \eqn{r}.  In addition, in time processes, dependence often
appears between close observations,  and  with high \eqn{r} values it is more difficult that the
J-function is able to discriminate  between dependent and independent processes.
By this reason, the argument \code{rTest} allows us to fix a maximum value of \eqn{r}
and only J(r) estimations for \eqn{r<rTest}  will be used to
calculate the  test statistic. The value \code{rTest} is  drawn in the plot of the J-function
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

 \item{r}{Vector of values \eqn{r} where  J-function is evaluated.}
 \item{NHJr}{Estimated values of function J(r).}
 \item{NHDr}{Estimated values of function  D(r).}
 \item{NHFr}{Estimated values of function J(r).}
 \item{JenvL}{Lower limits of the envelope for J(r).}
 \item{JenvU}{Upper limits of the envelope for J(r).}
 \item{JStatOb}{Observed value of the statistic used to test the independence assumption.}
 \item{JStatTr}{ Sample of the values of the  test statistic obtained by random translation.}
 \item{pv}{P-value of the independence test.}
 \item{T}{Length of the observation period of the process.}
 \item{L}{Net of values L used to calculate F-funtion. }
}


\references{
Cronie, O. and van Lieshout, M.N.M. (2015). Summary statistics for inhomogeneous marked point
processes. Ann Inst Stat Math. DOI 10.1007/s10463-015-0515-z

Lotwick, H.W. and Silverman, B.W. (1982). Methods for analysing Spatial processes of several types of points. 
\emph{J.R. Statist. Soc. B}, 44(3), pp. 406-13

Stoyan et al (2001). On the estimation of distance distribution functions for points
processes and random sets. Image Anal Stereol, 20, 65-69

Van Lieshout, M.N.M. (2006) A J-function for marked point patterns. AISM, 58, 235-259. DOI
10.1007/s10463-005-0015-7
}



\seealso{\code{\link{NHK.fun}}, \code{\link{NHD.fun}}, \code{\link{NHF.fun}} }


\examples{

set.seed(122)
lambda1<-runif(100, 0.05, 0.1)
set.seed(121)
lambda2<-runif(100, 0.01, 0.2)
pos1<-simNHPc.fun(lambda=lambda1,fixed.seed=123)$posNH  
pos2<-simNHPc.fun(lambda=lambda2,fixed.seed=123)$posNH

aux<-NHJ.fun(lambdaC=lambda1, lambdaD=lambda2, posC=pos1,nTrans=50, 
	 posD=pos2, rTest=7, dplot='J', cores=1,test=TRUE)
aux$pv


#Sets with two processes
#pos3<-simNHPc.fun(lambda=lambda1,fixed.seed=321)$posNH  
#pos4<-simNHPc.fun(lambda=lambda2,fixed.seed=321)$posNH 
#aux<-NHJ.fun(lambdaC=cbind(lambda1,lambda2), lambdaD=cbind(lambda1,lambda2), 
#	posC=c(pos1,pos2), typeC=c(rep(1, length(pos1)), rep(2, length(pos2))), 
#	posD=c(pos3, pos4), typeD=c(rep(1, length(pos3)), rep(2, length(pos4))), 
#	dplot='J', test=TRUE)
#aux$pv

}
