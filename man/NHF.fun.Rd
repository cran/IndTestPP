\name{NHF.fun}
\alias{NHF.fun}
\alias{NHFaux}



\title{It estimates the cross  F-function  for  two sets of point processes}

\description{
This function estimates the cross F-function, between two sets, \eqn{C} and \eqn{D}, of (homogenous or nonhomogeneous)  point processes. 
The F-function  is evaluated in a grid of  values \eqn{r},  and it
 can be optionally plotted.


It calls the auxiliary functions NHFaux.fun and other functions not intended for users.}

\usage{NHF.fun(lambdaD, T=NULL, Ptype='inhom', posD, typeD=1, r=NULL,L=NULL, dplot=TRUE,
tit='F(r)')}


\arguments{


  \item{lambdaD}{A matrix. Each column is the intensity vector of one of the point process in 
\eqn{D}. If there is only one process in \eqn{D}, it can be a vector oe even a numeric value
if the process is homogeneous.}

  \item{T}{Numeric value. Length of the observation period. It only must be specified
 if all the processes are homogeneous the number of rows in \code{lambdaC} and \code{lambdaD} is 1.}

  \item{Ptype}{Optional. Label: 'hom' or 'inhom'.  The first one indicates that 
all the point processes in sets \eqn{C} and \eqn{D} are homogeneous. In that case,  columns of arguments
\code{lambdaC} and \code{lambdaD} can be a  number or a constant vector.}


 \item{posD}{Numeric vector.  Time position of the  points in all the point processes in \eqn{D}.}

  \item{typeD}{Numeric vector with the same length as \code{posD}.  Code of the point process  
in \eqn{D}  where the point  in the same row in \code{posD}  has occurred. The code must be the  
column number  where the intensity of that process  is in matrix \code{lambdaD}. }

  \item{r}{Numeric vector. Values where  F-function  must be evaluated.}

  \item{L}{Optional. Numeric vector. Net of values during the observation period used to calculate 
the F-function. If the value is NULL, a default vector, formed by all the integers in the observation period, is used}

  \item{dplot}{Optional. Logical flag. If it is true, the plot of the F-function is performed.}

  \item{tit}{Optional.  The title to be used in the plot of the F-function. }

}




\details{
This function estimates the cross F-function  for the processes in set \eqn{D}, using the estimator suggested by Cronie and 
van Lieshout (2015), adapted to time point processes. 

The cross F-function, also known as empty space function,  is the distribution function of 
the distances from an arbitray point in  the space to the nearest point in a process in \eqn{D}.
In homogeneous processes, it  estimates  the probability that  at least one point  in  processes
in \eqn{D} occurs  at a  distance lower than \eqn{r} of an arbitray point in the space. 
If the processes are nonhomogenous, the  inhomogenous  version of the function, adjusted for time varying intensities, 
is used. 


See  \code{\link{NHJ.fun}} for details on default values of \code{r} 
and \code{L}.


The occurrence points in all the processes in \eqn{D} must be part of the input.
Since the number of points  will be possibly different in each  process, a matrix  cannot be used. Instead
 two vectors  with the same length are used: the first one contains  the occrrence points in all 
the processes while  the second one indicates the point process  
in \eqn{D}  where the point in the same row in \code{posD}  has occurred. The codes used in 
\code{typeD} are the column number  where the intensity of that process  is in matrix \code{lambdaD}. 

}


\value{ A list with elements

 \item{r}{Values \eqn{r} where the  F-function is evaluated.}
 \item{NHFr}{Estimated values of the F-function evaluated at values \eqn{r}.}
 \item{T}{Length of the observation period of the process.}
  \item{L}{Net of values in the observation period used to calculate 
the F-function. }

}



\references{
Cronie, O. and van Lieshout, M.N.M. (2015). Summary statistics for inhomogeneous marked point
processes. Ann Inst Stat Math. DOI 10.1007/s10463-015-0515-z

Stoyan et al (2001). On the estimation of distance distribution functions for points
processes and random sets. Image Anal Stereol, 20, 65-69


Van Lieshout, M.N.M. (2006) A J-function for marked point patterns. AISM, 58, 235-259. DOI
10.1007/s10463-005-0015-7
}


\seealso{\code{\link{NHK.fun}}, \code{\link{NHJ.fun}}, \code{\link{NHD.fun}} }


\examples{

set.seed(123)
lambda1<-runif(500, 0.05, 0.1)
pos1<-simNHPc.fun(lambda=lambda1, fixed.seed=123)$posNH

aux<-NHF.fun(lambdaD=lambda1, posD=pos1, typeD=1)
aux$NHFr

#Set D with two processes
#lambda2<-runif(1000, 0.01, 0.2)
#pos2<-simNHPc.fun(lambda=lambda2, fixed.seed=123)$posNH
#NHF.fun(lambdaD=cbind(lambda1,lambda2), posD=c(pos1,pos2), 
#	typeD=c(rep(1, length(pos1)), rep(2, length(pos2))) )

}