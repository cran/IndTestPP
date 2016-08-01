\name{NHD.fun}
\alias{NHD.fun}
\alias{NHDaux}
\alias{prodN2}
\alias{HDFaux}
\alias{nearestD}
\alias{nMenr}

\title{It estimates the cross  D-function  for two sets of point processes}

\description{This function estimates
the  cross nearest neighbour distance distribution function, D,  between 
two sets, \eqn{C} and \eqn{D}, of (homogenous or nonhomogeneous) point processes. 
The D-function  is evaluated in a grid of  values \eqn{r},  and it
 can be optionally plotted.

It calls the auxiliary functions NHDaux.fun and  other  functions, not intended for users.}

\usage{
NHD.fun(lambdaC, lambdaD, T=NULL,Ptype='inhom', posC, typeC=1, posD, typeD=1, 
	r = NULL, dplot = TRUE, tit = "D(r)")}


\arguments{ 
\item{lambdaC}{A matrix. Each column is the intensity vector of one of the point processes in 
\eqn{C}. If there is only one process in \eqn{C}, it can be a vector or even a numeric value
if the process is homogeneous.}

  \item{lambdaD}{A matrix. Each column is the intensity vector of one of the point process in 
\eqn{D}. If there is only one process in \eqn{D}, it can be a vector oe even a numeric value
if the process is homogeneous.}

  \item{T}{Numeric value. Length of the observation period. It only must be specified
 if all the processes are homogeneous the number of rows in \code{lambdaC} and \code{lambdaD} is 1.}

  \item{Ptype}{Optional. Label: 'hom' or 'inhom'.  The first one indicates that 
all the point processes in sets \eqn{C} and \eqn{D} are homogeneous. In that case,  columns of arguments
\code{lambdaC} and \code{lambdaD} can be a  number or a constant vector.}


  \item{posC}{Numeric vector.  Time position of the  points in all the point processes in \eqn{C}.}

  \item{typeC}{Numeric vector with the same length as \code{posC}.  Code of the point process  
in \eqn{C}  where  points in \code{posC} have occurred. See Details.}


  \item{posD}{Numeric vector.  Time position of the  points in all the point processes in \eqn{D}.}

  \item{typeD}{Numeric vector with the same length as \code{posD}.  Code of the point process  
in \eqn{D}  where  points in \code{posD} have occurred. }

  \item{r}{Numeric vector. Values where D-function  must be evaluated.}

  \item{dplot}{Optional. A logical flag. If it is TRUE, a plot of the D-function is shown.}

  \item{tit}{Optional.  The title to be used in the plot of the D-function. }
}

\details{
This function estimates the  cross nearest neighbour distance disribution function D, between 
two sets \eqn{C} and \eqn{D} of point processes, using the estimator suggested by 
Cronie and van Lieshout (2015), adapted to time point processes. 

The  D-function  is the distribution function of the  distances from a point in a process 
in \eqn{C} to the nearest point in a process  \eqn{D}. 
In homogeneous proceesses, it  estimates the probability that  at least one point  
in a process  in set \eqn{D} occurs  at a  distance lower than \eqn{r} of a given point in a process in set \eqn{C}. 
If the processes are nonhomogenous, the  inhomogenous  version of the function, adjusted for time varying intensities, 
is used. It is calculated using the Hanisch estimator,  see Van Lieshout (2006)


Small values of cross D-function suggest  few points in processes in \eqn{D} in the r-neighbourhood 
of points of processes in \eqn{C}.
Large values indicate that points in processes in \eqn{D} are attracted by those of processes in \eqn{C}.

For inference about independence of the processes, K and J-functions should be used.

The occurrence points in all the processes in \eqn{C} must be part of the input.
Since the number of points  will be possibly different in each  process, a matrix  cannot be used. Instead
 two vectors  with the same length are used: the first one contains  the occrrence points in all 
the processes while  the second one indicates the point process  
in \eqn{C}  where the point in the same row in \code{posC}  has occurred. The codes used in 
\code{typeC} are the column number  where the intensity of that process  is in matrix \code{lambdaC}. 
The same for set \eqn{D}. 

See  \code{\link{NHJ.fun}} for details on default values of \code{r} 
and \code{L}.
}


\value{ A list with elements
 \item{r}{Vector of values where  D-function is evaluated.}
 \item{NHDr}{Estimations of D-function at values \eqn{r}.}
 \item{T}{Length of the observation period.}
}


\references{
Cronie, O. and van Lieshout, M.N.M. (2015). Summary statistics for inhomogeneous marked point
processes. Ann Inst Stat Math. DOI 10.1007/s10463-015-0515-z

Stoyan et al (2001). On the estimation of distance distribution functions for points
processes and random sets. Image Anal Stereol, 20, 65-69

Van Lieshout, M.N.M. (2006) A J-function for marked point patterns. AISM, 58, 235-259. DOI
10.1007/s10463-005-0015-7
}



\seealso{\code{\link{NHK.fun}}, \code{\link{NHJ.fun}}, \code{\link{NHF.fun}} }


\examples{

#Sets  C and D with one  independent NHPP
set.seed(123)
lambda1<-runif(500, 0.05, 0.1)
set.seed(124)
lambda2<-runif(500, 0.01, 0.2)
pos1<-simNHPc.fun(lambda=lambda1, fixed.seed=123)$posNH
pos2<-simNHPc.fun(lambda=lambda2, fixed.seed=123)$posNH

aux<-NHD.fun(lambdaC=lambda1, lambdaD=lambda2, posC=pos1, typeC=1, posD=pos2, typeD=1)
aux$NHDr


#Sets  C and D with two independent NHPPs
#pos3<-simNHPc.fun(lambda=lambda1, fixed.seed=321)$posNH
#pos4<-simNHPc.fun(lambda=lambda2, fixed.seed=321)$posNH

#NHD.fun(lambdaC=cbind(lambda1,lambda2), lambdaD=cbind(lambda1,lambda2), posC=c(pos1,pos2), 
#	typeC=c(rep(1, length(pos1)), rep(2, length(pos2))), posD=c(pos3, pos4), 
#	typeD=c(rep(1, length(pos3)), rep(2, length(pos4))))



}