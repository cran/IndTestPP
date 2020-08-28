\name{NHD}
\alias{NHD}
\alias{NHDaux}
\alias{prodN2}
\alias{HDFaux}
\alias{nearestD}
\alias{nMenr}

\title{Estimating the D-function}

\description{This function estimates
the  cross nearest neighbour distance distribution function, D,  between 
two sets, \eqn{C} and \eqn{D}, of (homogenous or nonhomogeneous) point processes. 
The D-function  is evaluated in a grid of  values \eqn{r},  and it
 can be optionally plotted.

It calls the auxiliary functions NHDaux and  other  functions, not intended for users.}

\usage{NHD(lambdaC, lambdaD, T=NULL,Ptype='inhom', posC, typeC=1, posD, typeD=1, 
	r = NULL, dplot = TRUE, tit = "D(r)",...)}


\arguments{ 
\item{lambdaC}{A matrix of positive values. Each column is the intensity vector of one of the point processes in 
\eqn{C}. If there is only one process in \eqn{C}, it can be a vector or even a numeric value
if the process is homogeneous.}

  \item{lambdaD}{A matrix of positive values. Each column is the intensity vector of one of the point process in 
\eqn{D}. If there is only one process in \eqn{D}, it can be a vector or even a numeric value
if the process is homogeneous.}

  \item{T}{Numeric value. Length of the observed period. It only must be specified
 if the number of rows in \code{lambdaC} and \code{lambdaD} is 1.}

  \item{Ptype}{Optional. Label: "hom" or "inhom".  The first one indicates that 
all the point processes in sets \eqn{C} and \eqn{D} are homogeneous.}


  \item{posC}{Numeric vector.  Occurrence times of the  points in all the point processes in \eqn{C}.}

  \item{typeC}{Numeric vector with the same length as \code{posC}.  Code of the point process  
in \eqn{C}  where  points in \code{posC} have occurred. See Details.}


  \item{posD}{Numeric vector. Occurrence times of the  points in all the point processes in \eqn{D}.}

  \item{typeD}{Numeric vector with the same length as \code{posD}.  Code of the point process  
in \eqn{D}  where  points in \code{posD} have occurred. }

  \item{r}{Numeric vector.  Values where the D-function  must be evaluated. If it  is NULL, a default 
vector is used, see Details.}

  \item{dplot}{Optional. A logical flag. If it is TRUE,  the D-function is plotted.}

  \item{tit}{Optional.  The title to be used in the plot of the D-function. }

  \item{...}{Further arguments to  be passed to the function \code{\link{plot}}.}
}

\details{

The information about the processes  is provided by  arguments \code{posC},  the vector of all the occurrence times  
in the processes in \eqn{C}, and  \code{typeC},  the vector of  the code of the point process  in set \eqn{C}   where each point in \code{posC} has occurred;  
the second set D is characterized analogously by \code{typeD} and \code{posD}. 


This function estimates the  D-function  between 
two sets,  \eqn{C} and \eqn{D}, of (homogenous or nonhomogeneous)  point processes, see 
Cebrian et al (2020) for details of the estimation.  The  D-function  is the distribution 
function of the  distances from a point in a process 
in \eqn{C} to the nearest point in a process  \eqn{D}. 
In homogeneous proceesses, it  estimates the probability that  at least one point  
in a process  in set \eqn{D} occurs  at a  distance lower than \eqn{r} of a given point in a process in set \eqn{C}. 
If the processes are nonhomogenous, the  inhomogenous  version of the function, adjusted for time varying intensities, 
is used. It is calculated using the Hanisch estimator,  see Van Lieshout (2006)
Small values of the D-function suggest  few points in processes in \eqn{D} in the r-neighbourhood 
of points of processes in \eqn{C}.
Large values indicate that points in processes in \eqn{D} are attracted by those of processes in \eqn{C}.

For inference about independence of the processes, K and J-functions should be used.


If argument \code{r} is NULL,   the following grid is used to evaluate the function

		 r1<-max(20, floor(T/20))

		 r<-seq(1,r1,by=2)

		 if (length(r)>200) r<-seq(1,r1,length.out=200)
}


\value{ A list with elements:
 \item{r}{Vector of values \eqn{r} where the  D-function is estimated.}
 \item{NHDr}{Estimated values of \eqn{D_{CD}(r)}.}
 \item{T}{Length of the observed period.}
}


\references{
Cebrian, A.C., Abaurrea, J.  and Asin, J. (2020).   Testing  independence between two  point processes in time.
 \emph{Journal of Simulation and Computational Statistics}.

Van Lieshout, M.N.M. (2006) A J-function for marked point patterns. AISM, 58, 235-259. DOI
10.1007/s10463-005-0015-7
}



\seealso{\code{\link{NHK}}, \code{\link{NHJ}}, \code{\link{NHF}} }


\examples{

#Sets C and D with  independent NHPPs
set.seed(123)
lambda1<-runif(500, 0.05, 0.1)
set.seed(124)
lambda2<-runif(500, 0.01, 0.2)
pos1<-simNHPc(lambda=lambda1, fixed.seed=123)$posNH
pos2<-simNHPc(lambda=lambda2, fixed.seed=123)$posNH
aux<-NHD(lambdaC=lambda1, lambdaD=lambda2, posC=pos1, typeC=1, posD=pos2, typeD=1)
aux$NHDr

#Example with  independent NHPPs
#pos3<-simNHPc(lambda=lambda1, fixed.seed=321)$posNH
#pos4<-simNHPc(lambda=lambda2, fixed.seed=321)$posNH
#aux<-NHD(lambdaC=cbind(lambda1,lambda2),lambdaD=cbind(lambda1,lambda2),posC=c(pos1,pos2),
# typeC=c(rep(1, length(pos1)), rep(2, length(pos2))), posD=c(pos3, pos4),
# typeD=c(rep(1, length(pos3)), rep(2, length(pos4))))
#aux$NHDr

}