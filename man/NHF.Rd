\name{NHF}
\alias{NHF}
\alias{NHFaux}



\title{Estimating the F-function}

\description{
This function estimates the  F-function in a set  of homogenous or nonhomogeneous  point processes, \eqn{D}. 
The F-function  is evaluated in a grid of  values \eqn{r},  and it can be optionally plotted.


It calls the auxiliary functions NHFaux and other functions not intended for users.}

\usage{NHF(lambdaD, T=NULL, Ptype='inhom', posD, typeD=1, r=NULL,L=NULL, dplot=TRUE,
tit='F(r)',...)}


\arguments{


  \item{lambdaD}{A matrix of positive values. Each column is the intensity vector of one of the point process in 
\eqn{D}. If there is only one process in \eqn{D}, it can be a vector or even a numeric value
if the process is homogeneous.}

  \item{T}{Numeric value. Length of the observed period. It only must be specified
 if the number of rows in \code{lambdaC} and \code{lambdaD} is 1.}

  \item{Ptype}{Optional. Label: "hom" or "inhom".  The first one indicates that 
all the point processes in sets \eqn{C} and \eqn{D} are homogeneous. }


 \item{posD}{Numeric vector.  Occurrence times of the  points in all the point processes in \eqn{D}.}

  \item{typeD}{Numeric vector with the same length as \code{posD}.  Code of the point process  
in \eqn{D}  where the point  in the same row in \code{posD}  has occurred. The code must be the  
column number  where the intensity of that process  is in matrix \code{lambdaD}. }

  \item{r}{Numeric vector. Values where the  F-function  must be evaluated. If it  is NULL, a default 
vector is used, see Details}

  \item{L}{Optional. Numeric vector. Values in the observed period used to calculate 
the  F-function. If it  is NULL, a default vector is used, see Details.}

  \item{dplot}{Optional. Logical flag. If it is true, the  F-function is plotted.}

  \item{tit}{Optional.  The title to be used in the plot of the F-function.}


  \item{...}{Further arguments to  be passed to  the function \code{\link{plot}}.}

}




\details{

The information about the processes  is provided by  arguments \code{posD},  the vector of all the occurrence times  
in the processes in \eqn{C}, and  \code{typeD},  the vector of  the code of the point process  in set \eqn{D}
where each point in \code{posD} has occurred.

This function estimates the  F-function in a set \eqn{D} of homogenous or nonhomogeneous time point processes,  see 
Cebrian et al (2020) for details of the estimation. 
The F-function, also known as empty space function,  is the distribution function of 
the distances from an arbitray point in  the space to the nearest point in a process in \eqn{D}.
In homogeneous processes, it  estimates  the probability that  at least one point  in  processes
in \eqn{D} occurs  at a  distance lower than \eqn{r} of an arbitray point in the space. 
If the processes are nonhomogenous, the  inhomogenous  version of the function, adjusted for time varying intensities, 
is used. 


If argument \code{r} is NULL,   the following grid is used to evaluate the function

		 r1<-max(20, floor(T/20))

		 r<-seq(1,r1,by=2)

		 if (length(r)>200) r<-seq(1,r1,length.out=200)


If argument \code{L} is NULL, the following grid is used 

        L <- seq(1, T, by = 2)
        if (length(L) > 5000)  L <- seq(1, T, by = round((T - 1)/199))

}


\value{ A list with elements:


 \item{r}{Vector of values \eqn{r} where the  F-function is estimated.}
 \item{NHFr}{Estimated values of \eqn{F_{D}(r)}.}
 \item{T}{Length of the observed period of the process.}
 \item{L}{Grid of  L values to calculate the F-funtion. }

}



\references{
Cebrian, A.C., Abaurrea, J.  and Asin, J. (2020).   Testing  independence between two  point processes in time.
 \emph{Journal of Simulation and Computational Statistics}.
}


\seealso{\code{\link{NHK}}, \code{\link{NHJ}}, \code{\link{NHD}} }


\examples{

set.seed(123)
lambda1<-runif(500, 0.05, 0.1)
pos1<-simNHPc(lambda=lambda1, fixed.seed=123)$posNH

aux<-NHF(lambdaD=lambda1, posD=pos1, typeD=1)
aux$NHFr

#Set D with two processes ***
#lambda2<-runif(1000, 0.01, 0.2)
#pos2<-simNHPc(lambda=lambda2, fixed.seed=123)$posNH
#aux<-NHF(lambdaD=cbind(lambda1,lambda2), posD=c(pos1,pos2), 
#	typeD=c(rep(1, length(pos1)), rep(2, length(pos2))) )
#aux$NHFr

}