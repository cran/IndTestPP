\name{simNHPc.fun}
\alias{simNHPc.fun}
\alias{buscar}


\title{Generate the occurrence points of a Poisson process in continuous time}

\description{This function generates the occurrence points  
of a homogenous or nonhomogeneous Poisson process (NHPP) with  a given  intensity \eqn{\lambda(t)}, 
in a  continuous period  of time (0, T). 

It calls the auxiliary function \code{buscar} (not intended
for the users), see Details section.}


\usage{simNHPc.fun(lambda, fixed.seed=NULL)}



\arguments{
  \item{lambda}{Numeric vector of length T, the obsevation period  length. Intensity \eqn{\lambda(t)}  used to generate the PP.}

  \item{fixed.seed}{An integer or NULL. If it is an integer, that is the value  used to set the seed 
in  random  generation processes. It it is NULL, a random seed is used.}

}

\details{The generation of the NHPP points consists in two steps. 
First,  the points of a homogeneous PP  of intensity 1 are generated using 
independent exponentials. Then, the  homogeneous occurrence times are transformed into 
the points  of a  non homogeneous process with intensity \eqn{\lambda(t)}.
This transformation is performed by the auxiliary function \code{buscar} 
(not intended for the user).

The lenght of the period where the processes are generated is  determined by the length of 
the argument \code{lambda}.

Homogenous  processes  are  generated if the intensity vector  \code{lambda} is  constant 
(that is if all the values are equal).}

\value{A list with elements
  \item{posNH }{Numeric vector. Occurrence points of the PP.}
  \item{lambda }{Input argument.}
  \item{fixed.seed}{Input argument.}
}



\seealso{ \code{\link{simHPc.fun}},  \code{\link{IndNHPP.fun}}}


\references{

Cebrian, A.C., Abaurrea, J. and Asin, J. (2015). {NHPoisson}: An {R} Package for 
Fitting and Validating Nonhomogeneous Poisson Processes. 
\emph{Journal of Statistical Software}, 64(6), 1-24.

Ross, S.M. (2006). \emph{Simulation.} Academic Press.

}

\examples{

#Generation  of a HPP 
aux<-simNHPc.fun(lambda=rep(0.1,200),fixed.seed=123)
aux$posNH


#Generation of a NHPP 
set.seed(123)
lambdat<-runif(500, 0.01,0.1)
aux<-simNHPc.fun(lambda=lambdat,fixed.seed=123)
aux$posNH
}



