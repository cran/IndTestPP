\name{simHPc.fun}
\alias{simHPc.fun}


\title{Generate  a given number of occurrence points in a homogenous Poisson process in continuous time}

\description{This function generates  a given number of occurrence points  
in a homogenous Poisson process (HPP)  in continuous time. }



\usage{simHPc.fun(lambda, nEv, fixed.seed=NULL)}



\arguments{
  \item{lambda}{Numeric value. Intensity \eqn{\lambda}  used to generate the PP.}

 \item{nEv}{Optional. Positive integer. Number of points to be generated in the PPs.}


  \item{fixed.seed}{An integer or NULL. If it is an integer, that is the value  used to set the seed 
in  random  generation processes. It it is NULL, a random seed is used.}

}

\details{The points in a HPP are  generated using 
independent exponentials with mean \eqn{\lambda}. 

Points in a HPP can also be generated using \code{\link{simNHPc.fun}}. The main difference
is that here, the number of points to be generated  is given, while in the other function,
points are generated in a period of a given length T.
}



\value{A list with elements
  \item{posN}{Numeric vector. Occurrence points of the PP.}
  \item{T}{Length of the period where the given number of points are generated.}
  \item{fixed.seed}{Input argument.}
}



\seealso{ \code{\link{simNHPc.fun}},  \code{\link{IndNHPP.fun}}}


\references{


Ross, S.M. (2006). \emph{Simulation.} Academic Press.

}

\examples{
aux<-simHPc.fun(lambda=0.01, nEv=50,fixed.seed=123)
aux$posH


}



