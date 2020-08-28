\name{simHPc}
\alias{simHPc}


\title{Generating points in a homogenous Poisson process}

\description{This function generates  a given number of occurrence points  
in a homogenous Poisson process (HPP)  in continuous time. }



\usage{simHPc(lambda, nEv, fixed.seed=NULL)}



\arguments{
  \item{lambda}{Numeric  positive value. Intensity \eqn{\lambda}  used to generate the HPP.}

 \item{nEv}{Optional. Positive integer. Number of points to be generated in the HPPs.}


  \item{fixed.seed}{An integer or NULL. If it is an integer, that is the value  used to set the seed 
in  random  generation processes. It it is NULL, a random seed is used.}

}

\details{The points in a HPP are  generated using 
independent exponentials with mean \eqn{\lambda}. 

Points in a HPP can also be generated using \code{\link{simNHPc}}. The main difference
is that  in \code{\link{simHPc}} the number of points to be generated  is given, while  \code{\link{simNHPc}}
generates points in a period of a given length T.
}



\value{A list with elements:
  \item{posN}{Numeric vector. Occurrence points of the HPP.}
  \item{T}{Length of the period where the given number of points are generated.}
  \item{fixed.seed}{Input argument.}
}



\seealso{ \code{\link{simNHPc}},  \code{\link{IndNHPP}}}


\references{


Ross, S.M. (2006). \emph{Simulation.} Academic Press.

}

\examples{
aux<-simHPc(lambda=0.01, nEv=50,fixed.seed=123)
aux$posH


}



