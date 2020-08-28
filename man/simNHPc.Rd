\name{simNHPc}
\alias{simNHPc}
\alias{buscar}


\title{Generating points in a Poisson process}

\description{This function generates the occurrence points  
in a homogenous or nonhomogeneous Poisson process (NHPP) with  a given  intensity \eqn{\lambda(t)}, 
in a  continuous period  of time (0, T). 

It calls the auxiliary function \code{buscar} (not intended for the users), see Details.
}

\usage{simNHPc(lambda, fixed.seed=NULL, algor="Thinning")}

\arguments{
  \item{lambda}{Numeric vector. Intensity \eqn{\lambda(t)}  used to generate the Poisson process.  Its length determines  
	the length of the observed period.}

  \item{fixed.seed}{An integer or NULL. If it is an integer, that is the value  used to set the seed 
in  random  generation processes. It it is NULL, a random seed is used.}

  \item{algor}{ Optional. Character string.  The algorithm  used to generate the process, it can be
      "Inversion" or "Thinning"; see Details.}

}

\details{Two algorithms  to generate the NHPP points  are implemented. "Inversion" is based on the inversion algortihm, 
see Ross(2006), and it consists in two steps. 
First,  the points of a homogeneous Poisson process  of intensity one are generated using 
independent exponentials. Then, the  homogeneous occurrence times are transformed into 
the points  of a  nonhomogeneous process with intensity \eqn{\lambda(t)}.
This transformation is performed by the auxiliary function \code{buscar} 
(not intended for the user).

The algorithm "Thinning", see Banerjee et al. (2014),  generates  the occurrences times
in a homogeneous Poisson process with intensity \eqn{\lambda_{max}=\max_t \lambda(t)} and the resulting points are retained 
with probability \eqn{\lambda(t_i)/\lambda_{max}}.

The "Inversion" algorithm requires positive values of  the argument \code{lambda} and it is slower, but
the "Thinning" algorithm may yield excesive rejection according to Ross (2006).

The lenght of the period where the processes are generated is  determined by the length of 
the argument \code{lambda}.

Homogenous  processes  are  generated if the intensity vector  \code{lambda} is  constant 
(that is if all the values are equal).
}

\value{A list with elements:
  \item{posNH }{Numeric vector. Occurrence points of the Poisson process.}
  \item{lambda }{Input argument.}
  \item{fixed.seed}{Input argument.}
}

\seealso{ \code{\link{simHPc}},  \code{\link{IndNHPP}}}

\references{

Banerjee, S., Carlin, B.P. and Gelfand, A. E. (2014) \emph{Hierarchical modeling and 
analysis for spatial data}.CRC Press.

Ross, S.M. (2006). \emph{Simulation.} Academic Press.
}

\examples{

#Generation  of a Homogeneous Poisson process 
aux<-simNHPc(lambda=rep(0.1,200),fixed.seed=123, algor='Inversion')
aux$posNH

#Generation of a NHPP 
set.seed(123)
lambdat<-runif(500, 0.01,0.1)
aux<-simNHPc(lambda=lambdat,fixed.seed=123, algor='Thinning')
aux$posNH
}



