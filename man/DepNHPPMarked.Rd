\name{DepNHPPMarked}
\alias{DepNHPPMarked}


\title{Generating dependent  point processes from  a marked Poison Process}

\description{ This function  generates \eqn{d} dependent  (homogeneous or nonhomogeneous) point processes 
using a marked Poisson process, where the marks are generated by a Markov chain process defined by a  transition matrix.  
}

\usage{DepNHPPMarked(lambdaTot, MarkovM, inival = 1, dplot=TRUE, fixed.seed=NULL,...)}


\arguments{
  \item{lambdaTot}{Numeric vector. Intensity values  of the Poisson process used to generate the dependent processes.}

  \item{MarkovM}{Matrix. Trasition probabilities of  the d-state Markov chain used to generate the marks of the process.}

  \item{inival}{Optional. Initial  mark value used to generate the series of marks.}

  \item{dplot}{Optional. A logical flag. If it is TRUE,  the  marginal processes  are plotted.}

  \item{fixed.seed}{Optional. An integer or NULL. Value used to set the seed 
in random generation processes; if it is NULL, a random seed is used.}

  \item{...}{Further arguments to  be passed to the function \code{\link{plot}}.}
}

\details{

Points  of the marked Poisson process are generated in continuous time, using the  following procedure: 
First, a trajectory of the underlying Poisson process is generated.  Then, the  mark series is generated 
using a d-state Markov chain. The mark series takes values in 1,2,...,d and determines in which of the d processes  
the points occur.

The  marginal processes defined by the marks  are not Poisson  unless the generated marks are
independent observations,  see Isham (1980).  

A transition matrix \eqn{P = (p_{ij})} with equal rows leads to \eqn{d} independent point processes, and the more 
similar the rows of P, the less dependent the resulting processes.   The spectral gap (\code{\link{SpecGap}}) 
measures  the dependence between the generated processes,   see Abaurrea et al. (2014).

Tha marginal processes of the marked process can be optionally plotted using \code{dplot=TRUE}.
}


\value{
 A list with elements
  \item{posNH}{A list of  \eqn{d} vectors,  containing the occurrence points in each  marginal point process. The name of the 
	elements of the list are N1, N2,..., Nd.}
  \item{posNHG}{Numeric vector of the occurrences times of the generated Poisson process.}
  \item{mark}{Vector of the  generated marks.}
  \item{lambdaTot}{Input argument.}
  \item{MarkovM}{Input argument.}
}

\references{
Abaurrea, J. Asin, J. and Cebrian, A.C. (2015). A Bootstrap Test of Independence Between Three Temporal Nonhomogeneous Poisson Processes 
and its Application to Heat Wave Modeling. \emph{Environmental and Ecological Statistics},  22(1), 127-144.

Isham, V. (1980). Dependent thinning of point processes. \emph{J. Appl. Probab.}, 17(4), 987-95.
}



\seealso{\code{\link{DepNHPPqueue}}, \code{\link{DepNHNeyScot}}, \code{\link{DepNHCPSP}}, 
	\code{\link{IndNHPP}}, \code{\link{SpecGap}}  }

\examples{

# Generation of three dependent  point processes using a marked PP
set.seed(123)
lambdaTot<-runif(1000)/10
aux<-DepNHPPMarked(lambdaTot=lambdaTot,
	MarkovM=cbind(c(0.3,0.1,0.6), c(0.1, 0.6, 0.3), c(0.6, 0.3,0.1)),fixed.seed=123)
print(cbind(aux$posNH, aux$mark))
print(aux$posNHs)
}
