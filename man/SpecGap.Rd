\name{SpecGap}
\alias{SpecGap}


\title{Stationary distribution of a matrix and its spectral gap}

\description{This function calculates the stationary distribution of  the transition matrix 
of a Markov chain process and its spectral gap.}


\usage{SpecGap(P)}


\arguments{
  \item{P}{Matrix. It must be a markovian matrix.}
}

\details{
The spectral gap of a matrix \eqn{P} measures the convergence speed of \eqn{P} to a matrix \eqn{P_I}
with  all the rows  equal to \eqn{(\pi_1,\pi_2,... \pi_k)}, the stationary
distribution of \eqn{P}. It takes values in [0,1].

The spectral gap of a transition matrix can be used as a dependence measure between the  marginal processes
defined by a  marked Poisson procces with  discrete marks generated by a Markov chain  with that transition matrix, see
Cebrian et al (2020) for details.

}

\value{A list with elements
  \item{SG}{Spectral gap value of the matrix.}
  \item{pi}{Vector of the stationary distribution of the matrix.}

}

\seealso{ \code{\link{DepNHPPMarked}}}

\references{
 Cebrian, A.C., Abaurrea, J.  and Asin, J. (2020).   Testing  independence between two  point processes in time.
 \emph{Journal of Simulation and Computational Statistics}.
}

\examples{

P<-cbind(c(0.7, 0.1, 0.2), c(0.2, 0.7, 0.1), c(0.1, 0.2, 0.7))
SpecGap(P)
}



