\name{TranM}
\alias{TranM}

\title{Estimation of the transition matrix of a Markov chain}

\description{
It estimates the transition matrix of a Makov chain  to model the dependence between
the  discrete marks of a marked point process.  The estimator is the MLE based on count data.}

\usage{TranM(marcas = NULL, d = NULL, vecpro = NULL)}


\arguments{
  \item{marcas}{Integer vector. It contains the  discrete marks of the marked point process. The order 
of the  marks in the vector must correspond to  the points in the process sorted over time.}
  \item{d}{Integer. Number of states of the Markov chain, that is the  number of different marks of the 
marked point process.}
  \item{vecpro}{A list with d elements.  Element "i" of the list  must  be a vector including the  occurrence  times of the
points  in the marked point process with marks equal to "i".}
}

\details{
The input of this function must  be a marked point process. It can be defined  by the sequence of marks of 
all the points in the process (arguments \code{marcas} and \code{d}), or alternatively  by a vector of \code{d}
 point processes (argument \code{vecpro}). If  \code{marcas}  or \code{d} are NULL, 
\code{vecpro} must be provided. If they are not NULL, they are used to define the marked Poisson process.
}

\value{ \item{prob}{The  estimated transition matrix of probabilities.} }


\seealso{\code{\link{DepNHPPMarked}}}

\examples{

TranM(marcas = c(1,3,3,2,1,2,1,1), d = 3)

TranM(vecpro=list(n1=c(2,7,9,23), n2=c(4,5,21), n3=c(2,8,9,12,16)))
}

