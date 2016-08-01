\name{CPSPpoints.fun}
\alias{CPSPpoints.fun}

\title{Calculates the  occurrence times of the three indicator processes of a bivariate
Common Poisson shock process}


\description{This function calculates the  occurrence times of the points
 in the three indicator processes of a bivariate Common Poisson shock process (CPSP), using as input 
information, the two marginal processes \eqn{N_1} and \eqn{N_2}.
}

\usage{CPSPpoints.fun(X, Y, date = NULL)}

\arguments{
  \item{X}{Binary vector. The first CPSP marginal process; occurrence points are marked with 1 and the other with 0.}
  \item{Y}{Binary vector.  The second CPSP marginal process; occurrence points are marked with 1 and the other with 0.}

  \item{date}{Optional. A vector or matrix indicating the date of each observation.}
}

\details{
A  bivariate CPSP \eqn{N}   is usually specified by its two marginal,  and possibly dependent, processes
\eqn{N_1} and \eqn{N_2}, which are the  observed processes. However,  \eqn{N} can be  decomposed into 
three independent indicator processes: \eqn{N_{(1)}}, \eqn{N_{(2)}}  
and \eqn{N_{(12)}}, which are the processes of the points  occurring
only in the first  marginal process, only in the second and  in both of them (simultaneous points).
The union of  \eqn{N_{(1)}}  and \eqn{N_{(12)}}, and \eqn{N_{(2)}}  
and \eqn{N_{(12)}}  gives  respectively the two marginal processes. 
}

\value{A list with components
  \item{PxX }{Vector of the  occurrence points  in \eqn{N_{(1)}}.}
  \item{PxY}{Vector of the  occurrence points  in \eqn{N_{(2)}}.}
  \item{PxXY}{Vector of the  occurrence points  in \eqn{N_{(12)}}.}
  \item{X}{Input argument.}
  \item{Y}{Input argument.}
  \item{date}{Input argument.}
}

\references{
Abaurrea, J. Asin, J. and Cebrian, A.C. (2015). A Bootstrap Test of Independence Between Three Temporal Nonhomogeneous Poisson Processes 
and its Application to Heat Wave Modeling. \emph{Environmental and Ecological Statistics}, 22(1), 127-144.
}


\seealso{
\code{\link{CPSPPOTevents.fun}}
}


\examples{

set.seed(123)
X<-as.numeric(runif(100)<0.10)
set.seed(124)
Y<-as.numeric(runif(100)<0.15)

aux<-CPSPpoints.fun(X=X,Y=Y)
}