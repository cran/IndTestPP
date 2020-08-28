\name{BinPer}
\alias{BinPer}

\title{Percentage of concordant intervals}
\description{It calculates the proportion of  the number of intervals with at least 
one point in both processes, and  the sum of  the number 
of intervals with at least one point in  one process, \eqn{n_{x,y}/(n_{x,y}+n_{x,0}+n_{0,y})}.}

\usage{BinPer(posx, posy, ll, T)}

\arguments{
  \item{posx}{Numeric vector. Occurrence points in the first process, \eqn{N_x}.}
  \item{posy}{Numeric vector. Occurrence points in the second process, \eqn{N_y}.}
  \item{ll}{Numeric value. Lenght of the intervals where the number of points are counted.}
  \item{T}{Numeric value. Length of the observed period of the point processes.}
}


\details{In order to be useful, an adequate length of interval ll,  depending on the expected dependence,
has to be selected.}
\value{
  \item{binper}{Percentage of concordant intervals.}
}

\seealso{ \code{\link{depchi}}, \code{\link{CountingCor}}
}

\examples{
#generation of two HPP
aux1<-simNHPc(lambda=rep(0.08,200),fixed.seed=123)
aux2<-simNHPc(lambda=rep(0.12,200),fixed.seed=125)
BinPer(aux1$posNH, aux2$posNH,ll=5, T=200)
}