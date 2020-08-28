\name{CountingCor}
\alias{CountingCor}
\alias{countP}


\title{Correlation between the  counting variables in two point processes}

\description{This function  calculates the correlation coefficient between the  number of points
in intervals  of length \eqn{ll},  in two point processes.}



\usage{CountingCor(posx, posy, ll, T,  method='spearman', lambdax=NULL, 
	lambday=NULL)}


\arguments{

  \item{posx}{Numeric vector.  Occurrence times of the  points in the first  point process.}

  \item{posy}{Numeric vector.  Occurrence times of the  points in the second  point process.}

  \item{ll}{Numeric value. Lenght of the intervals where the number of points are counted.}

  \item{T}{Numeric value. Length of the observed period of the point processes.}

  \item{method}{Character string.  Correlation coefficient  to be calculated. One of "pearson", "kendall", 
	or "spearman"; 	see \code{\link{cor.test}} for definitions.}

  \item{lambdax}{Numeric vector. Intensity vector  of the first point process.}

  \item{lambday}{Numeric vector. Intensity vector  of the second point process.}


}

\details{

 This function calculates \eqn{\rho_{xy,I_l}=Cor(X_{I_l}, Y_{I_l})}, where  
\eqn{X_{I_l}} and \eqn{Y_{I_l}} are the number of points  in an  interval \eqn{I_l} 
in processes \eqn{N_x} and \eqn{N_y}, respectively.

In order to calculate the number of points in each interval in a process, the function  \code{CountP} is used.

}

\value{
  \item{ccor}{Estimated correlation.}
}


\seealso{\code{\link{depchi}},\code{\link{BinPer}}  }

\examples{
#generation of two HPP
aux1<-simNHPc(lambda=rep(0.08,200),fixed.seed=123)
aux2<-simNHPc(lambda=rep(0.12,200),fixed.seed=125)

CountingCor(aux1$posNH, aux2$posNH,ll=20, method="kendall",T=200)
}