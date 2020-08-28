\name{CondTest}
\alias{CondTest}
\alias{calcNmu}


\title{Conditional test of independence between two Poisson process}

\description{It calculates a test of independence between 
two Poisson process, based on the analysis of the occurrences in the second process, 
given that there is an occurrence in the first one.  Two different approaches to calculate  
the p-value are implemented. 

It calls the  auxiliary function  \code{calcNmu}, not intended for the users.}

\usage{
CondTest(posx, posy, lambday, r, changer = TRUE, type = "All", 
	plotRes = FALSE, ...)
}

\arguments{
  \item{posx}{Numeric vector. Occurrence points in the first process, \eqn{N_x}.}

  \item{posy}{Numeric vector. Occurrence points in the second process, \eqn{N_y}.}

  \item{lambday}{Numeric vector. Intensity at each  time in \eqn{N_y}.}

  \item{r}{Numeric value. The radius of the intervals centered on the occurrence times in \eqn{N_x}.}

  \item{changer}{Optional. Logical flag. If it is TRUE, when the defined intervals overlap, their lengths are 
changed to  obtain disjoint intervals. The two  overlapping intervals are shortened by  half of the overlapped
period.  In general, the resulting intervals are not centered.}

  \item{type}{Optional. Label "Poisson",  "Normal" or "All". Approach to be used to calculate test p-values.}

  \item{plotRes}{Logical flag. If it is TRUE,  the  residual differences   \eqn{(y_i-\mu_i)/\mu_i^{(1/2)}} are  
	plotted.}


  \item{\dots}{Further arguments to  be passed to the function  \code{\link{plot}}.}
}


\details{The  underlying idea of the tests is to analyze the behaviour of the  second process \eqn{N_y}, given that
 a point has occured in  the first one, \eqn{N_x}. Under independence between \eqn{N_x} and \eqn{N_y}, \eqn{N_y}
 should be a Poisson process with intensity lambday.

Intervals of length 2r centered on  each point in \eqn{N_x} are defined. To analyze
 the behaviour of \eqn{N_y}, two approaces are implemented, both based on the idea that the number  of points 
in each interval should be a Poisson  of mean \eqn{\mu_i} equal to the integral of lambday in the interval.

"Poisson" option: under the null,  and if the intervals  are independent (that is if they do not overlap)
 the number of points in all them should be a Poisson of mean  \eqn{\mu}, equal to the sum of all the \eqn{\mu_i}.
 The p-values is calculated as  \eqn{2*min (  (P(Y<yo)+P(Y=yo)/2), (P(X>yo)+P(Y=yo)/2))}, where Y is a r.v.
 with distibution Poisson(\eqn{\mu}) and \eqn{yo} is the sum of the observed number 
of points in all the intervals.   Since  the  p-values are based on a discrete distribution, they 
are valid but not exact p-values.

"Normal" option: under  the null,  the variables \eqn{(N_i-\mu_i)/(\mu_i^{1/2})}  must be
 zero mean  and variance one variables but they are not identically distributed. Under general conditions, 
the mean of the variables  \eqn{(N_i-\mu_i)/(\mu_i^{1/2})} can be approximated by 
a Normal distribution using the   Central limit theorem  under the Lindeberg condition for r.v which are independent 
but not identically distributed.   The conditions  to have a valid Normal aprroximation are quite weak,  even  
with  a complex intensity, mean values  of \eqn{\mu_i}  around 0.6 are valid with \eqn{n_x=50}, and around  
0.3 with \eqn{n_x=100}.


}

\value{ A list with elements
  \item{pvP}{P-value obtained with the 'Poisson' approach.}
  \item{PvN}{P-value obtained with the 'Normal' approach.}
  \item{Ni}{Number of  occurrences in each  interval.}
  \item{mui}{Theoretical mean of the number of  occurrences in each  interval under the independence assumption.}
  \item{Res}{Vector of th residual differences.}
  \item{linf}{Lower bound of each interval.}
  \item{lsup}{Upper bound of each interval.}
  \item{mmu}{ Mean of the mui vector. It is used to  check the conditions of the approximation of the Normal test.}

}

\references{
 Cebrian, A.C., Abaurrea, J.  and Asin, J. (2020).   Testing  independence between two  point processes in time.
 \emph{Journal of Simulation and Computational Statistics}.
}



\seealso{ \code{\link{TestIndNH}},  \code{\link{NHK}},  \code{\link{NHJ}},  \code{\link{DutilleulPlot}} }


\examples{

#Two dependent Poisson processes from  a NHCPSP
set.seed(30)
lambdao1<-runif(3000)/20
set.seed(31)
lambdao2<-runif(3000)/10
set.seed(32)
lambda12<-runif(3000)/20
lambdaiM<-cbind(lambdao1,lambdao2,lambda12)
aux<-DepNHCPSP(lambdaiM=lambdaiM, d=2,fixed.seed=123, dplot=FALSE)

zz<-CondTest(posx=aux$posNH[[1]],posy=aux$posNH[[2]],lambday=aux$lambdaM[,2], r=2)
zz$pvP
zz$pvN

# Two independent non homogeneous  Poisson processes  
lambdao1<-runif(6000)/20
set.seed(124)
lambdao2<-runif(6000)/10
aux1<-simNHPc(lambda=lambdao1, fixed.seed=123)
aux2<-simNHPc(lambda=lambdao2, fixed.seed=124)

zz<-CondTest(posx=aux1$posNH, posy=aux2$posNH, lambday= aux2$lambda, r=3)
zz$pvP
zz$pvN

}
