\name{CondTest.fun}
\alias{CondTest.fun}
\alias{calcNmu}


\title{Conditional Test of independence between two Poisson process}

\description{ It calculates a test of independence between 
two Poisson process (PP), based on the analysis of the occurrences in the second process, given that there is an occurrence in
 the first one.  Two different approaches to calculate  the p-value are implemented. 

It calls the  auxiliary 
function  \code{calcNmu}, not intended for the users.}

\usage{
CondTest.fun(pos1, pos2, lambda2, r, changer = TRUE, type = "All", plotRes = FALSE, ...)
}

\arguments{
  \item{pos1}{Numeric vector. Occurrence points in the first PP, \eqn{N_1}}

  \item{pos2}{Numeric vector. Occurrence points in the second PP, \eqn{N_2}}

  \item{lambda2}{Numeric vector. Intensity in each  time in \eqn{N_2}}

  \item{r}{Numerical value. The radius of the intervals centered on the occurrence times in \eqn{N_1}}

  \item{changer}{Optional. Logical flag. If it is TRUE, when the defined intervals overlap, their lengths are 
changed to  obtain not overlapped intervals. The two  overlapped intervals are shortened by  half of the overlapped
period.  In general, the resulting intervals are not centered .}

  \item{type}{Optional. Label 'Poisson',  'Normal' or 'All'. Approach to be used to calculate test p-values.}

  \item{plotRes}{Logical flag. If it is TRUE,  residuals are  plotted.}


  \item{\dots}{Further arguments to pass to the  residual \code{\link{plot}}.}
}


\details{The  underlying idea of the tests is to analyze the behaviour of the  second PP \eqn{N_2}, given that
 a point has occured in  the first one, \eqn{N_1}. Under independence between \eqn{N_1} and \eqn{N_2}, \eqn{N_2}
 should be a PP with intensity lambda2.

 Fo the analysis,   intervals of length 2r centered on  each point in \eqn{N_1} are defined. To analyze
 the behaviour of \eqn{N_2}, two approaces are implemented, both based on the idea that the number  of points 
in each interval should be a Poisson  of mean \eqn{\mu_i} equal to the integral of lambda2 in the interval.

'Poisson' option: under the null,  and if the intervals  are independent (that is if they do not overlap)
 the number of points in all them should be a Poisson of mean  \eqn{\mu}, equal to the sum of all the \eqn{\mu_i}.
 The p-values is 
calculated as  \eqn{2*min (  (P(X<po)+P(X=po)/2), (P(X>po)+P(X=po)/2))}, where X is a r.v with distibution Poisson(\eqn{\mu})
and po is the sum of the observed number 
of points in all the intervals. Obviously,since the p-value  is calculated from a discrete distribution, it will not be 
uniformly distributed, and the size of the test P( reject H0 when it is true) will not be 0.05. The size depends
on the value of the Poisson mean, only when it X can be approximated by a Normal, the size will be 0.05




'Normal' option: under  the null,  the variables \eqn{(N_i-\mu_i)/(\mu_i**0.5)}  must be
 zero mean  and variance one variables but they are not identically distributed, and consequently
 some approximation has to be used to calculate the p-value.  Since under the null,  each \eqn{N_i} is a Poisson (\eqn{\mu_i}), 
if the means mui are high enough, each variable \eqn{(N_i-\mu_i)/(\mu_i**0.5)} can be approximated by a N(0,1) 
distribution.  If \eqn{\mu_i} are not high enough, still, the mean of the variables  \eqn{(N_i-\mu_i)/(\mu_i**0.5)}
 can be approximated by 
a Normal distribution using the   Central limit theorem  under the Lindeberg condition for r.v which are independent 
but not identically distributed. To apply this approximation a  big 
sample size (number of points in \eqn{N_1}) is needed. Empirically, it has been seen that the smaller the values of mui,
a bigger sample size is required to have a valid  normal approximation. As a reference, with mui values 
between 0 and 1, a sample size of  50 is satisfactory

Hence, if the processes  are independent, the variables \eqn{(N_i-\mu_i)/(\mu_i**0.5)}  must have
 zero mean  and variance one, and \eqn{Nn} the sample mean of  \eqn{(N_i-\mu_i)/(\mu_i**0.5)}  can be approximated  by a 
\eqn{N(0, 1/n**0.5)} distribution, so that the p-value is calculated as \eqn{2*P(Nn>abs(Nn0))=2*P(Z>abs(Nn0)*n**0.5)}. 


}

\value{
  \item{Ni }{Number of  occurrences in each  interval}
  \item{mui }{Theoretical mean of the number of  occurrences in each  interval under the independence assumption}
  \item{Res }{ Residual vector obtained from each  interval}
  \item{pvP }{P-value obtained from the 'Poisson' approach}
  \item{PvN }{P-value obtained from the 'Normal' approach}
  \item{linf}{Lower limit of each interval}
  \item{lsup}{Upper limit of each interval}
}

\references{
  
Billingsley, P. (1995). Probability and Measure. 3rd Ed. John Wiley and sons.


}



\seealso{ \code{\link{TestIndNH.fun}}, \code{\link{DutilleulPlot.fun}} }


\examples{

#Two dependent Poisson processes from  a NHCPSP
set.seed(123)
lambdao1<-runif(1000)/10
set.seed(124)
lambdao2<-runif(1000)/10
set.seed(125)
lambda12<-runif(1000)/20
aux<-DepNHCPSP.fun(lambdai1=lambdao1, lambdai2=lambdao2, lambdai12=lambda12,
	fixed.seed=123)

zz<-CondTest.fun(pos1=aux$posNH1,pos2=aux$posNH2,lambda2=aux$lambda2, r=3)
zz$pvP
zz$pvN


}
