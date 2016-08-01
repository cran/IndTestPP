\name{DepNHCPSP.fun}
\alias{DepNHCPSP.fun}


\title{Generates  a trajectory of  two dependent Poisson  processes from a Common Poisson shock process}

\description{ This function  generates dependent  (homogeneous or nonhomogeneous)  Poisson processes  
obtained as the  two marginal processes of a bivariate Common Poisson shock process. }


\usage{DepNHCPSP.fun(lambdai1, lambdai2, lambdai12,fixed.seed=NULL)}


\arguments{
  \item{lambdai1}{Numeric vector. Intensity values  of  \eqn{N_{(1)}}.}
  \item{lambdai2}{Numeric vector. Intensity values  of  \eqn{N_{(2)}}.}
  \item{lambdai12}{Numeric vector. Intensity values  of  \eqn{N_{(12)}}.}
  \item{fixed.seed}{Optional. An integer or NULL. Value used to set the seed 
	in random generation processes; if it is NULL, a random seed is used.}
}


\details{
A  bivariate CPSP \eqn{N}   is usually specified by its two marginal,  and possibly dependent, processes
\eqn{N_1} and \eqn{N_2}, which are the  observed processes. However,  \eqn{N} can be  decomposed into 
three independent indicator processes: \eqn{N_{(1)}}, \eqn{N_{(2)}}  
and \eqn{N_{(12)}}, which are the processes of the points  occurring
only in the first  marginal process, only in the second and  in both of them (simultaneous points).
The union of  \eqn{N_{(1)}}  and \eqn{N_{(12)}}, and \eqn{N_{(2)}}  
and \eqn{N_{(12)}}  gives  respectively the two marginal processes.  

In a CPSP, the indicator processes are  three independet PPs and the CPSP  distribution   is  completely  
specified by  them. The intensity vector  of  \eqn{N_{1}},
 is \code{lambdai1}+\code{lambdai12} and  the intensity  vector of  \eqn{N_{2}} is 
\code{lambdaoi2}+\code{lambdai12}. Conditionally independent marginal processes  
are obtained if and only if \code{lambdai12}=0.

The decomposition into indicator processes can be readily applied  for data generation, and it reduces
 to the generation of three independet PPs.  Points in each process are generated 
in continuous time.
}



\value{
 A list with elements
  \item{posNH1}{A numeric vector which contains the  points in   \eqn{N_{1}}}
  \item{posNH2}{A numeric vector which contains the  points in  \eqn{N_{2}}}
  \item{posNHi1}{A numeric vector which contains the  points in  \eqn{N_{(1)}} }
  \item{posNHi2}{A numeric vector which contains the  points in  \eqn{N_{(2)}}}
  \item{posNHi12}{A numeric vector which contains the  points in  \eqn{N_{(12)}}}
  \item{lambda1}{Numeric vector  which contains the intensity vector  of  \eqn{N_{1}}}
  \item{lambda2}{Numeric vector  which contains the intensity vector  of  \eqn{N_{2}}}
}


\references{
Abaurrea, J. Asin, J. and Cebrian, A.C. (2015). Modeling and projecting the occurrence of bivariate extreme heat events using a nonhomogeneous
common Poisson shock process. Stochastic and Environmental Research and risk assessment, 29(1), 309-322.

}


\seealso{\code{\link{DepNHNeyScot.fun}}, \code{\link{DepNHPPqueue.fun}}, \code{\link{DepMarkedNHPP.fun}}}

\examples{
set.seed(123)
lambdai1<-runif(200,0,0.1)
set.seed(124)
lambdai2<-runif(200,0,0.07)
set.seed(125)
lambdai12<-runif(200,0,0.15)

aux<-DepNHCPSP.fun(lambdai1=lambdai1, lambdai2=lambdai2, lambdai12=lambdai12,
	fixed.seed=123)

aux$posNH1
aux$posNH2
}
