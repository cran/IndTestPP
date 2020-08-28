\name{DepNHCPSP}
\alias{DepNHCPSP}
\alias{PsimNHPc}


\title{Generating a Common Poisson Shock Process}

\description{This function  generates  the \eqn{d} marginal processes of a Common Poisson Shock Process,  
which are \eqn{d} dependent Poisson processes. 
Both homogeneous  and nonhomogeneous processes can be generated.  In the case \eqn{d=2}, 
the processes can be optionally plotted.}


\usage{DepNHCPSP(lambdaiM, d,dplot=TRUE, pmfrow=c(2,1), fixed.seed = NULL, ...)  }


\arguments{
  \item{lambdaiM}{Matrix. Each column contains the intensity values  of a indicator process .}
  \item{d}{Numeric value.  Dimension (number of marginal processes) of the CPSP.}
  \item{dplot}{Optional. A logical flag. If it is TRUE and d=2,  the  marginal and indicator processes  are plotted.}
  \item{pmfrow}{Optional.  A vector of  the form (nr, nc) to be supplied as argument \code{mfrow} in  \code{\link{par}}. }
  \item{fixed.seed}{Optional. An integer or NULL. Value used to set the seed 
	in random generation processes; if it is NULL, a random seed is used.}
  \item{...}{Further arguments to  be passed to  the function \code{\link{plot}}.}
}


\details{
A   CPSP \eqn{N}   is usually specified by its  marginal,  and possibly dependent, processes
\eqn{N_1},  \eqn{N_2}...,  \eqn{N_d}, which are the  observed processes. However,  \eqn{N} can be  decomposed into 
 m independent indicator processes: \eqn{N_{(1)}}, \eqn{N_{(2)}}, ..., \eqn{N_{(12)}}, ..., \eqn{N_{(1...d)}},
 which are the processes of the points  occurring
only in the first  marginal process, only in the second,...,  simultaneously in  the two first marginal processes, ...
and  in all the marginal processes simultaneously.  The number of indicator processes is m, the sum of 
n choose i for \eqn{i=1, ..., d}. The value m must also be the number of columns of the matrix in argument \code{lambdaiM}.
The marginal process \eqn{N_{i}}  is obtained as the union of all the indicator processes where the index i appears, 
\eqn{N_{.i.}}. The intensity of  \eqn{N_{i}} is the sum of the intensities of all the indicator processes \eqn{N_{.i.}}.

The decomposition into indicator processes can be readily applied  for the generation of a CPSP: it reduces
 to the generation of m independet PPs, see Cebrian et al. (2020) for details.  
Points are generated in continuous time.

 In order to  generate d independent Poisson processes, the function  \code{\link{IndNHPP}} has be used. 


In the bivariate case \eqn{d=2}, the  points in the  marginal  \eqn{N_{1}}, \eqn{N_{2}}  and indicator  
\eqn{N_{(1)}}, \eqn{N_{(2)}}  and \eqn{N_{(12)}}  processes can be optionally plotted. 
}

\value{
 A list with elements
  \item{posNH}{A list of d vectors containing the occurrence points of the d marginal processes. 
The name of the elements of the list are N1, N2...,Nd.}
  \item{posNHG}{A list of m vectors containing the occurrence points of the m indicator processes.}
  \item{lambdaM}{Matrix. Each column is the intensity vector  of  a marginal process.}
}


\references{
Abaurrea, J. Asin, J. and Cebrian, A.C. (2015). Modeling and projecting the occurrence of bivariate extreme heat events using a nonhomogeneous
common Poisson shock process. Stochastic and Environmental Research and risk assessment, 29(1), 309-322.

Cebrian, A.C., Abaurrea, J.  and Asin, J. (2020).   Testing  independence between two  point processes in time.
 \emph{Journal of Simulation and Computational Statistics}.

}


\seealso{\code{\link{DepNHNeyScot}}, \code{\link{DepNHPPqueue}}, \code{\link{DepNHPPMarked}}, \code{\link{IndNHPP}}}

\examples{

set.seed(123)
lambdai1<-runif(200,0,0.1)
set.seed(124)
lambdai2<-runif(200,0,0.07)
set.seed(125)
lambdai12<-runif(200,0,0.05)
set.seed(126)
lambdai123<-runif(200,0,0.01)
lambdaiM<-cbind(lambdai1, lambdai2,lambdai1, lambdai12, lambdai12, lambdai12, lambdai123)
aux<-DepNHCPSP(lambdaiM=lambdaiM, d=3, fixed.seed=123)

#lambdaiM<-cbind(lambdai1, lambdai2, lambdai12)
#aux<-DepNHCPSP(lambdaiM=lambdaiM, d=2,fixed.seed=123, dplot=TRUE)
}
