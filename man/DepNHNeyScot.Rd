\name{DepNHNeyScot}
\alias{DepNHNeyScot}
\alias{GenSons}

\title{Generating a multivariate  Neyman-Scott cluster process}

\description{This function  generates  a multivariate  Neyman-Scott cluster process,
that is  a vector of \eqn{d} dependent  (homogeneous or nonhomogeneous) point processes
which are Neyman-Scott processes with the same trajectory of  cluster centers. 


It calls the auxiliary function \code{GenSons} (not intended for the
     users), see Details.
}


\usage{DepNHNeyScot(lambdaParent, d, lambdaNumP = 1, dist = "normal", sigmaC = 1, 
                    minC = -1, maxC = 1, dplot=TRUE, fixed.seed=NULL,...)}


\arguments{
  \item{lambdaParent}{Numeric vector. Intensity values  of the  Poisson process used to generate 
the  centers of the clusters of the Neyman-Scott process.}
  \item{d}{Integer. Number of dependent processes to be generated.}
  \item{lambdaNumP}{Optional. Numeric vector. Mean values of the number of sons of each dependent process. If its length
	is equal to 1, the same value is used to generate all the dependent processes.}
  \item{dist}{Optional. Label "normal" or "uniform". Distribution  used to generate the  points of each 
	cluster.}
  \item{sigmaC}{Optional. Numeric vector.  Only used if dist="normal". Standard deviation of the normal distribution.   
	If its length is equal to 1, the same value is used in the \eqn{d} processes.}
  \item{minC}{Optional. Numeric  vector. Only used if dist="uniform". Lower bounds of the  Uniform distribution.  If its length
	is equal to 1, the same value is used in the d processes.}
  \item{maxC}{Optional. Numeric  vector. Only used if dist="uniform". Upper bounds of the  Uniform distribution. If its length
	is equal to 1, the same value is used in the \eqn{d} processes.}
  \item{dplot}{Optional. A logical flag. If it is TRUE,  the  generated marginal  processes  are plotted.}
  \item{fixed.seed}{Optional. An integer or NULL. Value used to set the seed 
	in random generation processes; if it is NULL, a random seed is used.}
  \item{...}{Further arguments to  be passed to  the function \code{\link{plot}}.}
}

\details{A Neyman-Scott  process is a Poisson cluster process  where the points in each cluster are randomly
distributed around the cluster center, see Neyman and Scott (1958) and Entekhabi et al. (1989).  

Homogeneous  or NH Neyman-Scott processes in continuous time and with the same trajectory of  cluster centers
are generated, so that \eqn{d} dependent processe are obtained.   First, the Poisson 
process of the cluster centers  is generated. Then, the number  of points in each cluster  
is generated using a Poisson distribution with  means which can be different in each process. 
The distances of each point in the cluster to its centre  can be generated using  two distributions 
a N(0, sigmaC)  or a Uniform(minC, maxC).  

It is noteworthy that high values of \code{sigmaC}  or  the range \code{maxC-minC} lead to a high variability 
around the centre and  to a low dependence between the  processes.

The marginal processes of the generated vector  can be optionally plotted.
}



\value{
 A list with elements:
  \item{posNH}{A list of  d vectors, containing the occurrence points of  the \eqn{d} dependent processes. The name of the 
	elements of the list are N1, N2,..., Nd.}
  \item{sizeCL}{A list of  d vectors. Each vector contains the  size (number of points) of each cluster in  a processes. The name of the 
	elements of the list are size1, size2,..., sized.}
}


\references{

Cebrian, A.C., Abaurrea, J.  and Asin, J. (2020).   Testing  independence between two  point processes in time.
 \emph{Journal of Simulation and Computational Statistics}.

Neyman, J., & Scott, E. L. (1958). Statistical approach to problems of cosmology. 
\emph{Journal of the Royal Statistical Society. Series B (Methodological)}, 1-43.

Entekhabi, D., Rodriguez-Iturbe, I., & Eagleson, P. S. (1989). Probabilistic 
representation of the temporal rainfall process by a modified Neyman-Scott 
Rectangular Pulses Model: Parameter estimation and validation. 
\emph{Water Resources Research}, 25(2), 295-302.

}


\seealso{\code{\link{IndNHNeyScot}}, \code{\link{DepNHPPqueue}}, \code{\link{DepNHPPMarked}}, \code{\link{DepNHCPSP}}}

\examples{
# Generation of three dependent Neyman-Scott processes with  normal distances 
set.seed(123)
lambdaParent<-runif(100,0,0.1)

DepNHNeyScot(lambdaParent=lambdaParent, d=3, lambdaNumP = c(2,3,2),
	 dist = "normal", sigmaC = c(3,2,2),fixed.seed=123)

}
