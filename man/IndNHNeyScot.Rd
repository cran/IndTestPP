\name{IndNHNeyScot}
\alias{IndNHNeyScot}


\title{Generating a vector of independent  Neyman-Scott cluster processes}

\description{ This function  generates a vector of \eqn{d} independent  
(homogeneous or nonhomogeneous) Neyman-Scott cluster processes  with  independent 
trajectories of  cluster centers with the same intensity. 


It calls the auxiliary function \code{GenSons} (not intended for the
     users), see Details.
}


\usage{
IndNHNeyScot(lambdaParent, d, lambdaNumP = 1,  dist = "normal", 
	sigmaC = 1, minC = -1, maxC = 1, dplot=TRUE,fixed.seed=NULL,...)}

\arguments{
  \item{lambdaParent}{Numeric vector. Intensity   of the Poisson process used to generate 
the  independent trajectories of the cluster centres of the Neyman-Scott process.}
  \item{d}{Integer. Number of independent processes to be generated.}
  \item{lambdaNumP}{Optional. Numeric vector. Mean values of the number of sons of each marginal process. If its length
	is equal to 1, the same value is used to generate all the processes.}
  \item{dist}{Optional. Label "normal" or "uniform". Distribution  used to generate the  point locations of each 
	cluster.}
  \item{sigmaC}{Optional. Numeric vector. Standard deviation of the normal distribution.  Only used if dist="normal". 
        If its length is equal to 1, the same value is used in the d processes.}
  \item{minC}{Optional. Numeric  vector. Lower bounds of the  Uniform distribution. Only used if dist="uniform". If its length
	is equal to 1, the same value is used in the d processes.}
  \item{maxC}{Optional. Numeric  vector. Upper bounds of the  Uniform distribution. Only used if dist="uniform". If its length
	is equal to 1, the same value is used in the d processes.}
  \item{dplot}{Optional. A logical flag. If it is TRUE,  the  generated marginal  processes  are plotted.}
  \item{fixed.seed}{Optional. An integer or NULL. Value used to set the seed 
	in random generation processes; if it is NULL, a random seed is used.}
  \item{...}{Further arguments to  be passed to  the function \code{\link{plot}}.}}


\details{
A Neyman-Scott  process is a Poisson cluster process  where the points in each cluster are randomly
distributed around the cluster center, see Neyman and Scott (1958) and Entekhabi et al. (1989).  


To generate each process in the vector, an independent  trajectory  of the Poisson 
process of the cluster centres  is generated first. Then, the number  of points in each cluster  
is generated using a Poisson distribution  with mean value \eqn{\mu_{P_i}}  (i=1,...d). Finally, 
the distances to the centre of each point  in the cluster is be generated using  
one of the two distributions available, N(0, sigmaC)  or Uniform(minC, maxC).  


The lenght of the period where the processes are generated is  determined by the length of 
the argument \code{lambdaParent}.

Homogenous  processes  are  generated if the intensity vector  \code{lambdaParent} is  constant 
(that is if  all the values are equal).

The marginal processes of the generated vector  can be optionally plotted.

}



\value{
 A list with elements:
  \item{posNH }{A list of  \eqn{d} vectors, each one containing the time occurrences of one of the 
	marginal processes. The name of the elements of the list are N1, N2,...,Nd.}
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


\seealso{\code{\link{DepNHNeyScot}}, \code{\link{IndNHPP}}}

\examples{
set.seed(123)
lambda<-runif(1000)/10

IndNHNeyScot(lambdaParent=lambda, d=3, lambdaNumP = c(2,3,2),  dist = "normal", 
	sigmaC = 2, fixed.seed=123)

}

