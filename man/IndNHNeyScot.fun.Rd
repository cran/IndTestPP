\name{IndNHNeyScot.fun}
\alias{IndNHNeyScot.fun}


\title{Generates trajectories of independent  Neyman-Scott cluster processes}

\description{ This function  generates independent  (homogeneous or nonhomogeneous) 
Neyman-Scott cluster processes  with  independent trajectories of  cluster centers with the same intensity. 


It calls the auxiliary function \code{GenSons.fun} (not intended for the
     users), see Details section.
}


\usage{
IndNHNeyScot.fun(lambdaParent, d, lambdaNumP = 1,  dist = "normal", 
	sigmaC = 1, minC = -1, maxC = 1,fixed.seed=NULL)}

\arguments{
  \item{lambdaParent}{Numeric vector. Intensity values  of the  underlying PP used to generate 
the  centers of the clusters of the Neyman-Scott process.}
  \item{d}{Integer. Number of independent processes to be generated.}
  \item{lambdaNumP}{Optional. Numeric vector. Mean values of the number of sons of each dependent process. If its length
	is equal to 1, the same value is used to generate all the dependent processes.}
  \item{dist}{Optional. Label "normal" or "uniform". Distribution  used to generate the  point locations of each 
	cluster.}
  \item{sigmaC}{Optional. Numeric vector. Standard deviation of the normal distribution.  Only used if dist="normal". If its length
	is equal to 1, the same value is used in the d processes.}
  \item{minC}{Optional. Numeric  vector. Lower limits of the  Uniform distribution. Only used if dist="uniform". If its length
	is equal to 1, the same value is used in the d processes.}
  \item{maxC}{Optional. Numeric  vector. Upper limits of the  Uniform distribution. Only used if dist="uniform". If its length
	is equal to 1, the same value is used in the d processes.}
  \item{fixed.seed}{Optional. An integer or NULL. Value used to set the seed 
	in random generation processes; if it is NULL, a random seed is used.}
}


\details{A Neyman-Scott  process is a Poisson cluster process  where the points in each cluster are randomly 
distributed around the cluster center.  For generating each process, 
an independent  trajectory  of the Poisson 
process of the cluster centers  is generated first. Then, the number  of points in each cluster  
is generated using a Poisson distribution  with mean value \eqn{\mu_{P_i}}  (i=1,...d). Finally, th
e point  locations  around each center  can be generated using  two distributions N(0, sigmaC)  or Uniform(minC, maxC).  


The lenght of the period where the processes are generated is  determined by the length of 
the argument \code{lambdaParent}.

Homogenous  processes  are  generated if the intensity vector  \code{lambdaParent} is  constant 
(that is if  all the values are equal).

}



\value{
 A list with elements
  \item{posNH }{A list of  d vectors, each one containing the time occurrences of one of the dependent processes. The name of the 
	elements of the list are PP1, PP2...PPd}
}


\references{
Abaurrea, J. Asin, J. and Cebrian, A.C. (2014). A Bootstrap Test of Independence Between Three Temporal Nonhomogeneous Poisson Processes 
and its Application to Heat Wave Modeling. \emph{Environmental and Ecological Statistics}.

}


\seealso{\code{\link{DepNHNeyScot.fun}}, \code{\link{IndNHPP.fun}}}

\examples{
set.seed(123)
lambda<-runif(1000)/10

IndNHNeyScot.fun(lambdaParent=lambda, d=3, lambdaNumP = c(2,3,2),  dist = "normal", 
	sigmaC = 2, fixed.seed=123)

}

