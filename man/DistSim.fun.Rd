\name{DistSim.fun}
\alias{DistSim.fun}
\alias{DistSimfix.fun}

\title{Generates   a set of  up to three independent  processes  and calculates the set of close points 
and the mean distance  for each point in  the first  process}

\description{Given a  point process, this function  generates  a set of one or two more processes  
 and calculates the set of close points and the  mean distance 
for each point \eqn{t_{x_i}} in the first process. 
Two types of  processes,  Poisson processes (PP) and  Neyman-Scott  
cluster processes (NSP), are implemented up to now. Homogeneous and nonhomogeneous processes can be generated in both
cases.

If a seed  must be fixed in the  generation process, function \code{DistSimfix.fun}  has to be used.
}

\usage{
DistSim.fun(posx, NumProcess=2, type = "Poisson", lambdaMarg = NULL, 
	lambdaParent = NULL, lambdaNumP=NULL, dist = "normal", sigmaC = 1, 
	minC = -1, maxC = 1, PA = FALSE,info=FALSE,...)

DistSimfix.fun(posx, NumProcess=2, type = "Poisson", lambdaMarg = NULL, 
	lambdaParent = NULL,lambdaNumP=NULL, dist = "normal", sigmaC = 1, 
	minC = -1, maxC = 1, PA = FALSE,info=FALSE,fixed.seed=1,...)}


\arguments{
  \item{posx}{Numeric vector. Position of the occurrence points in the first process.}

  \item{NumProcess}{Optional. Integer equal to 2 or 3,  the number of processes involved.}


  \item{type}{Optional. Label "Poisson" or "PoissonCluster". Type of point processes to be generated. 
	Up to now, only two types are available: Poisson processes ("Poisson") and Neyman-Scott cluster 
	processes  ("PoissonCluster").}

  \item{lambdaMarg}{Two-column matrix. Only used when \emph{type="Poisson"}. Each column
	is the intensity  \eqn{\lambda (t)} used  to generate the PPs.}

  \item{lambdaParent}{Numeric vector. Only used when \emph{type="PoissonCluster"}. Intensity values  
	of the  underlying PP used to generate the  centers of the clusters of the NSP.}

  \item{lambdaNumP}{Numeric vector (length \eqn{\le 2}). Only used when \emph{type="PoissonCluster"}. 
	Mean values of the number of sons of each  process. If its length is 1 and \code{NumProcess=2},
	the same value is used for both	processes.}

  \item{dist}{Optional. Label "normal" or "uniform". Only used when \emph{type="PoissonCluster"}.   
	Distribution  used to generate the  point locations in each cluster.}

  \item{sigmaC}{Optional. Numeric vector.  Only used when \emph{type="PoissonCluster"} and  \emph{dist="normal"}.
	Standard deviation of the normal distribution.   If its length	is  1 and \code{NumProcess=2},
	the same value is used for both processes.}

  \item{minC}{Optional. Numeric vector. Only used when \emph{type="PoissonCluster"} and  \emph{dist="uniform"}.
	Lower limits of the  Uniform distribution.  If its length is 1 and \code{NumProcess=2},
	the same value is used for both processes.}

  \item{maxC}{Optional. Numeric vector. Only used when \emph{type="PoissonCluster"} and  \emph{dist="uniform"}.
	 Upper limits of the  Uniform distribution. If its length is 1 and \code{NumProcess=2},
	the same value is used for both processes.}


  \item{PA}{Optional. Logical flag. If it is TRUE, the close point relation  is
	broadened by including the previous and the following points to the overlapping intervals.}

   \item{info}{Optional. Logical flag. If it is TRUE,  information about the generated points is shown  
	on the screen and dotcharts  and bivariate charts of the  occurrence points of the three processes 
	are displayed.}

  \item{fixed.seed}{Optional. Only available in \code{DistSimfix.fun}. Integer value used to set the seed 
	in random generation procedures.}

  \item{...}{Further arguments to pass to \code{\link{plot}} and to \code{\link{dotchart}} if  the argument
\code{info=T} }


}


\details{
Up to now,  generation of two types of processes (Poisson, "Poisson", and Neyman-Scott cluster 
	processes,"PoissonCluster") is available. Generation of NHPPs is done using the inversion approach, see Cebrian et al. (2014) and 
\code{\link{simNHPc.fun}}. For generation of NSPs, see \code{\link{IndNHNeyScot.fun}}  

The only difference between \code{DistSim.fun} and \code{DistSimfix.fun} is that the first one uses a random seed while in 
the second one a seed is set by the argument \code{fixed.seed}.

This function is  mainly used in \code{\link{TestIndNH.fun}}. 

The lenght of the period where the processes are generated is  determined by the length of 
the argument \code{lambdaParent} or the number of rows of \code{lambdaMarg}.Homogenous  processes  are  generated if the intensity vectors in  \code{lambdaParent} or in
\code{lambdaMarg} are  constant (that is if  all the values in the vector are equal).
}


\value{
 \item{DistTri}{Vector of the mean distance to the points in the set of close points, for each 
\eqn{t_{x_i}} in the first process.}
}


\references{
Abaurrea, J. Asin, J. and Cebrian, A.C. (2014). A Bootstrap Test of Independence Between Three Temporal Nonhomogeneous Poisson Processes 
and its Application to Heat Wave Modeling. Environmental and Ecological Statistics.

Cebrian, A.C. Abaurrea, J. Asin, J. (2014). NHPoisson: An R Package for Fitting and Validating  Nonhomogeneous Poisson Processes.
  Journal of Statistical Software. To appear.
}


\seealso{\code{\link{TestIndNH.fun}}, \code{\link{DistObs.fun}},  \code{\link{IndNHNeyScot.fun}},  
\code{\link{simNHPc.fun}} }

\examples{


#Observed process: PP; simulated processes: two independent PPs 
set.seed(123)
lambdax<-runif(200, 0.01,0.15)
set.seed(124)
lambday<-runif(200, 0.005,0.1)
set.seed(125)
lambdaz<-runif(200, 0.005,0.2)
posaux<-simNHPc.fun(lambda=lambdax, fixed.seed=123)$posNH

DistSimfix.fun(posx=posaux, type = "Poisson", 
	lambdaMarg = cbind(lambday,lambdaz), fixed.seed=123)
#DistSim.fun(posx=posaux, type = "Poisson", 
#	lambdaMarg = cbind(lambday,lambdaz))

}
