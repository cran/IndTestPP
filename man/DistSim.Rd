\name{DistSim}
\alias{DistSim}
\alias{DistSimfix}

\title{Generates a vector  of independent  processes,   and calculates the set of close points 
and the mean distance  for each point in  the first  process}

\description{This function generates   a vector  of two (or three) independent homogeneous or nonhomogeneous 
processes  conditionally on the first one,  by simulating the second (and  the third) process  using a parametric model 
(Poisson processes or  Neyman-Scott  cluster processes).  

It also calculates the set of close points  and the  mean distance in the generated vector, for each point \eqn{t_{x_i}} 
in the first process. 

\code{DistSimfix} allows to  fix a seed in the  generation process.
}

\usage{DistSim(posx, NumProcess=2, type = "Poisson", lambdaMarg = NULL, 
	lambdaParent = NULL, lambdaNumP=NULL, dist = "normal", sigmaC = 1, 
	minC = -1, maxC = 1, PA = FALSE,info=FALSE,...)

DistSimfix(posx, NumProcess=2, type = "Poisson", lambdaMarg = NULL, 
	lambdaParent = NULL,lambdaNumP=NULL, dist = "normal", sigmaC = 1, 
	minC = -1, maxC = 1, PA = FALSE,info=FALSE,fixed.seed=1,...)}


\arguments{
  \item{posx}{Numeric vector. Position of the occurrence points in the first process.}

  \item{NumProcess}{Optional. Integer equal to 2 or 3,  the number of processes in the vector.}


  \item{type}{Optional. Label "Poisson" or "PoissonCluster". Type of point processes to be generated. 
	Up to now, only two types are available: Poisson processes ("Poisson") and Neyman-Scott cluster 
	processes  ("PoissonCluster").}

  \item{lambdaMarg}{Two-column matrix. Only used when \emph{type="Poisson"}. Each column
	is the intensity  \eqn{\lambda (t)} used  to generate the processes.}

  \item{lambdaParent}{Numeric vector. Only used when \emph{type="PoissonCluster"}. Intensity values  
	of the Poisson process used to generate the  centres of the clusters of the Neyman-Scott process.}

  \item{lambdaNumP}{Numeric vector (length \eqn{\le 2}). Only used when \emph{type="PoissonCluster"}. 
	Mean values of the number of sons in each  process. If its length is 1 and \code{NumProcess=2},
	the same value is used for both	processes.}

  \item{dist}{Optional. Label "normal" or "uniform". Only used when \emph{type="PoissonCluster"}.   
	Distribution  used to generate the  point distances in each cluster.}

  \item{sigmaC}{Optional. Numeric vector.  Only used when \emph{type="PoissonCluster"} and  \emph{dist="normal"}.
	Standard deviation of the normal distribution.   If its length	is  1 and \code{NumProcess=2},
	the same value is used for both processes.}

  \item{minC}{Optional. Numeric vector. Only used when \emph{type="PoissonCluster"} and  \emph{dist="uniform"}.
	Lower bounds of the  Uniform distribution.  If its length is 1 and \code{NumProcess=2},
	the same value is used for both processes.}

  \item{maxC}{Optional. Numeric vector. Only used when \emph{type="PoissonCluster"} and  \emph{dist="uniform"}.
	 Upper bounds of the  Uniform distribution. If its length is 1 and \code{NumProcess=2},
	the same value is used for both processes.}


  \item{PA}{Optional. Logical flag. If it is TRUE, the close point relation  is
	broadened by including the previous and the following points to the overlapping intervals.}

   \item{info}{Optional. Logical flag. If it is TRUE,  information about the generated points is shown  
	on the screen and dotcharts  and bivariate charts of the  occurrence points of the three processes 
	are displayed.}

  \item{fixed.seed}{Optional. Only available in \code{DistSimfix}. Integer value used to set the seed 
	in random generation procedures.}

  \item{...}{Further arguments to be passes to  the functions \code{\link{plot}} and \code{\link{dotchart}} if  argument
\code{info=T}. }


}


\details{
This function is  mainly used  in the application of a parametric bootstrap approach   to  generate  a pair of independent
processes with the same marginal distributions than the observed ones. To that aim, the first process is fixed and the others are generated using a parametric model.
These processes are used for example to build  a test to assess the independecne
between two or three processes, see \code{\link{TestIndNH}}.


Two types of processes (Poisson, "Poisson", and Neyman-Scott cluster 
processes,"PoissonCluster")  can be generated. Generation of nonhomogeneous Poisson processes is done using the inversion algorithm, see 
\code{\link{simNHPc}}. For generation of Neyman-Scott processes, see \code{\link{IndNHNeyScot}}.

The function  also calculates the set of close points and the  mean distance 
for each point \eqn{t_{x_i}} in the first process, in the  new generated vector of processes.


The lenght of the period where the processes are generated is  determined by the length of 
the argument \code{lambdaParent} or the number of rows of \code{lambdaMarg}. Homogenous  processes  are  generated if the intensity vectors in  \code{lambdaParent} or in
\code{lambdaMarg} are  constant (that is if  all the values in the vector are equal).


If a seed  must be fixed in the  generation process, function \code{DistSimfix}  has to be used.
The functions \code{DistSim} and \code{DistSimfix} are  similar, the difference is that the first one uses a 
random seed  to generate the processes, while the second one uses a seed  set by the argument \code{fixed.seed}.
}


\value{
 \item{DistTri}{Vector of the mean distances of each point \eqn{t_{x_i}}  calculated in the generated processes.}
}


\references{
Abaurrea, J. Asin, J. and Cebrian, A.C. (2015). A Bootstrap Test of Independence Between Three Temporal Nonhomogeneous Poisson Processes 
and its Application to Heat Wave Modeling. \emph{Environmental and Ecological Statistics}, 22(1), 127-144.
}


\seealso{\code{\link{TestIndNH}}, \code{\link{DistObs}},  \code{\link{IndNHNeyScot}},  
\code{\link{simNHPc}} }

\examples{


#Calculation of the distances in a vector of  three independent Poisson processes   
#conditionally to the first one

set.seed(123)
lambdax<-runif(200, 0.01,0.15)
posaux<-simNHPc(lambda=lambdax, fixed.seed=123)$posNH

set.seed(124)
lambday<-runif(200, 0.005,0.1)
set.seed(125)
lambdaz<-runif(200, 0.005,0.2)

DistSimfix(posx=posaux, type = "Poisson", lambdaMarg = cbind(lambday,lambdaz), 
	fixed.seed=123, info=TRUE)
#DistSim(posx=posaux, type = "Poisson", lambdaMarg = cbind(lambday,lambdaz))

}
