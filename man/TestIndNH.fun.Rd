\name{TestIndNH.fun}
\alias{TestIndNH.fun}
\alias{mirank.fun}
\alias{miKS.fun}
\alias{fn2}
\alias{fn2fix}



\title{Calculate a bootstrap  test  to check the independence between up to three  nonhomogeneous  
Poisson  or point processes in time}

\description{This function calculates the  test  by Abaurrea et al. (2014). The statistic  is based on the close point 
relation, which adapts the crossed nearest neighbour distance ideas 
of spatial point processes to the case of  (homogenoeus or nonhomogeneous) point processes in time.  The test can be applied to two or three homogeneous or non homogeneous
Poisson processes or to any point process which can be simulated. Currently, it is implemented for Poisson processes and for Neyman-Scott cluster 
 processes. The p-value is obtained  using  a parametric bootstrap approach.}


\usage{TestIndNH.fun(posx, posy, posz=NULL, NumProcess=2, alpha = 0.05, 
	nsim = 100, PA = FALSE, cores = 1, type = "Poisson", 
	lambdaMarg = NULL,   lambdaParent = NULL, lambdaNumP = NULL, 
	dist = "normal", sigmaC = 1, minC = -1, maxC = 1,fixed.seed=NULL)}


\arguments{
  \item{posx}{Numeric vector. Position of the occurrence points in the first process.}
  \item{posy}{Numeric vector. Position of the occurrence points in the second process.}
  \item{posz}{Numeric vector. Position of the occurrence points in the third process. Only
	used if \code{NumProcess=3}.}

  \item{NumProcess}{Optional. Integer equal to 1 or 2. Number of processes added to the first one.}


  \item{alpha}{Optional. Significance level  used to obtain  a decision (reject-no reject)  based on the  test p-value.}

  \item{nsim}{Optional. Positive integer. Number of simulations performed to calculate the  test.}

  \item{PA}{Optional. Logical flag. If it is TRUE, the close point relation  is
broadened by including the previous and the following points to the overlapping
intervals.}

  \item{cores}{Optional. Number of cores of the computer to be used in the calculations. }

  \item{type}{Optional. Label "Poisson" or "PoissonCluster". Type of point processes to be generated in 
the parametric bootstrap. Up to now, only two types are available: Poisson processes ("Poisson") and Neyman-Scott cluster processes  ("PoissonCluster").}

  \item{lambdaMarg}{Matrix  of dimension \eqn{T \times (NumProcess-1)}. Only used if \emph{type='Poisson'}. 
Each column is the  intensity vector to generate the  Poisson processes.}

  \item{lambdaParent}{Numeric vector. Only used if \emph{type='PoissonCluster'}. Intensity vector of the  PP 
used to generate the  centers of the clusters of the Neyman-Scott process.}

  \item{lambdaNumP}{Numeric vector with 1 or 2 values. Only used if \emph{type='PoissonCluster'}.  Mean values of the number of 
	sons of  the  processes to be generated. If its length	is equal to 1, the same value is used in both processes.}

  \item{dist}{Optional. Label "normal" or "uniform".  Only used if \emph{type='PoissonCluster'}.
Distribution  used to generate the  point locations of each cluster.}

  \item{sigmaC}{Optional. Numeric vector with 1 or 2 values.  Only used if \emph{type='PoissonCluster'} and \emph{dist='normal'}. Standard 
	deviation of the normal distribution.   If its length	is equal to 1, the same value is used in both processes.}

  \item{minC}{Optional. Numeric vector with 1 or 2 values. Only used if \emph{type='PoissonCluster'} and \emph{dist='uniform'}. 
Lower limits of the  Uniform distribution. If its lengthis equal to 1, the same value is used in  both processes.}

  \item{maxC}{Optional. Numeric vector with 1 or 2 values. Only used if \emph{type='PoissonCluster'} and \emph{dist='uniform'}. 
Upper limits of the  Uniform distribution. If its lengthis equal to 1, the same value is used in  both processes.}

  \item{fixed.seed}{Optional. An integer or NULL. If it is an integer, that is the value used to set the seed 
	in random generation processes. It it is NULL, a random seed is used.}

}


\details{ 
The underlying idea  of the test is to compare, for  each  point in  the first process, the behavior  of its  set of close points 
in the observed vector  process \eqn{(N_x, N_y, N_z)}, and in simulated  vector processes with the
same characteristics  and mutually  independent components. In the simulated  vectors, the process \eqn{N_x} is fixed and  
second and third processes with  intensities  \eqn{\lambda_y} and \eqn{\lambda_z} (the intensities of
 \eqn{N_y} and \eqn{N_z}) are generated. 
If the observed  behavior is significantly different,  independence is rejected.
The  distribution of the statistic is obtained using a Monte Carlo approach if the intensities \eqn{\lambda_y(t)} and \eqn{\lambda_z(t)} 
are known, or a parametric bootstrap if they have  been estimated. See Abaurrea et al. (2014) for more details


It is noteworthy that  is being assumed that the processes are Poisson or Neyman-Scott cluster processes.  Then, 
if necessary,    validation of that assumption should be previously carried out.

The lenght of the  observation period is  determined by the length of  the intensity vector \eqn{\lambda}, that
is \emph{lambdaParent} (if  \emph{type='PoissonCluster'})  or the  first  element of the dimension of \emph{lambdaMarg} (if 
\emph{type='PoissonC'}.

Homogenous  processes  are  generated if the intensity vector  \code{lambda} is  constant 
(that is if all the values are equal).

The test \code{\link{TestIndLS.fun}}, which uses a 
non parametric bootstrap approach, is available  for any  homogeneous  point process.


}



\value{A list with elements    

  \item{KSpv}{P-value of the independence test.}
  \item{reject }{Binary vale indicating if the test is rejected ( value 1) or not (value 0) at the  alpha significance level.}
  \item{KSest}{Statistic of the independence test.}
}

\references{
Abaurrea, J. Asin, J. and Cebrian, A.C. (2014). A Bootstrap Test of Independence Between Three Temporal Nonhomogeneous Poisson Processes 
and its Application to Heat Wave Modeling. \emph{Environmental and Ecological Statistics}.}


\seealso{\code{\link{TestIndLS.fun}},\code{\link{CondTest.fun}}, \code{\link{DutilleulPlot.fun}},
	\code{\link{DistSim.fun}}, \code{\link{DistObs.fun}}, \code{\link{uniongentri.fun}}}


\examples{

#Test applied to 3  independent NHPP
set.seed(123)
lambdax<-runif(150, 0.01,0.1)
set.seed(124)
lambday<-runif(150, 0.02,0.1)
set.seed(125)
lambdaz<-runif(150, 0.015,0.1)
posx<-simNHPc.fun(lambdax, fixed.seed=123)$posNH
posy<-simNHPc.fun(lambday, fixed.seed=124)$posNH
posz<-simNHPc.fun(lambdaz, fixed.seed=125)$posNH

aux<-TestIndNH.fun(posx, posy, posz, nsim=50, type='Poisson', 
	NumProcess=3,lambdaMarg=cbind(lambday,lambdaz), fixed.seed=321)
aux$KSpv




#Test applied to 3 dependent NS cluster processes with 2 cores
#set.seed(123)
#lambdaParent<-runif(500,0,0.1)
#DepPro<-DepNHNeyScot.fun(lambdaParent=lambdaParent, d=3, lambdaNumP = 3, 
#	 dist = "normal", sigmaC = 1, fixed.seed=123,cores=2)
#posx<-DepPro$PP1
#posy<-DepPro$PP2
#posz<-DepPro$PP3
#aux<-TestIndNH.fun(posx, posy, posz, cores=1, type='PoissonCluster',
#	NumProcess=3,lambdaParent = lambdaParent, lambdaNumP = 3, 
#	dist = "normal", sigmaC = 1, fixed.seed=123, nsim=200)
#aux$KSpv


}

