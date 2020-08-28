\name{TestIndNH}
\alias{TestIndNH}
\alias{mirank}
\alias{miKS}
\alias{fn2}
\alias{fn2fix}



\title{Parametric bootstrap  test  of independence between point processes}

\description{This function calculates a parametric bootstrap test (PaB)  to study the independence 
between  two or three homogeneous or nonhomogeneous point processes in time.
The statistic  is based on the close point  sets of the points in the first process.  Currently, it is implemented for Poisson processes and for Neyman-Scott cluster 
 processes.}

\usage{TestIndNH(posx, posy, posz=NULL,  alpha = 0.05, nsim = 100, PA = FALSE, cores = 1, 
	type = "Poisson", lambdaMarg = NULL,   lambdaParent = NULL, lambdaNumP = NULL, 
	dist = "normal", sigmaC = 1, minC = -1, maxC = 1,fixed.seed=NULL)}


\arguments{
  \item{posx}{Numeric vector. Position of the occurrence points in the first process.}
  \item{posy}{Numeric vector. Position of the occurrence points in the second process.}
  \item{posz}{Numeric vector. Position of the occurrence points in the third process.  By default is null,
and only two processes are tested.}


  \item{alpha}{Optional. Significance level  used to obtain  a decision (reject-no reject)  based on the  test p-value.}

  \item{nsim}{Optional. Positive integer. Number of simulations to calculate the  test.}

  \item{PA}{Optional. Logical flag. If it is TRUE, the close point relation  is
broadened by including the previous and the following points to the overlapping
intervals.}

  \item{cores}{Optional. Number of cores of the computer to be used in the calculations. }

  \item{type}{Optional. Label "Poisson" or "PoissonCluster". Type of point processes to be generated in 
the parametric bootstrap. Up to now, only two types are available: Poisson processes ("Poisson") and Neyman-Scott cluster processes  ("PoissonCluster").}

  \item{lambdaMarg}{Matrix of positive values and dimension \eqn{T \times N_P} with \eqn{N_P}=1 or 2.
 Only used if \emph{type="Poisson"}. Each column is the  intensity vector to generate the  processes \eqn{N_y} and \eqn{N_z}.}

  \item{lambdaParent}{Numeric vector. Only used if \emph{type="PoissonCluster"}. Intensity vector of the  process 
used to generate the  centers of the clusters of the Neyman-Scott process.}

  \item{lambdaNumP}{Numeric vector with 1 or 2 values. Only used if \emph{type="PoissonCluster"}.
  Mean values of the number of 	sons of  the  processes to be generated. If its length	is equal to 1 and 
  there are three processes, the same value is used  to generate \eqn{N_y} and \eqn{N_z}.}

  \item{dist}{Optional. Label "normal" or "uniform".  Only used if \emph{type="PoissonCluster"}.
Distribution  used to generate the  point  distances to the centre in each cluster.}

  \item{sigmaC}{Optional. Numeric vector with 1 or 2 values.  Only used if \emph{type="PoissonCluster"}
 and \emph{dist='normal'}. Standard deviation of the normal distribution.   If its length is equal to 1, 
the same value is used in both processes.}

  \item{minC}{Optional. Numeric vector with 1 or 2 values. Only used if \emph{type="PoissonCluster"} and \emph{dist='uniform'}. 
Lower bounds of the  Uniform distribution.  If its length is equal to 1 and there are three processes, the same value is used  
	to generate \eqn{N_y} and \eqn{N_z}.}

  \item{maxC}{Optional. Numeric vector with 1 or 2 values. Only used if \emph{type="PoissonCluster"} and \emph{dist='uniform'}. 
Upper bounds of the  Uniform distribution.  If its length is equal to 1 and there are three processes, the same value is used  
	to generate \eqn{N_y} and \eqn{N_z}.}

  \item{fixed.seed}{Optional. An integer or NULL. If it is an integer, that is the value used to set the seed 
	in random generation processes. It it is NULL, a random seed is used.}

}

\details{ 

The underlying idea  of the test is to compare, for  each  point in  the first process, the behavior  of its  set of close points 
in the  vector of observed   processes \eqn{(N_x, N_y, N_z)}, and in  new  vectors of  independent processes with the
observed  marginal distribution. The new independent vectors are obtained using a parametric bootstrap approach, 
see Abaurrea et al. (2015): the process \eqn{N_x} is fixed and  second and third processes are   generated using a parametric model with  intensities  \eqn{\lambda_y} and \eqn{\lambda_z}. 
 Currently, it is implemented for Poisson processes and for Neyman-Scott cluster  processes.
If the observed  behavior is significantly different,  independence is rejected. 


The test statistic is  the one used in \code{\link{TestIndLS}}, but the p-value is obtained using a  
Monte Carlo approach if the intensities \eqn{\lambda_y(t)} and \eqn{\lambda_z(t)} 
are known, or a parametric bootstrap if they have  been estimated. The test \code{\link{TestIndLS}} 
can only be applied to  homogeneous  processes, but it does not require any assumption about
the distribution of the marginal processes.

It is noteworthy that  when the test is applied, it is being assumed that the processes  follow a parametric model 
with the given intensities. If necessary,  validation of that assumption should be previously carried out.

The lenght of the  observed period is  determined by the length of  the intensity vector \eqn{\lambda}, that
is \emph{lambdaParent} (if  \emph{type="PoissonCluster"})  or the  first  element of the dimension of \emph{lambdaMarg} (if 
\emph{type="PoissonC"}. It can be applied to homogeneous processes, using an intensity vector  (\code{lambda}) 
with equal values.


}



\value{A list with elements:   

  \item{pv}{P-value of the independence test.}
  \item{reject }{Binary variable indicating if the test is rejected (1) or not (0) at an  alpha significance level.}
  \item{est}{Sample of the KS statistics. The first value corresponds to the observed processes and the others to the generated processes.}
}

\references{
Abaurrea, J. Asin, J. and Cebrian, A.C. (2015). A Bootstrap Test of Independence Between Three Temporal Nonhomogeneous Poisson Processes 
and its Application to Heat Wave Modeling. \emph{Environmental and Ecological Statistics}.}


\seealso{\code{\link{TestIndLS}},\code{\link{CondTest}}, \code{\link{DutilleulPlot}},
	\code{\link{DistSim}}, \code{\link{DistObs}}, \code{\link{uniongentri}}}


\examples{

#Test applied to 3  independent NHPP
set.seed(123)
lambdax<-runif(150, 0.01,0.1)
set.seed(124)
lambday<-runif(150, 0.02,0.1)
set.seed(125)
lambdaz<-runif(150, 0.015,0.1)
posx<-simNHPc(lambdax, fixed.seed=123)$posNH
posy<-simNHPc(lambday, fixed.seed=124)$posNH
posz<-simNHPc(lambdaz, fixed.seed=125)$posNH

aux<-TestIndNH(posx, posy, posz, nsim=50, type='Poisson', 
	lambdaMarg=cbind(lambday,lambdaz), fixed.seed=321)
aux$pv


#Test applied to 3 dependent NS cluster processes with 2 cores
#set.seed(123)
#lambdaParent<-runif(500,0,0.1)
#DepPro<-DepNHNeyScot(lambdaParent=lambdaParent, d=3, lambdaNumP = 3, 
#	 dist = "normal", sigmaC = 1, fixed.seed=123,cores=2)
#posx<-DepPro$PP1
#posy<-DepPro$PP2
#posz<-DepPro$PP3
#aux<-TestIndNH(posx, posy, posz, cores=1, type='PoissonCluster',
#	lambdaParent = lambdaParent, lambdaNumP = 3, 
#	dist = "normal", sigmaC = 1, fixed.seed=123, nsim=200)
#aux$pv


}

