\name{TestIndLS}
\alias{TestIndLS}
\alias{fn2B}




\title{Lotwick-Silverman test  of independence between point processes}

\description{This function calculates a  test  based on the Lotwick-Silverman  (LoS) approach to study the independence 
between  two or three homogeneous point processes in time.
The statistic  is based on the close point  sets of the points in the first process.}


\usage{TestIndLS(posx, posy, posz=NULL, T,  alpha = 0.05, nTrans = 100, PA = FALSE, 
	cores=1,fixed.seed=NULL)}


\arguments{
  \item{posx}{Numeric vector. Position of the occurrence points in the first process.}
  \item{posy}{Numeric vector. Position of the occurrence points in the second process.}
  \item{posz}{Numeric vector. Position of the occurrence points in the third process. Only
	used if there are 3 processes.}
  \item{T}{Numeric value. Length of the observed period of the processes.}

  \item{alpha}{Optional. Significance level  used to obtain  a decision (reject-no reject)  based on the  test p-value.}

  \item{nTrans}{Optional. Positive integer. Number of translations to calculate the  test.}

  \item{PA}{Optional. Logical flag. If it is TRUE, the close point relation  is
broadened by including the previous and the following points to the overlapping
intervals.}

  \item{cores}{Optional. Number of cores of the computer to be used in the calculations. }

 \item{fixed.seed}{Optional. An integer or NULL. If it is an integer, that is the value used to set the seed 
	in random generation processes. It it is NULL, a random seed is used.}

}


\details{ 
The underlying idea  of the test is to compare, for  each  point in  the first process, the behavior  of its  set of close points 
in the  vector of observed   processes \eqn{(N_x, N_y, N_z)}, and in  new  vectors of  independent processes with the
same marginal distribution. The new independent vectors are obtained using a LoS approach, see Lotwick and Silverman (1982):
the process \eqn{N_x} is fixed and  second and third processes are  obtained by shifting the original ones a random amount. 
This  translation keeps the distribution of the homogeneous processes,   but breaks any dependence between them.
If the observed  behavior is significantly different,  independence is rejected. More details can be foun in Cebrian et al. (2020).


The test statistic is the 
one used in \code{\link{TestIndNH}}, but the p-value is obtained using a  LoS approach,
so that it does not require any assumption about the marginal distribution of the processes,
not even the marginal intensities. The test \code{\link{TestIndNH}},   
can be applied  to study nonhomogeneous processes, but it requires a parametric model for the second process.
}

\value{A list with elements:    

  \item{pv}{P-value of the independence test.}
  \item{reject }{Binary variable indicating if the test is rejected (1) or not (0) at an  alpha significance level.}
  \item{est}{Sample of the KS statistics. The first value corresponds to the observed processes and the others to the generated processes.}
}

\references{
 Cebrian, A.C., Abaurrea, J.  and Asin, J. (2020).   Testing  independence between two  point processes in time.
 \emph{Journal of Simulation and Computational Statistics}.

Lotwick, H.W. and Silverman, B.W. (1982). Methods for analysing Spatial processes of several types of points. 
\emph{J.R. Statist. Soc. B}, 44(3), pp. 406-13


}


\seealso{\code{\link{TestIndNH}}, \code{\link{CondTest}}, \code{\link{DutilleulPlot}}, 
	\code{\link{DistShift}}}


\examples{

#Test applied to three independent HPP

posx<-simNHPc(lambda=rep(0.1,200),fixed.seed=123)$posNH
posz<-simNHPc(lambda=rep(0.15,200),fixed.seed=124)$posNH
posy<-simNHPc(lambda=rep(0.1,200),fixed.seed=125)$posNH


aux<-TestIndLS(posx, posy, posz,T=200,
	cores=1,fixed.seed=321)
aux$pv

}

