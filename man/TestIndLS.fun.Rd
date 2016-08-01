\name{TestIndLS.fun}
\alias{TestIndLS.fun}
\alias{fn2B}




\title{Calculate a  Lotwick-Silverman test to check the independence between up to three   
  homogeneous point processes in time}

\description{This function calculates a  test of type Lotwick-Silverman  (LS)  to check the independence 
between up to three homogeneous point processes in time.
The statistic  is based on the close point 
relation, which adapts the crossed nearest neighbour distance ideas 
of spatial point processes to the case of  point processes in time.
 The p-value is obtained  using  a  bootstrap based on  a  type LS approach.}


\usage{TestIndLS.fun(posx, posy, posz=NULL, T,  alpha = 0.05, nTrans = 100, 
	PA = FALSE, cores=1,fixed.seed=NULL)}


\arguments{
  \item{posx}{Numeric vector. Position of the occurrence points in the first process.}
  \item{posy}{Numeric vector. Position of the occurrence points in the second process.}
  \item{posz}{Numeric vector. Position of the occurrence points in the third process. Only
	used if there are 3 processes in the set.}
  \item{T}{Numeric value. Length of the observation period of the processes.}

  \item{alpha}{Optional. Significance level  used to obtain  a decision (reject-no reject)  based on the  test p-value.}

  \item{nTrans}{Optional. Positive integer. Number of translations performed to calculate the  test.}

  \item{PA}{Optional. Logical flag. If it is TRUE, the close point relation  is
broadened by including the previous and the following points to the overlapping
intervals.}

  \item{cores}{Optional. Number of cores of the computer to be used in the calculations. }

 \item{fixed.seed}{Optional. An integer or NULL. If it is an integer, that is the value used to set the seed 
	in random generation processes. It it is NULL, a random seed is used.}

}


\details{ 
The underlying idea  of the test is to compare, for  each  point in  the first process, the behavior  of its  set of close points 
in the observed   process  vector \eqn{(N_x, N_y, N_z)}, and in a new process  vector  with the
same characteristics  and mutually  independent components. In the new vectors, the process \eqn{N_x} is fixed and  
second and third processes are  obtained by a translation of the original ones. If the observed  behavior 
is significantly different,  independence is rejected.
 The test statistic is the same as the one used in \code{\link{TestIndNH.fun}}, but
the p-value is obtained using a different bootstrap technique, based on a LS approach, see 
Lotwick and Silverman (1982). 


This test provides a nonparametric way to test independence. The key 
idea is to wrap  the processes onto a torus by identifying opposite sides, keeping the  first process fixed and
 translating the  other processes  randomly over the torus.  The translation 
  keeps the distribution of the processes (since they are homogenous)  but breaks any dependence between them, without the 
need of parametric models to describe the univariate marginal patterns, not even the intensity 
of the processes. 


}



\value{A list with elements    

  \item{KSpv}{P-value of the independence test.}
  \item{reject }{Binary vale indicating if the test is rejected ( value 1) or not (value 0) at the  alpha significance level.}
  \item{KSest}{Statistic of the independence test.}
}

\references{
Abaurrea, J. Asin, J. and Cebrian, A.C. (2015). A Bootstrap Test of Independence Between Three Temporal Nonhomogeneous Poisson Processes 
and its Application to Heat Wave Modeling. \emph{Environmental and Ecological Statistics}.


Lotwick, H.W. and Silverman, B.W. (1982). Methods for analysing Spatial processes of several types of points. 
\emph{J.R. Statist. Soc. B}, 44(3), pp. 406-13


}


\seealso{\code{\link{TestIndNH.fun}}, \code{\link{CondTest.fun}}, \code{\link{DutilleulPlot.fun}}, 
	\code{\link{DistShift.fun}}}


\examples{

#Test applied to three independent NHPP
posx<-simHPc.fun(0.1, 20, fixed.seed=123)$posH
posz<-simHPc.fun(0.15,22, fixed.seed=124)$posH
posy<-simHPc.fun(0.1, 18, fixed.seed=125)$posH
T<-max(posx,posy,posz)+10

aux<-TestIndLS.fun(posx, posy, posz,T=T,
	cores=1,fixed.seed=321)
aux$KSpv

}

