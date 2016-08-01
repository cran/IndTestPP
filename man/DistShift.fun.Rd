\name{DistShift.fun}
\alias{DistShift.fun}


\title{Shifts, conditionally on the first process, the remaining processes in a set of  up to three 
 processes  and calculates the set of close points 
and the mean distance  for each point in  the first  process}

\description{ Given a set of  up to three processes,  this function  fixes the first one and 
shiftes the  other ones a given distance. Then, it calculates the set of close points and the  mean distance 
for each point \eqn{t_{x_i}} in the first process. It can be used with homogeneous and non homogeneous processes.

}

\usage{
DistShift.fun(posx,posy,posz=NULL, T,  shii1, shii2=NULL, PA = FALSE,info=FALSE,...)
}


\arguments{
  \item{posx}{Numeric vector. Position of the occurrence points in the first process.}
  \item{posy}{Numeric vector. Position of the occurrence points in the second process.}
  \item{posz}{Optional. Numeric vector. Position of the occurrence points in the third process.}
  \item{T}{Numeric value. Length of the observation period of the processes.}

  \item{shii1}{Numeric value. Distance used to shift the points in the second process. It must be a positive value lower than  the length of  each lambda vector.}

  \item{shii2}{Optional. Numeric value. Distance used to shift the points in the third process. If there is only two processes it must be NULL. It must be a positive value lower than  the length of  each lambda vector.}

  \item{PA}{Optional. Logical flag. If it is TRUE, the close point relation  is
	broadened by including the previous and the following points to the overlapping intervals.}

   \item{info}{Optional. Logical flag. If it is TRUE,  information about the generated points is shown  
	on the screen and dotcharts  and bivariate charts of the  occurrence points of the three processes 
	are displayed.}

  \item{...}{Further arguments to pass to \code{\link{plot}} and to \code{\link{dotchart}} if  the argument
\code{info=T} }


}


\details{
Given a set of three (or two) point processes, this function aims to  transform the set of processes by a translation,
conditionally on the first process. The idea of this translation is to  keep the distribution of the
processes   but break any dependence between them, without the 
need of parametric models to describe the univariate marginal patterns. The only information required is
the marginal intensities of the processes. 


The key idea, see Lotwick and Silverman (1982),  is to wrap  the processes onto a torus by identifying opposite sides,
The first process is fixed, while the others are translated over the torus. In nonhomogenous processes, translation
 may change their distribution,  and to compensate, the intensity must also be translated. 


Then, the function calculates the set of close points and the  mean distance 
for each point \eqn{t_{x_i}} in the first process, in the  new shifted set of processes.


This function is  mainly used  to develop a Lotwick-Silverman type test to check the independecne
between the processes, see \code{\link{TestIndLS.fun}}.

For homogenous  processes,  the intensity vectors in  \code{lambda} must be constant 
(that is  all the values in a column must be equal).



}


\value{
 \item{DistTri}{Vector of the mean distance to the points in the set of close points, for each 
\eqn{t_{x_i}} in the first process.}
}


\references{

Cebrian, A.C. Abaurrea, J. Asin, J. (2014). NHPoisson: An R Package for Fitting and Validating  Nonhomogeneous Poisson Processes.
  Journal of Statistical Software.

Lotwick, H.W. and Silverman, B.W. (1982). Methods for analysing Spatial processes of several types of points. 
\emph{J.R. Statist. Soc. B}, 44(3), pp. 406-13
}


\seealso{\code{\link{TestIndLS.fun}}, \code{\link{DistSim.fun}} }

\examples{

set.seed(123)
lambdax<-runif(200, 0.01,0.17)
set.seed(124)
lambday<-runif(200, 0.015,0.15)
set.seed(125)
lambdaz<-runif(200, 0.005,0.1)
posx<-simNHPc.fun(lambda=lambdax, fixed.seed=123)$posNH
posy<-simNHPc.fun(lambda=lambday, fixed.seed=123)$posNH
posz<-simNHPc.fun(lambda=lambdaz, fixed.seed=123)$posNH

DistShift.fun(posx=posx,posy=posy,posz=posz,T=200, 
	shii1=59, shii2=125 )

}
