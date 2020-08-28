\name{DistShift}
\alias{DistShift}


\title{Generates  by translation a vector  of independent  processes,   and calculates the set of close points 
and the mean distance  for each point in  the first  process}

\description{This function generates   a vector  of two (or three) independent  processes,  conditionally on the first one,   
by shifting the second (and the third) process. 

It also calculates the set of close points and the  mean distance  in the generated vector, 
for each point \eqn{t_{x_i}} in the first process. 

}

\usage{
DistShift(posx,posy,posz=NULL, T,  shii1, shii2=NULL, PA = FALSE, info=FALSE,...)
}


\arguments{
  \item{posx}{Numeric vector. Position of the occurrence points in the first process.}
  \item{posy}{Numeric vector. Position of the occurrence points in the second process.}
  \item{posz}{Optional. Numeric vector. Position of the occurrence points in the third process.}
  \item{T}{Numeric value. Length of the observed period of the processes.}
  \item{shii1}{Numeric value. Distance used to shift the points in the second process. It must be a positive 
value lower than  T.}
  \item{shii2}{Optional. Numeric value. Distance used to shift the points in the third process. It must be NULL, 
if there are two processes, and a positive value lower than T, if there are three.}

  \item{PA}{Optional. Logical flag. If it is TRUE, the close point relation  is
	broadened by including the previous and the following points to the overlapping intervals.}

   \item{info}{Optional. Logical flag. If it is TRUE,  information about the generated points is shown  
	on the screen and dotcharts  and bivariate charts of the  occurrence points of the three processes 
	are displayed.}

  \item{...}{Further arguments to be passed to the functions \code{\link{plot}} and  \code{\link{dotchart}} 
if  the argument \code{info=T} }


}


\details{

This function is  mainly used  in the application of the Lotwicck- Silverman approach, see Lotwick and Silverman (1982),    to  generate  a pair of independent
processes with the same marginal distributions than the observed ones. These processes are used for example to build  
a test to assess  independence between two or three processes, see \code{\link{TestIndLS}}.

The key idea is to wrap  the processes onto a  circumference by identifying the opposite sides of the time interval where they are observed.
The first process is fixed, while the others are shifted over the circumference a  given amount. 
The idea of this translation is to  keep the  marginal distribution of the processes   but  to break any dependence between them, without the 
need of parametric models to describe the  marginal patterns. 

The function  also calculates the set of close points and the  mean distance 
for each point \eqn{t_{x_i}} in the first process, in the  new shifted vector of processes.


}


\value{
 \item{DistTri}{The vector of the mean distances of   points \eqn{t_{x_i}} in the shifted processes.}
}


\references{


Lotwick, H.W. and Silverman, B.W. (1982). Methods for analysing Spatial processes of several types of points. 
\emph{J.R. Statist. Soc. B}, 44(3), pp. 406-13
}


\seealso{\code{\link{TestIndLS}}, \code{\link{DistSim}} }

\examples{

set.seed(123)
lambdax<-runif(200, 0.01,0.17)
set.seed(124)
lambday<-runif(200, 0.015,0.15)
set.seed(125)
lambdaz<-runif(200, 0.005,0.1)
posx<-simNHPc(lambda=lambdax, fixed.seed=123)$posNH
posy<-simNHPc(lambda=lambday, fixed.seed=123)$posNH
posz<-simNHPc(lambda=lambdaz, fixed.seed=123)$posNH

aux<-DistShift(posx=posx, posy=posy, posz=posz, T=200, shii1=59, shii2=125 )

}
