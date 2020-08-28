\name{NHK}
\alias{NHK}
\alias{NHKaux}
\alias{NHKaux2}
\alias{NHKaux3}
\alias{Kenv}

\title{Estimating cross K-function   and testing independence}

\description{This function estimates the  cross K-function  between 
two sets, \eqn{C} and \eqn{D},  of (homogenous or nonhomogeneous)  point processes in time.  
It  is evaluated in a grid of distances \eqn{r},  and it can be optionally plotted.  A test to 
assess the independence between the sets of processes, based on  the cross K-function,   is also implemented.

It calls the auxiliary functions NHKaux, NHKaux2, NHKaux3 and Kenv, not intended for users.}

\usage{NHK(lambdaC, lambdaD, T=NULL, posC, typeC=1, posD, typeD=1, r=NULL, test=TRUE,
 typeEst=2,  nTrans=1000, conf=0.95, rTest=NULL, typePlot=" ",tit=NULL,
 cores=1,fixed.seed=NULL,...)}

\arguments{

\item{lambdaC}{A matrix of positive values. Each column is the intensity vector of one of the point processes in 
	\eqn{C}. If there is only one process in \eqn{C}, it can be a vector or even a numeric value
	if the process is homogeneous.}

  \item{lambdaD}{A matrix of positive values. Each column is the intensity vector of one of the point process in 
	\eqn{D}. If there is only one process in \eqn{D}, it can be a vector or even a numeric value
	if the process is homogeneous.}

  \item{T}{Numeric value. Length of the observed period. It only must be specified
	 if all the processes are homogeneous, that is if the number of rows in \code{lambdaC} and \code{lambdaD} is 1.}


  \item{posC}{Numeric vector.  Occurrence times of the  points in all the point processes in \eqn{C}.}

  \item{typeC}{Numeric vector with the same length as \code{posC}.  Code of the point process  
	in \eqn{C}  where the points in \code{posC}  have occurred; see Details.}


  \item{posD}{Numeric vector.  Occurrence times of the  points in all the point processes in \eqn{D}.}

  \item{typeD}{Numeric vector with the same length as \code{posD}.  Code of the point process  
	in \eqn{D}  where the points in \code{posD}  have occurred.  }

  \item{r}{Optional. Numeric vector.  Grid values where  the K-function  must be evaluated. If it  is NULL, 
	a default vector is used; see Details.}

  \item{test}{Optional. Logical flag. If it is TRUE, a  test of independence
 	and a 95\%  envelope   for the K-function are calculated.}

   \item{typeEst}{Optional. Two possible values: 1 or 2, which determines which one of the two  available estimators  
	of the function \eqn{K_{ij}} has to be used; see Details.}

  \item{nTrans}{Optional. Numeric value. Only used if \code{test=TRUE}. Number of translations to
	be performed in the test and envelope calculation.}

  \item{conf}{Optional. Numeric value  in (0,1). Confidence level of the envelope for the K-function.}

  \item{rTest}{Optional. Numeric value. Maximum value of \eqn{r} used to calculate the test statistc, see Details.}

  \item{typePlot}{Optional. Character string. If it is "Kfun"  or "Kest" a plot of the values \eqn{\hat K_{xy}(r)}  
	or \eqn{\hat K_{xy}(r)/2r} is shown. With any other value, no plot is carried out.}

  \item{tit}{Optional.  Title to be used in the plot of the K-function. }

  \item{cores}{Optional. Number of cores of the computer to be used in the calculations. }

  \item{fixed.seed}{An integer or NULL. If it is an integer, that is the value  used to set the seed 
	in  random  generation processes. It it is NULL, a random seed is used.}

  \item{...}{Further arguments to  be passed to  the function \code{\link{plot}}.}

}

\details{
The information about the processes  is provided by  arguments \code{posC},  the vector of all the occurrence times  
in the processes in \eqn{C}, and  \code{typeC},  the vector of  the code of the point process  in set \eqn{C}   
where each point in \code{posC} has occurred; the second set \eqn{D} is characterized analogously by 
\code{typeD} and \code{posD}. 


This function estimates the cross K function  between 
two sets,  \eqn{C} and \eqn{D}, of (homogenous or nonhomogeneous)  point processes. Two different estimators
are available, see Cebrian et al (2020) for  details. The cross K-function measures the  dependence between two 
point processes (or  two sets of point processes) and counts the expected number of points  in any 
of the processes in \eqn{D},  within a given distance of a point in any of the processes in \eqn{C}, 
adjusted for time varying intensity in the case of nonhomogenous processes.  
The cross K-function  of independent  Poisson processes  is the length of the considered intervals,  
\eqn{K_{CD}(r)=2r}. Then, values \eqn{K_{CD}(r)/2r>1} indicate attraction between the 
 processes, while values lower than 1 indicate repulsion.


If argument \code{r} is NULL,   the following r-grid is used to evaluate the function

		 r1<-max(20, floor(T/20))

		 r<-seq(1,r1,by=2)

		 if (length(r)>200) r<-seq(1,r1,length.out=200)




Testing independence:

In order to test the independence hypothesis using this function,  a test based on a Lotwick-Silverman approach,
see Lotwick and Silverman (1982),  is implemented. This test provides a nonparametric way to test independence 
given the marginal intensities of the processes. Using the Lotwick-Silverman approach,  not only the p-value of 
the test  but also an envelope for the \eqn{K(r)} values is calculated. The test  statistic is based on the  mean
of   values \eqn{K(r)/(2r)} evaluated in a given grid of r values.

In point  processes, dependence often appears between close observations,  and  with high \eqn{r} values it is more difficult that the
K-function is able to discriminate  between dependent and independent processes.
By this reason, the argument \code{rTest} allows us to fix a maximum value of \eqn{r}
so that only K(r) estimations for \eqn{r<rTest}  will be used to
calculate the  test statistic. The value \code{rTest} is  drawn in the plot of the K-function
 as a vertical grey line.




}

\value{ A list with elements:


 \item{r}{Vector of values \eqn{r} where the cross K-function is estimated.}
 \item{NHKr}{Estimated values of  \eqn{K_{ij}(r)}.}
 \item{KenvL}{Lower bounds of the envelope of\eqn{K_{ij}(r)}.}
 \item{KenvU}{Upper bounds of the envelope of \eqn{K_{ij}(r)}.}
 \item{KStatOb}{Observed value of the test statistic.}
 \item{KStatTr}{Sample of the values of the  test statistic obtained by random translations.}
 \item{pv}{P-value of the test.}
 \item{T}{Length of the observed period of the processes.}

}


\references{
 Cebrian, A.C., Abaurrea, J.  and Asin, J. (2020).   Testing  independence between two  point processes in time.
 \emph{Journal of Simulation and Computational Statistics}.

Lotwick, H.W. and Silverman, B.W. (1982). Methods for analysing Spatial processes of several types of points. 
\emph{J.R. Statist. Soc. B}, 44(3), pp. 406-13
}



\seealso{\code{\link{NHD}}, \code{\link{NHJ}}, \code{\link{NHF}} }



\examples{
set.seed(122)
lambda1<-runif(100, 0.05, 0.1)
set.seed(121)
lambda2<-runif(100, 0.01, 0.2)
pos1<-simNHPc(lambda=lambda1, fixed.seed=123)$posNH 
pos2<-simNHPc(lambda=lambda2, fixed.seed=123)$posNH

aux<-NHK(lambdaC=lambda1, lambdaD=lambda2, posC=pos1, posD=pos2, typePlot='Kest',
	nTrans=200, cores=1, typeEst=2, fixed.seed=120)
aux$pv

#Sets with two processes
#pos3<-simNHPc(lambda=lambda1, fixed.seed=321)$posNH
#pos4<-simNHPc(lambda=lambda2, fixed.seed=321)$posNH
#aux<-NHK(lambdaC=cbind(lambda1,lambda2), lambdaD=cbind(lambda1,lambda2), posC=c(pos1,pos2), 
#	typeC=c(rep(1, length(pos1)), rep(2, length(pos2))), posD=c(pos3, pos4), 
#	typeD=c(rep(1, length(pos3)), rep(2, length(pos4))),  typeplot='Kest', fixed.seed=120)
#aux$pv

}
