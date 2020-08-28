\name{NHJ}
\alias{NHJ}
\alias{NHJaux}
\alias{NHDFaux}
\alias{Jenv}

\title{Estimating the cross J-function  and testing independence}

\description{This function estimates the  cross J-function  between 
two sets, \eqn{C} and \eqn{D},  of (homogenous or nonhomogeneous)  point processes in time.  
It  is evaluated in a grid of distances \eqn{r},  and it can be optionally plotted.  A test to 
assess the independence between the sets of processes, based on  the cross J-function,   is also implemented.


It calls the auxiliary functions NHJaux and Jenv, not intended for users.}

\usage{NHJ(lambdaC, lambdaD,T=NULL, Ptype="inhom", posC, typeC=1, posD, typeD=1, r=NULL,
L=NULL,test=FALSE,nTrans=100, rTest=NULL, conf=0.95, dplot=NULL, 
tit=c("J-function","D-function","F-function"),mfrow=NULL,cores=1,fixed.seed=NULL,...)}


\arguments{
\item{lambdaC}{A matrix of positive values. Each column is the intensity vector of one of the point processes in 
\eqn{C}. If there is only one process in \eqn{C}, it can be a vector or even a numeric value
if the process is homogeneous.}

\item{lambdaD}{A matrix of positive values. Each column is the intensity vector of one of the point process in 
\eqn{D}. If there is only one process in \eqn{D}, it can be a vector or even a numeric value
if the process is homogeneous.}

\item{T}{Numeric value. Length of the observed period. It only must be specified
 if the number of rows in \code{lambdaC} and \code{lambdaD} is 1.}

\item{Ptype}{Optional. Label: "hom" or "inhom".  The first one indicates that 
all the point processes in sets \eqn{C} and \eqn{D} are homogeneous.}

  \item{posC}{Numeric vector.  Occurrence times of the  points in all the point processes in \eqn{C}.}

  \item{typeC}{Numeric vector with the same length as \code{posC}.  Code of the point process  
in \eqn{C}  where the points in \code{posC}  have occurred. See Details.}

  \item{posD}{Numeric vector.  Occurrence times of the  points in all the point processes in \eqn{D}.}

  \item{typeD}{Numeric vector with the same length as \code{posD}.  Code of the point process  
in \eqn{D}  where the points in \code{posD}  have occurred.}

  \item{r}{Optional. Numeric vector. Values where J-function  must be evaluated. If it  is NULL, a default 
vector is used, see Details.}

  \item{L}{Optional. Numeric vector. Values in the observed period used to calculate 
the J-function. If it  is NULL, a default vector is used, see Details.}

  \item{test}{Optional. Logical flag. If it is TRUE, a  test of independence
  and a 95\%  envelope   for the J-function are calculated.}

  \item{nTrans}{Optional. Numeric value. Only used if \code{test=TRUE}. Number of translations to
be performed in the test and envelope calculation.}


  \item{rTest}{Optional. Numeric value. Maximum value of \eqn{r} used to calculate the 
independence test statistc, see Details.}

  \item{conf}{Optional. Numeric value  in (0,1). Confidence level of the envelope for the J-function.}

  \item{dplot}{Optional. Label "JDF" or "J". If it is "JDF", plots of J, D and F-functions
 are displayed. If it is "J", only  J-function is plotted.}

  \item{tit}{Optional.  A vector with  one or  three titles to be used in the plots of J, D and F-functions.}
 
 \item{mfrow}{Optional. Argument to be passed to \code{\link{par}} for the plot of the J-function.}

  \item{cores}{Optional. Number of cores of the computer to be used in the calculations. }

  \item{fixed.seed}{An integer or NULL. If it is an integer, that is the value  used to set the seed 
in  random  generation processes. It it is NULL, a random seed is used.}

  \item{...}{Further arguments to  be passed to  the function \code{\link{plot}}.}
}

\details{

The information about the processes  is provided by  arguments \code{posC},  the vector of all the occurrence times  
in the processes in \eqn{C}, and  \code{typeC},  the vector of  the code of the point process  in set \eqn{C}
where each point in \code{posC} has occurred;  the second set \eqn{D} is characterized analogously by 
\code{typeD} and \code{posD}. 


This function estimates the cross J-function  between 
two sets,  \eqn{C} and \eqn{D}, of (homogenous or nonhomogeneous)  time point processes, see 
Cebrian et al (2020) for details of the estimation. The J-function measures the interpoint dependence between points in any 
of the processes in \eqn{D},  and points in any of the processes in \eqn{C}, adjusted for time varying intensity in the case 
of nonhomogenous processes. The cross J-function is defined as \eqn{J_{CD}(r)=(1-D_{CD}(r))/(1-F_D(r))}, 
if \eqn{F_D(r)<1} and it is not calculated otherwise.  It compares \eqn{D_{CD}(r)}, the distribution 
function of the distances from a  point  in any of the processes in set \eqn{C} to the nearest
point in any of the processes in set \eqn{D},  to \eqn{F_{D}(r)},  the distribution 
function of the distances from a fixed point   in the space to the nearest
point in any of the processes in set \eqn{D}.

If argument \code{r} is NULL,   the following grid is used to evaluate the function

		 r1<-max(20, floor(T/20))

		 r<-seq(1,r1,by=2)

		 if (length(r)>200) r<-seq(1,r1,length.out=200)


If argument \code{L} is NULL, the following grid is used 

        L <- seq(1, T, by = 2)
        if (length(L) > 5000)  L <- seq(1, T, by = round((T - 1)/199))





Testing independence:

If the processes in \eqn{C}  are independent of the processes in \eqn{D} given the marginal structure 
of the processes, the J-funtion is equal to 1, since D(r)=F(r). Hence, deviations of J(r) estimations from 1, 
suggest dependence betweent  the two sets of processes. The test  statistic is based on the  mean
of   values \eqn{|J(r)-1|} evaluated in a given grid of r values.

A test based on a Lotwick-Silverman approach, see Lotwick and Silverman (1982),  is implemented. This test 
provides a nonparametric way to test independence given the marginal intensities of the processes. 
Using the Lotwick-Silverman approach,  not only the p-value of 
the test  but also an envelope for the \eqn{J(r)} values is calculated. 


In point  processes, dependence often appears between close observations,  and  with high \eqn{r} values it is more 
difficult that the J-function is able to discriminate  between dependent and independent processes.
By this reason, the argument \code{rTest} allows us to fix a maximum value of \eqn{r}
so that only \eqn{J(r)} estimations for \eqn{r<rTest}  will be used to
calculate the  test statistic. The value \code{rTest} is  drawn in the plot of the J-function
 as a vertical grey line.
}

\value{A list with elements:

 \item{r}{Vector of values \eqn{r} where the J-function is estimated.}
 \item{NHJr}{Estimated values of \eqn{J_{CD}(r)}.}
 \item{NHDr}{Estimated values of \eqn{D_{CD}(r)}.}
 \item{NHFr}{Estimated values of \eqn{F_{D}(r)}.}
 \item{JenvL}{Lower bounds of the envelope of \eqn{J_{CD}(r)}.}
 \item{JenvU}{Upper bounds of the envelope for \eqn{J_{CD}(r)}.}
 \item{JStatOb}{Observed value of the statistic.}
 \item{JStatTr}{Sample of the values of the  test statistic obtained by random translations.}
 \item{pv}{P-value of the independence test.}
 \item{T}{Length of the observed period of the process.}
 \item{L}{Grid of  L values to calculate  the F-funtion. }
}


\references{

 Cebrian, A.C., Abaurrea, J.  and Asin, J. (2020).   Testing  independence between two  point processes in time.
 \emph{Journal of Simulation and Computational Statistics}.

Cronie, O. and van Lieshout, M.N.M. (2015). Summary statistics for inhomogeneous marked point
processes. \emph{Ann Inst Stat Math}. 

Lotwick, H.W. and Silverman, B.W. (1982). Methods for analysing Spatial processes of several types of points. 
\emph{J.R. Statist. Soc. B}, 44(3), pp. 406-13
}

\seealso{\code{\link{NHK}}, \code{\link{NHD}}, \code{\link{NHF}} }


\examples{
set.seed(120)
lambda1<-runif(100, 0.05, 0.1)
set.seed(121)
lambda2<-runif(100, 0.01, 0.2)
pos1<-simNHPc(lambda=lambda1,fixed.seed=123)$posNH  
pos2<-simNHPc(lambda=lambda2,fixed.seed=123)$posNH

aux<-NHJ(lambdaC=lambda1, lambdaD=lambda2, posC=pos1,nTrans=50, 
	 posD=pos2, rTest=7, dplot='J', cores=1,test=TRUE)
aux$pv

#Sets with two processes
#pos3<-simNHPc(lambda=lambda1,fixed.seed=300)$posNH  
#pos4<-simNHPc(lambda=lambda2,fixed.seed=30)$posNH 
#aux<-NHJ(lambdaC=cbind(lambda1,lambda2), lambdaD=cbind(lambda1,lambda2), 
#	posC=c(pos1,pos2), typeC=c(rep(1, length(pos1)), rep(2, length(pos2))), 
#	posD=c(pos3, pos4), typeD=c(rep(1, length(pos3)), rep(2, length(pos4))), 
#	dplot='J', test=TRUE)
#aux$pv
}