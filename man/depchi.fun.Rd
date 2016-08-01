\name{depchi.fun}
\alias{depchi.fun}


\title{Estimates extremal dependence  measures between two variables}

\description{ This function estimates the  extremal dependence coefficients  \eqn{\chi} and \eqn{\bar \chi}  
 by Coles  et al. (1999). 
It also  plots the functions \eqn{\chi(u)} and \eqn{\bar \chi(u)}  against a grid of values in [0,1] 
to analyse the extremal dependence of two variables.}

\usage{depchi.fun(X, Y, thresval = c(0:99)/100, tit = "", indgraph = TRUE, 
	bothest = TRUE, xlegend = "topleft")}


\arguments{
  \item{X}{Numeric vector. Values of the   first variable.}
  \item{Y}{Numeric vector. Values of the  second variable.}
  \item{thresval}{Numeric vector. Grid  values   where the functions  \eqn{\chi(u)} and \eqn{\bar \chi(u)} are evaluated.}
  \item{tit}{Character string. A title for the plots.}
  \item{indgraph}{Logical flag. If it is TRUE, plots are carried out in    individual windows. 
	 If it is FALSE, windows with 2 \eqn{\times} 1 plots   are used.}
  \item{bothest}{Logical flag. If it is TRUE,  two estimated  coefficientes (for X given Y  and  for Y given X)
are displayed in the same plot. Otherwise, only the coefficient  for Y given X is plotted.}
  \item{xlegend}{Optional. Label "topleft" or"bottomright". Position where the legend on the graph  will be located.}
}

\details{

The extremal dependence between  two variables X and Y
is the tendency for one variable to be large, given that the other one is large.
The  extremal dependence coefficients \eqn{\chi} and \eqn{\bar \chi}  are defined as
\eqn{\chi= \lim_{u \to 1} \chi(u)} and  \eqn{\bar \chi(u)= 2log(P(U>u)/log P(U>u, V>u)-1},
where \eqn{\chi(u)= P(U>u |V>u)}  and  (U,V) are  the  transformed uniform marginals of the variables X and Y.


\eqn{\chi} is on the scale [0, 1], with the set (0, 1] corresponding to asymptotic
dependence, and the measure \eqn{\bar \chi} falls within the range [-1, 1], with the set [-1, 1) 
corresponding to asymptotic independence. Thus, the complete pair (\eqn{\chi},  \eqn{\bar \chi})
is required as a summary of extremal dependence: (\eqn{\chi > 0}, \eqn{\bar \chi =1}) signifies asymptotic 
dependence, in which case the value of \eqn{\chi} determines a measure of strength of dependence within 
the class; alternatively, (\eqn{\chi=0}, \eqn{\bar \chi <1}) signifies asymptotic independence, in which case the value of
\eqn{\bar \chi}  determines the strength of dependence within this class.
Full details can be found in Coles  et al. (1999).

In the \eqn{\chi} plot, the expected behaviour under independence of X and Y is also plotted.

}

\value{ A list with elements
  \item{chiX }{Estimated \eqn{\chi} function for Y given X evaluated at the threshold grid.}
  \item{chiY }{Estimated \eqn{\chi} function for X given Y evaluated at the threshold grid.}
  \item{chiBX }{Estimated \eqn{\bar \chi} function for Y given X evaluated at the threshold grid.}
  \item{chiBY }{Estimated \eqn{\bar \chi} function for X given Y evaluated at the threshold grid.}
  \item{PX }{Estimation of the probabilities \eqn{P(U<thresval)}}
  \item{PY }{Estimation of the probabilities \eqn{P(V<thresval)}}
  \item{PXY }{Estimation of the probabilities \eqn{P[(U<thresval)\&(V<thresval)]} }
  \item{thresval }{Input argument}
}

\references{
Coles, S., Heffernan, J. and Tawn, J. (1999) Dependence measures for extreme value analysis.
Extremes, 2, 339-365.
}

\seealso{\code{\link{TestIndNH.fun}}, \code{\link{DutilleulPlot.fun}}, \code{\link{CondTest.fun}} }

\examples{

data(BarTxTn)

aux<-depchi.fun(X=BarTxTn$Tx,Y=BarTxTn$Tn, thresval = c(0:99)/100, 
	tit = "Barcelona", xlegend = "topleft")
}