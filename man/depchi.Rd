\name{depchi}
\alias{depchi}


\title{Estimating extremal dependence coefficientes}

\description{This function estimates  and plots the  extremal dependence functions
 \eqn{\chi(u)} and \eqn{\bar \chi(u)}  against a grid of values in [0,1] 
to analyse the extremal dependence of two variables.}

\usage{depchi(X, Y, thresval = c(0:99)/100, tit = "", indgraph = TRUE, bothest = TRUE, 
       xlegend = "topleft",mfrow=c(1,2),...)}


\arguments{
  \item{X}{Numeric vector. Values of the   first variable.}
  \item{Y}{Numeric vector. Values of the  second variable.}
  \item{thresval}{Numeric vector. Grid  values   where the functions  \eqn{\chi(u)} and \eqn{\bar \chi(u)} are evaluated.}
  \item{tit}{Character string. A title for the plots.}
  \item{indgraph}{Logical flag. If it is TRUE, plots are shown in separate windows. 
	 If it is FALSE, the layout in \code{mfrow} is used.}
  \item{bothest}{Logical flag. If it is TRUE,  two estimated  coefficientes (for X given Y  and  for Y given X)
are displayed in the same plot. Otherwise, only the coefficient  for Y given X is plotted.}
  \item{xlegend}{Optional. Label "topleft","bottomright", etc. Position where the legend on the graph  will be located.}
  \item{mfrow}{Optional. A vector of the form c(2, 1) or c(1,2). It gives the layout to draw the two figures in the function. }  
\item{...}{Further arguments to  be passed to  the function \code{\link{plot}}.}
}

\details{

The extremal dependence between  two variables X and Y
is the tendency for one variable to be large, given that the other one is large.
The  extremal dependence coefficients \eqn{\chi} and \eqn{\bar \chi}  are defined as
\eqn{\chi= \lim_{u \to 1} \chi(u)} where \eqn{\chi(u)= P(U>u |V>u)}  and  (U,V) are  the  
transformed uniform marginals of the variables X and Y.

\eqn{\bar \chi= \lim_{u \to 1} \bar \chi(u)} where \eqn{\bar \chi(u)= 2log P(U>u)/log P(U>u, V>u)-1}.

The function plots  \eqn{\chi(u)} and \eqn{\bar \chi(u)}. These graphs 
can be used to estimate \eqn{\hat \chi} and \eqn{\widehat{\bar \chi}}, the limits of the functions.
In the \eqn{\chi (u)} plot, the expected behaviour under independence of X and Y is also plotted.



\eqn{\chi} is on the scale [0, 1], with the set (0, 1] corresponding to asymptotic
dependence, and the measure \eqn{\bar \chi} falls within the range [-1, 1], with the set [-1, 1) 
corresponding to asymptotic independence. See  Coles  et al. (1999) for more details on the definition 
and interpretation of these indexes.

}

\value{ A list with elements
  \item{chiX }{Estimated \eqn{\chi (u)} function for Y given X evaluated at the threshold grid.}
  \item{chiY }{Estimated \eqn{\chi (u)} function for X given Y evaluated at the threshold grid.}
  \item{chiBX }{Estimated \eqn{\bar \chi (u) } function for Y given X evaluated at the threshold grid.}
  \item{chiBY }{Estimated \eqn{\bar \chi (u)} function for X given Y evaluated at the threshold grid.}
  \item{PX }{Estimation of the probabilities \eqn{P(U<thresval)}.}
  \item{PY }{Estimation of the probabilities \eqn{P(V<thresval)}.}
  \item{PXY }{Estimation of the probabilities \eqn{P[(U<thresval)\&(V<thresval)]}. }
  \item{thresval }{Input argument.}
}

\references{
Coles, S., Heffernan, J. and Tawn, J. (1999) Dependence measures for extreme value analysis.
Extremes, 2, 339-365.
}

\seealso{\code{\link{CountingCor}}, \code{\link{BinPer}} }

\examples{

data(TxBHZ)

aux<-depchi(X=TxBHZ$TxZ,Y=TxBHZ$TxH, thresval = c(0:99)/100, 
	tit = "Tx Zaragoza and Tx Huesca", xlegend = "bottom",indgraph="FALSE")
}