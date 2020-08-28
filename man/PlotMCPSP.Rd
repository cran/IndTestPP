\name{PlotMCPSP}
\alias{PlotMCPSP}


\title{Plotting  the  occurrence points  of the marginal processes in  a CPSP}


\description{This function  plots the points in the  two marginal processes \eqn{N_{1}}, \eqn{N_{2}}  
of a bivariate Common Poisson shock process (CPSP). 
}

\usage{PlotMCPSP(pos1,pos2, T, date=NULL, axispoints=NULL, ...)}

\arguments{
  \item{pos1}{Numeric vector  of the  points in  \eqn{N_{1}} }
  \item{pos2}{Numeric vector  of the  points in  \eqn{N_{2}}}
  \item{T}{Numeric value. The length of the observed period of the CPSP.}
  \item{date}{Optional. A vector indicating the date of each observation to be used in the axis of the plot.}
  \item{axispoints}{Optional. Numeric vector with the  points in the time index in which axis ticks and labels 
    (from \code{date}) will be drawn.}
  \item{...}{Further arguments to  be passed to the function \code{\link{plot}}.}
}

\details{

The  points in  the  two marginal processes \eqn{N_{1}}, \eqn{N_{2}}   of a bivariate CPSP are plotted versus the time index. 
The simultaneous points (points of the indicator process  \eqn{N_{(12)}}) are drawn in red.

If  one of the arguments \code{date} and \code{axispoints} is NULL, default axis are used. 
Otherwise, the values in \code{axispoints} are used as the  points in the time index in which axis ticks 
and labels, from \code{date},  are drawn.
}

\value{A plot.}

\seealso{ \code{\link{CPSPpoints}}, \code{\link{PlotICPSP}} }


\examples{
data(TxBHZ)
T<-length(TxBHZ$TxH)
dateT<-cbind(TxBHZ$year,TxBHZ$month,TxBHZ$day)  #year, month and day of the month
marca<- c(1:T)[c(1,diff(dateT[,1]))==1]  # points at  first day  of the year
BivEv<-CPSPPOTevents(N1=TxBHZ$TxH,N2=TxBHZ$TxZ,thres1=37.8, thres2=36.4, date=dateT,
                    axispoints=marca)
PlotMCPSP(pos1=union(BivEv$Px1, BivEv$Px12),pos2=union(BivEv$Px2,BivEv$Px12), T=T)

marca<- c(1:T)[c(1,diff(dateT[,1]))==1]
PlotMCPSP(pos1=union(BivEv$Px1, BivEv$Px12),pos2=union(BivEv$Px2,BivEv$Px12), T=T, 
	date=dateT[,1], axispoints=marca)

}