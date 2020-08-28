\name{PlotICPSP}
\alias{PlotICPSP}


\title{Plotting  the  occurrence points  of the indicator processes in  a CPSP}


\description{This function  plots the points in the three indicator processes \eqn{N_{(1)}}, \eqn{N_{(2)}}  
and \eqn{N_{(12)}} of a bivariate Common Poisson shock process (CPSP). 
}

\usage{PlotICPSP(posi1,posi2, posi12, T, date=NULL,axispoints=NULL, ...)}

\arguments{
  \item{posi1}{Numeric vector  of the  points in  \eqn{N_{(1)}} }
  \item{posi2}{Numeric vector  of the  points in  \eqn{N_{(2)}}}
  \item{posi12}{Numeric vector  of the  points in  \eqn{N_{(12)}}}
  \item{T}{Numeric value. The length of the observed period of the CPSP.}
  \item{date}{Optional. A vector indicating the date of each observation to be used in the axis of the plot.}
  \item{axispoints}{Optional. Numeric vector with the  points in the time index in which axis ticks and labels 
	(from \code{date}) will be drawn.}
  \item{...}{Further arguments to  be passed to  the function \code{\link{plot}}.}
}

\details{

A CPSP \eqn{N} can be  decomposed into three independent indicator processes: \eqn{N_{(1)}}, \eqn{N_{(2)}}  
and \eqn{N_{(12)}},  the processes of the points  occurring
only in the first  marginal process, only in the second and  in both of them (simultaneous points).

 The  points in  the three indicator processes are plotted versus the time index. If  one of the arguments \code{date} and
\code{axispoints} is NULL, default axis are used. Otherwise, the values in \code{axispoints} are used as
the  points in the time index in which axis ticks and labels from \code{date} are drawn.
}

\value{A plot.}




\seealso{
 \code{\link{CPSPpoints}}, \code{\link{PlotMCPSP}}, \code{\link{PlotMargP}}}


\examples{

data(TxBHZ)
T<-length(TxBHZ$TxH)
dateT<-cbind(TxBHZ$year,TxBHZ$month,TxBHZ$day)  #year, month and day of the month
marca<- c(1:length(TxBHZ$TxH))[c(1,diff(dateT[,1]))==1]  #points at first day of the year
BivEv<-CPSPPOTevents(N1=TxBHZ$TxH,N2=TxBHZ$TxZ,thres1=37.8, thres2=36.4, date=dateT, 
                     axispoints=marca)
PlotICPSP(posi1=BivEv$Px1,posi2=BivEv$Px2, posi12=BivEv$Px12, T=T)
PlotICPSP(posi1=BivEv$Px1,posi2=BivEv$Px2, posi12=BivEv$Px12, T=T, date=dateT[,1], 
          axispoints=marca)
}