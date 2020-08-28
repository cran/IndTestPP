\name{CPSPPOTevents}
\alias{CPSPPOTevents}

\title{Identifying the  occurrence points  of the indicator processes in  the CPSP from a POT approach}


\description{This function calculates the  occurrence times and other characteristics (length, maximum and mean intensity) of the  extreme events
 of the three indicator processes of a bivariate Common Poisson Shock Process (CPSP) obtained from a 
Peak Over Threshold (POT) approach.}

\usage{CPSPPOTevents(N1,N2,thres1,thres2, date=NULL, dplot=T, pmfrow=c(2,1), 
	axispoints=NULL,...)}

\arguments{
  \item{N1}{Numeric vector. Series \eqn{(x_i)} whose threshold exceedances define the first CPSP marginal process.}
  \item{N2}{Numeric vector. Series \eqn{(y_i)} whose threshold exceedances define the second CPSP marginal process.}
  \item{thres1}{Numeric value. Threshold  used to define the extreme events in \eqn{(x_i)}.}
  \item{thres2}{Numeric value. Threshold used to define the extreme events  in \eqn{(y_i)}.}
  \item{date}{Optional. A vector or matrix indicating the date of each observation.}
  \item{dplot}{Optional. A logical flag. If it is TRUE,  the  marginal and indicator processes  are plotted.}
  \item{pmfrow}{Optional.  A vector of  the form (nr, nc) to be supplied as value of the argument \code{mfrow} in  \code{\link{par}}. }
  \item{axispoints}{Optional. Numeric vector with the  points in the time index where axis ticks and labels (from the first column in \code{date})
	 have to be drawn.}
  \item{...}{Further arguments to  be passed to  the function \code{\link{plot}}.}
}

\details{
A CPSP \eqn{N} can be  decomposed into three independent indicator processes: \eqn{N_{(1)}}, \eqn{N_{(2)}}  
and \eqn{N_{(12)}},  the processes of the points  occurring
only in the first  marginal process, only in the second and  in both of them (simultaneous points).
 In the CPSP resulting from applying a POT approach, the events in  \eqn{N_{(1)}}
are  a run of consecutive observations where \eqn{x_i} exceeds its  extreme threshold but \eqn{y_i} does not exceed 
its extreme threshold. An extreme event  in  \eqn{N_{(2)}} is defined analogously. A simultaneous  event,  or event 
in  \eqn{N_{(12)}}, is a run where both series  exceed  their thresholds. 

For the events  defined in each indicator process, three magnitudes (length, maximum intensity and mean intensity) 
are calculated together with the   initial point and the point of maximum excess of each event. In  
\eqn{N_{(12)}}, the maximum and the  mean intensity  in  both  series \eqn{(x_i)} and
\eqn{(y_i)} are  calculated. 

The occurrence point of each event is  the time  in the run where the maximum   of the sum of the excesses of   
\eqn{(x_i)} and \eqn{(y_i)} over their threholds   occurs;  if an observation does not exceed  
its corresponding threshold,  that  excess is 0. According to this definition, the occurrence point in 
\eqn{N_{(1)}} is the point  with maximum intensity in \eqn{(x_i)} within  the run. 

 
The vectors \code{inddat1}, \code{inddat2} and \code{inddat12}, elements of the output list,
mark the observations  that should be used in the estimation of each indicator  process.  The 
observations in an extreme event which are not the occurrence point are marked with 0
and treated as non observed in the estimation process.  The rest are marked with 1  and 
must be included in the likelihood function. See  function \code{fitPP.fun} in package NHPoisson
for more details on the use of these indexes in the estiamtion of a point process.

 The  points in the  marginal  \eqn{N_{1}}, \eqn{N_{2}}  and indicator  
\eqn{N_{(1)}}, \eqn{N_{(2)}}  and \eqn{N_{(12)}}  processes can be optionally plotted. 
If  \code{date} is NULL, default axis are used. Otherwise, the values in \code{axispoints} are used 
as the  points in the time index where axis ticks and labels, from  the first column in \code{date},  
have to be drawn.  If  \code{axispoints} is NULL, a default grid of points  is built  using  the 
function \code{marca}.
}

\value{A list with components

  \item{Im1}{Vector of  mean excesses (over the threshold) of the extreme events in \eqn{N_{(1)}}.}
  \item{Ix1}{Vector of  maximum excesses (over the threshold) of the extreme events in \eqn{N_{(1)}}.}
  \item{L1}{Vector of  lengths of the extreme events in \eqn{N_{(1)}}.}
  \item{Px1}{Vector of  points of  maximum excess of the extreme events in \eqn{N_{(1)}}.}
  \item{Pi1}{Vector of  initial points of the extreme events in \eqn{N_{(1)}}.}
  \item{inddat1 }{Index  of the observations to be used in the estimation process  of \eqn{N_{(1)}}.}
  \item{Im2}{Vector of  mean excesses (over the threshold) of the extreme events in \eqn{N_{(2)}}.}
  \item{IxY}{Vector of maximum excesses (over the threshold) of the extreme events in \eqn{N_{(2)}}.}
  \item{L2}{Vector of  lengths of the extreme events in \eqn{N_{(2)}}.}
  \item{Px2}{Vector of  points of  maximum excess of the extreme events in \eqn{N_{(2)}}.}
  \item{Pi2}{Vector of   initial points of the extreme events in \eqn{N_{(2)}}.}
  \item{inddat2 }{Index of the observations to be used in the estimation process  of  
	\eqn{N_{(2)}}.}
  \item{Im121}{Vector of  mean excesses of the series \eqn{(x_i)} in \eqn{N_{(12)}}.}
  \item{Ix121}{Vector of  maximum excesses  the series \eqn{(x_i)} in \eqn{N_{(12)}}.}
  \item{Im122}{Vector of  mean excesses of the series \eqn{(y_i)} in \eqn{N_{(12)}}.}
  \item{Ix122}{Vector of  maximum excesses  the series \eqn{(y_i)} in \eqn{N_{(12)}}.}
  \item{L12}{Vector of lengths of the extreme events in \eqn{N_{(12)}}.}
  \item{Px12}{Vector of points of  maximum excess of the extreme events in \eqn{N_{(12)}}.}
  \item{Pi12}{Vector of  initial points of the extreme events in \eqn{N_{(12)}}.}
  \item{inddat12}{Index of the observations to be used in the estimation process  of \eqn{N_{(12)}}.}
  \item{N1}{Input argument.}
  \item{N2}{Input argument.}
  \item{thres1}{Input argument.}
  \item{thres1}{Input argument.}
  \item{date}{Input argument.}
}

\references{
Abaurrea, J. Asin, J. and Cebrian, A.C. (2015). A Bootstrap Test of Independence Between Three Temporal Nonhomogeneous Poisson Processes 
and its Application to Heat Wave Modeling. \emph{Environmental and Ecological Statistics}, 22(1), 127-144.
}


\seealso{ \code{\link{CPSPpoints}}, \code{\link{PlotMCPSP}}, \code{\link{PlotICPSP}} }


\examples{
data(TxBHZ)
dateT<-cbind(TxBHZ$year,TxBHZ$month,TxBHZ$day)  #year, month and day of the month
marca<- c(1:length(TxBHZ$TxH))[c(1,diff(dateT[,1]))==1]  # points at  first day  of the year
BivEv<-CPSPPOTevents(N1=TxBHZ$TxH,N2=TxBHZ$TxZ,thres1=378,thres2=364, date=dateT, 
       axispoints=marca)
}