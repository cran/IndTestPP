\name{CPSPPOTevents.fun}
\alias{CPSPPOTevents.fun}

\title{Calculates the  occurrence times of the three indicator processes of the bivariate
Common Poisson shock process resulting from applying a POT approach}


\description{This function calculates the  occurrence times and other characteristics (length, maximum and mean intensity) of the  extreme events
 of the three indicator processes of a bivariate Common Poisson shock process (CPSP) obtained from a POT approach. 
}

\usage{CPSPPOTevents.fun(X, Y, thresX, thresY, date = NULL)}

\arguments{
  \item{X}{Numeric vector. Series \eqn{(x_i)} whose threshold exceedances define the first CPSP marginal process.}
  \item{Y}{Numeric vector. Series \eqn{(y_i)} whose threshold exceedances define the second CPSP marginal process.}
  \item{thresX}{Numeric value. Threshold  used to define the extreme events in \eqn{(x_i)}.}
  \item{thresY}{Numeric value. Threshold used to define the extreme events  in \eqn{(y_i)}.}
  \item{date}{Optional. A vector or matrix indicating the date of each observation.}
}

\details{

A CPSP \eqn{N} can be  decomposed into three independent indicator processes: \eqn{N_{(1)}}, \eqn{N_{(2)}}  
and \eqn{N_{(12)}},  the processes of the points  occurring
only in the first  marginal process, only in the second and  in both of them (simultaneous points).
 In the CPSP resulting from applying a POT approach, the events in  \eqn{N_{(1)}}
are  a run of consecutive days where \eqn{(x_i)} exceeds its  extreme threshold but \eqn{(y_i)} does not exceed its extreme threshold. An extreme event  in  \eqn{N_{(2)}}
 is defined analogously. A simultaneous  event,  or event in  \eqn{N_{(12)}},
is a run where both series  exceed  their thresholds. 

For the events  defined in each indicator process, three magnitudes (length, maximum intensity and mean intensity) 
are calculated together with the   initial point and the point of maximum excess of each event. In the 
\eqn{N_{(12)}}, the maximum and the  mean intensity of  both \eqn{(x_i)} and
\eqn{(y_i)} are also calculated. The occurrence point of each event is located at the time of the maximum   sum of the excesses over 
the threholds  (where an excess is 0  if the observation does not exceed  
its corresponding threshold). According to this definition, the occurrence point in 
\eqn{N_{(1)}} is the point  with maximum intensity in \eqn{(x_i)} within  the run. 

 
The vectors \code{inddatX}, \code{inddatY} and \code{inddatXY}, elements of the output list,
 mark the observations  that should be used in the estimation of each indicator  process.  The 
observations in an extreme event which are not the occurrence point are marked with 0
and treated as non observed in the estimation process.  The rest are marked with 1  and 
must be included in the likelihood function.}

\value{A list with components
  \item{ImX }{Vector of  mean excesses (over the threshold) of the extreme events in \eqn{N_{(1)}}.}
  \item{IxX}{Vector of  maximum excesses (over the threshold) of the extreme events in \eqn{N_{(1)}}.}
  \item{LX }{Vector of  lengths of the extreme events in \eqn{N_{(1)}}.}
  \item{PxX }{Vector of  points of  maximum excess of the extreme events in \eqn{N_{(1)}}.}
  \item{PiX }{Vector of  initial points of the extreme events in \eqn{N_{(1)}}.}
  \item{inddatX }{Index equal to 1 in the observations which should be used in the estimation process  of \eqn{N_{(1)}}, and  to 0  in the others.}
  \item{ImY }{Vector of  mean excesses (over the threshold) of the extreme events in \eqn{N_{(2)}}.}
  \item{IxY}{Vector of maximum excesses (over the threshold) of the extreme events in \eqn{N_{(2)}}.}
  \item{LY }{Vector of  lengths of the extreme events in \eqn{N_{(2)}}.}
  \item{PxY }{Vector of  points of  maximum excess of the extreme events in \eqn{N_{(2)}}.}
  \item{PiY }{Vector of   initial points of the extreme events in \eqn{N_{(2)}}.}
  \item{inddatY }{Index equal to 1 in the observations which should be used in the estimation process  of  
	\eqn{N_{(2)}} and  to 0  in the others.}
  \item{ImXYx }{Vector of  mean excesses of the series \eqn{(x_i)} in \eqn{N_{(12)}}.}
  \item{IxXYx }{Vector of  maximum excesses  the series \eqn{(x_i)} in \eqn{N_{(12)}}.}
  \item{ImXYy }{Vector of  mean excesses of the series \eqn{(y_i)} in \eqn{N_{(12)}}.}
  \item{IxXYy }{Vector of  maximum excesses  the series \eqn{(y_i)} in \eqn{N_{(12)}}.}
  \item{LXY }{Vector of lengths of the extreme events in \eqn{N_{(12)}}.}
  \item{PxXY }{Vector of points of  maximum excess of the extreme events in \eqn{N_{(12)}}.}
  \item{PiXY }{Vector of  initial points of the extreme events in \eqn{N_{(12)}}.}
  \item{inddatXY }{Index equal to 1 in the observations which should be used in the estimation process  of 
	\eqn{N_{(12)}} and  to 0  in the others.}
  \item{X}{Input argument.}
  \item{Y}{Input argument.}
  \item{thresX}{Input argument.}
  \item{thresY}{Input argument.}
  \item{date}{Input argument.}
}

\references{
Abaurrea, J. Asin, J. and Cebrian, A.C. (2015). A Bootstrap Test of Independence Between Three Temporal Nonhomogeneous Poisson Processes 
and its Application to Heat Wave Modeling. \emph{Environmental and Ecological Statistics}, 22(1), 127-144.
}


\seealso{
 \code{\link{CPSPpoints.fun}}}


\examples{
data(BarTxTn)
dateB<-cbind(BarTxTn$ano,BarTxTn$mes,BarTxTn$diames)
BarBivEv<-CPSPPOTevents.fun(X=BarTxTn$Tx,Y=BarTxTn$Tn,thresX=318,
	thresY=220, date=dateB)
}