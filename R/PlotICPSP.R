PlotICPSP <-
function(posi1,posi2, posi12, T, date=NULL,axispoints=NULL, ...)
{
	if (max(posi1, posi2, posi12)>T) stop('There are some points outside the observation period')
	posi<-0-T/10

	if (!is.null(date)&!is.null(axispoints))
	{ 
    	if (length(date) != T)        stop("date  must have  T observations")
    	if (max(axispoints)> T)        stop("Values in axispoints must be lower or equal than T")
	plot(posi1, rep(3, length(posi1)) ,  xlab='Time', ylab='', yaxt='n', xaxt='n', ylim=c(0,4),
		xlim=c(posi, T),pch=16,main="Indicator processes of a CPSP", ...)
	axis(1, at=axispoints, labels=date[axispoints]) 
	}
	else
	{
	plot(posi1, rep(3, length(posi1)) ,  xlab='Time index', ylab='', yaxt='n', ylim=c(0,4),
		xlim=c(posi, T),pch=16,main="Indicator processes of a CPSP", ...)
	}
      points(posi2, rep(2, length(posi2)) ,  pch=16)
	points(posi12, rep(1, length(posi12)) ,  pch=16)
	text(posi,3,expression(N[(1)]),adj=0,cex=1.2, col='blue')
	text(posi,2,expression(N[(2)]),adj=0,cex=1.2, col='blue')
	text(posi,1,expression(N[(12)]),adj=0,cex=1.2, col='blue')
}
