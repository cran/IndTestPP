PlotMCPSP <-
function(pos1,pos2, T, date=NULL, axispoints=NULL, ...)
{
	if (max(pos1, pos2)>T) stop('There are some points outside the observation period')
	posi<-0-T/10

	if (!is.null(date)&!is.null(axispoints)){ 
    	if (length(date) != T)        stop("date  must have  T observations")
    	if (max(axispoints)> T)        stop("Values in axispoints must be lower or equal than T")
	plot(pos1, rep(2, length(pos1)) ,  xlab='Time', ylab='', yaxt='n' ,  xaxt='n' ,ylim=c(0,3), 
		 xlim=c(posi, T), pch=16,main="Marginal processes of a CPSP", ...)
	axis(1, at=axispoints, labels=date[axispoints]) 
	}
	else
	{
	plot(pos1, rep(2, length(pos1)) ,  xlab='Time index', ylab='', yaxt='n' ,  ylim=c(0,3), 
		 xlim=c(posi, T), pch=16,main="Marginal processes of a CPSP", ...)
	}
      points(pos2, rep(1, length(pos2)) ,  pch=16)
	posi12<- intersect(pos1, pos2)
	points(posi12, rep(2, length(posi12)) ,  pch=16, col=2)
	points(posi12, rep(1, length(posi12)) ,  pch=16, col=2)
	text(posi,2,expression(N[1]),adj=0,cex=1.2, col='blue')
	text(posi,1,expression(N[2]),adj=0,cex=1.2, col='blue')
}
