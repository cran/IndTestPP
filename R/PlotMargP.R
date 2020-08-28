PlotMargP <-
function(listpos,T,  date=NULL,axispoints=NULL, tcex=1.2, ...)
{
	if (is.list(listpos)==F) stop('Argument listpos must be a list')
	d<-length(listpos)
	if (max(unlist(listpos))>T) stop('There are occurrence points  higher than T')
	posi<-0-T/10
	ind<-c(1:T)
	et<-paste('N',c(1:d), sep='')
	if (!is.null(date)&!is.null(axispoints))
	{ 
  	  	if (length(date) != T)        stop("date  must have  T observations")
    		if (max(axispoints)> T)        stop("Values in axispoints must be lower or equal than T")
		plot(listpos[[1]], rep(d, length(listpos[[1]])) ,  xlab='Time', ylab='', yaxt='n' ,  xaxt='n' ,ylim=c(0,(d+1)), 
			 xlim=c(posi, T), pch=16,main="Marginal processes", ...)
		axis(1, at=axispoints, labels=date[axispoints]) 
	}
	else
	{
		plot(listpos[[1]], rep(d, length(listpos[[1]])), type='p', xlab='Time index', ylab='', yaxt='n' ,  ylim=c(0,(d+1)), 
		 xlim=c(posi, T), pch=16,main="Marginal processes", ...)
	}
	text(posi,d,et[1],adj=0,cex=tcex, col='blue')


	for (i in c(2:d))
	{
   		points(listpos[[i]], rep((d-i+1), length(listpos[[i]])) ,  pch=16,...)
		text(posi,(d-i+1),et[i],adj=0,cex=tcex, col='blue')
	}

}
