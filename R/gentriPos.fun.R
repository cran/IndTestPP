gentriPos.fun <-
function(dataXY,dataYZ,info=FALSE,procName=c('X','Y','Z'),xlab=NULL,...)
{
	XT<-NULL
	YT<-NULL
	ZT<-NULL
	iXT<-NULL
	iYT<-NULL
	iZT<-NULL
	lxy<-length(dataXY[,1])
	indy<-NULL
	nn<-NULL

	f2<-function(i)
	{
		# a pair  x0-y0 is fixed
		indy<<-(dataYZ[,1]==dataXY[i,2])
		#given x0-y0, indy marks the  pairs y0-z  of  datYZ with y0  in the first coordinate 
		nn<<-sum(indy)

		XT<<-c(XT,rep(dataXY[i,1],nn))
		YT<<-c(YT,rep(dataXY[i,2],nn))
		ZT<<-c(ZT,dataYZ[indy==TRUE,2])
		iXT<<-c(iXT,rep(dataXY[i,3],nn))
		iYT<<-c(iYT,rep(dataXY[i,4],nn))
		iZT<<-c(iZT,dataYZ[indy==TRUE,4])
	}

	indx<- c(1:lxy)
	tmp<-sapply(indx, f2)
#tmp is not neccesary, the  useful result is XT,YT, ZT, iXT, iYT, iZT that  are updated in each iteration

	ntot<-length(XT)
	if (info==TRUE)	
	{

	cat(paste('Processes: ', procName[1],procName[2], procName[3], '.  Number of 3-tuples of points: ', ntot, sep=''),fill=TRUE)
	lnum<-length(XT)
	if (is.null(xlab)) xlab<-'time'

	plot( XT,rep(3,lnum), ylab='', ylim=c(0,4), xlab=xlab,
		xlim=c(min(XT,YT,ZT,na.rm=T),max(XT,YT,ZT,na.rm=T)), yaxt='n',...)
	points(YT,rep(2,lnum))
	points( ZT,rep(1,lnum))
	segments(XT, rep(3,lnum),YT, rep(2, lnum))
	segments(YT, rep(2, lnum),ZT, rep(1,lnum))
	axis(side=2, at=c(3:1),labels=procName)
	title('Occurrence points and 3-tuples of the sets of close points')

	}


	return(cbind(XT,YT,ZT,iXT,iYT,iZT))	

}
