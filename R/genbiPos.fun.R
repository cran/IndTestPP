genbiPos.fun <-
function(posx,posy, info=FALSE, PA=FALSE, procName=c('X','Y'),xlab=NULL,...)
{
	nx<-length(posx)
	ny<-length(posy)
	indy<-c(1:ny)

# first point in x
	ulty<-sum(posy<posx[1])+1
	aux<-as.numeric(posy<posx[1])
	aux[ulty]<-1
	if ((PA=='T')&(ulty<ny)) aux[ulty+1]<-1 #the posterior point is added ( if there is posterior point)
	n1<-sum(aux)
	x<-rep(posx[1],n1)
	y<-posy[aux==1]
	yi<-indy[aux==1]
	xi<-rep(1, n1)

#  equivalent to a FOR loop but faster to compute. it calcualtes for each point in posx, tx,
# the points in posy that intersect the tx interval

	f1<-function(i,posx,posy,ny)
	{
		ulty<-(sum(posy<posx[i])+1) 
		aux<-rep(0,ny)
		aux[(posx[i-1]<posy)&(posy<posx[i])]<-1
		if (ulty<=ny) {aux[ulty]<-1}

		if(PA==TRUE)
		{

			if (ulty<ny) aux[(ulty+1)]<-1 #the posterior point is added ( if there is posterior point)
			ultzero<-sum(posy<=posx[i-1] )# position of the previous point to those that  intersect posx[i] interval
			if (ultzero>0) aux[ultzero]<-1 #the previous point is added ( if there is previous point)
		}

		n1<-sum(aux)
		x<<-c(x,rep(posx[i],n1))
		y<<-c(y,posy[aux==1])
		yi<<-c(yi,indy[aux==1])
		xi<<-c(xi, rep(i, n1))
	}



	indx<- c(2:nx)
	tmp<-sapply(indx,f1, posx=posx, posy=posy, ny=ny)

#tmp is not neccesary, the  useful result is x,y,xi, yi that  are updated in each iteration

	ntot<-length(x)
	nna<-sum(is.na(y)==T)
	indreal<-(is.na(y)==F)
	
	if (info==TRUE) 
	{	cat(paste('Processes: ', procName[1],' and ',procName[2],sep=''),fill=TRUE)
		cat('    Number of pairs of close points: ', ntot,fill=TRUE)
		cat('    Number of  points  intersectioning with the last (censured) interval: ', nna,fill=TRUE)
		cat('    Number of  complete  pairs of close points: ', (ntot-nna),fill=TRUE)
		cat(fill=T)
		cat(fill=T)
		if (is.null(xlab)) xlab<-'time'
		lnum<-length(x)
		plot(x,rep(2,lnum), ylab='', ylim=c(0,3),xlab=xlab,
			xlim=c(min(x,y, na.rm=T),max(x,y,na.rm=T)), yaxt='n',...)
		points(y,rep(1,lnum))
		segments(x, rep(2,lnum),y, rep(1, lnum))
		axis(side=2, at=c(2:1),labels=procName)
		title('Occurrence points and pairs of close points')
	}

	return(cbind(x[indreal],y[indreal],xi[indreal],yi[indreal]))
# the output elimates the NA values(generated for points with #no intersection at the end of the process
}
