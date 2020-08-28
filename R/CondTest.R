CondTest <-
function(posx,posy,lambday, r,changer=TRUE,type='All',plotRes=FALSE,...)
{

#llama a calcNmu
	T<-length(lambday)
	if (max(posx,posy)>T) stop('Time occurrences outside the observed period T are not possible')
	pvP<-NULL
	pvN<-NULL
	pvL<-NULL
	l1<-length(posx)
	if (length(r)==1)  r<-rep(r,l1)
	if (length(r)!=l1) stop('Length of r must be 1 or the lenght of posx')
	linf<-pmax(0,posx-r)
	lsup<-pmin(T, posx+r)
	overlI<-lsup[1:(l1-1)]-linf[2:l1]
	overlI[overlI<0]<-0 # if the intervals do not overlap, overlI is set to 0
	#overlI contains the  overlapping lengths between consecutive intervals. if the intervals overlap, overlI is positive
	if (max(overlI)>0) 
	{	cat("WARNING: there are overlapping intervals. The independence hypothesis is not guaranteed.", fill=TRUE)
		cat(fill=T)
	}
	if (changer==TRUE) 
	#if TRUE changes the intervals to non overlapping intervals
	{
		chang<-overlI/2
		lsup[1:(l1-1)]<-lsup[1:(l1-1)]-chang
		linf[2:l1]<-linf[2:l1]+chang
	cat("The intervals have been shortened to obtain disjoint  intervals.", fill=TRUE)
	cat("The length of the intersection priods are:", fill=TRUE)
		print(round(overlI[overlI>0],1))
	}

	longint<-lsup-linf
	cat('The shortest length of the considered intervals is: ',min(longint), fill=T)
	CNp<-apply(cbind(linf,lsup),MARGIN=1,FUN=calcNmu,lambday=lambday,posy=posy)	
	Ni<-CNp[2,]
	mui<-CNp[1,]
	cat(fill=T)
	cat('The median of the mui values is: ',round(median(mui, na.rm=T),1), fill=T)
	cat(fill=T)
	Res<-(Ni-mui)/mui^0.5	
	if (plotRes==TRUE) plot(Res, xlab='index', ylab='Res. Differences', ...)


	if (type=='Poisson'|type=='All')
	{
		NG<-sum(Ni)
		muG<-sum(mui)
		pvPa<-ppois(NG, lambda=muG, lower.tail=F)+dpois(NG, lambda=muG)/2
		pvP<-2*min(pvPa, 1-pvPa)
	}


	if (type=='Normal'|type=='All')
	{

		mRes<-abs(mean(Res))*l1^0.5 # to transform  the mean in a N(0,1)
		pvN<-2*pnorm(abs(mRes), lower.tail=F)
	}

	mmu<-mean(mui)
	names(pvP)<-"Poisson p-value"
	names(pvN)<-"Normal p-value"

	return(list(pvP=pvP,pvN=pvN,Ni=Ni,mui=mui,Res=Res,linf=linf,lsup=lsup,mmu=mmu))


}
