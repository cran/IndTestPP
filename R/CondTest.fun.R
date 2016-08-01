CondTest.fun <-
function(pos1,pos2,lambda2, r,changer=TRUE,type='All',plotRes=FALSE,...)
{

#llama a calcNmu
	T<-length(lambda2)
	if (max(pos1,pos2)>T) stop('Time occurrences outside the observed period T are not possible')
	pvP<-NULL
	pvN<-NULL
	pvL<-NULL
	l1<-length(pos1)
	if (length(r)==1)  r<-rep(r,l1)
	if (length(r)!=l1) stop('Length of r must be 1 or the lenght of pos1')
	linf<-pmax(0,pos1-r)
	lsup<-pmin(T, pos1+r)
	overlI<-lsup[1:(l1-1)]-linf[2:l1]
	overlI[overlI<0]<-0 # if the intervals do not overlap, overlI is set to 0
	#overlI contains the  overlapping lengths between consecutive intervals. if the intervals overlap, overlI is positive
	if (max(overlI)>0) 
	{	cat("WARNING: there are overlapped intervals. The independence hypothesis 
		is not guaranteed. The length of the overlapped subintervals are:", fill=TRUE)
		print(round(overlI[overlI>0],1))
	}
	if (changer==TRUE) 
	#if TRUE changes the intervals to non overlapping intervals
	{
		chang<-overlI/2
		lsup[1:(l1-1)]<-lsup[1:(l1-1)]-chang
		linf[2:l1]<-linf[2:l1]+chang
	}

	longint<-lsup-linf
	cat('The shortest length of the considered intervals is: ',min(longint), fill=T)
	CNp<-apply(cbind(linf,lsup),MARGIN=1,FUN=calcNmu,lambda2=lambda2,pos2=pos2)	
	Ni<-CNp[2,]
	mui<-CNp[1,]
	Res<-(Ni-mui)/mui**0.5	
	if (plotRes==TRUE) plot(Res, xlab='index', ylab='Residuals', ...)


	if (type=='Poisson'|type=='All')
	{
		NG<-sum(Ni)
		muG<-sum(mui)
		pvPa<-ppois(NG, lambda=muG, lower.tail=F)+dpois(NG, lambda=muG)/2
		pvP<-2*min(pvPa, 1-pvPa)
	}


	if (type=='Normal'|type=='All')
	{

		mRes<-abs(mean(Res))*l1**0.5 # to transform  the mean in a N(0,1)
		pvN<-2*pnorm(mRes, lower.tail=F)
#		pvN<-t.test(Res, mu=0)$p.value
	}

#	if (type=='LRT'|type=='All')
#	{
#		LL0<-sum(log(dpois(Ni,lambda=mui)))
#		LL<-sum(log(dpois(Ni,lambda=Ni)))
#		LRT<--2*(LL0-LL)
#		pvL<-pchisq(LRT, df=l1, lower.tail=F) no funciona bien en las simulaciones con lambda<10
#	}

	return(list(Ni=Ni,mui=mui,Res=Res,pvP=pvP,pvN=pvN,linf=linf,lsup=lsup))


}
