NHJ <-
function(lambdaC, lambdaD,T=NULL, Ptype='inhom', posC, typeC=1, posD, typeD=1, r=NULL,L=NULL, 
	 test=FALSE, nTrans=100, rTest=NULL, conf=0.95, dplot=NULL, tit=c('J-function','D-function','F-function'),mfrow=NULL,
	cores=1,fixed.seed=NULL,...)
{
	lambdaC<-as.matrix(lambdaC)
	lambdaD<-as.matrix(lambdaD)

	if (max(dim(lambdaC)[1],dim(lambdaD)[1])==1)
		{if (is.null(T)==TRUE)	stop('Argument T (length of the observation period) must be specified')
		else {Ptype<-'hom'}}

	if(is.null(T)==TRUE)  T<-max(dim(lambdaC)[1],dim(lambdaD)[1])
	if(dim(lambdaC)[1]==1) lambdaC<-as.matrix(rep(lambdaC,T))
	if(dim(lambdaD)[1]==1) lambdaD<-as.matrix(rep(lambdaD,T))
	if (length(typeC)==1) typeC<-rep(1,length(posC))
	if (length(typeD)==1) typeD<-rep(1,length(posD))



	if (is.null(r))
	{
		 r1<-max(10, floor(T/10))
		 r<-seq(1,r1,by=2)
		 if (length(r)>200) r<-seq(1,r1,length.out=200)
	}
	if (max(2*r+1)>T)  stop ('Some r values are too large  and they lead to 
			intervals longer than the observation period T')
   if (is.null(L)) {
        L <- seq(1, T, by = 2)
        if (length(L) > 5000) 
            L <- seq(1, T, by = round((T - 1)/199))
    }
    if (is.null(rTest)) rTest<-max(r)

	NHJauxr<-NHJaux(lambdaC=lambdaC, lambdaD=lambdaD, posC=posC,typeC=typeC,
		posD=posD, typeD=typeD, r=r,L=L,T=T, Ptype=Ptype)
	NHJr<-NHJauxr$NHJr
	NHDr<-NHJauxr$NHDr
	NHFr<-NHJauxr$NHFr
	ri<-sum(is.na(NHJr)==F)+1
      if (ri<length(r)) cat('J-function cannot be calulated for values r >= ',r[ri]  ,' since F(r)=1', fill=T)

	if (rTest> max(r)) stop('rTest  must be lower or equal than the maximum  value in argument r')


	pv<-NULL
	JenvU<-NULL
	JenvL<-NULL
	JenvM<-NULL
	JStatOb<-NULL
	JStatTr<-NULL

    nylim <- c(0.99 * min(NHJr, na.rm = T), 1.01 * max(NHJr,na.rm = T))


	if (test==TRUE)
	{
		NHJrT<-NHJr[r<=rTest]
		rT<-r[r<=rTest]
		if (is.null(fixed.seed)==F) {set.seed(fixed.seed)}
		shi<-round(runif(nTrans,1,(T-1)))
		cl<-makeCluster(cores)
		clusterExport(cl, objects(, envir = .GlobalEnv))
		Jmat<-parSapply(cl,shi, FUN=Jenv, lambdaC=lambdaC, lambdaD=lambdaD, posC=posC,
			typeC=typeC, posD=posD, typeD=typeD, r=r,L=L,T=T, Ptype=Ptype)
		stopCluster(cl)

		 JenvL<-apply(Jmat, MARGIN=1, quantile, p=(1-conf)/2, na.rm=T)
		 JenvU<-apply(Jmat, MARGIN=1, quantile, p=1-(1-conf)/2, na.rm=T)
		 JenvM<-apply(Jmat, MARGIN=1, mean, na.rm=T)
                 JenvMT<-JenvM[r<=rTest]


		JStatOb<-mean(abs(NHJrT-1), na.rm=T)
		JmatT<-Jmat[r<=rTest,]
		JStatMat<-abs(JmatT-1)
		JStatTr<-apply(JStatMat, MARGIN=2,mean, na.rm=T)


		pv<-(sum(JStatTr>=JStatOb))/(nTrans+1)
		nylim<-c(0.99*min(JenvL, NHJr, na.rm=T), 1.01*max(JenvU, NHJr, na.rm=T))
	}

	if (is.null(mfrow)) mfrow<- c(min(2,nchar(dplot)), min(2,nchar(dplot)))
	par(mfrow=mfrow)


	if (grepl('J', dplot))  
	{	
		plot(r,NHJr,pch=16, ylab='J(r)', xlab='r', main=tit[1], ylim=nylim,...)
		lines(r,NHJr)
		abline(h=1, col='blue')
		if (test==TRUE)
		{
			lines(r, JenvL, lty=2, col='red')
			lines(r, JenvU, lty=2, col='red')
			lines(r, JenvM, , col='red')
			abline(v=rTest, col="grey", lty=2)
		}
	}

	if (grepl('D', dplot))
	{  plot(r,NHDr,pch=16, ylab='D(r)', xlab='r', main=tit[2],...)
	   lines(r,NHDr)}
	if (grepl('F', dplot))
	{  plot(r,NHFr,pch=16, ylab='F(r)', xlab='r', main=tit[3], type='l',...)
	   lines(r,NHFr)}
	names(pv)<-"p-value"

	return(list(r=r, NHJr=NHJr, NHDr=NHDr,NHFr=NHFr,JenvL=JenvL, JenvU=JenvU, JenvM=JenvM,
	JStatOb=JStatOb, JStatTr=JStatTr,pv=pv, T=T, L=L))
}
