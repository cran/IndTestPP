NHK.fun <-
function(lambdaC, lambdaD, T=NULL, posC, typeC=1, posD, typeD=1, r=NULL, 
	 test=TRUE, nTrans=100, conf=0.95, rTest=20, dplot=FALSE, tit='K-function',
	 cores=1,fixed.seed=NULL)
{

	lambdaC<-as.matrix(lambdaC)
	lambdaD<-as.matrix(lambdaD)

	if ((dim(lambdaC)[1]==1)&(dim(lambdaD)[1]==1)&(is.null(T)==TRUE)) 
		stop('Argument T (length of the observation period) must be specified')
	if(is.null(T)==TRUE)  T<-max(dim(lambdaC)[1],dim(lambdaD)[1])
	if(dim(lambdaC)[1]==1) lambdaC<-as.matrix(rep(lambdaC,T))
	if(dim(lambdaD)[1]==1) lambdaD<-as.matrix(rep(lambdaD,T))
	if (dim(lambdaD)[1]!=dim(lambdaC)[1]) stop("Arguments lambdaC and lambdaD must have the same  number of rows")

	if (length(typeC)==1) typeC<-rep(1,length(posC))
	if (length(typeD)==1) typeD<-rep(1,length(posD))
	if (length(posC)!=length(typeC)) stop("Arguments typeC and posC must have the same length")
	if (length(posD)!=length(typeD)) stop("Arguments typed and posD must have the same length")

	if (is.null(r))
	{
		 r1<-max(10, floor(T/10))
		 r<-seq(1,r1,by=2)
		 if (length(r)>200) r<-seq(1,r1,length.out=200)
	}

	if (max(2*r+1)>T)  stop ('Some r values are too large  and they lead to 
			intervals longer than the observation period T')
	if (rTest> max(r)) stop('rTest  must be lower or equal than the maximum  value in argument r')


	NHKr<-NHKaux(lambdaC=lambdaC, lambdaD=lambdaD,T=T, posC=posC,
		typeC=typeC, posD=posD, typeD=typeD, r=r, dplot=dplot, tit=tit)
	nylim<-c(0.99*min(NHKr), 1.01*max(NHKr))

	pv<-NULL
	KenvI<-NULL
	KenvS<-NULL
	KStatTr<-NULL

	if (test==TRUE)
	{
		NHKrT<-NHKr[r<=rTest]
		rT<-r[r<=rTest]
		if (is.null(fixed.seed)==F) {set.seed(fixed.seed)}
		shi<-round(runif(nTrans,1,(T-1)))

		cl<-makeCluster(cores)
		clusterExport(cl, objects(, envir = .GlobalEnv))

		Kmat<-parSapply(cl,shi, FUN=Kenv, lambdaC=lambdaC, lambdaD=lambdaD, posC=posC,
			typeC=typeC, posD=posD, typeD=typeD, r=r,T=T)

		stopCluster(cl)
		KenvL<-apply(Kmat, MARGIN=1, quantile, p=(1-conf)/2)
		KenvU<-apply(Kmat, MARGIN=1, quantile, p=1-(1-conf)/2)
		KenvM<-apply(Kmat, MARGIN=1, mean)
	      KenvMT<-KenvM[r<=rTest]

		KStatOb<-max(NHKrT/(2*rT+1))
		KmatT<-Kmat[r<=rTest,]
		KStatMat<-KmatT/(2*rT+1)
		KStatTr<-apply(KStatMat, MARGIN=2,max)
		pv<-(sum(KStatTr>=KStatOb))/(nTrans+1)


#  correction by Davison and Hinkley (1997), Bootstrap Methods and their Application, p. 141
		nylim<-c(0.99*min(KenvL, NHKr), 1.01*max(KenvU, NHKr))
	}


	if (dplot==TRUE)  
	{	plot(r,NHKr,pch=16, ylab='K(r)', xlab='r', main=tit, ylim=nylim)
		lines(r,NHKr)
		if (test==TRUE)
		{
			lines(r, KenvL, lty=2, col='red')
			lines(r, KenvU, lty=2, col='red')
			lines(r, KenvM, lty=1, col='red')
			abline(v=rTest, col="grey", lty=2)
		}
	}


	return(list(r=r, NHKr=NHKr, KenvL=KenvL, KenvU=KenvU, 
	KStatOb=KStatOb, KStatTr=KStatTr,pv=pv,T=T))
}
