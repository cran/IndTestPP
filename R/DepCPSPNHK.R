DepCPSPNHK <-
function(posx, posy, lambdaix, lambdaiy, lambdaixy, r=NULL, typeEst=1,
	  nsim=1000, conf=0.95,tit=NULL, cores=1,fixed.seed=NULL,...)
{


	T<-length(lambdaix)
	if (length(lambdaiy)!=T) stop("lambdaix and lambdaiy must have the  same length")

	if (length(lambdaixy)!=T) stop("lambdaix and lambdaixy must have the  same length")


	if (max(posx, posy)>T) stop("There are some  occurrence points outside the observation period")
	if (is.null(r))
	{
		 r1<-max(10, floor(T/10))
		 r<-seq(1,r1,by=2)
		 if (length(r)>200) r<-seq(1,r1,length.out=200)
	}

	if (max(2*r+1)>T)  stop ('Some r values are too large  and they lead to intervals longer than the observation period T')

	lambdax<-lambdaix+lambdaixy
	lambday<-lambdaiy+lambdaixy

	
	NHKr<-NHKaux(lambdaC=as.matrix(lambdax), lambdaD=as.matrix(lambday),T=T, 
	posC=posx, posD=posy, typeC=rep(1,length(posx)), typeD=rep(1,length(posy)), r=r, typeEst=typeEst)


	cl<-makeCluster(cores)
	clusterExport(cl, objects(, envir = .GlobalEnv))

	Kmat<-parSapply(cl,c(1:nsim), FUN=DepCPSPKenv,  lambdaix=lambdaix, lambdaiy=lambdaiy,
		lambdaixy=lambdaixy, lambdax=lambdax, lambday=lambday, r=r,T=T,  typeEst=typeEst, 
		fixed.seed=fixed.seed)
	stopCluster(cl)

	KenvL<-apply(Kmat, MARGIN=1, quantile, p=(1-conf)/2)
	KenvU<-apply(Kmat, MARGIN=1, quantile, p=1-(1-conf)/2)
	KenvM<-apply(Kmat, MARGIN=1, mean)


	nylim<-c(0.99*min(KenvL, NHKr), 1.01*max(KenvU, NHKr))
	if (is.null(tit)) tit<-'K function'
	plot(r,NHKr,pch=16, ylab='K(r)', xlab='r', main=tit, ylim=nylim,...)
	lines(r,NHKr)
	lines(r, KenvL, lty=2, col='red')
	lines(r, KenvU, lty=2, col='red')
	lines(r, KenvM, lty=1, col='red')


	return(list(r=r, NHKr=NHKr, KenvL=KenvL, KenvU=KenvU, T=T))
}
