DepqueueNHK <-
function(posx, posy, lambda,  T=NULL, nserv='infty', ddist='exp',argd=1,r=NULL, 
	typeEst=1, nsim=1000, conf=0.95,tit=NULL, cores=1,fixed.seed=NULL,...)
{

	type<-'H'
      if (length(lambda)==1)
	{
		if (is.null(T)) stop('Argument T  or a constant vector of length T must be provided in homogeneous processes')
		lambda<-rep(lambda, T)
	}
	if (sum(diff(lambda))!=0) type<-'NH'

	T<-length(lambda)
	
	tt<-c(1:T)
	if (max(posx, posy)>T) stop("There are some  occurrence points outside the observation period")

	if (is.null(r))
	{
		 r1<-max(10, floor(T/10))
		 r<-seq(1,r1,by=2)
		 if (length(r)>200) r<-seq(1,r1,length.out=200)
	}

	if (max(2*r+1)>T)  stop ('Some r values are too large  and they lead to intervals longer than the observation period T')
  
      typex<-rep(1,length(posx))
      typey<-rep(1,length(posy))
	lambdaM<-cbind(lambda)
	if (type=='NH') 
	{
		dendist<-do.call(paste('d',ddist,sep=''), append(list(x=tt),matrix(argd, nrow=1)) )
		convlambda<-convolve(lambdaM[,1],rev(dendist),type='o')[1:T]
		lambdaM<-cbind(lambdaM, convlambda)
	}
	else lambdaM<-cbind(lambdaM, lambda)

	NHKr<-NHKaux(lambdaC=matrix(lambdaM[,1],ncol=1), lambdaD=matrix(lambdaM[,2], ncol=1),T=T, posC=posx, posD=posy, typeC=typex, typeD=typey, 
		r=r, typeEst=typeEst)


	cl<-makeCluster(cores)
	clusterExport(cl, c('simNHPc',objects(, envir = .GlobalEnv)))

	Kmat<-parSapply(cl,c(1:nsim), FUN=DepqueueKenv, lambdaM=lambdaM,  T=T, nserv=nserv,
		ddist=ddist,argd=argd, r=r,  typeEst=typeEst, fixed.seed=fixed.seed)
	stopCluster(cl)

	KenvL<-apply(Kmat, MARGIN=1, quantile, p=(1-conf)/2)
	KenvU<-apply(Kmat, MARGIN=1, quantile, p=1-(1-conf)/2)
	KenvM<-apply(Kmat, MARGIN=1, mean)


	nylim<-c(0.99*min(KenvL, NHKr), 1.01*max(KenvU, NHKr))
	if (is.null(tit)) tit<-'K function. Envelope for input and output processes of a queue'
	plot(r,NHKr,pch=16, ylab='K(r)', xlab='r', main=tit, ylim=nylim,...)
	lines(r,NHKr)
	lines(r, KenvL, lty=2, col='red')
	lines(r, KenvU, lty=2, col='red')
	lines(r, KenvM, lty=1, col='red')


	return(list(r=r, NHKr=NHKr, KenvL=KenvL, KenvU=KenvU, T=T))
}
