DepqueueKenv <-
function(i,lambdaM, T=NULL, nserv='infty', ddist='exp',argd=1,
	r, typeEst, fixed.seed)
{
	if (!is.null(fixed.seed)) fixed.seed<-fixed.seed+i

	auxpos<-DepNHPPqueue(lambda=lambdaM[,1], d=2 , T=T,nserv=nserv, Clambda=FALSE,
		 ddist=ddist, argd=argd, dplot=FALSE, fixed.seed=fixed.seed)$posNH

	Kaux<-NHKaux(lambdaC=matrix(lambdaM[,1],ncol=1), lambdaD=matrix(lambdaM[,2], ncol=1),T=T, posC=auxpos[[1]],
		 posD=auxpos[[2]], typeC<-rep(1,length(auxpos[[1]])), typeD<-rep(1,length(auxpos[[2]])), 
		 r=r, typeEst=typeEst)
	return(Kaux)
}
