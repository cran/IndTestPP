NHF.fun <-
function(lambdaD,T=NULL,Ptype='inhom', posD, typeD=1, r=NULL,L=NULL, dplot=TRUE,tit='F(r)')
{
	lambdaD<-as.matrix(lambdaD)
	if (dim(lambdaD)[1]==1)
	{ if (is.null(T)==TRUE)	stop('Argument T (length of the observation period) must be specified')
	   else Ptype<-'hom'	}
	else 	      T<-dim(lambdaD)[1]

	if (is.null(r))
	{
		 r1<-max(10, floor(T/10))
		 r<-seq(1,r1,by=2)
		 if (length(r)>200) r<-seq(1,r1,length.out=200)
	}
	if (max(2*r+1)>T)  stop ('Some r values are too large  and they lead to 
			intervals longer than the observation period T')
	if (is.null(L)) 
	{
		L<-seq(1, T, by=2)
		if (length(L)>200) L<-seq(1,T,by = round((T - 1)/199))
	}

	if (Ptype=='hom') 
	{
		NHFr<-HDFaux(punt=L, posD=posD,   r=r, T=T)
	}
	else
	{
		if (length(typeD)==1) typeD<-rep(1,length(posD))
		if (length(posD)!=length(typeD)) stop("Arguments typeD and posD must have the same length")

		#F(r) estimation
		NHFraux<-sapply(r,FUN=NHFaux,L=L,lambdaD=lambdaD,posD=posD, typeD, T=T)
		NHFr<-1-NHFraux
	}

	if (dplot==TRUE) 
	{	plot(r,NHFr,pch=16, ylab='F(r)', xlab='r', main=tit, cex=0.5)
      	lines(r,NHFr)
	}
	return(list(r=r,NHFr=NHFr, T=T, L=L))
}
