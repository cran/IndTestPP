NHD.fun <-
function(lambdaC, lambdaD,T=NULL,Ptype='inhom', posC, typeC=1, posD, typeD=1,  r=NULL, 
	dplot=TRUE,tit='D(r)')
{

#lambda must be matrices, each column the lambda in each time position in a process 
#pos andtype contain the  time position of the occurrences and type the number of the process where it occurs
#since in each  process the number of occurrences is different , a matrix cannot be used

	lambdaC<-as.matrix(lambdaC)
	lambdaD<-as.matrix(lambdaD)

	if (max(dim(lambdaC)[1],dim(lambdaD)[1])==1)
		{if (is.null(T)==TRUE)	{stop('Argument T (length of the observation period) must be specified')}
		else {Ptype<-'hom'}}

	if(is.null(T)==TRUE)  T<-max(dim(lambdaC)[1],dim(lambdaD)[1])

	if (is.null(r))
	{
		 r1<-max(10, floor(T/10))
		 r<-seq(1,r1,by=2)
		 if (length(r)>200) r<-seq(1,r1,length.out=200)
	}

	if (max(2*r+1)>T)  stop ('Some r values are too large  and they lead to 
			intervals longer than the observation period T')


	if (Ptype=='hom') 
	{
		NHDr<-HDFaux(punt=posC, posD=posD,   r=r, T=T)
	}
	else
	{
		if (length(typeC)==1) typeC<-rep(1,length(posC))
		if (length(typeD)==1) typeD<-rep(1,length(posD))
		if (length(posC)!=length(typeC)) stop("Arguments typeC and posC must have the same length")
		if (length(posD)!=length(typeD)) stop("Arguments typed and posD must have the same length")

		if(dim(lambdaC)[1]==1) lambdaC<-as.matrix(rep(lambdaC,T))
		if(dim(lambdaD)[1]==1) lambdaD<-as.matrix(rep(lambdaD,T))
		if (dim(lambdaD)[1]!=dim(lambdaC)[1]) stop("Arguments lambdaC and lambdaD must have the same  number of rows")

		#D(r) estimation en [2,] y LWnuC estimation in [1,]
		NHDraux<-sapply(r, FUN = NHDaux,lambdaC=lambdaC, lambdaD=lambdaD,
			posC=posC,typeC=typeC, posD=posD, typeD=typeD, T=T)
		NHDr<-1-NHDraux[2,]/NHDraux[1,]
	}

	if (dplot==TRUE) 
	{	plot(r,NHDr,pch=16, ylab='D(r)', xlab='r', main=tit, cex=0.5)
		lines(r,NHDr)
	}

	return(list(r=r,NHDr=NHDr, T=T))
}
