NHJaux <-
function(lambdaC, lambdaD, posC,typeC, posD, typeD, r,L, T,Ptype='inhom')
{
	if (Ptype=='hom') 
	{
		NHDr<-HDFaux(punt=posC, posD=posD,   r=r,  T=T)
		NHFr<-HDFaux(punt=L, posD=posD,   r=r, T=T)
		NHJr<-(1-NHDr)/(1-NHFr)
	}
	else
	{
		NHDFraux<-sapply(r, FUN = NHDFaux,lambdaC=lambdaC, lambdaD=lambdaD,
			posC=posC,typeC=typeC, posD=posD, typeD=typeD, L=L,T=T)
		NHDr<-NHDFraux[1,]
		NHFr<-NHDFraux[2,]
		NHJr<-(1-NHDr)/(1-NHFr)
	}
	NHJr[NHFr==1]<-NA

	return(list(NHJr=NHJr,NHDr=NHDr,NHFr=NHFr))
}
