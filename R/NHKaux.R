NHKaux <-
function(lambdaC, lambdaD,T, posC, typeC=1, posD, typeD=1,r=NULL, 
	dplot=TRUE,tit='K(r).')
{
	NHKr<-sapply(r, FUN=NHKaux2 ,lambdaC=lambdaC, lambdaD, posC=posC, 
		typeC=typeC, posD=posD, typeD=typeD, T=T)
	return(NHKr)
}
