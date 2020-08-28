NHKaux <-
function(lambdaC, lambdaD,T, posC, typeC, posD, typeD,r=NULL,typeEst)
{
	NHKraux<-sapply(r, FUN=NHKaux2 ,lambdaC=lambdaC, lambdaD, posC=posC, 
		typeC=typeC, posD=posD, typeD=typeD, T=T, typeEst=typeEst)
	NHKr<-NHKraux
	return(NHKr)
}
