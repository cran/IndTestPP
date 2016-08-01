Kenv <-
function(shi,  lambdaC, lambdaD, posC, typeC=1, posD, typeD=1, 
		r=NULL,L=NULL, T=NULL)
{
	posDs<-posD+shi
	posDs[posDs>T]<-posDs[posDs>T]-T
	lambdaDs<-rbind(as.matrix(lambdaD[(T-shi+1):T,]),as.matrix(lambdaD[1:(T-shi),]))
	Kaux<-NHKaux(lambdaC=lambdaC, lambdaD=lambdaDs,T=T, posC=posC,
		typeC=typeC, posD=posDs, typeD=typeD, r=r, dplot=FALSE)
	return(Kaux)
}
