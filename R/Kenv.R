Kenv <-
function(shi,  lambdaC, lambdaD, posC, typeC=1, posD, typeD=1, 
		r=NULL,L=NULL, T=NULL,typeEst)
{
	posDs<-posD+shi
	posDs[posDs>T]<-posDs[posDs>T]-T
	posDs<-sort(posDs)
	lambdaDs<-rbind(matrix(lambdaD[(T-shi+1):T,], nrow=shi),matrix(lambdaD[1:(T-shi),], nrow=(T-shi)))
	Kaux<-NHKaux(lambdaC=lambdaC, lambdaD=lambdaDs,T=T, posC=posC,
		typeC=typeC, posD=posDs, typeD=typeD, r=r, typeEst=typeEst)
	return(Kaux)
}
