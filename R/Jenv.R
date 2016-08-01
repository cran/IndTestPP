Jenv <-
function(shi,  lambdaC, lambdaD, posC, typeC=1, posD, typeD=1, 
		r=NULL,L=NULL, T=NULL, Ptype='inhom')
{

	posDs<-posD+shi
	posDs[posDs>T]<-posDs[posDs>T]-T
	lambdaDs<-rbind(as.matrix(lambdaD[(T-shi+1):T,]),as.matrix(lambdaD[1:(T-shi),]))
	Jaux<-NHJaux(lambdaC=lambdaC, lambdaD=lambdaDs, posC=posC,
		typeC=typeC, posD=posDs, typeD=typeD, r=r,L=L, T=T,Ptype=Ptype)$NHJr
	return(Jaux)
}
