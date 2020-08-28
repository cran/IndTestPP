Jenv <-
function(shi,  lambdaC, lambdaD, posC, typeC=1, posD, typeD=1, 
		r=NULL,L=NULL, T=NULL, Ptype='inhom')
{

	posDs<-posD+shi
	posDs[posDs>T]<-posDs[posDs>T]-T
	nncol=dim(lambdaD[2])
	lambdaDs<-rbind(matrix(lambdaD[(T-shi+1):T,], ncol=dim(lambdaD)[2]),matrix(lambdaD[1:(T-shi),], ncol=dim(lambdaD)[2]))
	Jaux<-NHJaux(lambdaC=lambdaC, lambdaD=lambdaDs, posC=posC,
		typeC=typeC, posD=posDs, typeD=typeD, r=r,L=L, T=T,Ptype=Ptype)$NHJr
	return(Jaux)
}
