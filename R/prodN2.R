prodN2 <-
function(x,r,L1D,posD, typeD)
{
	aux<-(abs(posD-x) <=r)
	posDsel<-posD[aux]
	typeDsel<-typeD[aux]
	if (sum(posDsel)==0) {PL1Dx<-1}
	else	{PL1Dx<-prod(L1D[cbind(ceiling(posDsel), typeDsel)],na.rm=TRUE)}

	return(PL1Dx)	
}
