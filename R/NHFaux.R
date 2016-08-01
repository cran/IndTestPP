NHFaux <-
function(r,L, lambdaD,posD,typeD, T)
{

	posLW<-L[(L>=r)&(L<=(T-r))]

	L1D<-(1-min(lambdaD)/lambdaD)

	L1L0<-sapply(posLW, FUN = prodN2, r=r,L1D=L1D,posD=posD, typeD=typeD)


	NHF<-sum(L1L0)/length(posLW)

	return(NHF)

}
