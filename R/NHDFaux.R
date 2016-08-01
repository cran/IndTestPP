NHDFaux <-
function(r, lambdaC, lambdaD,posC,typeC, posD,typeD,L, T)
{
	posWC<-posC[(posC>=r)&(posC<=(T-r))]

	typeWC<-typeC[(posC>=r)&(posC<=(T-r))]
	lambdaWC<-lambdaC[cbind(ceiling(posWC),typeWC)] # the result is a vector


	L1D<-1-min(lambdaD)/lambdaD #it is a matrix
	lWnuC<-sum(1/lambdaWC, na.rm=TRUE)

	L1C0<-sapply(posWC, FUN = prodN2, r=r,L1D=L1D,posD=posD, typeD=typeD)
	NHDr<-1-sum(L1C0/lambdaWC)/lWnuC


# F
	posLW<-L[(L>=r)&(L<=(T-r))]
	L1L0<-sapply(posLW, FUN = prodN2, r=r,L1D=L1D,posD=posD, typeD=typeD)
	NHFr<-1-sum(L1L0)/length(posLW)


	return(c(NHDr, NHFr))
}
