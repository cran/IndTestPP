NHDaux <-
function(r,lambdaC, lambdaD,posC,typeC, posD,typeD, T)
{

	posWC<-posC[(posC>=r)&(posC<=(T-r))]
	typeWC<-typeC[(posC>=r)&(posC<=(T-r))]

	lambdaWC<-lambdaC[cbind(ceiling(posWC),typeWC)] # the result is a vector
	lWnuC<-sum(1/lambdaWC, na.rm=TRUE)
	L1D<-1-min(lambdaD)/lambdaD #it is a matrix
#if processes in D are homogeneous, L1D=0  all values; in that way
#the product in pag 13  can be only 0 or 1, depending if the point in C
#has   the nearest neighbourg in D at a distance  <=r or  not
#the expression in pp13 must count the number of points in C with the nearest 
# the nearest neighbourg in D at a distance  >r 
	L1C0<-sapply(posWC, FUN = prodN2, r=r,L1D=L1D,posD=posD, typeD=typeD)
	NHD<-sum(L1C0/lambdaWC)

	return(c(lWnuC,NHD))
}
