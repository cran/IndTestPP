NHKaux2 <-
function(ri, lambdaC, lambdaD,  posC, typeC, posD, typeD, T)
{
	Ku<-sapply(c(1:length(posC)), FUN =NHKaux3 ,lambdaC=lambdaC, lambdaD=lambdaD,
		posC=posC, typeC=typeC,	 posD=posD, typeD=typeD, T=T,ri=ri)
	Kr<-sum(Ku)
	return(Kr)
}
