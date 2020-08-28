countP <-
function(pos,ll, T, lambda=NULL)
{
vpos<-rep(0,T)
vpos[pos]<-1
k<-ceiling(T/ll)
indice<-rep(c(1:k), each=ll)[1:T]
countP<-tapply(vpos, INDEX=indice, FUN=sum)
if (!is.null(lambda)) 
{
lambdaP<-tapply(lambda, INDEX=indice, FUN=sum)
countPc<-(countP-lambdaP)/lambdaP**0.5
}
else countPc<-countP
return(countPc)

}
