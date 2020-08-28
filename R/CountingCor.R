CountingCor <-
function(posx, posy, ll, T,  method='spearman', lambdax=NULL, 
	lambday=NULL)
{

puntx<-countP(posx,ll, T=T, lambda=lambdax)
punty<-countP(posy,ll, T=T, lambda=lambday)

ccor<-cor.test(puntx,punty, method=method)$estimate
return(ccor)
}
