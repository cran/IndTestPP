BinPer <-
function(posx, posy, ll, T)
{

puntx<-factor(as.numeric(countP(posx,ll, T=T)>0), levels=c(0,1))
punty<-factor(as.numeric(countP(posy,ll, T=T)>0), levels=c(0,1))


aux<-table(puntx,punty)
bper<-aux[2,2]/(sum(aux)-aux[1,1])

cat('Percentage of concordant intervals: ', round(bper,3), fill=T)
return(bper)
}
