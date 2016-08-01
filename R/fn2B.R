fn2B <-
function(i,posx, posy=posy, posz=posz, PA=FALSE, NumProcess=2, shi1, shi2, T=T)
{
	aux<-DistShift.fun(posx=posx, NumProcess=NumProcess,  posy=posy, posz=posz, 
		shii1=shi1[i],shii2=shi2[i], PA=PA, info=FALSE, T=T)
	return(aux)
}
