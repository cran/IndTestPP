SpecGap <-
function(P)
{
	aux<-eigen(t(P),symmetric = FALSE)
	piE<-aux$vectors[,1]/sum(aux$vectors[,1])
	gap<-1-Mod(aux$values[2])
	return(list(SG=gap, pi=piE))
}
