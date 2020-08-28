PsimNHPc <-
function(i,lambdaiM, fixed.seed)
{
	if (!is.null(fixed.seed)) fixed.seed<-fixed.seed+i
	aux<-simNHPc(lambda = lambdaiM[,i], fixed.seed = fixed.seed)$posNH
	return(aux)
}
