NHKaux3 <-
function (j, lambdaC , lambdaD , posC , 
    typeC , posD , typeD , T , ri , typeEst) 
{
    Kv <- 0
    u <- posC[j]
    if (sum(abs(posD - u) <= ri) > 0)
    {
        posWD <- posD[abs(posD - u) <= ri]
        typeWD <- typeD[abs(posD - u) <= ri]
	if (typeEst==1) 
	{		Kv<-sum(1/as.matrix(lambdaD)[cbind(ceiling(posWD), typeWD)])  #lambda en tj en D
	}
	if (typeEst==2) 	
	{
		lambdaDIaux<-matrix(lambdaD[c(max(1,ceiling(u-ri)):min(T,ceiling(u+ri))),], ncol=dim(lambdaD)[2])
		lambdaDI<-apply(lambdaDIaux, MARGIN=2, FUN=sum)/(2*ri)
		posWDlength<-tapply(posWD, INDEX=typeWD, FUN=length)
		typeWDlength<-tapply(typeWD, INDEX=typeWD, FUN=max)
		Kv<-sum(posWDlength/lambdaDI[typeWDlength])    #lambdatotal en D 
	}

       	Kv <- Kv/(T*as.matrix(lambdaC)[cbind(ceiling(u), typeC[j])])
	}
    return(Kv)
}
