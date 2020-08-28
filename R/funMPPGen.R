funMPPGen <-
function (i, funMPP.name, funMPP.args, fun.name, fun.args = NULL) 
{
    posNH <- do.call(funMPP.name, funMPP.args)[1]
    fun.args2 <- c(posNH, fun.args)
    aux <- do.call(fun.name, fun.args2)
    return(aux)
}
