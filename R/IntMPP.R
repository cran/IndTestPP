IntMPP <-
function (funMPP.name, funMPP.args, fun.name, fun.args = NULL, nsim=1000, clevel = 0.95, 
    cores = 1, fixed.seed = NULL) 
{
    cl <- makeCluster(cores)
    clusterExport(cl, c('simNHPc',objects(, envir = .GlobalEnv)))

    if (!is.null(fixed.seed)) 
      clusterSetRNGStream(cl = cl, iseed = fixed.seed)
    simval <- parSapply(cl, c(1:nsim), FUN = funMPPGen, funMPP.name=funMPP.name,
    funMPP.args=funMPP.args,fun.name = fun.name, fun.args = fun.args)
    stopCluster(cl)



    if (!is.matrix(simval))      simval <- matrix(simval, nrow =1 )

    valmed <- apply(simval, MARGIN = 1, FUN = mean)
    valinf <- apply(simval, MARGIN = 1, FUN = quantile, p = 1 - 
        clevel)
    valsup <- apply(simval, MARGIN = 1, FUN = quantile, p = clevel)
    cat("Lower  bound  of CI: ", valinf, fill = T)
    cat("Point estimator: ", valmed, fill = T)
    cat("Upper bound of CI: ", valsup, fill = T)
    obj <- list(valmed = valmed, valinf = valinf, valsup = valsup, 
         nsim = nsim, fixed.seed = fixed.seed)
    return(obj)
}
