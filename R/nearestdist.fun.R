nearestdist.fun <-
function(posx, posy)
{
    nearestdist <- sapply(posx, FUN = pdist.fun, posy = posy)
    return(nearestdist)
}
