nearestdist <-
function(posx, posy)
{
    nearestdist <- sapply(posx, FUN = pdist, posy = posy)
    return(nearestdist)
}
