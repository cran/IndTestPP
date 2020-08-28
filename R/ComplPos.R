ComplPos <-
function(pos, T, type='Pos')
{
	if (max(pos)>T) stop('All the positions in pos must be lower  or equal than T')
	Npos<-rep(0,T)
	if (type=='Pos') Npos[pos]<-pos
	if (type=='Bin') Npos[pos]<-1
	return(Npos)
}
