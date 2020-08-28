TranM <-
function(marcas=NULL, d=NULL, vecpro=NULL)
{
	if(is.null(marcas))
	{
		if (is.list(vecpro)==TRUE)
		{
		d<-length(vecpro)
		names(vecpro) <- letters[1:d]
		points<-unlist(vecpro)
		aux<-substr(names(points), 1, 1) 
		lookup <- setNames(seq_along(letters), letters)
		marcasaux<-lookup[aux]
 		marcas<-marcasaux[order(points)]

		} else  stop("vecpro must be a list if marcas is NULL")
	}

	markt0<-factor(marcas[1:(length(marcas)-1)], levels=c(1:d))
	markt1<-factor(marcas[2:length(marcas)], levels=c(1:d))
	tabla<-table(markt0, markt1)
	prob<-tabla/rowSums(tabla)
	return(prob)

}
