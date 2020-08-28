miKS <-
function(i,mat)
{
	ks.test(mat[i,], 'punif',0,1)$statistic
}
