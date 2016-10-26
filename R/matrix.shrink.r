### shrink matrix by column factors
### input: matrix, column factor, shrink type
### output: shrinked matrix
matrix.shrink <- function(mat,colfac,type=c("mean","sum")){
    type <- match.arg(type)
    if(!is.matrix(mat) || ncol(mat)<2 || !is.factor(colfac) || ncol(mat)!=length(colfac)) stop("Invalid Input")
    if(type=="mean") sapply(split(seq_len(ncol(mat)),colfac),function(x) rowMeans(mat[,x,drop=FALSE]))
    else sapply(split(seq_len(ncol(mat)),colfac),function(x) rowSums(mat[,x,drop=FALSE]))
}
