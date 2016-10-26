### filtering matrix by row trend
### input: matrix, a chosen trend
### output: a sub-matrix
trend.filter.matrix <- function(rawmat,trend=c("increasing","decreasing","nondecreasing","nonincreasing"),na.rm=TRUE){
    trend <- match.arg(trend)
    if(!is.matrix(rawmat) || ncol(rawmat)<2) stop("Invalid Input")
    tmpmat <- rawmat[,-1,drop=FALSE] - rawmat[,-ncol(rawmat),drop=FALSE]
    if(trend=="increasing")
        idx <- apply(tmpmat,1,function(x) all(x>0,na.rm=na.rm))
    else if(trend=="decreasing")
        idx <- apply(tmpmat,1,function(x) all(x<0,na.rm=na.rm))
    else if(trend=="nondecreasing")
        idx <- apply(tmpmat,1,function(x) all(x>=0,na.rm=na.rm))
    else
        idx <- apply(tmpmat,1,function(x) all(x<=0,na.rm=na.rm))
    rawmat[idx,,drop=FALSE]
}
