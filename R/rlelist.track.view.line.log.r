### plot/view RleList track lines in log scale
### input: RleList (all values >= 0), other graphic parameters
### output: lines' plot with log height
rlelist.track.view.line.log <- function(rlt,start=1L,constant=1,ylim=c(1,64),
                                        add=TRUE,col=rep("blue",length(rlt)),
                                        lty=rep(1,length(rlt)),...){
    library(IRanges)
    if(!is.element(class(rlt),c("SimpleRleList","CompressedRleList")) ||
       length(unique(sapply(rlt,length)))!=1)
        stop("RleList error in rlelist.track.view.line.log")
    if(!add) {
        layout(matrix(seq_len(length(rlt)),length(rlt),1))
        par(mar = c(2,2,0,0))
    }
    for(i in seq_len(length(rlt))){
        if(i==1) rle.track.view.line.log(rlt[[i]],start=start,
               constant=constant,ylim=ylim,col=col[i],lty=lty[i],...)
        else if(add) rle.track.view.line.log(rlt[[i]],start=start,
               constant=constant,add=add,col=col[i],lty=lty[i],...)
        else rle.track.view.line.log(rlt[[i]],start=start,
               constant=constant,ylim=ylim,add=add,col=col[i],lty=lty[i],...)
    }
}
