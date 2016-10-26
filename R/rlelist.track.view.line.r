### view RleList track lines
### input: RleList, other graphic parameters
### output: lines' plot
rlelist.track.view.line <- function(rlt,start=1L,ylim=c(1,256),add=TRUE,
                                    col=rep("blue",length(rlt)),
                                    lty=rep(1,length(rlt)),...){
    library(IRanges)
    if(!is.element(class(rlt),c("SimpleRleList","CompressedRleList")) ||
       length(unique(sapply(rlt,length)))!=1)
        stop("RleList error in rlelist.track.view.line")
    if(!add) {
        layout(matrix(seq_len(length(rlt)),length(rlt),1))
        par(mar = c(2,2,0,0))
    }
    for(i in seq_len(length(rlt))){
        if(i==1) rle.track.view.line(rlt[[i]],start=start,ylim=ylim,
               col=col[i],lty=lty[i],...)
        else if(add) rle.track.view.line(rlt[[i]],start=start,add=add,
                                         col=col[i],lty=lty[i],...)
        else rle.track.view.line(rlt[[i]],start=start,ylim=ylim,add=add,
                                 col=col[i],lty=lty[i],...)
    }
}

