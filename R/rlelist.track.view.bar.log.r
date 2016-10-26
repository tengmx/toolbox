### plot/view RleList track bars in log scale
### input: RleList (all values >= 0), other graphic parameters
### output: bar plots with log height
rlelist.track.view.bar.log <- function(rlt,start=1L,constant=1,ylim=c(1,64),
                                       col=rep("blue",length(rlt)),...){
    library(IRanges)
    if(!is.element(class(rlt),c("SimpleRleList","CompressedRleList")) ||
       length(unique(sapply(rlt,length)))!=1)
        stop("RleList error in rlelist.track.view.bar.log")
    layout(matrix(seq_len(length(rlt)),length(rlt),1))
    par(mar = c(2,2,0,0))
    for(i in seq_len(length(rlt))){
        rle.track.view.bar.log(rlt[[i]],start=start,constant=constant,
                               ylim=ylim,col=col[i],...)
    }
}

