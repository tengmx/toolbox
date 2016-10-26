### mean smooth RleList by sliding window
### input: RleList, window size
### output: smoothed RleList
rlelist.window.smooth <- function(x,window){
    library(IRanges)
    if(!is.element(class(x),c("SimpleRleList","CompressedRleList")) ||
       window <2 || sum(sapply(x,length)<window)>0)
        stop("RleList/window error in rlelist.window.smooth")
    rlt <- RleList()
    for(i in seq_len(length(x))){
        rlt[[i]] <- rle.window.smooth(x[[i]],window)
    }
    rlt
}
