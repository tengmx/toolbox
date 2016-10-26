### mean smooth Rle by sliding window
### input: Rle, window size
### output: smoothed Rle
rle.window.smooth <- function(x,window){
    library(IRanges)
    if(class(x)!="Rle" || window<2 || length(x)<window)
        stop("Rle/window error in rle.window.smooth")
    window_ranges <- IRanges(start=seq_len(length(x)-window+1),width=window)
    Rle(viewMeans(Views(x,window_ranges)))
}
