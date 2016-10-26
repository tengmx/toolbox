### plot/view Rle track bars in log scale
### input: Rle (all values >= 0), other graphic parameters
### output: bar plot with log height
rle.track.view.bar.log <- function(x,start=1L,constant=1,log='y',
                                   border=FALSE,space=0,axes=FALSE,...,
                                   ybreak=c(constant,2^seq_len(14)+constant),
                                   labels=ybreak-constant){
    library(IRanges)
    if(class(x)!="Rle") stop("Rle error in rle.track.view.bar.log")
    x <- x + constant
    idx <- seq(1L,length(x),floor(length(x)/4))
    tmp <- barplot(as.numeric(x),axes=axes,border=border,space=space,log=log,...)
    if(!axes) {
        axis(side=2, at=ybreak,labels=labels)
        axis(side=1, at=tmp[idx], labels=seq_len(length(x))[idx]+start-1L)
    }
}

