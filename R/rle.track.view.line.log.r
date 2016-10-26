### plot/view Rle track line in log scale
### input: Rle (all values >= 0), other graphic parameters
### output: line plot with log height
rle.track.view.line.log <- function(x,start=1L,constant=1,add=FALSE,
                                    type="s",log='y',axes=FALSE,...,
                                    ybreak=c(constant,2^seq_len(14)+constant),
                                    labels=ybreak-constant){
    library(IRanges)
    if(class(x)!="Rle") stop("Rle error in rle.track.view.line.log")
    x <- x + constant
    if(add) points(c(start(x),length(x))+start-1L,c(runValue(x),tail(runValue(x),1)),
                   type=type,...)
    else plot(c(start(x),length(x))+start-1L,c(runValue(x),tail(runValue(x),1)),
              type=type,log=log,axes=axes,...)
    if(!add & !axes) {
        axis(side=2, at=ybreak,labels=labels)
        axis(side=1)
    }
}
