### plot/view Rle track line
### input: Rle, other graphic parameters
### output: line plot
rle.track.view.line <- function(x,start=1L,add=FALSE,type="s",... ){
    library(IRanges)
    if(class(x)!="Rle") stop("Rle error in rle.track.view.line")
    if(add) points(c(start(x),length(x))+start-1L,c(runValue(x),tail(runValue(x),1)),
                   type=type,...)
    else plot(c(start(x),length(x))+start-1L,c(runValue(x),tail(runValue(x),1)),
              type=type,...)
}
