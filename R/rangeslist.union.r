### union operation for ranges in a RangesList
### input: rangesList/GRangesList, minimum of range size, maximum of range size
### output: GRanges as a reduced union
rangeslist.union <- function(rangeslist,minw=0L,maxw=Inf){
    if(minw<0 || maxw<0 || minw>maxw)
        stop("Invalid thresholds 'minw' or 'maxw'.\n")
    library(GenomicRanges)
    ranges <- reduce(unlist(rangeslist))
    ranges[width(ranges)>=minw & width(ranges)<=maxw]
}
