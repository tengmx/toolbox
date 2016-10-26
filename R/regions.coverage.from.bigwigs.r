### generate coverage signal profiles for genomic regions from bigwigs
### input: bigwig files, GRanges
### output: RleList of region's coverage
regions.coverage.from.bigwigs <- function(bwfs,regions){
    library(rtracklayer)
    if(class(regions)!="GRanges")
        stop("Invalid Input")
    rlt <- list()
    for(i in seq_len(length(bwfs))){
        bwfs_i <- BigWigFile(bwfs[i])
        x <- summary(bwfs_i,regions,size=width(regions),type="mean")
        rlt[[i]] <- lapply(x,function(xi) mcols(xi)$score)
    }
    rlt
}
