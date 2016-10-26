### generate coverage signal profiles for a genomic region from bigwigs
### input: bigwig files, GRange (a region)
### output: RleList of region's coverage
region.coverage.from.bigwigs <- function(bwfs,region){
    library(rtracklayer)
    if(class(region)!="GRanges" || length(region)!=1)
        stop("Invalid Input")
    rlt <- RleList()
    for(i in seq_len(length(bwfs))){
        bwfs_i <- BigWigFile(bwfs[i])
        x <- summary(bwfs_i,region,size=width(region),type="mean")
        rlt <- c(rlt,RleList(mcols(x[[1]])$score))
    }
    rlt
}
