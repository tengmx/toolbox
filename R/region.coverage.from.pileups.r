### generate coverage signal profiles for a genomic region from pileups
### input: list of RleList(pileups: multi/single samples, each sample as chrom-wise
### Rles), GRange(a region in GRanges), simple scaling indicator
### output: RleList of region's coverage
region.coverage.from.pileups <- function(pileups,region,weight=FALSE){
    library(GenomicRanges)
    if(!is.list(pileups) || class(region)!="GRanges" || !is.logical(weight) ||
       !is.element(class(pileups[[1]]),c("SimpleRleList","CompressedRleList")) ||
       length(region)!=1)
        stop("Invalid Input")
    rlt <- RleList()
    if(weight==F){
        for(i in seq_len(length(pileups))){
            covi <- pileups[[i]][[match(as.character(seqnames(region)),
                                        names(pileups[[i]]))]]
            rlt <- c(rlt, RleList(viewApply(Views(covi,ranges(region)),
                                            function(x) as(x, "Rle"))[[1]]))
        }
    }else{
        chrlibsize <- lapply(pileups,sum)
        libsize <- sapply(chrlibsize,sum)
        maxidx <- which(libsize==max(libsize))[1]
        regionchrlib <- sapply(chrlibsize,function(x)
                               x[match(as.character(seqnames(region)),names(x))])
        scaler <- regionchrlib[maxidx]/regionchrlib
        for(i in seq_len(length(pileups))){
            covi <- pileups[[i]][[match(as.character(seqnames(region)),
                                        names(pileups[[i]]))]]
            rlt <- c(rlt, RleList(viewApply(Views(covi,ranges(region)),function(x)
                                            as(x, "Rle"))[[1]]*scaler[i]))
        }
    }
    rlt
}
