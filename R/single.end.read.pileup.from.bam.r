### pileup profile for single-end bam
### input: BAM file, MapQ threshold, read length, read shift bp, human genome build
### output: pileup coverage for the BAM
single.end.read.pileup.from.bam <- function(bamfile,mapQ=30L,readLength=36L,
                                            shiftSize=100L,genome='hg19'){
    if(mapQ<0 || readLength<1 || shiftSize<0) stop("Invalid Input")
    library(Rsamtools)
    library(BSgenome)
    genome <- getBSgenome(genome)
    param <- ScanBamParam(what = c("rname","strand","pos","qwidth","mapq"),
                  flag=scanBamFlag(isNotPrimaryRead=F,isUnmappedQuery=F,
                      isNotPassingQualityControls=F))
    bam <- scanBam(bamfile, param=param)
    reads <- GRanges(seqnames=bam[[1]]$rname,IRanges(start=bam[[1]]$pos,
                         width=bam[[1]]$qwidth),
                     strand=bam[[1]]$strand,mapq=bam[[1]]$mapq)
    reads <- reads[mcols(reads)$mapq>=mapQ]
    reads <- resize(reads,readLength)
    if(shiftSize!=0){
        reads[strand(reads)=="+"] <- shift(reads[strand(reads)=="+"],shiftSize)
        reads[strand(reads)=="-"] <- shift(reads[strand(reads)=="-"],-shiftSize)
    }
    seqlengths(reads) <- seqlengths(Hsapiens)[seqlevels(reads)]
    reads <- trim(reads)
    coverage(reads)
}
