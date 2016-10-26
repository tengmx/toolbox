### generate coverage signal profiles for a genomic region from bam
### input: bam file, GRange (a region)
### output: RleList of region's coverage
region.coverage.from.bam <- function(bam,region,mapQ=NA,readLength=NA,
                                      shiftSize=NA,dup=TRUE,strands=TRUE,genome="hg19"){
    library(Rsamtools)
    library(GenomicRanges)
    library(BSgenome)
    genome <- getBSgenome(genome)
    param <- ScanBamParam(what = c("rname","strand","pos","qwidth","mapq"),which=region,
                          flag=scanBamFlag(isNotPrimaryRead=F,isUnmappedQuery=F,
                              isNotPassingQualityControls=F))
    bam <- scanBam(bam, param=param)
    reads <- GRanges(seqnames=bam[[1]]$rname,IRanges(start=bam[[1]]$pos,
                 width=bam[[1]]$qwidth),strand=bam[[1]]$strand,mapq=bam[[1]]$mapq)
    if(!is.na(mapQ))
        reads <- reads[mcols(reads)$mapq>=mapQ]
    if(!dup)
        reads <- unique(reads)
    if(!is.na(readLength))
        reads <- resize(reads,readLength)
    if(!is.na(shiftSize)){
        reads[strand(reads)=="+"] <- shift(reads[strand(reads)=="+"],shiftSize)
        reads[strand(reads)=="-"] <- shift(reads[strand(reads)=="-"],-shiftSize)
    }
    seqlengths(reads) <- seqlengths(genome)[seqlevels(reads)]
    reads <- trim(reads)
    if(strands){
        readspos <- reads[strand(reads)=='+']
        readsneg <- reads[strand(reads)=='-']
        cvgpos <- coverage(readspos)
        cvgneg <- coverage(readsneg)
        covposi <- cvgpos[[match(as.character(seqnames(region)),names(cvgpos))]]
        covnegi <- cvgneg[[match(as.character(seqnames(region)),names(cvgneg))]]
        viewpos <- viewApply(Views(covposi,ranges(region)),function(x) as(x, "Rle"))[[1]]
        viewneg <- viewApply(Views(covnegi,ranges(region)),function(x) as(x, "Rle"))[[1]]
        list(viewpos,viewneg)
    }else{
        cvg <- coverage(reads)
        covi <- cvg[[match(as.character(seqnames(region)),names(cvg))]]
        viewApply(Views(covi,ranges(region)),function(x) as(x, "Rle"))[[1]]
    }
}
