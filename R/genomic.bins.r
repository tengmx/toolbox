### generate genome bins
### input: chromosomes, bin size, genome build
### output: GRanges of all bins
genomic.bins <- function(chrom,binsize=10000L,genome="hg19"){
    library(GenomicRanges)
    library(BSgenome)
    genome <- getBSgenome(genome)
    if(binsize<2 || !all(is.element(chrom,seqlevels(genome))))
        stop("Invalid Input")
    allbins <- GRanges()
    seqlevels(allbins) <- chrom
    seqlengths(allbins) <- chrlength <- seqlengths(genome)[chrom]
    for(chr in seq_len(length(chrom))){
        binstarts <- seq(1,chrlength[chr],binsize)
        if(chrlength[chr]%%binsize == 0){
            binends <- seq(binsize,chrlength[chr],binsize)
        }else{
            binends <- c(seq(binsize,chrlength[chr],binsize),chrlength[chr])
        }
        if(length(binstarts)!=length(binends)){
            stop("bins number error",call.=TRUE)
        }
        chrombins <- GRanges(seqnames=Rle(chrom[chr],length(binstarts)),
                             ranges=IRanges(start=binstarts,end=binends),
                             strand=Rle("*",length(binstarts)))
        seqlevels(chrombins) <- chrom
        seqlengths(chrombins) <- chrlength
        allbins <- c(allbins, chrombins)
    }
    allbins
}

