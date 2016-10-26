### regions' summaried (mean, max, min) signal from one sample pileup
### input: coverage pileup in RleList, regions in SORTED GRanges
### output: numerical vectors, each of values correponding to one region
regions.signal.from.pileup <- function(pileup,regions,type=c("mean","max","min")){
    if(!is.element(class(pileup),c("SimpleRleList","CompressedRleList")) || class(regions)!="GRanges" || !is.character(type))
        stop("Invalid Input")
    library(GenomicRanges)
    type <- match.arg(type)
    chrom <- unique(seqnames(regions))
    signal <- c()
    for(chr in seq_len(length(chrom))){
        regions_chr <- ranges(regions[seqnames(regions)==chrom[chr]])
        pileup_chr <- pileup[[match(chrom[chr],names(pileup))]]
        if(!is.null(pileup_chr)){
            if(type=="mean"){
                signal <- c(signal,viewMeans(Views(pileup_chr,regions_chr)))
            }else if(type=="max"){
                signal <- c(signal,viewMaxs(Views(pileup_chr,regions_chr)))
            }else{
                signal <- c(signal,viewMins(Views(pileup_chr,regions_chr)))
            }
        }else{
            signal <- c(signal,rep(NA,length(regions_chr)))
        }
    }
    signal
}
