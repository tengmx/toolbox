### load ENCODE peak file into GRanges
### input: broad/narrow peak file, gzipped status, peak file type
### output: GRanges for all peaks
encodepeak.to.granges <- function(peakfile,gz=TRUE,format=c("broad", "narrow"),
                                  meta=TRUE){
    if (!is.logical(gz) || length(gz) != 1L)
        stop("'gz' must be a length-1 logical vector.\n")
    format <- match.arg(format)
    library(GenomicRanges)
    if(gz)
        peakfile <- gzfile(peakfile)
        peaktbl <- read.delim(peakfile,stringsAsFactors=F,quote = "",header=F,sep="")
        if(meta){
            peaks <- GRanges(peaktbl$V1,IRanges(start=peaktbl$V2+1,end=peaktbl$V3),
                             strand=gsub('\\.','\\*',peaktbl$V6),signalValue=peaktbl$V7,
                             pValue=peaktbl$V8,qValue=peaktbl$V9)
            if(format=="narrow")  mcols(peaks)$peak <- peaktbl$V10
        }else{
            peaks <- GRanges(peaktbl$V1,IRanges(start=peaktbl$V2+1,end=peaktbl$V3),
                             strand=gsub('\\.','\\*',peaktbl$V6))
        }
        peaks
}
