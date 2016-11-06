### load ENCODE peak files into GRangesList
### input: broad/narrow peak files, gzipped status
### output: GRanges for all peaks
encodepeaks.to.grangeslist <- function(peakfiles,gz=FALSE,meta=TRUE,
                                       seqLevels=paste0('chr',c(1:22,'X','Y','M'))){
    if (!is.logical(gz) || length(gz) != 1L)
        stop("'gz' must be a length-1 logical vector.\n")
    library(GenomicRanges)
    peaklist <- GRangesList()
    seqlevels(peaklist,force=T) <- seqLevels
    for(i in seq_len(length(peakfiles))){
        pkf <- peakfiles[i]
        type <- strsplit(pkf,split='\\.')[[1]]
        if("narrowPeak" %in% type || "bed" %in% type){
            tmp <- encodepeak.to.granges(pkf,gz=gz,format="narrow",meta=meta)
        }else if("broadPeak" %in% type){
            tmp <- encodepeak.to.granges(pkf,gz=gz,format="broad",meta=meta)
        }else if("regionPeak" %in% type){
            tmp <- encodepeak.to.granges(pkf,gz=gz,format="broad",meta=meta)
        }else{
            stop("type error in encodepeaks.to.grangeslist!")
        }
        tmp <- sortSeqlevels(tmp)
        seqlevels(tmp,force=T) <- seqLevels
        peaklist[[i]] <- tmp
        cat("convert peak file ",i,' ',basename(pkf),'\n')
    }
    peaklist
}
