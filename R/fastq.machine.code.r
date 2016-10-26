### fastq machine code parse
### input: bam file
### output: sequencing info
fastq.machine.code <- function(bam, n=100){ ## non-NCBI SRRXXXXXX
    library(Rsamtools)
    firstn <- unlist(scanBam(BamFile(bam,yieldSize=n),
                             param=ScanBamParam(what="qname")))
    firstn <- sapply(strsplit(firstn, split="_|:"), function(x) x[1:4])
    codefreq <- apply(firstn, 1, function(x) length(unique(x)))
    if(codefreq[1] != 1){
        return(c(instrument="multiple", run=NA, flowid=NA, flowlane=NA))
    }else if(codefreq[2] != 1){
        return(c(instrument=unique(firstn[1,]), run=NA, flowid=NA,
                 flowlane=NA))
    }else if(codefreq[3] != 1){
        return(c(instrument=unique(firstn[1,]), run=NA, flowid=NA,
                 flowlane=unique(firstn[2,])))
    }else if(codefreq[4] != 1){
        return(c(instrument=unique(firstn[1,]), run=unique(firstn[2,]),
                 flowid=NA, flowlane=unique(firstn[3,])))
    }else{
        return(c(instrument=unique(firstn[1,]), run=unique(firstn[2,]),
                 flowid=unique(firstn[3,]), flowlane=unique(firstn[4,])))
    }
}
