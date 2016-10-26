### create k-length dna sequence profile, complementary seqs are counted as one
### input: k
### output: list of k-mers
kmers <- function(k=2L){
    if(!is.integer(k)) stop("Invalid 'k', ingeter required.")
    library(Biostrings)
    for(i in seq_len(k)){
        if(i==1){
            allseqs <- c('A','C','G','T')
        }else{
            allseqs <- paste0(rep(allseqs,each=4),c('A','C','G','T'))
        }
    }
    allseqs_rev <- as.character(reverseComplement(DNAStringSet(allseqs)))
    merger <- cbind(allseqs,allseqs_rev)
    merger <- unique(t(apply(merger,1,sort)))
    kseqs <- lapply(seq_len(nrow(merger)),function(x) unique(merger[x,]))
    names(kseqs) <- merger[,1]
    kseqs
}
