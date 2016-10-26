### find consecutive non-NA in a vector
### input: vector, number
### output: location of elements of which consecutive i non-NA starts
consecutive.elements <- function(x, i){ ## x is a numeric vector with missing elements, missing data is NA
    if(!is.vector(x) || !is.numeric(i) || length(x) <= i ) stop("Invalid Input")
    i <- floor(i)
    vl <- length(x)
    dat <- sapply(seq_len(i),function(j) x[(j):(vl-i+j)])
    which(sapply(seq_len(nrow(dat)),function(k) all(!is.na(dat[k,]))))
}

