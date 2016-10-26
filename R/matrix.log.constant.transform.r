### log transform on matrix, with contant firstly added
### input: matrix, log base, constant
### output: matrix in log scale
matrix.log.constant.transform <- function(signal,base=c(2,10,exp(1)),constant=0.5){
    if(!is.matrix(signal) || min(signal)<0 || base<=1 || constant<0)
        stop("Invalid Input")
    base <- match.arg(base)
    signal[signal==0] <- constant
    if(base==2){
        logsig <- log2(signal)
    }else if(base==10){
        logsig <- log10(signal)
    }else{
        logsig <- log(signal,base)
    }
    logsig
}
