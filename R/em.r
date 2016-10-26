### EM normal
estep <- function(y,mu1,mu0,sigma1,sigma0,p){
    p1 <- dnorm(y, mean = mu1, sd = sigma1, log = FALSE)
    p0 <- dnorm(y, mean = mu0, sd = sigma0, log = FALSE)
    z <- (p1*p)/(p1*p+p0*(1-p))
    z
}
mstep <- function(y,z){
    p <- (2+sum(z))/(2*2+length(z))
    mu1 <- sum(z*y)/sum(z)
    mu0 <- sum((1-z)*y)/sum(1-z)
    sigma1 <- sqrt(sum(z*(y-mu1)^2)/sum(z))
    sigma0 <- sqrt(sum((1-z)*(y-mu0)^2)/sum(1-z))
    list(p=p,mu1=mu1,mu0=mu0,sigma1=sigma1,sigma0=sigma0,z=z)
}

em <- function(y,mu1=1.8,mu0=0.5,sigma1=0.1,sigma0=0.1,p=0.5){
    z <- estep(y,mu1,mu0,sigma1,sigma0,p)
    for(i in 1:2000){
        para <- mstep(y,z)
        z <- estep(y,para$mu1,para$mu0,para$sigma1,para$sigma0,para$p)
        if(i%%100==0) cat(i,'\n')
    }
    para
}
