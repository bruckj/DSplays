set.seed(123)
library(dplyr)

v <- runif(8999, 0.0, 1.0)
# expects one-dimensional numeric object.  Ignores NAs
mea <- function(x){
        if (length(x)[1]==0){
                return(NA)}
        else
                mea <- (x[!is.na(x)] %>% sum())/length(x)[1]
        return(mea)
}

std <- function(x){
        if (length(x)[1]==0){
                return(NA)}
        else
                std <- (x[!is.na(x)] - mea(x) )^2 %>% mea() %>% sqrt()
        return(std)
}

std(v) - sd(v)

mean(v)==mea(v)


confintmean <- function(v,level){
        msamp <- NA
        for (i in 1:1000) {
                samp <- sample(v,round(length(v)/1000))
                msamp[i] <- mea(samp) 
        }
        somsamp <- sort(msamp)
        mmean <- mea(msamp)
        mmin <- min(msamp)  
        mmax <- max(msamp)  

        i_mmean <- sum(somsamp<mmean) # place of mmean in the ordered list of elements
        ci_upp <- somsamp[(i_mmean+level*1000/2)]
        ci_low <- somsamp[(i_mmean-level*1000/2)]
        CI <- c(ci_upp, ci_low)
        
        return(CI)
        
}

confintmean(v,0.999)

