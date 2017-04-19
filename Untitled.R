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

confint(v)

confintmean <- function(v,level){
        measamp <- NA
        for (i in 1:100) {
                samp <- sample(v,round(length(v)/100))
                measamp[i] <- mea(samp) 
        }
        
        
        return(measamp)
        
        
}


