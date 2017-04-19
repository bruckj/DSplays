set.seed(123)
library(dplyr)

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



confintmean <- function(v,level){
        if (length(v)<1000){
                print("This version expects a vector longer than 999" )  
                return(NA)
        }else if(level<=0 | level>=1){
                print("This version expects  confidence level >0 and <1" )  
                return(NA)
        }else
                CI <- NA
                while(any(is.na(CI)) | length(CI)>2  ){
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
                        CI <- c(ci_low,ci_upp)
                }
                
        return(CI)
        }

####################

v <- runif(8999, 0.0, 1.0)
confintmean(v,0.10)


# I interpreted the text as Confidence Intervall for the mean (perhaps you meant to have it for the standard deviation as well, but I did not have enough time)

# Left to do in case this function was to be used:
# - correct std() to more exactly reproduce standard sd(), unless this formula is deemed better for the purpuse.
# - research to chose the sampling parameter here set to 1000 ideally, treating vectors shorter than 1000.
# - more conceptual treatment of the case when (i_mmean+level*1000/2) or i_mmean-level*1000/2) out of index range (currently restarting the whole process)

# if to be used as a proper function, dealing with parameters of wrong data types, or out of range












