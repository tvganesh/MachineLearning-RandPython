library(dplyr)
library(ggplot2)
library(FNN)
# Split the data into training and test sets
trainTestSplit <- function(df,trainPercent,seed1){
    ## Sample size percent
    samp_size <- floor(trainPercent/100 * nrow(df))
    ## set the seed 
    set.seed(seed1)
    idx <- sample(seq_len(nrow(df)), size = samp_size)
    idx
    
}

# Compute the R squared rsquared= 1 - RSS/TSS
Rsquared <- function(lmfit,newdf,y){
    yhat <- predict(lmfit,newdata=newdf)
    RSS <- sum((y - yhat)^2)
    TSS <- sum((y - mean(y))^2)
    rsquared <-1 - (RSS/TSS)
    rsquared
}

# Compute R squared for KNN predictions
knnRSquared <- function(yhat=knn$pred,y){
    RSS <- sum((test.Y - yhat)^2)
    TSS <- sum((test.Y - mean(test.Y))^2)
    rsquared <-1 - (RSS/TSS)
    rsquared
}

# Create a Min Max scaler
MinMaxScaler <- function(df){
    minx=sapply(df,min)
    maxx=sapply(df,max)
    d=(t(df) -minx)/(maxx-minx)
    e=t(d)
    e
}