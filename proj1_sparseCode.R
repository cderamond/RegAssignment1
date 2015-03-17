library(datasets)
library(ggplot2)
library(GGally)
#library(corrgram)
data <- mtcars[,c(1,2,4,9,10)]

#eda
#we will do a correlation matrix to discard variables
tol_cor <- .55

yvx <- cor(mtcars)[1,]
selSet <- names(yvx[ which(abs(yvx) > tol_cor)]) # we will include only the ones with |cor| > .55
data<- mtcars[,selSet]


pairsXY <- ggpairs(
        data,
        upper = list(continuous = "cor"),
        lower = list(continuous = "smooth", params = c(method = "lm", fill = "red"))
)

# create factors, unsure if i'll use
data$vs_f <- as.factor(data$vs)
data$am_f <- as.factor(data$am)
        levels(data$am_f) <- c("automatic", "manual") 



##

ggcorr(data[,-1])

