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



## eda advanced
source("./myplclust.R")
d <- dist(data)
clust <- hclust(d)
myplclust(clust,lab.col=data$am)

###
# svd
svd1 <- svd(scale(data[,-1]))
# get 3 top contributor
#maxContrib <- which.max(abs(svd1$v[,2])) no need for the max
top3Contrib <- sort(abs(svd1$v[,2]), 
                    decreasing = T, 
                    index.return=T)$ix [1:3] #straight from Leek professor, we take 3 top contributors

# graph for % of variation explained
par(mfrow = c(1, 2))
plot(svd1$d, xlab = "Column", ylab = "Singular value", pch = 19)
plot(svd1$d^2/sum(svd1$d^2), xlab = "Column", ylab = "Prop. of variance explained", 
     pch = 19)
#########
d <- dist(data[,top3Contrib])#
clust <- hclust(d)
plot(clust)

## we now know that by taking the top 3 contributor we should explain pVarEx %
pVarEx <- sum((svd1$d^2/sum(svd1$d^2))[1:3]) #round it to 2 decimals *100 

# k-means
kClust <- kmeans(data[,-1],centers=2, nstart = 10)
table(kClust$cluster,data$am)

## model fit
fdata$am <- as.factor(fdata$am)
fit <- lm(mpg ~., data = fdata)
summary(fit)


