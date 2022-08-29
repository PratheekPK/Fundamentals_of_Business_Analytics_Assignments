################################################################################
#Question 1
################################################################################

setwd("C:/Users/hp/Downloads")
options(scipen=999)
starbucks <- read.table("Starbucks HW2 Data (1).txt", header=T, sep="\t")
attach(starbucks)
K <- 5000
N <- nrow(starbucks)
train <- as.data.frame(starbucks[1:K,])
test <- as.data.frame(starbucks[(K+1):N,])
train.reg <- lm(recommend ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11 + X12 + X13 + X14 + X15 + X16+ X17 + X18 + X19 + X20 + X21 +X22, data=train) 
summary(train.reg)
test.Y <- test[,"recommend"]
test.X <- test[c("X1","X2","X3","X4","X5", "X6", "X7","X8", "X9","X10","X11","X12","X13","X14","X15","X16","X17","X18","X19","X20","X21","X22")]
test.preds <- as.vector(predict(object=train.reg, newdata=test.X))
test.preds
library(miscTools)
train.r2 <- rSquared(y=train[,"recommend"], resid=resid(train.reg))
test.r2  <- rSquared(y=test.Y, resid= (test.Y - test.preds))


################################################################################
#Question 2
################################################################################


X <- cbind(X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22)
new.data <- cbind.data.frame(recommend, X)
null.model <- lm(recommend ~ 1)
summary(null.model)
as.vector(fitted(null.model))
mean(recommend)
full.model <- lm(recommend ~ ., data = new.data)
summary(full.model)
forward.results <- step(object=null.model, direction="forward", scope=formula(full.model))
summary(forward.results)

################################################################################
#Question 3
################################################################################

library(dplyr)
X<-as.matrix(select(starbucks, c(X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22)))
summary.stats <- cbind(
  apply(X,2,mean),
  apply(X,2,min),
  apply(X,2,max),
  apply(X,2,sd))
colnames(summary.stats) <- c("Avg", "Min", "Max", "Std Dev")
round(summary.stats,2)
library(cluster)
library(NbClust)
library(factoextra)
nb <- NbClust(X, distance="euclidean", min.nc=2, max.nc=10, method="kmeans")
fviz_nbclust(nb)
cluster.results = kmeans(x = X, centers = 2, iter.max=1000, nstart=100)
cluster.numbers = cluster.results$cluster
segment_sizes = table(cluster.numbers)
t(round(cluster.results$centers,2))
most.satisfied = starbucks[cluster.results$cluster==1,]
all.other = starbucks[cluster.results$cluster==2,]
detach(starbucks)
attach(most.satisfied)
most.satisfied.reg <- lm(recommend ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11 + X12 + X13 + X14 + X15 + X16+ X17 + X18 + X19 + X20 + X21 +X22, data=most.satisfied) 
fitted.values(most.satisfied.reg)
mean(fitted.values(most.satisfied.reg))
detach(most.satisfied)
attach(all.other)
all.other.reg <- lm(recommend ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11 + X12 + X13 + X14 + X15 + X16+ X17 + X18 + X19 + X20 + X21 +X22, data=all.other) 
mean(fitted.values(all.other.reg))


################################################################################
#Question 4
################################################################################

all.other$X1[all.other$X1 < 5]  <- all.other$X1[all.other$X1 < 5] + 1
all.other$X2[all.other$X2 < 5]  <- all.other$X2[all.other$X2 < 5] + 1
all.other$X7[all.other$X7 < 5]  <- all.other$X7[all.other$X7 < 5] + 1
all.other$X8[all.other$X8 < 5]  <- all.other$X8[all.other$X8 < 5] + 1
all.other$X10[all.other$X10 < 5]  <- all.other$X10[all.other$X10 < 5] + 1
new.recommend <- predict(object=all.other.reg, newdata=all.other)
mean(new.recommend)



