#Importing the data
setwd("C:/Users/hp/Downloads/Foundations of Business Analytics/Homework 1")
data1 = read.table("starbucks final data.txt", header=T, sep="\t")

#Question 1
colSums(is.na(data1))
sum(is.na(data1))

#Question 2
omitted <- data1[complete.cases(data1),]
head(omitted)
dim(omitted)
dim(data1)

#Question 3
tot_inv_val=0
for (i in 1:22)
{
print(paste("For column ",i," the number of invalid values is ",sum((omitted[,i]>5)*1)+sum((omitted[,i]<0)*1)))
tot_inv_val = tot_inv_val + sum((omitted[,i]>5)*1)+sum((omitted[,i]<0)*1)
}
print(tot_inv_val)


#Question 4
for (i in 1:22)
{
omitted[,i][omitted[,i] > 5] <- 5
omitted[,i][omitted[,i] < 0] <- 0
print(paste("The frequency count for column ",i," is "))
print(table(omitted[,i]))
}

#Question 5
omitted$satis100[omitted$satis100 > 100] <- 100
omitted$satis100[omitted$satis100 < 0] <- 0
omitted$recommend[omitted$recommend > 10] <- 10
omitted$recommend[omitted$recommend < 0] <- 0

table(omitted$recommend)


for (i in 1:length(omitted)){
print(paste("The average of column ",i," is ",mean(omitted[,i])))
}
