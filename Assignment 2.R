####################################################################################################
# Question 2
####################################################################################################

# Reading in the dataset and making the data easier to use.

setwd("C:/Users/hp/Downloads")
library(readxl)
starbuck <- read_excel("C:/Users/hp/Downloads/Starbucks HW2 Data.xlsx")
attach(starbuck)
options(scipen=999)

# Running the linear regression model.

fit <- lm(profits ~ satis100 + recommend + Income)

# Checking the linear regression values

summary(fit)
satis100_val = 77 
recommend_val = 8
Income_val = 121500

coef = fit$coefficients

# Calculating the regression value for the given parameters.

cust1_value = coef[1] + satis100_val*coef[2] + recommend_val*coef[3] + Income_val*coef[4]

####################################################################################################
# Question 3
####################################################################################################

#Calculating the fail and exceed values

fail=(satis100 < 20)*1
exceed=(satis100 > 80)*1

# Building the linear regression model

fit2 <- lm(profits ~ fail + exceed + recommend + Income)
coef2 = fit2$coefficients

#Calculating the  number of fail and exceed values

number_of_fail_values = sum(fail)
number_of_exceed_values =sum(exceed)

#Calculating the absolute values of the difference between the fail and exceed coefficients
abs(coef2[2]-coef2[3])

