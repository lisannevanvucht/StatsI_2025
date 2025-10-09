#####################
#Project: ps01_answers
#Author: Lisanne van Vucht
#Date: 07-10-2025

# load libraries

# set wd
# clear global .envir
#####################

setwd("~/Documents/TCD/Quants I")

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}
lapply(c("paletteer"),  pkgTest)

#####################
# Problemset 1
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

#option 1: short way to do it
#n<30 so t-test for CI possible
t.test(y,conf.level = 0.90)

#option 2: long way to do it
n <-length(y)
mean_y <-mean (y)
sd_y <-sd(y)
se_y <-sd(y) / sqrt(n)

#using critical value, i.e. space under curve, from t-distribution 

t90 <- qt(1- (0.10/2), df = n - 1)
ci_90_lower_t <- mean_y -t90 *se_y
ci_90_upper_t <- mean_y + t90 * se_y

#results: mean of 98.44 and CI circa (94;103)

#step 2
#onesided test find out if mean IQ > 100 
#y is vector | mu = 0 hypotheses mean
#is our sample mean data greater than 0
#the mean, testing sample mean against 0 hypothesis mean
#mu = our given true value
#H0 - school mean is 100
#H1 = school mean is > 100
#a = 0.05 > 1-0.05 > 95%

t.test(y, mu = 100, alternative = "greater", conf.level = 0.95)

#define test statistics (TS)
TS <- (mean_y-100)/se_y
pt(q=TS, df =24)

#variant 1
1-pt(q=TS, df=24)

#variant 2
pt(q=TS, df = 24, lower.tail = FALSE)

#pt value gives you the probability of seeing the value TS in our distribution, what is the probability
#qt gives the x value and probability 
#used t because smaller than 30, now calculating the p value = probability of seeing value as the test statisic if our 0 hypothesis is true


#interpretation of results: p value = 0.72 so we can not reject H0
# in simpler terms: the school IQ average is not greater than the national IQ average of 100

#####################
# Problemset 2
#####################

#step 1: plot relationship among Y, X1, X2, X3
#what are correlations/accosiations among them

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2025/main/datasets/expenditure.txt", header=T)
#numerical / categorical data natural order


head(expenditure)
str(expenditure)

#category = non numerical, so we have some concerns 
?pairs
pairs(expenditure)
#state = non numerical 
expenditure[1,1]
expenditure[12,3]

#row and then column, we have two dimensions [..]. combine more into one

expenditure[,c(2,3,4,5),]

pairs(expenditure[,c(2,3,4,5),])

#one scatterplot = plot between two variables

plot(x = expenditure$X1, y = expenditure$Y)
#plot(y = expenditure$X1, x = expenditure$Y)

#we observe positive correlalations for (X1,Y), (X3, X1)
#for (X2,Y), (X1, X2), (X2, X3) not so strong but see a curvature which could imply a quadratic relationship


#step 2: Please plot the relationship between Y and Region? On average, which region has the highest per capita expenditure on housing assistance? 
#options plots: scatterplot, boxplot, ...

#plot(x = expenditure$Region, y = expenditure$Y)

#boxplot(expenditure$Region, expenditure$Y)


#factor variables are always categorical variables
#str(expenditure)

# indexing it like: df[,'x'] <-factor(df)
# better solution 
#labels: Northeast = 1,North Central = 2, South = 3, West = 4 

str(expenditure)
expenditure$Region = factor(expenditure$Region,
                            levels = c(1, 2, 3, 4),
                            labels = c("Northeast", "North Central", "South", "West"))
boxplot(formula = expenditure$Y ~ expenditure$Region)

#answer: On average the West region has the highest per capita expenditure on housing assistance 


# step 3: please plot the relationship between Y and X1  

plot(x = expenditure$X1, y = expenditure$Y) 

# answer: There appears to be a positive/moderate assocation between X1 and Y. 

# step 4: reproduce the above graph including one more variable Region and displaydifferent regions with different types of symbols and colors.
lapply(c("paletteer"),  pkgTest)

colors <- paletteer_c("viridis::inferno", n=4)
shapes <- c(17, 18, 19, 20)

plot(x = expenditure$X1, 
     y = expenditure$Y,
     col = colors[expenditure$Region],
     pch = shapes[expenditure$Region], 
     cex = 1,
     main = "Spending on housing against per capita income
     (by region)",
     xlab = "per capita income", 
     ylab = "per capita expenditure on housing/shelters")

