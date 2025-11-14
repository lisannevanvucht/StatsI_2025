#####################
#Project: ps03_answers
#Author: Lisanne van Vucht
#Date: 13-11-2025

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
library(ggplot2)
library(stargazer)


# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# read in data
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2025/main/datasets/incumbents_subset.csv")

#check data
head(inc.sub)
str(inc.sub)

#Q1 we are interested in knowing how the difference in campaign spending 
#between incumbent and challenger affects the incumbent’s vote share.

#1.1. regress the outcome variable (voteshare) 
#on the explanatory variable (difflog) 
regression1 <- lm(voteshare ~ difflog, data = inc.sub)

#see results
summary(regression1)

#1.2. make a scatterpolot of the two variables and add the regression line

ggplot(data = inc.sub, aes(x = difflog, y = voteshare)) + 
  geom_point(size = 0.5, shape = 22) + 
  geom_smooth(method = lm, color = "pink") + 
  labs(title = "Association between Campaign Spending Difference and Incumbent Vote Share", 
       x = "campaign spending", 
       y = "incumbents vote share") + 
  theme_bw()

#1.3. save the residuals of the model in a separate object.

residuals1 <- regression1$residuals
residuals1

#1.4. write the prediction equation 
summary(regression1)
# incumbent vote share = intercept + slope * difflog
# Incumbent vote share = 0.579031 + 0.041666 * difflog

# Q2: how is the difference in spending related to the presidential candidates vote share?

regression2 <- lm(presvote ~ difflog, data = inc.sub)
summary(regression2)

ggplot(data = inc.sub, aes(x = difflog, y = presvote)) + 
  geom_point(size = 0.5, shape = 22) + 
  geom_smooth(method = lm, color = "lightblue") + 
  labs(title = "Association between Campaign Spending Difference and Presidential Vote Share", 
       x = "Campaign spending", 
       y = "Voteshare presidential candidates") + 
  theme_bw()

residuals2 <- regression2$residuals
residuals2

# write the prediction equation
summary(regression2)
# presidential candidate vote share = intercept + slope * difflog
# presidential candidate vote share = 0.507583 + 0.023837 * difflog

#Q3: how is the presidential candidate's vote share associated with the 
#incumbent’s vote share?

regression3 <- lm(voteshare ~ presvote, data = inc.sub)
summary(regression3)

ggplot(data = inc.sub, aes(x = presvote, y = voteshare)) + 
  geom_point(size = 0.5, shape = 2) + 
  geom_smooth(method = lm, color = "green") + 
  labs(title = "Association between Presidential and Incumbent Vote Share ", 
       x = "Voteshare presidential candidates", 
       y = "Voteshare incumbent presidents' party") + 
  theme_bw()


#voteshare incumbent presidents' party = intercept + slope * voteshare presidential candidatess
#voteshare incumbent presidents' party = 0.441330 + 0.388018 * voteshare presidential candidatess


#Q4: regression of residuals
#residuals1: variation in voteshare not explained by difflog
#residuals2: variation in presvote not explained by difflog

#4.1.create new dataframe for regression: 
dataframe <- data.frame(residuals_1 = residuals1, residuals_2 = residuals2)

#4.2. regression: residuals1 on residuals2
regression4 <- lm(residuals_1 ~ residuals_2, data = dataframe)

#4.3. make scatterplot
ggplot(data = dataframe, aes(x = residuals_2, y = residuals_1)) + 
  geom_point(size = 2, shape = 2) + 
  geom_smooth(method = lm, color = "green") + 
  labs(title = "Association between Residuals of Presidential and Incument Models", 
       x = "residuals 2", 
       y = "residuals 1") + 
  theme_bw()

#4.4. prediction equation for residual regression

regression4

##residuals_1 = 0.2569 * residuals_2
# if we remove the part explained by difflog from both variables,
#a one-unit increase in the residual of presvote is associated with
#a 0.26 increase on avarage in the residual of voteshare.

# coefficient of presvote in reg5 and regression 4 is the same because both 
# regressions answer same question in 2 different ways; how much presidents' 
#voteshare influences voteshare for incumbent if account for spending differences

#Q5: multiple regression > voteshare affected by both difflog and presvote

#5.1. multiple regression

regression5 <- lm(voteshare ~ difflog + presvote, data = inc.sub)
summary(regression5)

summary(regression4)
summary(regression5)

# 5.2. prediction equation 
# voteshare_hat = 0.44864 + 0.03554 * difflog + 0.25688 * presvote
#i.e. the expected incumbent vote share increases by on avargae 0.036 for 
#each one-unit increase in difflog, and by about 0.257 for each 
#increase in presidential vote share, holding difflog constant.

#5.3 assocotion between regression4 and regression5
# coefficient of presvote in regression5 (0.2569) is the same as the coefficient 
#of residuals_2 in regression4 (0.2569). Both measure the effect of presidential 
#vote share on incumbent vote share while holding difflog constant.
#in regression5, we control for difflog directly in a multiple regression.
#in regression4, we first remove the part explained by difflog (using residuals) 
#and then regress the remaining variation in voteshare on the remaining variation
#in presvote. because both approaches isolate the same partial effect of presvote, 
#the coefficients match.


