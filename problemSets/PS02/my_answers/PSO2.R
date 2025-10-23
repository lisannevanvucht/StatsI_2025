#####################
#Project: ps02_answers
#Author: Lisanne van Vucht
#Date: 14-10-2025

# load libraries

# set wd
# clear global .envir
#####################


setwd("~/Documents/TCD/Quants I")

# remove objects
rm(list=ls())

######### question 1

#1. observed 
data <- matrix(NA, nrow= 2, ncol = 3)
rownames(data) <- c("upper class", "lower class")

colnames(data) <- c("not stopped", "bribe requested", "stopped/given warning")
data[1,] <- c(14,6,7)
data[2,] <- c(7,7,1)

#view 
data 

#2, columns/row total
Rsum <-rowSums(data)
Csum <- colSums(data)
#first row because first in formula
grand_total <-sum(data)

Rsum
Csum
grand_total

#rowSums#nominal or ordinal categoricial 

#test statistisc dif value 0 and alternative hypothesis value 

#chi square = sum observed - expected values / observed frequency

#variance calculation

#rowSums(x = data)
#colSums(x = data)

#expectation = mean usually 


#combination of variables by sum of all combination in the table, 


# why multiply in the chi square first row


#3. create matrix with expected values (E = expected)
expected <- outer(Rsum, Csum)/grand_total

expected

#4. Chisquare teststatistic manually (option 1)

chi_sq <- sum((data - expected)^2/expected)
chi_sq

# 4.1. to get a p value we have to compare it to a=0,1 = 90%
#data, formulate 0, a h, test stats, compare this in a reference distribution get a t value
#df sort of currencies for your formula, for each you have 
df <- (nrow(data) -1)*(ncol(data)-1)
df
#df = 2

#4.3. calculate p value, probability test statistic is within wit 0.1 a

#?pchisq
#p value give a value of 2, q what is the q value of 70% of seeing this n the distibution

p_value <- pchisq(chi_sq,2, lower.tail = FALSE)
p_value

#p value = 0,1502

#p value is 0,15 = larger so we do not reject it
#h0 no difference in how many times upper and lower class drivers are being stopped, they are dependent. we can not reject the 0 hypothesis so we assumme that they are statistically indpendent

#chi = 3.79

#short way to calculate this
chi_control <- chisq.test(data, correct = FALSE)
#correct false because Yates continuity correction only with 2x2 tables and we have 2x3

chi_control$statistic
chi_control$parameter
chi_control$p.value
chi_control$expected


#residuals, expected vs observed outcomes, residuals cannot be explained under the 0 hypothesis
#pearson residuals: (O - expected)/sqrt(expected)
residuals <- (data - expected) / sqrt(expected)

residuals



#values close to 0 → observed is close to expected → little or no contribution to the overall χ² statistic.
#Large absolute values (around ±2 or more) → observed count is quite different from what independence would predict → that cell contributes strongly to χ².
#positive value → observed > expected.
#negative value → observed < expected.

#with standardized residuals ranging from –1.10 to +1.09, none of the combinations
#of class (upper vs. lower) and outcome (not stopped, bribe requested, 
#stopped/given warning) stand out: for instance, the slightly positive residual 
#for lower class–bribe requested (1.09) and the negative one for lower 
#class–stopped/given warning (–1.10) signal only minor deviations from 
#expectation, not meaningful effects.

############################################
# question 2: Economics — Chattopadhyay & Duflo (2004)
# Data link (as given in PS02): 
# https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv
# Variables (from the qss-data spec): GP, village, reserved, female, irrigation, water
#   - reserved: indicator that the GP was reserved for a female GP head (treatment)
#   - water: number of new or repaired drinking water facilities (outcome)
#
# (a) Hypotheses (two-tailed):
#     H0: beta_reserved = 0   (reservation has no effect on the number of water facilities)
#     H1: beta_reserved != 0
############################################

download.file(
  url = "https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv",
  destfile = "women.csv",
  mode = "wb"
)

women

women <- read.csv("women.csv")

#check structure of data before modelling 

str(women)
head(women)
summary(women)

#does reservation of GP head position for women change the number of drinking water facilities?
#h0: reservation has no effect on water
#ha: reservation does affect water 

model <- lm(water ~ reserved, data = women)
summary(model)

##Residual standard error: 33.45 
## large res. error is quite large compared to the estimated effect of 9.25, meaning
##There’s a lot of variability in the number of water facilities that isn’t explained treatment

###R squared Only 1.7% of the variance in water facilities is explained by whether the seat was reserved or not
###effect exists but it does not explain much of the total differences between villages


###p-value overall model = 0.0197
####Even though the model explains only a small portion of the variance, 
#the effect is real and not likely due to random chance.
##small t-value: 2.344

# intercept 14,738 > dorpen zonder reservering hebben gemiddeld ongeveer 14.7 waterfaciliteiten.
# reserved 9252 > dorpen mét reservering hebben gemiddeld 9.25 meer waterfaciliteiten dan dorpen zonder reservering.


#conclusion: Reject H0: because there is a relation between reservation and water facilities

###Statistical significance ≠ large effect size because it explains small portain of the variance.


##Villages with a reserved female head GP have 9,25 extra water facilities on avarage 
###compared to villages without reserved head. 



