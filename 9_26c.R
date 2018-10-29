### 9.26 c ###

cdi <- read.table("http://users.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Appendix%20C%20Data%20Sets/APPENC02.txt", header=FALSE)
head(cdi)

#Note SBC == BIC

#Data Description
#This data set provides selected county demographic information (CDI) for 440 of the most
#populous counties in the United States. Each line of the data set has an identification number
#with a county name and state abbreviation and provides information on 14 variables for a
#single county. Counties with missing data were deleted from the data set. The information
#generally pertains to the years 1990 and 1992. The 17 variables are:

#Response Y: total # of serious crimes

#Consider the even-numbered cases to constitute the model-building data set to be used for the following analyses????

#Task: using the SBC criterion, obtain the 3 best subsets

#remove ID number, total population, county, state, region
cdi <- cdi[-c(1,2,3,5,17)]
head(cdi)

#name the variables
names(cdi) = c(paste0("X",1:5), "Y", paste0("X", 6:11))
head(cdi)

#fit the full model
fit <- lm(Y~., data=cdi)


#run best subsets
library(leaps)
regsubsets.out <- regsubsets(Y~., data=cdi, nbest= 2, method="exhaustive", really.big = T)
summary(regsubsets.out)

#Evaluate SBC/BIC
plot(regsubsets.out, scale = "bic", main = expression(BIC[p]))

#Best: X4, X5, X7, X10, X11


#Even observations
evens <- seq(2,400,2)
cdi <- cdi[evens, ]

regsubsets.out.evens <- regsubsets(Y~., data=cdi, nbest= 2, method="exhaustive", really.big = T)
summary(regsubsets.out.evens)
  
#Evaluate SBC/BIC
plot(regsubsets.out.evens, scale = "bic", main = expression(BIC[p]))

#Best: X4, X5, X8, X11

#X4 is number of active physicians, X5 is number of hospital beds, 
#X8 is % below poverty line X11 is total personal income

fit <- lm(Y~X4+X5+X8+X11, data=cdi)
summary(fit)

