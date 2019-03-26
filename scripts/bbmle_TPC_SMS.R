rm(list= ls()) ##clear R

install.packages("bbmle") ## RUN THE FIRST TIME TO INSTALL THIS PACKAGE.
install.packages("pracma") ## RUN THE FIRST TIME TO INSTALL THIS PACKAGE.


library(bbmle) ##load bbmle package, install this package if needed
library(pracma) ##load pracma package, install this package if needed

##this file is used to model TPC using 2 models: Gaussian and Expontentially modified Gaussian (EMG). 

##load your .txt / .csv files. It should have two columns (performance, temp)
mydata1 <-read.csv(file.choose(), sep=",", header=TRUE)
mydata2 <-read.csv(file.choose(), sep=",", header=TRUE)
# transforming your dataframe to x and y titles. Make sure your headers matches the "mydata$name" 
temp <- mydata1$exp_temp ##ENTER YOUR TEMP header
performance <- mydata1$Wei_Res_Rate ##ENTER YOUR RATE header
temp2 <- mydata2$exp_temp
performance2 <- mydata2$Wei_Res_Rate

plot(temp,performance)
points(temp2,performance2)
#ENTER KNOWN LETHAL TEMPERATURE LIMITED OF SPECIES
low.lethal <- 0
high.lethal <- 22

##ADD LOWER LIMIT DATA TO FORCE THE CURVE TO ZERO...THIS IS OPTIONAL
low.performance <- c(0,0,0,0,0,0)
low.temp <- c(low.lethal, low.lethal, low.lethal, low.lethal, low.lethal, low.lethal)
high.temp <- c(high.lethal,high.lethal,high.lethal,high.lethal,high.lethal,high.lethal)

temp <- append(temp, low.temp)
performance <- append(performance, low.performance)

df <- data.frame(temp, performance)

## CHANGE axes limits
plot(temp, performance, col = "orange", xlab = "Temperature", ylab = "Respiration Rate", 
     xlim = c(10, 24), ylim = c(0, 0.06), bty = "l", pch = 20, lwd = 5, 
     main = "Copper Rockfish Experiment")
points(temp2,performance2, col = "brown", pch = 20, lwd = 5)

plot(temp, performance, col = "black", xlab = "Temperature", ylab = "Respiration Rate", 
     xlim = c(10, 24), ylim = c(0, 0.06), bty = "l", pch = 20, lwd = 5, 
     main = "Black Rockfish Experiment")
points(temp2,performance2, col = "goldenrod", pch = 20, lwd = 5)

##########################################################################
#                          Gaussian function                             #
##########################################################################

##Gaussian is a simple function. It is a normal distribution fit.
##mu = To (Temperature where performance is the highest)
##k = the amplitude of the curve
##o = standard deviation of hte curve

## THE PROBLEM with the Gaussian function is that it is not biological relevant. 
## The right side distribution lacks a skew that describes performance under thermal stress to mortality
## Nevertheless I have the function here to compare to the EMG


## Create the Gaussian function (define constants and equation)
yhat = function(mu, o, k){
  P = k*exp(-(temp2-mu)^2/(2*o^2))
  return(P)
}

## Create a function to measure negative log likelihood. 
## Negative log likelihood (LL1) is used to predict the constants
LL1 = function (mu,o, k ,y=performance2){
  P = yhat(mu=mu, o=o, k=k)
  #get the total negative log likelihood
  loglike= -sum(dnorm(y,P,log=TRUE))
  return(loglike)
}

## bbmle package, used here to predict fit (constants), needs a predicted start value for each
## constant (mu, k, o). I am going to use the mean to estimate mu and std to estimate the Gaussian std
xmean <- mean(temp2)
xstd <- sd(temp2)

## Predict the constants using maximum likelihood (from bbmle package)
mle.gau = mle2(minuslogl = LL1, start = list(mu = xmean, o = xstd, k = max(performance2) ))

## Gaussian model summary
summary(mle.gau)
mu <- as.numeric(coef(mle.gau)["mu"])
o <- as.numeric(coef(mle.gau)["o"])
k <- as.numeric(coef(mle.gau)["k"])

## Plot the Gaussian model with data
## Here, we are plotting from 0 to 50
## Data will be plotted as black circles

newdata <- data.frame(temp = seq(from=0, to=50, by=0.1), number = seq(from=0, to=50, by=0.1))
newdata$gau <- k*exp(-(newdata$temp-mu)^2/(2*o^2))

## The Gaussian model will be in red
lines(newdata$temp, newdata$gau, col = "orange", lwd=3)

##########################################################################
#         Expontentially Modified Gaussian function I (EMG)              #
##########################################################################

## EMG, simply put, is THE combination of an expontential function and a Gaussian function
## This species model is an non-intercept form, where the asymptote of the function does not cross 0
## The function fits a skew model, in this case to describe the performance going into lethal temperature
## The constants are different from constants in the Gaussian function
## a = location of the asymptote
## b = unknown
## c = upper lethal temperature
## d = unknown
## e = unknown

## Create EMG function and sum negative likelihood
## The equation for EMG can be found in Angeletta 2006 (Table Curve 2D)
emg = function(p, N, k){ 
  a = p[1]
  b = p[2]
  c = p[3]
  d = p[4]
  e = p[5]
  yhat = a+b*d*2.5066282746310005024*exp(d^2/(2*e^2)+(c-N)/e)*(erf((c-N)/(1.4142135623730950488*d)+d/(1.4142135623730950488*(e)))-(e)/abs((e)))/(-2.0*(e))
  result = -sum(dnorm(k, yhat, log=TRUE)) 
}
parnames(emg) = c("a", "b", "c", "d", "e")

## Predict the initial constants using maximum likelihood (from bbmle package)
mle.emg <- mle2(emg, start = c(a = 0.012, b= 0.079, c= 20.64, d = 0.712, e = -9.9), data = list(N = temp, k = performance), method = "Nelder-Mead")
summary(mle.emg)

## Replacing the predicted constants with estimated constants for the next iteration
a <- as.numeric(coef(mle.emg)["a"])
b <- as.numeric(coef(mle.emg)["b"])
c <- as.numeric(coef(mle.emg)["c"])
d <- as.numeric(coef(mle.emg)["d"])
e <- as.numeric(coef(mle.emg)["e"])

## maximum likelihood estimate is run 10000 times in order to find the best fit
## AIC is used to compare the models, the best model is used for the next iteration

for (y in 1:1000) 
{

mle.type2 <- mle2(emg, start = c(a = a, b= b, c= c, d = d, e = e), data = list(N = temp, k = performance), method = "Nelder-Mead", control = list(maxit = 10000000))
summary(mle.type2)

AIC <- (vals.AIC <- AIC(mle.emg, mle.type2))

if (AIC$AIC[2] < AIC$AIC[1]) {
  mle.emg <- mle.type2
}

a <- as.numeric(coef(mle.emg)["a"])
b <- as.numeric(coef(mle.emg)["b"])
c <- as.numeric(coef(mle.emg)["c"])
d <- as.numeric(coef(mle.emg)["d"])
e <- as.numeric(coef(mle.emg)["e"])
}

## This is the final EMG function
newdata$emg <- a+b*d*2.5066282746310005024*exp(d^2/(2*e^2)+(c-newdata$temp)/e)*(erf((c-newdata$temp)/(1.4142135623730950488*d)+d/(1.4142135623730950488*(e)))-(e)/abs((e)))/(-2.0*(e))

## plot a blue line for EMG
lines(newdata$temp, newdata$emg, col = "blue", lwd=2)

##calculate Topt
To.EMG <- newdata$temp[as.numeric(which.max(newdata$emg))]

## print the estimated constants
summary(mle.emg)

## print Topt
To.EMG


##########################################################################
#         Expontentially Modified Gaussian function II (EMG)             #
##########################################################################

## EMG, simply put, is THE combination of an expontential function and a Gaussian function
## This species model is an non-intercept form, where the asymptote of the function does not cross 0
## The function fits a skew model, in this case to describe the performance going into lethal temperature
## The constants are different from constants in the Gaussian function
## a = location of the asymptote
## b = unknown
## c = upper lethal temperature
## d = unknown

## Create EMG function and sum negative likelihood
## The equation for EMG can be found in Angeletta 2006 (Table Curve 2D)
emg = function(p, N, k){
  a = p[1]
  b = p[2]
  c = p[3]
  d = p[4]
  yhat = a*c*2.5066282746310005024*exp(c*c/(2.0*(d)*(d))+(b-N)/(d))*(erf((b-N)/(c*c)+c/(c*(d)))-(d)/abs((d)))/(-2.0*(d))
  result = -sum(dnorm(k, yhat, log=TRUE)) 
}
parnames(emg) = c("a", "b", "c", "d")

## Predict the initial constants using maximum likelihood (from bbmle package)
mle.emg <- mle2(emg, start = c(a = 0.080, b= 19.3, c= 1.00, d = -4.01), data = list(N = temp2, k = performance2), method = "Nelder-Mead")
## Replacing the predicted constants with estimated constants for the next iteration
a <- as.numeric(coef(mle.emg)["a"])
b <- as.numeric(coef(mle.emg)["b"])
c <- as.numeric(coef(mle.emg)["c"])
d <- as.numeric(coef(mle.emg)["d"])

## maximum likelihood estimate is run 10000 times in order to find the best fit
## AIC is used to compare the models, the best model is used for the next iteration

for (y in 1:1000) 
{
  
  mle.type2 <- mle2(emg, start = c(a = a, b= b, c= c, d = d), data = list(N = temp2, k = performance2), method = "Nelder-Mead", control = list(maxit = 10000000))
  AIC <- (vals.AIC <- AIC(mle.emg, mle.type2))
  
  if (AIC$AIC[2] < AIC$AIC[1]) {
    mle.emg <- mle.type2
  }
  
  a <- as.numeric(coef(mle.emg)["a"])
  b <- as.numeric(coef(mle.emg)["b"])
  c <- as.numeric(coef(mle.emg)["c"])
  d <- as.numeric(coef(mle.emg)["d"])
}

## This is the final EMG function
newdata$emg <- a*c*2.5066282746310005024*exp(c*c/(2.0*(d)*(d))+(b-newdata$temp)/(d))*(erf((b-newdata$temp)/(c*c)+c/(c*(d)))-(d)/abs((d)))/(-2.0*(d))

## plot a blue line for EMG
lines(newdata$temp, newdata$emg, col = "goldenrod", lwd=3)

##calculate Topt
To.EMG <- newdata$temp[as.numeric(which.max(newdata$emg))]

## print the estimated constants
summary(mle.emg)

## print Topt
To.EMG

legend(12, 0.05, legend = c("Copper", "Quillback"), 
        pch = c(20, 20), col = c("orange", "brown"), lwd = c(3,3))
legend(12, 0.05, legend = c("Black", "Yellowtail"), 
       pch = c(20, 20), col = c("black", "goldenrod"), lwd = c(3,3))
