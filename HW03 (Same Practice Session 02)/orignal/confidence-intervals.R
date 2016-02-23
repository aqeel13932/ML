rm(list=ls())

# This is a procedure that given a list of observations that are assumed to come
# from a model y = y0 + epsilon where the error epsilon is assumed to have normal 
# distribution N(0, 1) outputs a symmetric confidence interval.   

GetConfidenceInterval <- function(y, confidence.level)
{
	if(length(y) == 1) stop("At least two observations are needed")
	if(confidence.level < 0 || confidence.level > 1) stop("Confidence level must be in the range [0,1]") 
	mean <- mean(y)
	mean.sd <- 1/sqrt(length(y))
    y1 <- qnorm((1 - confidence.level)/2, mean = mean, sd = mean.sd)
    y2 <- qnorm((1 - confidence.level)/2, mean = mean, sd = mean.sd, lower.tail = FALSE)
    return(c(y1, y2))	
}

GetConfidenceInterval(c(1,3,5), 0.95)

# This is a function that generates data that is in form y = y0 + epsilon where the 
# error epsilon has normal distribution N(0, 1) as assumed by the first algorithm.

GenerateData <- function(y0, n)
{
	return(y0 + rnorm(n, mean = 0, sd = 1))
}

# Test that the first algorithm works correctly, i.e., on average the algorithm manages
# to output such intervals that the fraction when the algorithm is correct is roughly
# around confidence level for large enough number of samples.

# Illustrative example with illusrative plot
# Test parameters
y0 <- 0
n <- 3
k <- 100
confidence.level <- 0.5

# Plot setup
plot(c(), c(), ylab = "Runs", xlab = "Guesses",  xlim = c(y0 - 3, y0 + 3), ylim = c(0, 1), axes = FALSE)
xlabels <- c(expression(y[0] - 3), expression(y[0] - 2), expression(y[0] - 1), expression(y[0]))
xlabels <- c(xlabels, expression(y[0] + 1), expression(y[0] + 2), expression(y[0] + 3))
Axis(side=1, at = c(-3:3), labels = xlabels)
lines(c(y0, y0), c(0,1), lwd = 3, lty = "solid")

# Test runs
success <- c(NA, k)
for(i in c(1:k))
{
	y <- GenerateData(y0, n)
	interval <- GetConfidenceInterval(y, confidence.level)
	success[i] <- (interval[1] <= y0) && (y0 <= interval[2])
	lines(interval, c(i/k, i/k), col = c("red", "blue")[1 +  success[i]])
}
title(sprintf("The correct value appeared in the interval in  %2.0f%% of runs", mean(success)*100))


# Do your own experiments to verify that the algorithm indeed works correctly
# Try different sizes of observation vector n and different confidence levels
# To get precise enough estimates on the fraction of successful runs you should 
# take k >= 1000. However, this kills the plot. So draw the plot separately.


# Simulation for the measurement procedure described in the second part of the exercise
# We only do a graphical illustration for one particular strategy and do not compute 
# required values

n <- c()
cost <- c()
success <- c()
for(k in c(1:20)){
	y0 <- runif(1, min = -100, max = 100)
	
	# We use a measurements strategy where we make randomly 2, ..., 100 low precision measurements 
	n[k] <- sample(c(2:100), 1)
	y <- GenerateData(y0, n[k])
	cost[k] <- 0.05 * n[k]
	
	# Lets estimate the interval for confidence level 50%
	interval <- GetConfidenceInterval(y, 0.50)
	cost[k] <- cost[k] + 1 * (interval[2] - interval[1])
	
	# Lets see whether the high precision scan was successful or not
	success[k] <- (interval[1] <= y0) && (y0 <= interval[2]) 
}

par(mfrow = c(2, 2))
barplot(n, space = 0.1, col = c("red", "blue")[1 + success], ylab = "Number of measurements", ylim=c(0, 100),  xlab = "Run")
barplot(0.05 * n, space = 0.1, col = c("red", "blue")[1 + success], ylab = "Cost of low-quality measurements", ylim=c(0, 5),  xlab = "Run")
barplot(cost - 0.05 * n, space = 0.1, col = c("red", "blue")[1 + success], ylab = "Cost of high-quality scan", xlab = "Run")
barplot(cost, space = 0.1, col = c("red", "blue")[1 + success], ylab = "Overall cost", ylim = c(0,10), xlab = "Run")

par(mfrow = c(1, 2))
barplot(n, space = 0.1, col = c("red", "blue")[1 + success], ylab = "Number of measurements", ylim=c(0, 100),  xlab = "Run")
plot(cumsum(cost)/cumsum(success), type = "s", xlab = "Run", ylab = "Average cost")
