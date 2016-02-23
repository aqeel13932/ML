rm(list=ls())
library(car)
source("data-sources.R")
load("exercise-4.Rdata")

################################################################################################################################################
#                                            Diagnostics: The effect of outliers
################################################################################################################################################

# Lets see what happens if we fit linear model to a data with few outliers
plot(outlier.data, pch = 16)
model <- lm(y~x+1, outlier.data)
ypred <- predict(model, newdata = outlier.data)
lines(outlier.data$x, ypred, col = "red", lwd = 2)
title("Outliers confuse the linear regression model")

# Corresponding diagnostics
# Direct observation of residuals to detect non-symmetries
par(mfrow = c(1, 2))
hist(residuals(model), xlab = "Residuals", main = "")
plot(residuals(model), xlab = "Datapoints", ylab = "Residuals")

# More eleboate diagnostics through qqPlot
# Note that the dashed lines are 95% prediction intervals for normal distribution
# which means that 95% of dots should be inside the dashed coridor.
# You can change size od the prediction intervals by setting the envelope value.
 
par(mfrow = c(1, 2))
qqPlot(residuals(model), envelope = 0.95, main = "Observed residuals")
qqPlot(rnorm(20), envelope = 0.95, main = "Theoretical residuals")

# The figure clearly indicates that a single residue is out of the range and 
# most probably corresponds to an outlier value.

# The simplest way to improve the prediction is to use prediction intervals as follows
ypred <- predict(model, newdata = outlier.data,  interval = "predict", level = 0.95)
plot(outlier.data, pch = 16)
lines(outlier.data$x, ypred[, "fit"], col = "red", lwd = 2)
lines(outlier.data$x, ypred[, "lwr"], col = "red", lty = "dashed")
lines(outlier.data$x, ypred[, "upr"], col = "red", lty = "dashed")

# Note that the outlier value is clearly outside the prediction interval and thus we can
# throw it out from the set of reliable data points this leads to the following algorithm

RemoveOutliers <- function(data, formula)
{
	model <- lm(formula, data)
	ypred <- predict(model, newdata = data, interval = "predict", level = 0.95)
	
	# A magic command to get the response variable name 
	response.var <- all.vars(terms(formula))[attr(terms(formula), "response")] 
    y <- data[ , response.var]
    
    return(data[ypred[ , "lwr"] <= y & y <= ypred[ , "upr"], ]) 
}

plot(RemoveOutliers(outlier.data, y ~ x + 1), pch = 16)

# Now we can fit the linear model without outliers and get much better results
new.model <- lm(y ~ x + 1, RemoveOutliers(outlier.data, y ~ x + 1))
plot(outlier.data, pch = 16)
lines(outlier.data$x, predict(model, newdata = outlier.data), col = "red", lwd = 2) 
lines(outlier.data$x, predict(new.model, newdata = outlier.data), col = "blue", lwd = 2) 

# Study the 
data <- FirstSource(100)



################################################################################################################################################
#                                            Diagnostics: Non-linear relations
################################################################################################################################################

# Lets see what happens if we fit linear model to data with quadratic depencency
par(mfrow = c(1, 2))
plot(nonlinear.data, pch = 16)
model <- lm(y~x+1, nonlinear.data)
ypred <- predict(model, newdata = outlier.data)
lines(nonlinear.data$x, ypred, col = "red", lwd = 2)
title("Residuals exhibit a clear pattern")
qqPlot(model, main = "While distribution of residuals is near normal")

# It is hard to detect such patterns matematically. If input values are taken
# at regular intervals, then it is possible to use autocorrelation measures. 
# A patern usually means that positive error follows a postive error 
# and negative error follows an negative error follows a negative error

# We can just look at the correlation of residual(i) and residuals(i + 1)  known as auto correaltion
res <- residuals(model)
cor(res[-length(res)], res[-1])
plot(res[-length(res)], res[-1], pch = 16)

# Or be even more scientific and test whether the correlation is nonzero or not
cor.test(res[-1], res[-length(res)])

# It is possible to find also confidence intervals for the correlation
interval <- cor.test(res[-1], res[-length(res)])$conf.int

# Sometimes the errors are not autocorrelated with lag 1 but with some other interval
# so you should consider correlation between different shifts of the residuals.
# However if the data is sampled irregularly, the best you can do is try to detect
# trends by the eye. 

# For that you should use crPlots which draws a plot for each input variable of the model
# Ideally you should see points that are randomly lokated at both sides of the line

model <- ??

crPlots(model) 
ceresPlots(model)

