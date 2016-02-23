rm(list=ls())
load("exercise-1.Rdata")

# Lets observe the data
plot(data)

# Lets observe the linear model
linear.model

# Lets extract coeficiens of the linear model
coef(linear.model)
beta <- coef(linear.model)
anothermodule <-lm(y~x, data = data)
coef(anothermodule)
# Use the linear model formula y = beta[1] + beta[2] * x  to predict values on data$x
ypred <- beta[1] + beta[2] * data$x

plot(data)
abline(a = beta[1],b = beta[2],col="red", lwd=3)
abline(a=,b=,col="blue",lwd=1)
points(data$x, ypred, col = "red", lwd = 2)
ypred.lm <- predict(linear.model, newdata = data)
points(data$x, ypred.lm, pch = 18, col = "blue")

result <-ypred==ypred.lm
result[result==TRUE]
# Does noty work
predict(linear.model, newdata = c(1:5))

predict(linear.model, newdata = data.frame(x=c(1:5)))

# Now do the same thing with matrix operations 
# Form the design matrix X  
n <- nrow(data)
X <- cbind(rep(1, n), data$x) 
X
# Express predictions using matrix algebra
ypred.mat <- X %*% beta

# Check that the result is the same 
t(ypred-ypred.mat)


# Compute mean of squares error using matrix algebra
n <- nrow(data)
mse <- 1/n * sum((data$y - ypred)^2)

# Try to get the same result using matrix operations and data$y and ypred
mse.simple <- 1/n * t(data$y-ypred) %*%(data$y-ypred)

# Try the full formula 
mse.complex <- 1/n * t(data$y-X %*% beta) %*%(data$y-X %*% beta) 

x <- data$x
y <- data$y


FitLM <- function(x, y)
{
	X <- cbind(rep(1, length(x)), x)
	A <- t(X) %*% X
	b <- t(X) %*% y
	beta <- solve(A, b)
	return(beta)
}

FitLM(data$x, data$y)

coef(lm(y ~ x + 1, data = data))


# Error vector

eps <- rnorm(100, mean = 0, sd = 0.05)


