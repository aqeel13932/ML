rm(list=ls())
load("exercise-1.Rdata")

# Lets observe the data
plot(data)

# Lets observe the linear model
linear.model

# Lets extract coeficiens of the linear model
coef(linear.model)
beta <- coef(linear.model)

# Use the linear model formula y = beta[1] + beta[2] * x  to predict values on data$x
ypred <- ??
plot(data)
points(data$x, ypred, col = "red", lwd = 2)
ypred.lm <- predict(linear.model, newdata = data)
points(data$x, ypred.lm, pch = 18, col = "blue")

predict(linear.model, newdata = data.frame(x=c(1:5)))

# Now do the same thing with matrix operations 
# Form the design matrix X  
n <- nrow(data)
X <- cbind(??, ??) 

# Express predictions using matrix algebra
ypred.mat <- ??

# Check that the result is the same 
t(ypred-ypred.mat)


# Compute mean of squares error using matrix algebra
n <- nrow(data)
mse <- 1/n * sum((data$y - ypred)^2)

# Try to get the same result using matrix operations and data$y and ypred
mse.simple <- 1/n * t(data$y-ypred) %*%(data$y-ypred)

# Try the full formula 
mse.complex <- 1/n * ?? 
