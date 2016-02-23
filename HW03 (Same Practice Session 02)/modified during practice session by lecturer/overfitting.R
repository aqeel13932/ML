rm(list=ls())

# Generate the data and noisy data
x <- c(1:10)
y <- c(1:10)
eps <- rnorm(10, sd = 1)
plot(x, y, pch = 16, col = "blue")
points(x, y + eps, pch = 16, col = "red")


# Generate data frames for later use
data <- data.frame(x = x, y = y)
noisy.data <- data.frame(x = x, y = y, eps = eps, ye = y + eps)
poly.10.data <- data.frame(x1 = x, x2 = x^2, x3 = x^3, x4 = x^4, x5 = x^5, x6 = x^6, x7 = x^7, x8 = x^8, x9 = x^9)

# Estimate coefficiens for the models y ~ x + 1 , ye ~ x + 1, eps ~ x + 1
model.y <-  
model.ye <- 
model.eps <- 

# Verify that the linearity indeed holds


# Estimate the noise impact on the coefficients  
# For that you have to sample noise vector and do the linear regression many times and then 
# draw the corresponding boxplots 

# This is an example if the noise comes from N(0,1)
# Note that the error eps does not have to be part of the data frame.
# As the column eps is missing from the data frame, GNU R uses the variable eps in regression 

k <- 100
beta <- matrix(NA, 2, k)
for(i in c(1:k))
{
	eps <- rnorm(10)
    model <- lm(eps ~ x + 1, data = data)	
    beta[ , i] <- coef(model)
}
boxplot(beta[1, ], beta[2, ], names = c(expression(beta[0]), expression(beta[1])))


# For the 9th order polynomial you can use the following lm command
lm(eps ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + 1,  poly.10.data)
