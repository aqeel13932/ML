# Clean workspace
rm(list=ls())

# Generate linearly dependent data samples
n <- 100
x <- c(1:n)/n - 0.5
y <- x + 1 + rnorm(n, mean = 0, sd = 0.2)

data <- data.frame(x = x, y = y)
linear.model <- lm(y ~ x + 1) 
save(data, linear.model, file = "exercise-1.Rdata")


rm(list=ls())
library(car)

# Generate data samples from a model y = x^2 - 2x + 1
n <- 100
x <- c(1:n)/n*2 
y <- x^2 -2*x + 1 + rnorm(n, mean = 0, sd = 0.1)
data <- data.frame(x = x, y = y)

n <- 200
x <- c(1:n)/n*2 
y <- 2*x  + sin(x*5*pi + 0.22)
weird.data <- data.frame(x = x, y = y)

save(Duncan, data, weird.data,  file = "exercise-3.Rdata")

rm(list=ls())

x <- c(1:20)/10 - 1
y <- x
y[19] <- 2
outlier.data <- data.frame(x = x, y = y) 
rm(list=c("x", "y"))

x <- c(1:20)/10 - 1
y <- x * (1 - x)
nonlinear.data <- data.frame(x = x, y = y)

save(outlier.data, nonlinear.data, file = "exercise-4.Rdata")