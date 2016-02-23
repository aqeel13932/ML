rm(list=ls())
load("exercise-3.Rdata")

# A quick example that shows the most important things in multivariate regression 

# Lets train the model
model <- lm(prestige ~ income + education + 1, data = Duncan)

# Observe basic properties of the model
summary(model)

# Export the model coeffients to a usable form 
coef(model)

# Extract confidence intervals for model parameters
confint(model)
confint(model, "(Intercept)", level = 0.7)


# Export prediction errors (residuals) to a usable form
residuals(model)

# Lets see how big is the prediction error in precentages for each occupation
plot(residuals(model)/Duncan$prestige*100)

# Lets compute mean square error 
mean(residuals(model)^2)

# Lets compute normalised mean square error
base.model <- lm(prestige ~ 1, data = Duncan)
mean(residuals(model)^2)/mean(residuals(base.model)^2)*100

# Shorter version
mean(residuals(model)^2)/mean((Duncan$prestige - mean(Duncan$prestige))^2) * 100

# Even more shorter version
mean(residuals(model)^2)/var(Duncan$prestige)*100


# Polynomial regression
# Lets look at the data
plot(data, pch=16)

# Lets try the linear regression
model <- lm(y ~ x + 1, data = data)
plot(data, pch = 16)
abline(a = coef(model)[1], b = coef(model)[2], col = "red", lwd=2)

# Let prepare the extended data matrix for fitting quadratic relations y ~ x^2 + x + 1
quadratic.data <- data.frame(x1 = data$x, x2 = data$x^2, y = data$y)

plot(data$x, predict(lm(y~x1+x2+1, quadratic.data), newdata = quadratic.data), type="l", col="red")
points(data, pch=16)
coef(lm(y~x1+x2+1, quadratic.data))
# Lets prepare the extended datamatrix for fitting polynomials with degree 10
deg10.data <- data.frame(x1 = data$x, x2 = data$x^2,x3=data$x^3,x4=data$x^4,x5=data$x^5,
                         x6=data$x^6,x7=data$x^7,x8=data$x^8,x9=data$x^9,x10=data$x^10, y = data$y)
plot(data$x, predict(lm(y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+1, deg10.data ), newdata = deg10.data), type="l", col="red")
points(data, pch=16)
coef(lm(y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+1, deg10.data))
coef(lm(y~.,data = deg10.data))
coef(lm(y~poly(x,10,raw=TRUE),data = data))
# Lets observe the distribution of residuals
plot(weird.data, pch = 16)
sin.data <- ??