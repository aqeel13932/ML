rm(list=ls())
setwd("/home/aqeel/Study/ML/HW03\ (Same\ Practice\ Session\ 02)")
load("exercise-1.Rdata")
#### First Question A #####
# Lets extract coeficiens of the linear model
beta <- coef(linear.model)
#Create y=X*B
#create (1,x) data columns
X<-cbind(rep(1,nrow(data)),data$x)
ypredict.mat<-X %*% beta
#Draw The points with two types of prediction
ypred.lm <- predict(linear.model, newdata = data)
plot(data)
points(data$x, ypredict.mat, col = "red")
points(data$x, ypred.lm, pch = 18, col = "blue")

# Check that the result is the same 
t(ypred.lm- ypredict.mat)


# compute mse using straightforward way
n <- nrow(data)
mse <- 1/n * sum((data$y - ypred.lm)^2)

# calculating mse using matrix
mse.matrix <- 1/n * t(data$y-ypred.lm) %*%(data$y-ypred.lm)
mse-mse.matrix


#### First Question B #####
FitLM<-function(x,y)
{
  X<-cbind(rep(1,length(x)),x)
  beta<-solve(t(X)%*%X,t(X)%*%y)
  return (beta)
}

FitLM(data$x,data$y)
coef(lm(y~x,data = data))
#Update Function to Take matrix or vector as X
FitLM<-function(x,y)
{
  if (!is.null(nrow(x)))
  {
    n<-nrow(x)
  }
  else
  {
    n<-length(x)
  }
  X<-cbind(rep(1,n),x)
  beta<-solve(t(X)%*%X,t(X)%*%y)
  return (beta)
}
#build the error (to get the same one for both operations)
e<-rnorm(n =length(data$x),mean = 0,sd = 0.05)
#put data in datafram to use them for lm function
df <- data.frame(X1=data$x,X2= (data$x)^2, E=e,Y=data$y)

FitLM(cbind(X1=data$x,X2= (data$x)^2, E=e),df$Y)
lm(Y~X1+X2+E,df)
## Generating x1+4x2+x3+e###
X<-cbind(X1=runif(100,0,1000),X2=4*runif(100,0,1000),X3=runif(100,0,1000),E=rnorm(100,0,0.05))
Y<-runif(100,0,1000)
df<-data.frame(cbind(X),Y=Y)
lm(Y~X1+X2+X3+E,df)
FitLM(X,Y)

####### First Question C ##########
msecalculator<-function(mv)
{
  ypr<-data$x*mv[,2]+mv[,1]
  mse.matrix <- 1/100 * t(data$y-ypr) %*%(data$y-ypr)
  return(mse.matrix)
}
m<-expand.grid(x=seq(0,2,0.1),y=seq(-5,5,0.1))
mse<-NULL
for (i in 1:nrow(m))
{
  mse<-rbind(mse,msecalculator(m[i,]))
}

library(plot3D)
plot3D::polygon3D(m$x,m$y,mse)
mse[which.min(mse)]
m[which.min(mse),]
