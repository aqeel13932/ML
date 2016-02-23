FirstSource <- function(n)
{
	x <- runif(n)
	y <- x + rnorm(n, sd = 0.01) 
	idx <- runif(n) >= 0.9
	y[idx] <- y[idx] + 1
	return(data.frame(x = x, y = y)) 
}

SecondSource <- function(n)
{
	x <- runif(n, 0, 2)
	y <- x * (1 - x) + rnorm(n, sd = 0.01)
	return(data.frame(x = x, y = y))  
}

ThirdSource <- function(n)
{
	x <- runif(n)
	y <- rnorm(n)
	return(data.frame(x = x, y = y)) 
}