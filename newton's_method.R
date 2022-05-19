# Clear workspace
rm(list = ls())

# Clear plots
dev.off(dev.list()["RStudioGD"])

# Get location of current script
fileloc <- dirname(rstudioapi::getSourceEditorContext()$path)

# Set working directory to script location
setwd(fileloc)

# Remove fileloc variable
rm(fileloc)

# Set locale to English
Sys.setlocale("LC_ALL","English")

library(tidyverse)

# Example taken from Chong and Zak (2013), p. 117
# Find the minimizer and the minimum value of the function:
f <- expression(0.5*x^2 - sin(x))

x <- seq(-10, 10, by = 0.01)
data1 <- as.data.frame(cbind(x, eval(f))) %>% 
  rename(fx = V2)

# Make function plot
ggplot(data1, aes(x = x)) + 
  geom_line(aes(y = fx), col = "red") + 
  ylab("f(x)")

# First derivative
df <- D(f, 'x')

# Second derivative
d2f <- D(df, 'x')

# Tolerance level
tol <- 1e-5

x_k <- 0.5

x <- x_k
x_k1 <- x - eval(df)/eval(d2f)

iter <- 1

repeat{
  x <- x_k
  x_k1 <- x - eval(df)/eval(d2f)
  if(abs(x_k1 - x_k) < tol){
    break
  }
  x_k <- x_k1
  iter <- iter + 1
}

# Print the minimizer and the minimum function value
x <- x_k1
cat(paste("The value of x minimizing f(x) equals", x, "."))
cat(paste("The minimum of f(x) equals"), eval(f),".")
cat(paste("It took", iter, "iterations to converge given the tolerance level."))
cat(paste("Since f''(x) equals", eval(d2f), "at the minimizer, this is a strict minimum.")) 
