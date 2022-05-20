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

# Tolerance level
tol <- 1e-5

# Chong and Zak (2013), Example 7.6, p. 119
g <- expression(x^3 - 12.2*x^2 + 7.45*x + 42)

x <- seq(-5, 20, by = 0.01)
data1 <- as.data.frame(cbind(x, eval(g))) %>% 
  rename(gx = V2)

# Make function plot
ggplot(data1, aes(x = x)) + 
  geom_line(aes(y = gx), col = "red") + 
  geom_hline(yintercept = 0) + 
  ylab("g(x)")

# First derivative
dg <- D(g, 'x')

# Find first root
x_km1 <- 13
x_k <- 12

iter <- 1

repeat{
  x <- x_km1
  g_km1 <- eval(g)
  x <- x_k
  g_k <- eval(g)
  x_k1 <- x_k - (x_k - x_km1)/(g_k - g_km1)*g_k
  if(abs(x_k1 - x_k) < tol){
    break
  }
  x_km1 <- x_k
  x_k <- x_k1
  iter <- iter + 1
}

x1 <- x_k1

# Second root
x_km1 <- 6
x_k <- 5

iter <- 1

repeat{
  x <- x_km1
  g_km1 <- eval(g)
  x <- x_k
  g_k <- eval(g)
  x_k1 <- x_k - (x_k - x_km1)/(g_k - g_km1)*g_k
  if(abs(x_k1 - x_k) < tol){
    break
  }
  x_km1 <- x_k
  x_k <- x_k1
  iter <- iter + 1
}

x2 <- x_k1

# Third root
x_km1 <- -6
x_k <- -5

iter <- 1

repeat{
  x <- x_km1
  g_km1 <- eval(g)
  x <- x_k
  g_k <- eval(g)
  x_k1 <- x_k - (x_k - x_km1)/(g_k - g_km1)*g_k
  if(abs(x_k1 - x_k) < tol){
    break
  }
  x_km1 <- x_k
  x_k <- x_k1
  iter <- iter + 1
}

x3 <- x_k1

cat(paste("The roots of the equation are", round(x1,1), ",", round(x2, 1),
          ",", "and", round(x3,1) , "."))
