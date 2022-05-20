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

# Chong and Zak (2013), Example 8.1, p. 136
# Find the minimizer of the function
f <- expression(4*(x - 4)^4 + (y - 3)^2 + 4*(z + 5)^4)

# Partial derivatives
pd1 <- D(f, "x")
pd2 <- D(f, "y")
pd3 <- D(f, "z")

# Starting point
vec <- c(4,2,-1)

# Evaluate gradient
x <- vec[1]
y <- vec[2]
z <- vec[3]

grad <- c(eval(pd1), eval(pd2), eval(pd3))

# Create function of alpha
argfun <- paste("(", x, "- alpha *", grad[1], "- 4 ) ^ 4 + (", 
                y, "- alpha *", grad[2], "- 3 ) ^ 2 + 4*(",
                z, "- alpha *", grad[3], "+ 5 ) ^ 4" )

f2 <- str2expression(argfun)

alpha <- seq(0, 0.01, by = 0.0001)
data1 <- as.data.frame(cbind(alpha, eval(f2))) %>% 
  rename(f_alpha = V2)

# Make function plot
ggplot(data1, aes(x = alpha)) + 
  geom_line(aes(y = f_alpha), col = "red") + 
  ylab("f(alpha)")

# First derivative
df2 <- D(f2, "alpha")

# Second derivative 
d2f2 <- D(df2, "alpha")

# Use Newton's method to find the alpha minimizer
tol <- 1e-5

alpha_k <- 0.075

iter <- 1

repeat{
  alpha <- alpha_k
  alpha_k1 <- alpha - eval(df2)/eval(d2f2)
  if(abs(alpha_k1 - alpha_k) < tol){
    break
  }
  alpha_k <- alpha_k1
  iter <- iter + 1
}

alpha <- alpha_k1

vec <- vec - alpha * eval(grad)
