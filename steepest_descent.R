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
                y, "- alpha *", grad[2], "- 3 ) ^ 2 + (",
                z, "- alpha *", grad[3], "+ 5 ) ^ 4" )

f2 <- str2expression(argfun)

# First derivative
df2 <- D(f2, "alpha")

# Second derivative 
d2f2 <- D(df2, "alpha")
