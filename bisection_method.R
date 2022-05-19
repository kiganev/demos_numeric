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

# Example taken from Chong and Zak (2013), p. 116
# Find the minimizer and the minimum value of the function:
f <- expression(x^4 - 14*x^3 + 60*x^2 - 70*x)

# First derivative
df <- D(f, 'x')

# Domain interval endpoints
lbound <- 0
ubound <- 2

# Tolerance level
tol <- 10e-10

while(ubound - lbound > tol){
  x <- lbound + (ubound - lbound)/2
  if(eval(df) > 0){
    lbound <- lbound
    ubound <- ubound - (ubound - lbound)/2
  } else if(eval(df) < 0){
    lbound <-  lbound + (ubound - lbound)/2
    ubound <- ubound
  }
}

# Print the minimizer and the minimum function value
x <- round(ubound, 4)
cat(paste("The value of x minimizing f(x) equals", x, "."))
cat(paste("The minimum of f(x) in the interval [0,2] equals"), eval(f), ".")
