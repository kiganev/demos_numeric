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
fun1 <- function(x){
  y <- x^4 - 14*x^3 + 60*x^2 - 70*x
  return(y)
}

# This here is the first derivative
fun2 <- function(x){
  dy <- 4*x^3 - 42*x^2 + 120*x - 70
  return(dy)
}

# Domain interval endpoints
lbound <- 0
ubound <- 2

# Tolerance level
tol <- 10e-10

while(ubound - lbound > tol){
  if(fun2(lbound + (ubound - lbound)/2) > 0){
    lbound <- lbound
    ubound <- ubound - (ubound - lbound)/2
  } else if(fun2(lbound + (ubound - lbound)/2) < 0){
    lbound <-  lbound + (ubound - lbound)/2
    ubound <- ubound
  }
}

# Print the minimizer and the minimum function value
round(ubound, 4)
fun1(ubound)
