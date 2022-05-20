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
