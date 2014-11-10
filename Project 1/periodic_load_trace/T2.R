# Project 1, Task 2.

# Set the working directory.
setwd("~/workspace/EP2400/Project 1/periodic_load_trace")

# Read the Predictors.
periodic.x = read.csv("X.csv")

# Read the observed response data.
periodic.y = read.csv("Y.csv")

# Create the training sets.
periodic.training.x = periodic.x[1:7000,]
periodic.training.y = periodic.y[1:7000,]

# Create the test sets.
periodic.test.x = periodic.x[7001:10000,]
periodic.test.y = periodic.y[7001:10000,]


# SubPart1 (Time Series Plot (Measurements vs Model Predictions))
# Data Set - Test Set

# Calculate the coefficients of the Linear Regression Method using the Training Set Data.





## Regression Method: (" Least Square Regression Method ")
## Y = DispFrames






