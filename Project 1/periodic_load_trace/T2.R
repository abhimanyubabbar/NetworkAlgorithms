# Project 1 Task 2


# Read Data into X
periodic.x = read.csv("X.csv")
# Read Data into Y.
periodic.y = read.csv("Y.csv")

# Set the working directory.
setwd("~/workspace/EP2400/Project 1/periodic_load_trace")


# Instead of choosing a sequential set, create a random set. (Replace = False.)
splitdata <- function(length,seed=NULL) {
  if(!is.null(seed)) {
    set.seed(seed)
  }
  index <- 1:length
  trainIndex <- sample(index, (0.7*length(index)) , replace=FALSE)
}

# Use any data to get a random training index. SEED = 29
trainIndex = splitdata(nrow(periodic.x),29)

# Create the training sets.
periodic.training.x = periodic.x[trainIndex,]
periodic.training.y = periodic.y[trainIndex,]

# Create the test sets
periodic.test.x = periodic.x[-trainIndex,]
periodic.test.y = periodic.y[-trainIndex,]


################## Regression Method = Least Square Regression Method.


# Create a copy holder object.
combinedTrainingMatrix <- periodic.training.x

# Append the response data columns in the predictor data set.

## Add DispFrames. 
combinedTrainingMatrix <- cbind(combinedTrainingMatrix, DispFrames = periodic.training.y$DispFrames)

## Add NoAudioPlayed
combinedTrainingMatrix <- cbind(combinedTrainingMatrix, NoAudioPlayed = periodic.training.y$NoAudioPlayed)

## Add NoRTPPkts
combinedTrainingMatrix <- cbind(combinedTrainingMatrix, NoRTPPkts = periodic.training.y$NoRTPPkts)


# Now perform the main linear regression tests.

## Regression For DispFrames.
lm.fit.DispFrames = lm(DispFrames~.-NoAudioPlayed, combinedTrainingMatrix)
lm.fit.DispFrames <- update(lm.fit.DispFrames, ~.-NoRTPPkts)
summary(lm.fit.DispFrames)

## Regression For NoAudioPlayed
lm.fit.NoAudioPlayed = lm(NoAudioPlayed~.-DispFrames, combinedTrainingMatrix)
lm.fit.NoAudioPlayed <- update(lm.fit.NoAudioPlayed, ~.-NoRTPPkts)
summary(lm.fit.NoAudioPlayed)


## Regression for NoRTPPkts
lm.fit.NoRTPPkts = lm(NoRTPPkts~.-NoAudioPlayed, combinedTrainingMatrix)
lm.fit.NoRTPPkts <- update(lm.fit.NoRTPPkts, ~.-DispFrames)
summary(lm.fit.NoRTPPkts)



################ Plotting Time Series Graph



















