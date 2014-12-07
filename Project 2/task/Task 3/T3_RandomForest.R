
# Reading the files.
args<-commandArgs(TRUE)
x.path = "X.csv"
y.path = "Y.csv"

if (length(args) >= 2) {
  x.path = args[1]
  y.path = args[2]
}

# Load the predictors flash crowd data.
stopifnot(file.exists(x.path)) # check if x data file exists
flashcrowd.x <- read.csv(x.path)

# Load the metrics for the flash crowd data.
stopifnot(file.exists(y.path)) # check if y data file exists
flashcrowd.y <- read.csv(y.path)


## ====================================================================================

# Part 1: Classification of the metrics as "low" or "high" based on the mean of the values.

# Calculating the mean of the Y Metrics.
flashcrowd.y.mean.DispFrames <- mean(flashcrowd.y[["DispFrames"]])
flashcrowd.y.mean.NoAudioPlayed <- mean(flashcrowd.y[["NoAudioPlayed"]])
flashcrowd.y.mean.NoRTPPkts <- mean(flashcrowd.y[["NoRTPPkts"]])

# Function to calculate the value as high or low for each of the Y metric. 
# Replace "high" with "1" and "low" with "0".
classify <- function(component, meanValue){
  
  # Create the vector to store the values.
  x <- vector(mode="numeric", length=0)
  
  # Iterate over the values and perform comparison.
  for(entry in component){
    if(entry >= meanValue){
      x <- c(x,1)
    }
    else{
      x <- c(x,0)
    }
  }
  # return the numeric vector.
  x
}

### Start with filtering of data.

# DispFrames classification vector.
flashcrowd.y.DispFrames.classify <- classify(flashcrowd.y[["DispFrames"]],flashcrowd.y.mean.DispFrames)

# NoAudioPlayed classification vector.
flashcrowd.y.NoAudioPlayed.classify <- classify(flashcrowd.y[["NoAudioPlayed"]],flashcrowd.y.mean.NoAudioPlayed)

# NoRTPPkts classification vector.
flashcrowd.y.NoRTPPkts.classify <- classify(flashcrowd.y[["NoRTPPkts"]],flashcrowd.y.mean.NoRTPPkts)

# Create a classification list.
flashcrowd.y.classification.list <- list("DispFrames.classify"=flashcrowd.y.DispFrames.classify, "NoAudioPlayed.classify"=flashcrowd.y.NoAudioPlayed.classify, "NoRTPPkts.classify"=flashcrowd.y.NoRTPPkts.classify)


## ============================================================

flashcrowd.combined <- cbind(flashcrowd.x,flashcrowd.y.classification.list)

## STEP 2: Divide the set into the training set and the test set.

# Instead of choosing a sequential set, create a random set. (Replace = False.)

splitdata <- function(length,seed=NULL) {
  if(!is.null(seed)) {
    set.seed(seed)
  }
  index <- 1:length
  train.index.return <- sample(index, (0.7*length(index)) , replace=FALSE)
}

# Use any data to get a random training index. SEED = 29
train.index <- splitdata(nrow(flashcrowd.combined),29)

# Create the training set.
flashcrowd.training.combined <- flashcrowd.combined[train.index,]
  
# Create the test set.
flashcrowd.test.combined <- flashcrowd.combined[-train.index,]
flashcrowd.test.y = flashcrowd.y[-train.index,]

# typeof(flashcrowd.combined)
# flashcrowd.test.combined <- subset(flashcrowd.test.combined, select = -c(NoAudioPlayed.classify,NoRTPPkts.classify))
# subset(df, select = c(a,c))

# We now have the training and test datasets with us.

# RANDOM FOREST APPROACH.

# Install Package if not present.
# install.packages("randomForest")

library(randomForest)
set.seed(1)

# Set the class of the y metrics as factors.
flashcrowd.training.combined$DispFrames.classify <- factor(flashcrowd.training.combined$DispFrames.classify)
flashcrowd.training.combined$NoAudioPlayed.classify <- factor(flashcrowd.training.combined$NoAudioPlayed.classify)
flashcrowd.training.combined$NoRTPPkts.classify <- factor(flashcrowd.training.combined$NoRTPPkts.classify  )

# Check to test the class of the variable.
# class(flashcrowd.training.combined$DispFrames.classify)

# Start the fitting of data.
# DispFrames.
DispFrames.rf <- randomForest(
    as.factor(DispFrames.classify) ~ .,
    data = subset(flashcrowd.training.combined, select = -c(NoAudioPlayed.classify,NoRTPPkts.classify)),
    mtry = 3,     # The mtry component in case of classification is best for root(predictor_size)
    importance = TRUE
)


# Now predict the values.
DispFrames.prediction = predict(DispFrames.rf,
                                newdata = flashcrowd.test.combined)


# Confusion Matrix + Error Rate.
confusion.matix.disp.frames <- table(flashcrowd.test.combined$DispFrames.classify,DispFrames.prediction)
error.rate.disp.frames = sum(confusion.matix.disp.frames[c(2, 3)])/sum(confusion.matix.disp.frames)*100 

# Comparison
# Original <- 6.8 %
# Random Forest <- 5.33 %


# ======================== 

# NoAudioPlayed
NoAudioPlayed.rf <- randomForest(
  as.factor(NoAudioPlayed.classify) ~ .,
  data = subset(flashcrowd.training.combined, select = -c(DispFrames.classify,NoRTPPkts.classify)),
  mtry = 3,     # The mtry component in case of classification is best for root(predictor_size)
  importance = TRUE
)


# Now predict the values.
NoAudioPlayed.prediction = predict(NoAudioPlayed.rf,
                                newdata = flashcrowd.test.combined)

# Confusion Matrix + Error Rate.
confusion.matix.no.audio.played <- table(flashcrowd.test.combined$NoAudioPlayed.classify,NoAudioPlayed.prediction)
error.rate.no.audio.played = sum(confusion.matix.no.audio.played[c(2, 3)])/sum(confusion.matix.no.audio.played)*100 #18.1%


# Comparison
# Original <- 18.1 %
# Random Forest <- 1.06 %


# =============================

# NoRTPPkts

NoRTPPkts.rf <- randomForest(
  as.factor(NoRTPPkts.classify) ~ .,
  data = subset(flashcrowd.training.combined, select = -c(DispFrames.classify,NoAudioPlayed.classify)),
  mtry = 3,     # The mtry component in case of classification is best for root(predictor_size)
  importance = TRUE
)


# Now predict the values.
NoRTPPkts.prediction = predict(NoRTPPkts.rf,
                                   newdata = flashcrowd.test.combined)

# Confusion Matrix + Error Rate.
confusion.matix.no.rttp.pkts <- table(flashcrowd.test.combined$NoRTPPkts.classify,NoRTPPkts.prediction)
error.rate.no.rttp.pkts = sum(confusion.matix.no.rttp.pkts[c(2, 3)])/sum(confusion.matix.no.rttp.pkts)*100 


# Comparison
# Original <- 19.4 %
# Random Forest <- 7.73 %


# =================================================






