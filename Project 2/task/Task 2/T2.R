# Get the current Directory.
getwd()

# Load the predictors flash crowd data.
flashcrowd.x <- read.csv("X.csv")

# Checking components of the list.
# flashcrowd.x[[1]] == Gives 10,000 values of the first column.
# length(flashcrowd.x) == Gives the length of the list.

# Load the metrics for the flash crowd data.
flashcrowd.y <- read.csv("Y.csv")

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

## Start Filtering the Data Now.

# DispFrames classification vector.
flashcrowd.y.DispFrames.classify <- classify(flashcrowd.y[["DispFrames"]],flashcrowd.y.mean.DispFrames)

# NoAudioPlayed classification vector.
flashcrowd.y.NoAudioPlayed.classify <- classify(flashcrowd.y[["NoAudioPlayed"]],flashcrowd.y.mean.NoAudioPlayed)

# NoRTPPkts classification vector.
flashcrowd.y.NoRTPPkts.classify <- classify(flashcrowd.y[["NoRTPPkts"]],flashcrowd.y.mean.NoRTPPkts)

# Create a classification list.
flashcrowd.y.classification.list <- list("DispFrames.classify"=flashcrowd.y.DispFrames.classify, "NoAudioPlayed.classify"=flashcrowd.y.NoAudioPlayed.classify, "NoRTPPkts.classify"=flashcrowd.y.NoRTPPkts.classify)



#### ==================== LOGISTIC REGRESSION.

## STEP 1: Append the classification list to the flashcrowd matrix x.

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


## STEP 3: Perform the logistic regression upon the variables.

# Use generalized linear model (glm) to calculate the coefficients based on the maximal likelihood function.

flashcrowd.training.combined <- as.data.frame(flashcrowd.training.combined)

# DispFrames.classify
DispFrames.glm.fit = glm(DispFrames.classify ~ all_..idle + X..memused + X..swpused + 
                         proc.s +	cswch.s + file.nr + sum_intr.s + 
                         rtps + ldavg.1 + tcpsck, 
                      data =flashcrowd.training.combined, 
                      family=binomial)

#summary(DispFrames.glm.fit)


# NoAudioPlayed.classify
NoAudioPlayed.glm.fit = glm(NoAudioPlayed.classify ~ all_..idle + X..memused + X..swpused + 
                            proc.s +  cswch.s + file.nr + sum_intr.s + 
                            rtps + ldavg.1 + tcpsck, 
                         data =flashcrowd.training.combined, 
                         family=binomial)

#summary(NoAudioPlayed.glm.fit)


# NoRTPPkts.classify
NoRTPPkts.glm.fit = glm(NoRTPPkts.classify ~ all_..idle + X..memused + X..swpused + 
                        proc.s +  cswch.s + file.nr + sum_intr.s + 
                        rtps + ldavg.1 + tcpsck, 
                      data =flashcrowd.training.combined, 
                      family=binomial)

#summary(NoRTPPkts.glm.fit)


## Now make predictions using the test set.


## DispFrames
DispFrames.glm.predict <- predict(DispFrames.glm.fit, 
                              type="response",
                              newdata=flashcrowd.test.combined)

DispFrames.glm.predict <- as.data.frame(DispFrames.glm.predict)

# Cleaning and converting the data

# Predictor Vector.
DispFrames.predicted.vector <- DispFrames.glm.predict$DispFrames.glm.predict
DispFrames.predicted.vector <- replace(DispFrames.predicted.vector, DispFrames.predicted.vector >= 0.5 , "high")
DispFrames.predicted.vector <- replace(DispFrames.predicted.vector, DispFrames.predicted.vector <  0.5 , "low")

# Actual Vector.
DispFrames.actual.vector <- flashcrowd.test.combined$DispFrames.classify
DispFrames.actual.vector <- replace(DispFrames.actual.vector, DispFrames.actual.vector == 1 , "high")
DispFrames.actual.vector <- replace(DispFrames.actual.vector, DispFrames.actual.vector == 0 , "low")


# Creating the confusion matrix.
table(DispFrames.predicted.vector,DispFrames.actual.vector)


## NoAudioPlayed
NoAudioPlayed.glm.predict <- predict(NoAudioPlayed.glm.fit, 
                                  type="response",
                                  newdata=flashcrowd.test.combined)

NoAudioPlayed.glm.predict <- as.data.frame(NoAudioPlayed.glm.predict)

# Cleaning and converting the data.

# Predictor Vector.
NoAudioPlayed.predicted.vector <- NoAudioPlayed.glm.predict$NoAudioPlayed.glm.predict
NoAudioPlayed.predicted.vector <- replace(NoAudioPlayed.predicted.vector, NoAudioPlayed.predicted.vector >= 0.5 , "high")
NoAudioPlayed.predicted.vector <- replace(NoAudioPlayed.predicted.vector, NoAudioPlayed.predicted.vector <  0.5 , "low")

# Actual Vector.
NoAudioPlayed.actual.vector <- flashcrowd.test.combined$NoAudioPlayed.classify
NoAudioPlayed.actual.vector <- replace(NoAudioPlayed.actual.vector, NoAudioPlayed.actual.vector == 1 , "high")
NoAudioPlayed.actual.vector <- replace(NoAudioPlayed.actual.vector, NoAudioPlayed.actual.vector == 0 , "low")

# Creating the confusion matrix.
table(NoAudioPlayed.predicted.vector,NoAudioPlayed.actual.vector)


# NoRTPPkts
NoRTPPkts.glm.predict <- predict(NoRTPPkts.glm.fit, 
                                     type="response",
                                     newdata=flashcrowd.test.combined)

NoRTPPkts.glm.predict <- as.data.frame(NoRTPPkts.glm.predict)

# Cleaning and converting the data.

# Predictor Vector.
NoRTPPkts.predicted.vector <- NoRTPPkts.glm.predict$NoRTPPkts.glm.predict
NoRTPPkts.predicted.vector <- replace(NoRTPPkts.predicted.vector, NoRTPPkts.predicted.vector >= 0.5 , "high")
NoRTPPkts.predicted.vector <- replace(NoRTPPkts.predicted.vector, NoRTPPkts.predicted.vector <  0.5 , "low")

# Actual Vector.
NoRTPPkts.actual.vector <- flashcrowd.test.combined$NoRTPPkts.classify
NoRTPPkts.actual.vector <- replace(NoRTPPkts.actual.vector, NoRTPPkts.actual.vector == 1 , "high")
NoRTPPkts.actual.vector <- replace(NoRTPPkts.actual.vector, NoRTPPkts.actual.vector == 0 , "low")

# Creating the confusion matrix.
table(NoRTPPkts.predicted.vector,NoRTPPkts.actual.vector)



##### ==================================== END ====================================