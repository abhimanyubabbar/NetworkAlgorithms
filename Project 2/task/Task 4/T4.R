
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



## CLASSIFICATION FUNCTION ======== 

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


## MEAN CALCULATION ========

flashcrowd.y.mean.DispFrames <- mean(flashcrowd.y[["DispFrames"]])
flashcrowd.y.mean.NoAudioPlayed <- mean(flashcrowd.y[["NoAudioPlayed"]])
flashcrowd.y.mean.NoRTPPkts <- mean(flashcrowd.y[["NoRTPPkts"]])


## EXECUTION ======== 

# DispFrames classification vector.
flashcrowd.y.DispFrames.classify <- classify(flashcrowd.y[["DispFrames"]],flashcrowd.y.mean.DispFrames)

# NoAudioPlayed classification vector.
flashcrowd.y.NoAudioPlayed.classify <- classify(flashcrowd.y[["NoAudioPlayed"]],flashcrowd.y.mean.NoAudioPlayed)

# NoRTPPkts classification vector.
flashcrowd.y.NoRTPPkts.classify <- classify(flashcrowd.y[["NoRTPPkts"]],flashcrowd.y.mean.NoRTPPkts)

# combined list.
flashcrowd.y.classification.list <- list("DispFrames.classify"=flashcrowd.y.DispFrames.classify, "NoAudioPlayed.classify"=flashcrowd.y.NoAudioPlayed.classify, "NoRTPPkts.classify"=flashcrowd.y.NoRTPPkts.classify)

# combined list 2.
flashcrowd.combined <- cbind(flashcrowd.x,flashcrowd.y.classification.list)



## SPLIT DATA FUNCTION ======== 

splitdata <- function(length,seed=NULL) {
  if(!is.null(seed)) {
    set.seed(seed)
  }
  index <- 1:length
  train.index.return <- sample(index, (0.7*length(index)) , replace=FALSE)
}

# Use any data to get a random training index. SEED = 29
train.index <- splitdata(nrow(flashcrowd.x),29)


### Divide the datasets in the training and the test sets.

## TRAINING SET ======= .
flashcrowd.training.x = flashcrowd.x[train.index,]
flashcrowd.training.combined = flashcrowd.combined[train.index,]

## TEST SET ======== .
flashcrowd.test.x = flashcrowd.x[-train.index,]
flashcrowd.test.combined = flashcrowd.combined[-train.index,]


#########  ===== SUBSET SELECTION METHODS ====== ########

## PACKAGE INSTALLATION =====

#install.packages("bestglm")
library(leaps)
library(bestglm)

# STEP 1: Ready the data, where the last row should be response.

DispFrames.bestglm.data <- subset(flashcrowd.training.combined, select = -c(NoAudioPlayed.classify,NoRTPPkts.classify))
DispFrames.bestglm.fit <- bestglm(as.data.frame(DispFrames.bestglm.data), family=binomial,IC="AIC",nvmax=5)

DispFrames.bestglm.fit$BestModels

# STEP 2: Predict the glm data.

DispFrames.glm.fit = glm(DispFrames.classify ~ all_..idle + sum_intr.s + ldavg.1 + tcpsck, 
                         data =flashcrowd.training.combined, 
                         family=binomial)


DispFrames.glm.predict <- predict(DispFrames.glm.fit, 
                                  type="response",
                                  newdata=flashcrowd.test.combined)

DispFrames.glm.predict <- as.data.frame(DispFrames.glm.predict)

# Predictor Vector for DispFrames
DispFrames.predicted.vector <- DispFrames.glm.predict$DispFrames.glm.predict
DispFrames.predicted.vector <- replace(DispFrames.predicted.vector, DispFrames.predicted.vector >= 0.5 , "high")
DispFrames.predicted.vector <- replace(DispFrames.predicted.vector, DispFrames.predicted.vector !=  'high' , "low")
#DispFrames.predicted.vector <- replace(DispFrames.predicted.vector, DispFrames.predicted.vector <  0.5 , "low")

# Actual Vector of DispFrames
DispFrames.actual.vector <- flashcrowd.test.combined$DispFrames.classify
DispFrames.actual.vector <- replace(DispFrames.actual.vector, DispFrames.actual.vector == 1 , "high")
DispFrames.actual.vector <- replace(DispFrames.actual.vector, DispFrames.actual.vector == 0 , "low")

# Creating the confusion matrix for DispFrames.
confusion.matix.disp.frames = table(DispFrames.actual.vector,DispFrames.predicted.vector)

# Error rate for DispFrames
error.rate.disp.frames = sum(confusion.matix.disp.frames[c(2, 3)])/sum(confusion.matix.disp.frames)*100 #6.8%



# NoAudioPlayed.classify

# STEP 1: Check for the best predictors.
NoAudioPlayed.bestglm.data <- subset(flashcrowd.training.combined, select = -c(DispFrames.classify,NoRTPPkts.classify))
NoAudioPlayed.bestglm.fit <- bestglm(as.data.frame(NoAudioPlayed.bestglm.data), family=binomial,IC="BIC",nvmax=4)

NoAudioPlayed.bestglm.fit$BestModel

# STEP 2: Perform the logistic regression with the variables that we calculated earlier.
NoAudioPlayed.glm.fit = glm(NoAudioPlayed.classify ~ cswch.s + file.nr + ldavg.1 + tcpsck, 
                            data =flashcrowd.training.combined, 
                            family=binomial)

# STEP 3: Fit for NoAudioPlayed
NoAudioPlayed.glm.predict <- predict(NoAudioPlayed.glm.fit, 
                                     type="response",
                                     newdata=flashcrowd.test.combined)

NoAudioPlayed.glm.predict <- as.data.frame(NoAudioPlayed.glm.predict)

# STEP 4: Cleaning and converting the data.

# Predictor Vector for NoAudioPlayed
NoAudioPlayed.predicted.vector <- NoAudioPlayed.glm.predict$NoAudioPlayed.glm.predict
NoAudioPlayed.predicted.vector <- replace(NoAudioPlayed.predicted.vector, NoAudioPlayed.predicted.vector >= 0.5 , "high")
NoAudioPlayed.predicted.vector <- replace(NoAudioPlayed.predicted.vector, NoAudioPlayed.predicted.vector <  0.5 , "low")

# Actual Vector for NoAudioPlayed
NoAudioPlayed.actual.vector <- flashcrowd.test.combined$NoAudioPlayed.classify
NoAudioPlayed.actual.vector <- replace(NoAudioPlayed.actual.vector, NoAudioPlayed.actual.vector == 1 , "high")
NoAudioPlayed.actual.vector <- replace(NoAudioPlayed.actual.vector, NoAudioPlayed.actual.vector == 0 , "low")

# STEP 5: Creating the confusion matrix for NoAudioPlayed
confusion.matix.no.audio.played = table(NoAudioPlayed.actual.vector,NoAudioPlayed.predicted.vector)

# STEP 6: Error rate for NoAudioPlayed
error.rate.no.audio.played = sum(confusion.matix.no.audio.played[c(2, 3)])/sum(confusion.matix.no.audio.played)*100 #18.1%


# NoRTPPkts.classify
# STEP 1: Check for the best predictors.

NoRTPPkts.bestglm.data <- subset(flashcrowd.training.combined, select = -c(NoAudioPlayed.classify,DispFrames.classify))
NoRTPPkts.bestglm.fit <- bestglm(as.data.frame(NoRTPPkts.bestglm.data), family=binomial,IC="BIC", nvmax=4)

NoRTPPkts.bestglm.fit$BestModels

# STEP 2: Perform the logistic regression with the variables that we calculated earlier.
NoRTPPkts.glm.fit = glm(NoRTPPkts.classify ~all_..idle + proc.s + file.nr + tcpsck, 
                        data =flashcrowd.training.combined, 
                        family=binomial)

# Best According to me : all_..idle + proc.s + file.nr + tcpsck.

# STEP 3: Prediction of the values.
NoRTPPkts.glm.predict <- predict(NoRTPPkts.glm.fit, 
                                 type="response",
                                 newdata=flashcrowd.test.combined)

NoRTPPkts.glm.predict <- as.data.frame(NoRTPPkts.glm.predict)

# STEP 4: Cleaning and converting the data.

# Predictor Vector for NoRTPPkts
NoRTPPkts.predicted.vector <- NoRTPPkts.glm.predict$NoRTPPkts.glm.predict
NoRTPPkts.predicted.vector <- replace(NoRTPPkts.predicted.vector, NoRTPPkts.predicted.vector >= 0.5 , "high")
NoRTPPkts.predicted.vector <- replace(NoRTPPkts.predicted.vector, NoRTPPkts.predicted.vector <  0.5 , "low")

# Actual Vector for NoRTPPkts
NoRTPPkts.actual.vector <- flashcrowd.test.combined$NoRTPPkts.classify
NoRTPPkts.actual.vector <- replace(NoRTPPkts.actual.vector, NoRTPPkts.actual.vector == 1 , "high")
NoRTPPkts.actual.vector <- replace(NoRTPPkts.actual.vector, NoRTPPkts.actual.vector == 0 , "low")

# STEP 5: Creating the confusion matrix for NoRTPPkts
confusion.matix.no.rttp.pkts = table(NoRTPPkts.actual.vector, NoRTPPkts.predicted.vector)

# STEP 6: Error rate for NoRTPPkts
error.rate.no.rttp.pkts = sum(confusion.matix.no.rttp.pkts[c(2, 3)])/sum(confusion.matix.no.rttp.pkts)*100 # 19.4%
