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
flashcrowd.test.y = flashcrowd.y[-train.index,]

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


## Fit for DispFrames
DispFrames.glm.predict <- predict(DispFrames.glm.fit, 
                              type="response",
                              newdata=flashcrowd.test.combined)

DispFrames.glm.predict <- as.data.frame(DispFrames.glm.predict)

# Cleaning and converting the data

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

# Time series plot that shows the measurements for samples in the test set for DispFrames
pch.values.dispframes = ifelse(DispFrames.predicted.vector == DispFrames.actual.vector, 1, 0)
col.values.dispframes = ifelse(DispFrames.predicted.vector == DispFrames.actual.vector, "blue", "red")

png(file="timeseries_classification_dispframes.png",width=500, height=400)
plot(DispFrames~c(1:nrow(flashcrowd.test.y)), 
     data=flashcrowd.test.y, 
     pch = pch.values.dispframes, col=col.values.dispframes,
     ylab="Video Frame Rate", xlab="Times",
     main="Video Frame Rate Classification")
abline(h=flashcrowd.y.mean.DispFrames, col="green")
legend("topleft", 
       legend = c("Correctly Classfied", "Incorrectly Classified"), 
       pch=c(1, 0),
       col=c("blue", "red"))
dev.off()

#Execution time for fitting DispFrames
exec.time.fit.dispframes = system.time(glm(DispFrames.classify ~ all_..idle + X..memused + X..swpused + 
                                             proc.s +  cswch.s + file.nr + sum_intr.s + 
                                             rtps + ldavg.1 + tcpsck, 
                                           data =flashcrowd.training.combined, 
                                           family=binomial)) # 0.070 sec

## Fit for NoAudioPlayed
NoAudioPlayed.glm.predict <- predict(NoAudioPlayed.glm.fit, 
                                  type="response",
                                  newdata=flashcrowd.test.combined)

NoAudioPlayed.glm.predict <- as.data.frame(NoAudioPlayed.glm.predict)

# Cleaning and converting the data.

# Predictor Vector for NoAudioPlayed
NoAudioPlayed.predicted.vector <- NoAudioPlayed.glm.predict$NoAudioPlayed.glm.predict
NoAudioPlayed.predicted.vector <- replace(NoAudioPlayed.predicted.vector, NoAudioPlayed.predicted.vector >= 0.5 , "high")
NoAudioPlayed.predicted.vector <- replace(NoAudioPlayed.predicted.vector, NoAudioPlayed.predicted.vector <  0.5 , "low")

# Actual Vector for NoAudioPlayed
NoAudioPlayed.actual.vector <- flashcrowd.test.combined$NoAudioPlayed.classify
NoAudioPlayed.actual.vector <- replace(NoAudioPlayed.actual.vector, NoAudioPlayed.actual.vector == 1 , "high")
NoAudioPlayed.actual.vector <- replace(NoAudioPlayed.actual.vector, NoAudioPlayed.actual.vector == 0 , "low")

# Creating the confusion matrix for NoAudioPlayed
confusion.matix.no.audio.played = table(NoAudioPlayed.actual.vector,NoAudioPlayed.predicted.vector)

# Error rate for NoAudioPlayed
error.rate.no.audio.played = sum(confusion.matix.no.audio.played[c(2, 3)])/sum(confusion.matix.no.audio.played)*100 #18.1%

# Time series plot that shows the measurements for samples in the test set for NoAudioPlayed
pch.values.no.audio.played = ifelse(NoAudioPlayed.predicted.vector == NoAudioPlayed.actual.vector, 1, 0)
col.values.no.audio.played = ifelse(NoAudioPlayed.predicted.vector == NoAudioPlayed.actual.vector, "blue", "red")

png(file="timeseries_classification_noaudioplayed.png",width=500, height=400)
plot(NoAudioPlayed~c(1:nrow(flashcrowd.test.y)), 
     data=flashcrowd.test.y, 
     pch = pch.values.no.audio.played, col=col.values.no.audio.played,
     ylab="Audio Buffer Rate", xlab="Times",
     main="Audio Buffer Rate Classification")
abline(h=flashcrowd.y.mean.NoAudioPlayed, col="green")
legend("topleft", 
       legend = c("Correctly Classfied", "Incorrectly Classified"), 
       pch=c(1, 0),
       col=c("blue", "red"))
dev.off()

#Execution time for fitting NoAudioPlayed
exec.time.fit.no.audio.played = system.time(glm(NoAudioPlayed.classify ~ all_..idle + X..memused + X..swpused + 
                                             proc.s +  cswch.s + file.nr + sum_intr.s + 
                                             rtps + ldavg.1 + tcpsck, 
                                           data =flashcrowd.training.combined, 
                                           family=binomial)) # 0.099 sec

# Fit for NoRTPPkts
NoRTPPkts.glm.predict <- predict(NoRTPPkts.glm.fit, 
                                     type="response",
                                     newdata=flashcrowd.test.combined)

NoRTPPkts.glm.predict <- as.data.frame(NoRTPPkts.glm.predict)

# Cleaning and converting the data.

# Predictor Vector for NoRTPPkts
NoRTPPkts.predicted.vector <- NoRTPPkts.glm.predict$NoRTPPkts.glm.predict
NoRTPPkts.predicted.vector <- replace(NoRTPPkts.predicted.vector, NoRTPPkts.predicted.vector >= 0.5 , "high")
NoRTPPkts.predicted.vector <- replace(NoRTPPkts.predicted.vector, NoRTPPkts.predicted.vector <  0.5 , "low")

# Actual Vector for NoRTPPkts
NoRTPPkts.actual.vector <- flashcrowd.test.combined$NoRTPPkts.classify
NoRTPPkts.actual.vector <- replace(NoRTPPkts.actual.vector, NoRTPPkts.actual.vector == 1 , "high")
NoRTPPkts.actual.vector <- replace(NoRTPPkts.actual.vector, NoRTPPkts.actual.vector == 0 , "low")

# Creating the confusion matrix for NoRTPPkts
confusion.matix.no.rttp.pkts = table(NoRTPPkts.actual.vector, NoRTPPkts.predicted.vector)

# Error rate for NoRTPPkts
error.rate.no.rttp.pkts = sum(confusion.matix.no.rttp.pkts[c(2, 3)])/sum(confusion.matix.no.rttp.pkts)*100 # 19.4%

# Time series plot that shows the measurements for samples in the test set for NoRTPPkts
pch.values.no.rttp.pkts = ifelse(NoRTPPkts.predicted.vector == NoRTPPkts.actual.vector, 1, 0)
col.values.no.rttp.pkts = ifelse(NoRTPPkts.predicted.vector == NoRTPPkts.actual.vector, "blue", "red")

png(file="timeseries_classification_nortppacket.png",width=500, height=400)
plot(NoRTPPkts~c(1:nrow(flashcrowd.test.y)), 
     data=flashcrowd.test.y, 
     pch = pch.values.no.rttp.pkts, col=col.values.no.rttp.pkts,
     ylab="RTP Packet Rate", xlab="Times",
     main="RTP Packet Rate Classification")
abline(h=flashcrowd.y.mean.NoRTPPkts, col="green")
legend("topleft", 
       legend = c("Correctly Classfied", "Incorrectly Classified"), 
       pch=c(1, 0),
       col=c("blue", "red"))
dev.off()

#Execution time for fitting NoAudioPlayed
exec.time.fit.no.rttp.pkts = system.time(glm(NoRTPPkts.classify ~ all_..idle + X..memused + X..swpused + 
                                               proc.s +  cswch.s + file.nr + sum_intr.s + 
                                               rtps + ldavg.1 + tcpsck, 
                                             data =flashcrowd.training.combined, 
                                             family=binomial)) # 0.074 sec

##### ==================================== END ====================================