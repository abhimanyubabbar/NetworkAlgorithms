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





##### ==================================== END ====================================