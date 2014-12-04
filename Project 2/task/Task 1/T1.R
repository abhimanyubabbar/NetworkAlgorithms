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
classify <- function(component, meanValue){
  
  # Create the vector to store the values.
  x <- vector(mode="character", length=0)
  
  # Iterate over the values and perform comparison.
  for(entry in component){
    if(entry >= meanValue){
      x <- c(x,'high')
    }
    else{
      x <- c(x,'low')
    }
  }
  # return this character vector.
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


##### ================  Bar Plotting.

## Calculate the occurences of classification categories.

# DispFrames
DispFrames.high.count <- sum(flashcrowd.y.DispFrames.classify == 'high')
DispFrames.low.count <- sum(flashcrowd.y.DispFrames.classify == 'low')

## barplot.
#barplot(c(DispFrames.high.count,DispFrames.low.count), ylab="Frequency" , ylim=c(0,length(flashcrowd.y.DispFrames.classify)), col=c("darkblue","red"), names.arg =c("high","low"), main="DispFrames Classification")


# NoAudioPlayed
NoAudioPlayed.high.count <- sum(flashcrowd.y.NoAudioPlayed.classify == 'high')
NoAudioPlayed.low.count <- sum(flashcrowd.y.NoAudioPlayed.classify == 'low')

## barplot
#barplot(c(NoAudioPlayed.high.count,NoAudioPlayed.low.count), ylab="Frequency" , ylim=c(0,length(flashcrowd.y.NoAudioPlayed.classify)), col=c("darkblue","red"), names.arg =c("high","low"), main="NoAudioPlayed Classification")


# NoRTPPkts
NoRTPPkts.high.count <- sum(flashcrowd.y.NoRTPPkts.classify == 'high')
NoRTPPkts.low.count <- sum(flashcrowd.y.NoRTPPkts.classify == 'low')

## barplot
#barplot(c(NoRTPPkts.high.count, NoRTPPkts.low.count), ylab="Frequency" , ylim=c(0,length(flashcrowd.y.NoRTPPkts.classify)), col=c("darkblue","red"), names.arg =c("high","low"), main="NoRTPPkts Classification")


# Construct a matrix from the values.
classification.matrix <- matrix( c(DispFrames.high.count, DispFrames.low.count, NoAudioPlayed.high.count, NoAudioPlayed.low.count, NoRTPPkts.high.count, NoRTPPkts.low.count), nrow=2, ncol=3)

# Set the rownames and col names for easy identification.
rownames(classification.matrix) <- c("high","low")
colnames(classification.matrix) <- c("DispFrames","NoAudioPlayed","NoRTPPkts")

# Bar Plot the Graph.
png(file="bar_plot.png",width=500, height=400)
barplot(classification.matrix, beside=TRUE, col=c("darkblue","red"), ylab="Frequency" , ylim=c(0,length(flashcrowd.y.NoAudioPlayed.classify)), names.arg =c("Video Frame Rate","Audio Buffer Rate","RTP Packet Rate"), main="Classification Y Metric", density = c(-1,20,-1,20,-1,20))
legend("topright", 
       legend = c("High", "Low"), 
       fill = c("Blue", "Red"),
       density = c(-1, 20))
dev.off()

##### ========================  Box Plot.

# Step1: Concatenate the whole column to the predictor data set.
flashcrowd.predictor.DispframesClassification.combined <- as.data.frame(c(flashcrowd.x,list("DispFrames.classify"=flashcrowd.y.DispFrames.classify)))

flashcrowd.predictor.DispframesClassification.combined.low <- flashcrowd.predictor.DispframesClassification.combined[ which( ! flashcrowd.predictor.DispframesClassification.combined$DispFrames.classify %in% "high") , ]
flashcrowd.predictor.DispframesClassification.combined.low$DispFrames.classify = factor(flashcrowd.predictor.DispframesClassification.combined.low$DispFrames.classify)

# Step2: BoxPlot the data
png(file="box_plot.png",width=500, height=400)
boxplot(
  formula = all_..idle ~ DispFrames.classify,
  data = flashcrowd.predictor.DispframesClassification.combined.low,
  boxwex  = 0.15,
  col     = "yellow",
  at = 1:1,
  main="X metrics for Low Video Frame Rate"
)

boxplot(
  formula = X..memused ~ DispFrames.classify,
  data = flashcrowd.predictor.DispframesClassification.combined.low,
  boxwex  = 0.15,
  add = TRUE,
  col ="orange",
  at = 1:1 - 0.4
)

boxplot(
  formula = X..swpused ~ DispFrames.classify,
  data = flashcrowd.predictor.DispframesClassification.combined.low,
  boxwex  = 0.15,
  add = TRUE,
  col ="navyblue",
  at = 1:1 + 0.4
)

axis(1, at=c(1:1, 1:1 - 0.4,1:1 + 0.4),labels=c("Idle CPU", "Memory Used", "Swap Used"), col.axis="black")
dev.off()


