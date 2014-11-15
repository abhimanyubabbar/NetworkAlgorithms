
# Read Data into X
periodic.x <- read.csv("X.csv")
# Read Data into Y.
periodic.y <- read.csv("Y.csv")


# Instead of choosing a sequential set, create a random set. (Replace = False.)
splitdata <- function(length,seed=NULL) {
  if(!is.null(seed)) {
    set.seed(seed)
  }
  index <- 1:length
  train.index.return <- sample(index, (0.7*length(index)) , replace=FALSE)
}

# Use any data to get a random training index. SEED = 29
train.index <- splitdata(nrow(periodic.x),29)

# Create the training sets.
periodic.training.x <- periodic.x[train.index,]
periodic.training.y <- periodic.y[train.index,]

# Create the test sets
periodic.test.x <- periodic.x[-train.index,]
periodic.test.y <- periodic.y[-train.index,]


################## Regression Method = Least Square Regression Method.


# Create a copy holder object.
combined.training.matrix <- periodic.training.x

# Append the response data columns in the predictor data set.

## Add DispFrames. 
combined.training.matrix <- cbind(combined.training.matrix, DispFrames = periodic.training.y$DispFrames)

## Add NoAudioPlayed
combined.training.matrix <- cbind(combined.training.matrix, NoAudioPlayed = periodic.training.y$NoAudioPlayed)

## Add NoRTPPkts
combined.training.matrix <- cbind(combined.training.matrix, NoRTPPkts = periodic.training.y$NoRTPPkts)


# Now perform the main linear regression tests.

## Regression For DispFrames.
lm.fit.DispFrames <- lm(DispFrames~.-NoAudioPlayed, combined.training.matrix)
lm.fit.DispFrames <- update(lm.fit.DispFrames, ~. -NoRTPPkts)
summary(lm.fit.DispFrames)

## Regression For NoAudioPlayed
lm.fit.NoAudioPlayed <-  lm(NoAudioPlayed~.-DispFrames, combined.training.matrix)
lm.fit.NoAudioPlayed <- update(lm.fit.NoAudioPlayed, ~. -NoRTPPkts)
summary(lm.fit.NoAudioPlayed)


## Regression for NoRTPPkts
lm.fit.NoRTPPkts <- lm(NoRTPPkts~.-NoAudioPlayed , combined.training.matrix)
lm.fit.NoRTPPkts <- update(lm.fit.NoRTPPkts, ~. -DispFrames)
summary(lm.fit.NoRTPPkts)

# Predict the values:
predicted.DispFrames <- predict(lm.fit.DispFrames,periodic.test.x)
predicted.NoAudioPlayed <- predict(lm.fit.NoAudioPlayed, periodic.test.x)
predicted.NoRTPPkts <- predict(lm.fit.NoRTPPkts, periodic.test.x)


################ Plotting Time Series Graph

# Now Create lot for the time series.

# DispFrames time series plot.,
png(file="dispframes_timeseries.png",width=500, height=400)
plot(predicted.DispFrames, col="blue", xlab="Number of Samples", ylab="Video frame rate (frames/sec)")
points(periodic.test.y[,'DispFrames'],col="red",pch = 4)
legend("topright",
       c("Predicted Frame Rate","Actual Frame Rate"), bty="o", col=c("blue", "red"), 
       horiz=FALSE, cex=1, pch=c(1,4))
dev.off()  

# No Audio Played Time Series Plot.
png(file="audioplayed_timeseries.png",width=500, height=400)
plot(predicted.NoAudioPlayed, col="blue", xlab="Number of Samples", ylab="Audio buffer rate (buffer/sec)")
points(periodic.test.y[,'NoAudioPlayed'],col="red",pch = 4)
legend("topright",
       c("Predicted Buffer Rate","Actual Buffer Rate"), bty="o", col=c("blue", "red"), 
       horiz=FALSE, cex=1, pch=c(1,4))
dev.off()

# NoRTPPkts Time series Plot.
png(file="rtppacket_timeseries.png",width=500, height=400)
plot(predicted.NoRTPPkts, col="blue", xlab="Number of Samples", ylab="RTP packet rate (packets/sec)")
points(periodic.test.y[,'NoRTPPkts'], col="red", pch=4)
legend("topright",
       c("Predicted RTP Rate","Actual RTP Rate"), bty="o", col=c("blue", "red"), 
       horiz=FALSE, cex=1, pch=c(1,4))
dev.off()




############# Plotting Against Each Other.

# Creating combined graphs.
combined.DispFrames <- cbind(predicted = predicted.DispFrames, actual = periodic.test.y[,'DispFrames'])
combined.NoAudioPlayed <- cbind(predicted = predicted.NoAudioPlayed, actual = periodic.test.y[,'NoAudioPlayed'])
combined.NoRTPPkts <- cbind(predicted = predicted.NoRTPPkts, actual = periodic.test.y[,'NoRTPPkts'])

#DispFrames.
png(file="dispframes_comparison.png",width=500, height=400)
plot(combined.DispFrames[,'actual'],combined.DispFrames[,'predicted'], col=c('blue','red'), xlab="Actual Video frame rate (frames/sec)", ylab="Predicted Video frame rate (frames/sec)", pch=c(1,4))
legend("topleft",
       c("Actual Frame Rate","Predicted Frame Rate"), bty="o", col=c("blue", "red"), 
       horiz=FALSE, cex=1, pch=c(1,4))
dev.off()


#NoAudioPlayed
png(file="audioplayed_comparison.png",width=500, height=400)
plot(combined.NoAudioPlayed[,'actual'],combined.NoAudioPlayed[,'predicted'], col=c('blue','red'), xlab="Actual Audio buffer rate (buffer/sec)", ylab="Predicted Audio buffer rate (buffer/sec)", pch=c(1,4))
legend("topright",
       c("Actual Audio Buffer Rate","Predicted Audio Buffer Rate"), bty="o", col=c("blue", "red"), 
       horiz=FALSE, cex=1, pch=c(1,4))
dev.off()

#NoRTPPkts
png(file="rtppacket_comparison.png",width=500, height=400)
plot(combined.NoRTPPkts[,'actual'],combined.NoRTPPkts[,'predicted'], col=c('blue','red'), xlab="Actual RTP packet rate (packet/sec)", ylab="Predicted RTP packet rate (packet/sec)", pch=c(1,4))
legend("bottomright",
       c("Actual RTP packet rate","Predicted RTP packet rate"), bty="o", col=c("blue", "red"), 
       horiz=FALSE, cex=1, pch=c(1,4))
dev.off()


########### Normalized Mean Absolute Error.

# NMAE DispFrames.
diff.DispFrames <- combined.DispFrames[, "actual"] - combined.DispFrames[, "predicted"]
diff.DispFrames <- abs(diff.DispFrames)
error.DispFrames <- mean(diff.DispFrames)/mean(combined.DispFrames[, "actual"])
# execution-time.
exe.DispFrames.time <- system.time(lm(DispFrames~.-NoAudioPlayed -NoRTPPkts, combined.training.matrix))

# NMAE NoAudioPlayed
diff.NoAudioPlayed <- combined.NoAudioPlayed[, "actual"] - combined.NoAudioPlayed[, "predicted"]
diff.NoAudioPlayed <- abs(diff.NoAudioPlayed)
error.NoAudioPlayed <- mean(diff.NoAudioPlayed)/mean(combined.NoAudioPlayed[, "actual"])
#execution-time.
exe.NoAudioPlayed.time <- system.time(lm(NoAudioPlayed~.-DispFrames -NoRTPPkts, combined.training.matrix))

# NMAE NoRTPPkts
diff.NoRTPPkts <- combined.NoRTPPkts[, "actual"] - combined.NoRTPPkts[, "predicted"]
diff.NoRTPPkts <- abs(diff.NoRTPPkts)
error.NoRTPPkts <- mean(diff.NoRTPPkts)/mean(combined.NoRTPPkts[, "actual"])
# execution-time
exe.NoRTPPkts.time <- system.time(lm(NoRTPPkts~.-NoAudioPlayed -DispFrames, combined.training.matrix))
