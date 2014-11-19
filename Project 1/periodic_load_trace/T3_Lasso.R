args<-commandArgs(TRUE)
x.path = "X.csv"
y.path = "Y.csv"

if (length(args) >= 2) {
  x.path = args[1]
  y.path = args[2]
}

# Read Data into X
stopifnot(file.exists(x.path)) # check if x data file exists
periodic.x <- read.csv(x.path)
# Read Data into Y.
stopifnot(file.exists(y.path)) # check if y data file exists
periodic.y <- read.csv(y.path)


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

################## Lasso Regression Method.

library(glmnet)
# Create a matrix from the list.
periodic.training.x.matrix <- model.matrix(~.,periodic.training.x )[,-1]
periodic.test.x.matrix <-  model.matrix(~.,periodic.test.x )[,-1]


# To get best lambda, set up the cross validations in the set.

# =========================================================== DispFrames.

cross.validations.DispFrames = cv.glmnet(periodic.training.x.matrix, periodic.training.y[,'DispFrames'])
#plot(cross.validations.DispFrames)

# Get the best lambda 
lambda.DispFrames <- cross.validations.DispFrames$lambda.min

# Calculate the coefficients through lasso.
lasso.fit.Dispframes <- glmnet(periodic.training.x.matrix, periodic.training.y[,'DispFrames'], alpha=1)
# fit.Dispframes
# plot(fit.Dispframes)

# predict the value.
predicted.DispFrames <- predict(lasso.fit.Dispframes,periodic.test.x.matrix,s=lambda.DispFrames)


# ============================================================ NoAudioPlayed.


cross.validations.NoAudioPlayed = cv.glmnet(periodic.training.x.matrix, periodic.training.y[,'NoAudioPlayed'])
#plot(cross.validations.NoAudioPlayed)

# Get the best lambda 
lambda.NoAudioPlayed <- cross.validations.NoAudioPlayed$lambda.min

# Calculate the coefficients through lasso.
lasso.fit.NoAudioPlayed <- glmnet(periodic.training.x.matrix, periodic.training.y[,'NoAudioPlayed'], alpha=1)
# plot(lasso.fit.NoAudioPlayed)

# Predict the value.
predicted.NoAudioPlayed <- predict(lasso.fit.NoAudioPlayed, periodic.test.x.matrix, s=lambda.NoAudioPlayed)


# ============================================================= NoRTPPkts.


cross.validations.NoRTPPkts = cv.glmnet(periodic.training.x.matrix, periodic.training.y[,'NoRTPPkts'])
#plot(cross.validations.NoRTPPkts)

# Get the best lambda 
lambda.NoRTPPkts <- cross.validations.NoRTPPkts$lambda.min

# Calculate the coefficients through lasso.
lasso.fit.NoRTPPkts <- glmnet(periodic.training.x.matrix, periodic.training.y[,'NoRTPPkts'], alpha=1)
# plot(lasso.fit.NoRTPPkts)

# Predict the value.
predicted.NoRTPPkts <- predict(lasso.fit.NoRTPPkts, periodic.test.x.matrix, s=lambda.NoRTPPkts)



# =============================================================       Plotting Time Series Graph

# Now Create lot for the time series.

# DispFrames time series plot.,
png(file="dispframes_timeseries.png",width=500, height=400)
plot(predicted.DispFrames, col="blue", xlab="Number of Samples", ylab="Video frame rate (frames/sec)",main="Video Frame Rate Predition Using Lasso Regresssion")
points(periodic.test.y[,'DispFrames'],col="red",pch = 4)
legend("topright",
       c("Predicted Frame Rate","Actual Frame Rate"), bty="o", col=c("blue", "red"), 
       horiz=FALSE, cex=1, pch=c(1,4))
dev.off()  

# No Audio Played Time Series Plot.
png(file="audioplayed_timeseries.png",width=500, height=400)
plot(predicted.NoAudioPlayed, col="blue", xlab="Number of Samples", ylab="Audio buffer rate (buffer/sec)",main="Audio Buffer Rate Predition Using Lasso Regresssion")
points(periodic.test.y[,'NoAudioPlayed'],col="red",pch = 4)
legend("topright",
       c("Predicted Buffer Rate","Actual Buffer Rate"), bty="o", col=c("blue", "red"), 
       horiz=FALSE, cex=1, pch=c(1,4))
dev.off()

# NoRTPPkts Time series Plot.
png(file="rtppacket_timeseries.png",width=500, height=400)
plot(predicted.NoRTPPkts, col="blue", xlab="Number of Samples", ylab="RTP packet rate (packets/sec)", main="RTP Packet Rate Predition Using Lasso Regresssion")
points(periodic.test.y[,'NoRTPPkts'], col="red", pch=4)
legend("topright",
       c("Predicted RTP Rate","Actual RTP Rate"), bty="o", col=c("blue", "red"), 
       horiz=FALSE, cex=1, pch=c(1,4))
dev.off()




############# Plotting Against Each Other.

# Creating combined graphs.
combined.DispFrames <- cbind(predicted = predicted.DispFrames[,"1"], actual = periodic.test.y[,'DispFrames'])
combined.NoAudioPlayed <- cbind(predicted = predicted.NoAudioPlayed[,"1"], actual = periodic.test.y[,'NoAudioPlayed'])
combined.NoRTPPkts <- cbind(predicted = predicted.NoRTPPkts[,"1"], actual = periodic.test.y[,'NoRTPPkts'])

#DispFrames.
png(file="dispframes_comparison.png",width=500, height=400)
plot(combined.DispFrames[,'actual'],combined.DispFrames[,'predicted'], col=c('blue','red'), xlab="Actual Video frame rate (frames/sec)", ylab="Predicted Video frame rate (frames/sec)", pch=c(1,4), main="Video Frame Rate Comparison Using Lasso Regresssion")
legend("topleft",
       c("Actual Frame Rate","Predicted Frame Rate"), bty="o", col=c("blue", "red"), 
       horiz=FALSE, cex=1, pch=c(1,4))
dev.off()


#NoAudioPlayed
png(file="audioplayed_comparison.png",width=500, height=400)
plot(combined.NoAudioPlayed[,'actual'],combined.NoAudioPlayed[,'predicted'], col=c('blue','red'), xlab="Actual Audio buffer rate (buffer/sec)", ylab="Predicted Audio buffer rate (buffer/sec)", pch=c(1,4), main="Audio Buffer Rate Comparison Using Lasso Regresssion")
legend("topright",
       c("Actual Audio Buffer Rate","Predicted Audio Buffer Rate"), bty="o", col=c("blue", "red"), 
       horiz=FALSE, cex=1, pch=c(1,4))
dev.off()

#NoRTPPkts
png(file="rtppacket_comparison.png",width=500, height=400)
plot(combined.NoRTPPkts[,'actual'],combined.NoRTPPkts[,'predicted'], col=c('blue','red'), xlab="Actual RTP packet rate (packet/sec)", ylab="Predicted RTP packet rate (packet/sec)", pch=c(1,4), main="RTP Packet Rate Comparison Using Lasso Regresssion")
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
exe.DispFrames.time <- system.time(glmnet(periodic.training.x.matrix, periodic.training.y[,'DispFrames'], alpha=1))

# NMAE NoAudioPlayed
diff.NoAudioPlayed <- combined.NoAudioPlayed[, "actual"] - combined.NoAudioPlayed[, "predicted"]
diff.NoAudioPlayed <- abs(diff.NoAudioPlayed)
error.NoAudioPlayed <- mean(diff.NoAudioPlayed)/mean(combined.NoAudioPlayed[, "actual"])
#execution-time.
exe.NoAudioPlayed.time <- system.time(glmnet(periodic.training.x.matrix, periodic.training.y[,'NoAudioPlayed'], alpha=1))

# NMAE NoRTPPkts
diff.NoRTPPkts <- combined.NoRTPPkts[, "actual"] - combined.NoRTPPkts[, "predicted"]
diff.NoRTPPkts <- abs(diff.NoRTPPkts)
error.NoRTPPkts <- mean(diff.NoRTPPkts)/mean(combined.NoRTPPkts[, "actual"])
# execution-time
exe.NoRTPPkts.time <- system.time(glmnet(periodic.training.x.matrix, periodic.training.y[,'NoRTPPkts'], alpha=1))


# ===============================