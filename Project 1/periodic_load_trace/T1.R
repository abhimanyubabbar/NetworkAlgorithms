# check the current working directory.
getwd()

#loading predictors data
periodic.x=read.csv("X.csv")

#Task 1.1

#stats for all ..idle
mean(periodic.x$all_..idle)  # 10.20498
max(periodic.x$all_..idle)  # 97.71
min(periodic.x$all_..idle)  # 0
quantile(periodic.x$all_..idle, c(.25, .90))  # 0.000 42.023
sd(periodic.x$all_..idle)  # 19.08028
var(periodic.x$all_..idle)  # 364.0573

#stats for X..memused
mean(periodic.x$X..memused)  # 20.89396
max(periodic.x$X..memused)  # 35.62
min(periodic.x$X..memused)  # 6.19
quantile(periodic.x$X..memused, c(.25, .90))  # 13.94 33.26
sd(periodic.x$X..memused)  # 7.928272
var(periodic.x$X..memused)  # 62.8575

#stats for X..swpused
mean(periodic.x$X..swpused)  # 0.1
max(periodic.x$X..swpused)  # 0.1
min(periodic.x$X..swpused)  # 0.1
quantile(periodic.x$X..swpused, c(.25, .90))  # 0.1 0.1
sd(periodic.x$X..swpused)  # 0
var(periodic.x$X..swpused)  # 0

#stats for proc.s
mean(periodic.x$proc.s)  # 7.844773
max(periodic.x$proc.s)  # 58
min(periodic.x$proc.s)  # 0
quantile(periodic.x$proc.s, c(.25, .90))  # 0 20
sd(periodic.x$proc.s)  # 8.735014
var(periodic.x$proc.s)  # 76.30047

#stats for cswch.s
mean(periodic.x$cswch.s)  # 7.844773
max(periodic.x$cswch.s)  # 84703
min(periodic.x$cswch.s)  # 2730
quantile(periodic.x$cswch.s, c(.25, .90))  # 31785.5 72596.1
sd(periodic.x$cswch.s)  # 20376.97
var(periodic.x$cswch.s)  # 415220705

#stats for file.nr
mean(periodic.x$file.nr)  # 2627.962
max(periodic.x$file.nr)  # 2976
min(periodic.x$file.nr)  # 2112
quantile(periodic.x$file.nr, c(.25, .90))  # 2496 2832
sd(periodic.x$file.nr)  # 167.8595
var(periodic.x$file.nr)  # 28176.82

#stats for sum_intr.s
mean(periodic.x$sum_intr.s)  # 20499.39
max(periodic.x$sum_intr.s)  # 36716
min(periodic.x$sum_intr.s)  # 2774
quantile(periodic.x$sum_intr.s, c(.25, .90))  # 17296.0 28962.3
sd(periodic.x$sum_intr.s)  # 4896.131
var(periodic.x$sum_intr.s)  # 23972095

#stats for rtps
mean(periodic.x$rtps)  # 0.025
max(periodic.x$rtps)  # 126
min(periodic.x$rtps)  # 0
quantile(periodic.x$rtps, c(.25, .90))  # 0 0
sd(periodic.x$rtps)  # 1.753876
var(periodic.x$rtps)  # 3.076083

#stats for ldavg.1
mean(periodic.x$ldavg.1)  # 78.00392
max(periodic.x$ldavg.1)  # 155.93
min(periodic.x$ldavg.1)  # 1.79
quantile(periodic.x$ldavg.1, c(.25, .90))  # 30.4 131.4
sd(periodic.x$ldavg.1)  # 44.96409
var(periodic.x$ldavg.1)  # 2021.77

#stats for tcpsck
mean(periodic.x$tcpsck)  # 49.0356
max(periodic.x$tcpsck)  # 86
min(periodic.x$tcpsck)  # 19
quantile(periodic.x$tcpsck, c(.25, .90))  # 35  69 
sd(periodic.x$tcpsck)  # 15.44682
var(periodic.x$tcpsck)  # 2021.77

#stats for tcpsck
mean(periodic.x$tcpsck)  # 49.0356
max(periodic.x$tcpsck)  # 86
min(periodic.x$tcpsck)  # 19
quantile(periodic.x$tcpsck, c(.25, .90))  # 35  69 
sd(periodic.x$tcpsck)  # 15.44682
var(periodic.x$tcpsck)  # 2021.77

#loading Y data
periodic.y=read.csv("Y.csv")

#stats for DispFrames
mean(periodic.y$DispFrames)  # 17.94674
max(periodic.y$DispFrames)  # 30.22
min(periodic.y$DispFrames)  # 0
quantile(periodic.y$DispFrames, c(.25, .90))  # 13.39 24.00
sd(periodic.y$DispFrames)  # 5.038104
var(periodic.y$DispFrames)  # 2021.77

#stats for DispFrames
mean(periodic.y$NoAudioPlayed)  # 30.85356
max(periodic.y$NoAudioPlayed)  # 153.59
min(periodic.y$NoAudioPlayed)  # 0
quantile(periodic.y$NoAudioPlayed, c(.25, .90))  # 0 42
sd(periodic.y$NoAudioPlayed)  # 18.56813
var(periodic.y$NoAudioPlayed)  # 344.7755

#stats for NoRTPPkts
mean(periodic.y$NoRTPPkts)  # 114.3165
max(periodic.y$NoRTPPkts)  # 553.64
min(periodic.y$NoRTPPkts)  # 0
quantile(periodic.y$NoRTPPkts, c(.25, .90))  # 70.160 196.007 
sd(periodic.y$NoRTPPkts)  # 63.80072
var(periodic.y$NoRTPPkts)  # 4070.532

# Task 1.2

# the number of samples with memory usage is larger than 80%
sum(periodic.x$X..memused > 80)  # 0

# the average number of used TCP sockets for samples with more than 18000 interrupts/sec
mean(periodic.x[periodic.x$sum_intr.s > 18000, ]$tcpsck)  # 50.12893

# the minimum memory utilisation for samples with swap space usage larger than 50%
min(periodic.x[periodic.x$X..swpuseds > 50, ]$X..memused)  # Inf, because zero rows are retuned for X..swpuseds > 50

# Task 1.3

#settings margins to get rid of empty spaces
par(mar=c(5,3,1,2)+0.1)

# Time series of percentage of idle CPU, used memory, and swap space (all in a single plot)
# First Creating appropriate plot axis
png(file="percentage_utilization.png",width=500, height=400)
plot(0, xlab="Time", ylab="Utilization %", xlim=c(0, nrow(periodic.x)), 
     ylim=c(min(periodic.x$all_..idle, periodic.x$X..swpused, periodic.x$X..memused), 
            max(periodic.x$all_..idle, periodic.x$X..swpused, periodic.x$X..memused)))
# Now plotting all three graphs in one
points(periodic.x$all_..idle, col="red", xlab = "Time", ylab = "Utilization %")
points(periodic.x$X..memused, col="green")
points(periodic.x$X..swpused, col="blue")
legend("topright",
       c("Idle CPU","Memory Used","Swap Used"), bty="n", fill=c("red", "green", "blue"), 
       horiz=FALSE, cex=1)
dev.off()

# Density plots for the video frame rate, the audio buffer rate, and the RTP packet rate

png(file="video_frame_density.png",width=500, height=400)
plot(density(periodic.y$DispFrames), main="", xlab="Video frame rate (frame/sec)", col="blue")
dev.off()

png(file="audio_buffer_density.png",width=500, height=400)
plot(density(periodic.y$NoAudioPlayed), main="", xlab="Audio buffer rate (buffer/sec)", col="blue")
dev.off()

png(file="rtt_packet_density.png",width=500, height=400)
plot(density(periodic.y$NoRTPPkts), main="", xlab="RTP packet rate (packet/sec)", col="blue")
dev.off()
