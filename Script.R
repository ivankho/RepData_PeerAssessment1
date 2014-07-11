csv_file <- unzip("repdata-data-activity.zip")
file <- read.csv(csv_file)

dates <- with(file, levels(date))

step_sum <- with(file, tapply(steps, date, sum)) #by dates
hist(step_sum, breaks = 10, col = "green", 
     main = "Histogram of Steps Taken Each Day", xlab = "Steps Taken Each Day")
mean <- mean(step_sum, na.rm = TRUE)
median <- median(step_sum, na.rm = TRUE)

stepMeans<- with(file, tapply(steps, interval, mean, na.rm=TRUE)) #by intervals
stepMeans <- as.numeric(stepMeans)
interval <- as.numeric(levels(factor(file$interval)))
plot(interval, stepMeans, type = "l", col = "red", xlab = "5-minute interval", 
     ylab = "Steps Averaged", main = "Steps Averaged Across All Days")

meanByInterval <- data.frame(stepMeans, interval)
maxInterval <- as.numeric(meanByInterval[with(meanByInterval, stepMeans == max(stepMeans)),][2])

numMissing <- sum(is.na(file$steps))

step_sum <- as.numeric(step_sum)
meanByDate <- data.frame(step_sum, dates)

filledFile <- file
for(i in 1:nrow(filledFile)){
    if(is.na(filledFile[i, 1])){
        filledFile[i, 1] <- 
            as.numeric(meanByInterval[meanByInterval[,2] == filledFile[i,3],][1])
    }
}

filledSums <- with(filledFile, tapply(steps, date, sum))
hist(filledSums, breaks = 10, col = "green", 
     main = "Histogram of Steps Taken Each Day (Filled)", xlab = "Steps Taken Each Day")
mean <- mean(filledSums, na.rm = TRUE)
median <- median(filledSums, na.rm = T)

filledFile$days <- with(filledFile, weekdays(as.Date(date)))
filledFile[filledFile$days != c("Saturday", "Sunday"),]$days <- "weekdays"
filledFile[filledFile$days == c("Saturday", "Sunday"),]$days <- "weekends"
filledFile$days <- with(filledFile, factor(days))


splitData <- split(filledFile, filledFile$days)
splitMeans <- lapply(splitData, function(df) tapply(df$steps, df$interval, mean))

data <- data.frame(Steps = as.vector(unlist(splitMeans)), 
                   Interval = rep(interval, 2), Days = rep(c("weekdays", "weekends"), 
                                                            each = 288))

xyplot(Steps ~ Interval | Days, data = data, layout = c(1, 2), 
       type = "l", ylab = "Number of Steps")
