# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
activity <- read.csv(unzip("activity.zip"))
activity$date <- as.Date(activity$date, "%Y-%m-%d")
```


## What is mean total number of steps taken per day?

```r
# Split the activities per day
days <- cut(activity$date, "day")
daily.activity <- split(activity, days)

# Calculate and plot the daily total steps
steps.sum <- sapply(daily.activity, function(x) {
    sum(x$steps, na.rm = TRUE)
})
hist(steps.sum, breaks = 10, main = "Total Daily Steps", xlab = "Daily steps")
```

![plot of chunk daily number of steps](figure/daily_number_of_steps.png) 

```r

# Report mean and median total daily steps
steps.mean <- round(mean(steps.sum))
steps.median <- median(steps.sum)

print(paste("Total daily steps mean =", steps.mean, ", median =", steps.median))
```

```
## [1] "Total daily steps mean = 9354 , median = 10395"
```



## What is the average daily activity pattern?

```r
# Split the data by the 5-minutes interval across all days
activity.5min <- split(activity, activity$interval)

# Calculate and plot the average steps per every 5-minutes interval
mean.5min <- sapply(activity.5min, function(x) {
    round(mean(x$steps, na.rm = T))
})
plot(unique(activity$interval), mean.5min, type = "l", main = "Daily Activity Pattern", 
    xlab = "5-minutes interval", ylab = "Average Steps")
```

![plot of chunk daily activity pattern](figure/daily_activity_pattern.png) 

```r
max.interval <- names(mean.5min[which.max(mean.5min)])
max.value <- mean.5min[which.max(mean.5min)]
```

The interval with maximum number of steps is 835, with average steps = 206

## Imputing missing values

```r
# Get the rows with incomplete cases i.e. has NA
incomplete <- activity[!complete.cases(activity), ]
na.count <- nrow(incomplete)

# Create a new data.frame from activity
activity.complete <- activity

# Get intervals with incomplete data
intervals <- unique(incomplete$interval)

# Fill missing data with the interval mean that was calculated previously
# i.e. mean.5min
for (i in 1:length(intervals)) {
    incomplete[incomplete$interval == intervals[i], "steps"] <- mean.5min[which(names(mean.5min) == 
        intervals[i])]
}

activity.complete[!complete.cases(activity.complete), ] <- incomplete  # now already completed

new.steps.sum <- sapply(split(activity.complete, days), function(x) {
    sum(x$steps)
})
hist(new.steps.sum, breaks = 10, main = "Total Daily Steps with Imputed Values", 
    xlab = "Daily steps")
```

![plot of chunk imputing missing values](figure/imputing_missing_values.png) 

```r

# Report mean and median total daily steps
new.steps.mean <- round(mean(new.steps.sum))
new.steps.median <- median(new.steps.sum)

print(paste("Total daily steps mean =", new.steps.mean, ", median =", new.steps.median))
```

```
## [1] "Total daily steps mean = 10766 , median = 10762"
```


It is clear that after imputing missing values the total number of steps increased particularly the daily total steps mean increased by **1412** and the median increased by **367** steps. Also, the histogram shape is more close to the normal distribution.


## Are there differences in activity patterns between weekdays and weekends?
