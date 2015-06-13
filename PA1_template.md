# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data

Load and store activity data:


```r
activity_data <- read.csv("activity.csv")
activity_data$date <- as.Date(activity_data$date, "%Y-%m-%d")
```

## What is mean total number of steps taken per day?

Calculate the total number of steps taken per day:


```r
Daily_Steps <- aggregate(steps ~ date, data = activity_data, sum, na.rm = TRUE)
```

Create and display a histogram plot:


```r
hist(Daily_Steps$steps, main = "Total Daily Steps", xlab = "Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

Calculate and print the mean and median steps taken per day:


```r
mean(Daily_Steps$steps)
```

```
## [1] 10766.19
```

```r
median(Daily_Steps$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

Calculate the average number of steps within each 5-minute interval:


```r
avg_steps <- tapply(activity_data$steps, activity_data$interval, mean, na.rm = TRUE)
```

Create a time series plot of the 5-minute interval and the average number of steps taken, averaged across all days:


```r
plot(row.names(avg_steps), avg_steps, type = "l", main = "Average Daily Activity", xlab = "5 Minute Intervals", ylab = "Average Across All Days")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png) 

Calculate and print which 5-minute interval, on average across all days, contains the maximum number of steps:


```r
max_steps <- which.max(avg_steps)

names(max_steps)
```

```
## [1] "835"
```

## Inputing missing values

Calculate and print the total number of missing values:


```r
missing_activity <- sum(is.na(activity_data))
missing_activity
```

```
## [1] 2304
```

Fill in all the missing values in the dataset:


```r
Average_Steps <- aggregate(steps ~ interval, data = activity_data, FUN = mean)

place <- numeric()

for (i in 1:nrow(activity_data)) {
    num <- activity_data[i, ]
    
    if (is.na(num$steps)) {
        steps <- subset(Average_Steps, interval == num$interval)$steps
    } else {
        steps <- num$steps
    }
    
    place <- c(place, steps)
}
```

Create a new dataset with missing data filled in:


```r
activity_filled <- activity_data

activity_filled$steps <- place
```

Make a histogram of the total number of steps taken each day:


```r
Complete_Daily <- aggregate(steps ~ date, data = activity_filled, sum, na.rm = TRUE)

hist(Complete_Daily$steps, main = "Total Daily Steps", xlab = "Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png) 

Calculate and print the mean and median total number of steps taken per day:


```r
mean(Complete_Daily$steps)
```

```
## [1] 10766.19
```

```r
median(Complete_Daily$steps)
```

```
## [1] 10766.19
```

The median is different than it was when calculated before the missing values were filled in.  Since the values were filled in with the mean of the respective 5-minute interval, the mean was not impacted, but the median was and is now the same as the mean.

## Are there differences in activity patterns between weekdays and weekends?

Create a new factor variable with two levels "weekday" and "weekend":


```r
week_day <- weekdays(activity_filled$date)

day_type <- vector()

for (i in 1:nrow(activity_filled)) {
    if (week_day[i] == "Saturday") {
        day_type[i] <- "Weekend"
    } else if (week_day[i] == "Sunday") {
        day_type[i] <- "Weekend"
    } else {
        day_type[i] <- "Weekday"
    }
}

activity_filled$day_type <- day_type

activity_filled$day_type <- factor(activity_filled$day_type)

day_type_diff <- aggregate(steps ~ interval + day_type, data = activity_filled, mean)

names(day_type_diff) <- c("interval", "daytype", "steps")
```

Make a panel plot with a time series plot of the 5-minute interval and the average number of steps taken, averaged across all weekday days or weekend days:


```r
library(lattice)
xyplot(day_type_diff$steps ~ day_type_diff$interval | day_type_diff$daytype, type = "l", layout = c(1, 2), xlab = "5-Minute Interval", ylab = "Average Number of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png) 
