# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
activity <- read.csv(unz("activity.zip", "activity.csv"), header = TRUE, stringsAsFactors = FALSE)
activity$date <- as.Date(activity$date, "%Y-%m-%d")
head(activity)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```


## What is mean total number of steps taken per day?
Calculate the total number of steps taken per day
Make a histogram of the total number of steps taken each day
Calculate and report the mean and median of the total number of steps taken per day


```r
total <- aggregate(steps ~ date, data = activity, FUN = sum)
hist(total$steps,xlab = "Steps", ylab = "Frequency", 
    main = "Total Steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)

```r
mean(total$steps)
```

```
## [1] 10766.19
```

```r
median(total$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?
Make a time series plot of the 5-minute interval (and the average number of steps taken, averaged across all days
Report Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
average <- aggregate(steps ~ interval, data = activity, FUN = mean)
plot(average$interval, average$steps,  type = "l",xlab = "Time interval", ylab = "Steps", 
    main = "Average steps per interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)

```r
average$interval[which.max(average$steps)]                                       #calculate 5-minute interval, on average across all the days in the                                                                                                #dataset, contains the maximum number of steps
```

```
## [1] 835
```


## Imputing missing values
Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
Create a new dataset that is equal to the original dataset but with the missing data filled in.
Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
NAval <- sum(is.na(activity$steps))
NAindex <- which(is.na(activity$steps))
imputed <- activity
for (i in NAindex){
  k <- intersect(imputed[i,3], average$interval) 
  imputed[i,1] <- average[average$interval == k, 2]
}
total2 <- aggregate(steps ~ date, data = imputed, FUN = sum)
hist(total2$steps,xlab = "Steps", ylab = "Frequency", 
    main = "Total Steps per day without NA")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)

## Are there differences in activity patterns between weekdays and weekends?
Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 


```r
days <- weekdays(imputed$date)
imputed$dayType <- ifelse(days == "воскресенье" | days == "суббота", "Weekend", "Weekday")
average2 <- aggregate(imputed$steps, by = list(imputed$interval, imputed$dayType), mean)
names(average2) <- c("interval", "dayType", "steps")
average2$dayType <- as.factor(average2$dayType)
library(lattice)
xyplot(steps ~ interval | dayType, average2, type = "l", layout = c(1, 2), lab = "Interval", ylab = "Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)
