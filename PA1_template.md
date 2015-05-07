Activity Monitoring Data Overview
========================================================

This report summarizes several key features from two months of activity monitoring data, obtained by devices like fitbit.  

This first code section simply downloads, reads, and processes the data with read.csv.

```r
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", 
    "zipped_activity_data.zip", method = "curl")
unzip("zipped_activity_data.zip")
activity <- read.csv("activity.csv")
```


Now, we compute some basic information on steps per day.  First, we compute and output the total number of steps taken each day. Then we present a simple histogram of that data. Last, we output the mean and median number of steps per day.


```r
Steps_Per_Day <- tapply(activity$steps, activity$date, sum)
Steps_Per_Day
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##         NA        126      11352      12116      13294      15420 
## 2012-10-07 2012-10-08 2012-10-09 2012-10-10 2012-10-11 2012-10-12 
##      11015         NA      12811       9900      10304      17382 
## 2012-10-13 2012-10-14 2012-10-15 2012-10-16 2012-10-17 2012-10-18 
##      12426      15098      10139      15084      13452      10056 
## 2012-10-19 2012-10-20 2012-10-21 2012-10-22 2012-10-23 2012-10-24 
##      11829      10395       8821      13460       8918       8355 
## 2012-10-25 2012-10-26 2012-10-27 2012-10-28 2012-10-29 2012-10-30 
##       2492       6778      10119      11458       5018       9819 
## 2012-10-31 2012-11-01 2012-11-02 2012-11-03 2012-11-04 2012-11-05 
##      15414         NA      10600      10571         NA      10439 
## 2012-11-06 2012-11-07 2012-11-08 2012-11-09 2012-11-10 2012-11-11 
##       8334      12883       3219         NA         NA      12608 
## 2012-11-12 2012-11-13 2012-11-14 2012-11-15 2012-11-16 2012-11-17 
##      10765       7336         NA         41       5441      14339 
## 2012-11-18 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 
##      15110       8841       4472      12787      20427      21194 
## 2012-11-24 2012-11-25 2012-11-26 2012-11-27 2012-11-28 2012-11-29 
##      14478      11834      11162      13646      10183       7047 
## 2012-11-30 
##         NA
```

```r
hist(Steps_Per_Day)
```

![plot of chunk steps_per_day](figure/steps_per_day.png) 

```r
mean(Steps_Per_Day, na.rm = TRUE)
```

```
## [1] 10766
```

```r
median(Steps_Per_Day, na.rm = TRUE)
```

```
## [1] 10765
```


Having shown some information on steps per day, we investigate steps per time interval.  Here we create a time series plot of average number of steps in each time interval.  Then we output the time interval with the highest number of steps.


```r
time_interval_means <- tapply(activity$steps, activity$interval, mean, na.rm = TRUE)
time_interval_means <- as.data.frame(time_interval_means)
plot(row.names(time_interval_means), time_interval_means$time_interval_means, 
    type = "l", xlab = "Time Interval", ylab = "Number of Steps", main = "Average Steps in Each Time Interval")
```

![plot of chunk steps_per_interval](figure/steps_per_interval.png) 

```r
time_interval_sums <- tapply(activity$steps, activity$interval, sum, na.rm = TRUE)
time_interval_sums <- as.data.frame(time_interval_sums)
which.max(time_interval_sums$time_interval_sums)
```

```
## 835 
## 104
```


Thus far, we have been simply removing missing values to handle the numerous NAs in the data.  Here, we devise a strategy for imputing that missing data.  We loop over the activity data, replacing missing step values with the mean steps for the time interval in which the missing value occered.  This simple method will have little effect on measures of central tendency, although it may effect more complex analyses like regression.  

We then show some summary data, including a histogram of the data with the NAs replaced as above.  The totals in the histogram are higher (since there are more values) but the overall shape of the distribution, along with the median and mean, is virtually unchanged.


```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

```r
activity_no_missing <- activity
for (i in 1:length(activity$steps)) {
    if (is.na(activity$steps[i])) {
        activity_no_missing$steps[i] <- mean(subset(activity, activity$interval == 
            activity$interval[i])$steps, na.rm = TRUE)
    }
}
Steps_Per_Day <- tapply(activity_no_missing$steps, activity_no_missing$date, 
    sum)
Steps_Per_Day
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##      10766        126      11352      12116      13294      15420 
## 2012-10-07 2012-10-08 2012-10-09 2012-10-10 2012-10-11 2012-10-12 
##      11015      10766      12811       9900      10304      17382 
## 2012-10-13 2012-10-14 2012-10-15 2012-10-16 2012-10-17 2012-10-18 
##      12426      15098      10139      15084      13452      10056 
## 2012-10-19 2012-10-20 2012-10-21 2012-10-22 2012-10-23 2012-10-24 
##      11829      10395       8821      13460       8918       8355 
## 2012-10-25 2012-10-26 2012-10-27 2012-10-28 2012-10-29 2012-10-30 
##       2492       6778      10119      11458       5018       9819 
## 2012-10-31 2012-11-01 2012-11-02 2012-11-03 2012-11-04 2012-11-05 
##      15414      10766      10600      10571      10766      10439 
## 2012-11-06 2012-11-07 2012-11-08 2012-11-09 2012-11-10 2012-11-11 
##       8334      12883       3219      10766      10766      12608 
## 2012-11-12 2012-11-13 2012-11-14 2012-11-15 2012-11-16 2012-11-17 
##      10765       7336      10766         41       5441      14339 
## 2012-11-18 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 
##      15110       8841       4472      12787      20427      21194 
## 2012-11-24 2012-11-25 2012-11-26 2012-11-27 2012-11-28 2012-11-29 
##      14478      11834      11162      13646      10183       7047 
## 2012-11-30 
##      10766
```

```r
hist(Steps_Per_Day)
```

![plot of chunk missing_values](figure/missing_values.png) 

```r
mean(Steps_Per_Day, na.rm = TRUE)
```

```
## [1] 10766
```

```r
median(Steps_Per_Day, na.rm = TRUE)
```

```
## [1] 10766
```



Finally, we consider weekdays against weekends.  We create a new boolean factor variable for whether the day is a weekday.  We then create a 2-panel time series plot to compare steps per time interval on weekdays to steps per time interval on weekends.  


```r
activity_no_missing$weekday = weekdays(as.Date(as.character(activity_no_missing$date), 
    "%Y-%m-%d"))
activity_no_missing$weekday <- !(activity_no_missing$weekday == "Saturday" | 
    activity_no_missing$weekday == "Sunday")
activity_no_missing$weekday <- as.factor(activity_no_missing$weekday)
weekday_activity <- subset(activity_no_missing, activity_no_missing$weekday == 
    TRUE)
weekend_activity <- subset(activity_no_missing, activity_no_missing$weekday == 
    FALSE)
par(mfrow = c(2, 1))
time_interval_means <- tapply(weekday_activity$steps, weekday_activity$interval, 
    mean, na.rm = TRUE)
time_interval_means <- as.data.frame(time_interval_means)
plot(row.names(time_interval_means), time_interval_means$time_interval_means, 
    type = "l", xlab = "Time Interval", ylab = "Number of Steps", main = "Weekday")

time_interval_means <- tapply(weekend_activity$steps, weekend_activity$interval, 
    mean, na.rm = TRUE)
time_interval_means <- as.data.frame(time_interval_means)
plot(row.names(time_interval_means), time_interval_means$time_interval_means, 
    type = "l", xlab = "Time Interval", ylab = "Number of Steps", main = "Weekend")
```

![plot of chunk weekdays](figure/weekdays.png) 
