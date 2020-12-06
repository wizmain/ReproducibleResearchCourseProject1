---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


### Loading Library

```r
library(ggplot2)
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

## Loading and preprocessing the data

```r
activity <- read.csv("activity.csv", header=TRUE, sep=",")
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

```r
# aggregate steps by date
steps_per_day <- aggregate(steps~date, data=activity, sum, na.rm=TRUE)
# make histogram
qplot(steps_per_day$steps, binwidth=500, main="Total Number of steps taken per day", xlab="Steps", ylab="Frequency")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
# mean
mean(steps_per_day$steps, na.rm=TRUE)
```

```
## [1] 10766.19
```

```r
# median
median(steps_per_day$steps, na.rm=TRUE)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

```r
# aggregate steps per interval
steps_per_interval <- aggregate(steps~interval, data=activity, mean, na.rm=TRUE)
head(steps_per_interval)
```

```
##   interval     steps
## 1        0 1.7169811
## 2        5 0.3396226
## 3       10 0.1320755
## 4       15 0.1509434
## 5       20 0.0754717
## 6       25 2.0943396
```

```r
# plot type="l"
plot(steps_per_interval$interval, steps_per_interval$steps, type="l", lwd=2, xlab="interval", ylab="mean of steps", main="steps per interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
# maximum number of steps
max(steps_per_interval$steps)
```

```
## [1] 206.1698
```

## Imputing missing values

```r
# sum of missing value
sum(is.na(activity$steps))
```

```
## [1] 2304
```

```r
# make new activity dataset
activity_imputed <- transform(activity, steps_imputed=steps)
# check out new column added
str(activity_imputed)
```

```
## 'data.frame':	17568 obs. of  4 variables:
##  $ steps        : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date         : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval     : int  0 5 10 15 20 25 30 35 40 45 ...
##  $ steps_imputed: int  NA NA NA NA NA NA NA NA NA NA ...
```

```r
# insert interval mean value
activity_imputed$steps_imputed <- ifelse(is.na(activity_imputed$steps), round(steps_per_interval$steps[match(activity_imputed$interval, steps_per_interval$interval)],0), activity_imputed$steps)
# data
head(activity_imputed)
```

```
##   steps       date interval steps_imputed
## 1    NA 2012-10-01        0             2
## 2    NA 2012-10-01        5             0
## 3    NA 2012-10-01       10             0
## 4    NA 2012-10-01       15             0
## 5    NA 2012-10-01       20             0
## 6    NA 2012-10-01       25             2
```

```r
# missing value count
sum(is.na(activity_imputed$steps_imputed))
```

```
## [1] 0
```

```r
# histogram by new data
steps_per_day_nona <- aggregate(steps~date, data=activity_imputed, sum)
qplot(steps_per_day_nona$steps, binwidth=500, main="Steps Per Day", xlab="Steps", ylab="Count")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
# average daily activity pattern
steps_per_interval_nona <- aggregate(steps~interval, data=activity_imputed, mean)
plot(steps_per_interval_nona$interval, steps_per_interval_nona$steps, type="l", lwd=2, xlab="interval", ylab="mean of steps", main="steps per interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-2.png)<!-- -->

```r
# new mean
mean(steps_per_day_nona$steps)
```

```
## [1] 10766.19
```

```r
# new median
median(steps_per_day_nona$steps)
```

```
## [1] 10765
```

## Are there differences in activity patterns between weekdays and weekends?

```r
activity_imputed <- mutate(activity_imputed, date_t=as.Date(date))
# set weekday in English
Sys.setlocale("LC_TIME", "en_US")
```

```
## [1] "en_US"
```

```r
# add weekday  column
activity_imputed <- mutate(activity_imputed, weekday=weekdays(date_t))
# add weeday_type column ( weekend, weekday )
activity_imputed <- mutate(activity_imputed, weekday_type=ifelse(weekday=='Saturday' | weekday=='Sunday', 'weekend', 'weekday'))
# checkout
head(activity_imputed)
```

```
##   steps       date interval steps_imputed     date_t weekday weekday_type
## 1    NA 2012-10-01        0             2 2012-10-01  Monday      weekday
## 2    NA 2012-10-01        5             0 2012-10-01  Monday      weekday
## 3    NA 2012-10-01       10             0 2012-10-01  Monday      weekday
## 4    NA 2012-10-01       15             0 2012-10-01  Monday      weekday
## 5    NA 2012-10-01       20             0 2012-10-01  Monday      weekday
## 6    NA 2012-10-01       25             2 2012-10-01  Monday      weekday
```

```r
steps_per_interval_weekday <- aggregate(steps~interval+weekday_type, data=activity_imputed, mean)

# make plot
ggplot(steps_per_interval_weekday, aes(x=interval,y=steps, color=weekday_type)) + geom_line() + labs(title="steps per interval(weekday)",x="interval", y="means of steps") + facet_wrap(~weekday_type, ncol=1, nrow=2)
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

