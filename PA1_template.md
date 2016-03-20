---
title: "Project1"
author: "Abhinav"
date: "20 March 2016"
output: html_document
---

Downloading file from URL 


```r
download.file(url='https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip',dest="dataset_project1.zip",mode="wb")
unzip ("dataset_project1.zip", exdir = ".")
data_project1 <- read.csv("activity.csv",colClass=c('integer', 'Date', 'integer'))
```

Calculating mean and median of total number of steps taken per day and plotting the histogram.


```r
total_date <- aggregate(steps ~ date, data_project1, sum)
fig_1<-barplot(total_date$steps, names.arg=total_date$date, xlab="Date", ylab="Sum of steps",)
```

![plot of chunk unnamed-chunk-2](https://github.com/abhinav-piplani/RepData_PeerAssessment1/blob/master/unnamed-chunk-2-1.png)

```r
mean(total_date$steps)
```

```
## [1] 10766.19
```

```r
median(total_date$steps)
```

```
## [1] 10765
```

Time Series Plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis), reporting the 5-minute interval which contains the maximum number of steps.


```r
total_interval <- aggregate(steps ~ interval, data_project1, mean)
fig_2<-plot(total_interval, type='l')
```

![plot of chunk unnamed-chunk-3](https://github.com/abhinav-piplani/RepData_PeerAssessment1/blob/master/unnamed-chunk-3-1.png)

```r
total_interval$interval[which.max(total_interval$steps)]
```

```
## [1] 835
```

Reporting the total number of missing values in the dataset and imputing the missing values.


```r
sum(is.na(data_project1$steps))
```

```
## [1] 2304
```

```r
modified_data <- merge(data_project1, total_date, by="date", suffixes=c("", ".median"))
na_values <- is.na(modified_data$steps)
modified_data$steps[na_values] <- modified_data$steps.median[na_values]
modified_data <- modified_data[, c(1:3)]
```

Plotting the histogram of the total number of steps taken each day  reporting the mean and median total number of steps taken per day. 
 

```r
total_date_new <- aggregate(steps ~ date, modified_data, sum)
fig_3<-barplot(total_date_new$steps, names.arg=total_date_new$date,xlab="Date", ylab="Sum of steps",)
```

![plot of chunk unnamed-chunk-5](https://github.com/abhinav-piplani/RepData_PeerAssessment1/blob/master/unnamed-chunk-5-1.png)

```r
mean(total_date_new$steps)
```

```
## [1] 10766.19
```

```r
median(total_date_new$steps)
```

```
## [1] 10765
```

Constructing a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
days <- function(dates) {
  f <- function(date) {
    if (weekdays(date) %in% c("Saturday", "Sunday")) { "weekend"}
    else {"weekday"}}
  sapply(dates, f)}
data_project1$days <- as.factor(days(data_project1$date))
str(data_project1)
```

```
## 'data.frame':	17568 obs. of  4 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  $ days    : Factor w/ 2 levels "weekday","weekend": 1 1 1 1 1 1 1 1 1 1 ...
```

```r
library(lattice)
x<- aggregate(steps ~ interval + days, data_project1, mean)
fig_4<-xyplot(steps ~ interval | days, data=x, layout=c(2,1), type='l')
fig_4
```

![plot of chunk unnamed-chunk-6](https://github.com/abhinav-piplani/RepData_PeerAssessment1/blob/master/unnamed-chunk-6-1.png)


