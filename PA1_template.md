# Reproducible Research: Peer Assessment 1

## Introduction
This report is created as part of Peer Assessment 1 for the Coursera class [Reproduceable Research](https://class.coursera.org/repdata-031/) by Roger D. Peng, PhD, Jeff Leek, PhD, Brian Caffo, PhD. This assignment makes use of [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip). The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset. 

The variables included in this dataset are:

* **steps**: Number of steps taking in a 5-minute interval (missing values are coded as NA)
* **date**: The date on which the measurement was taken in YYYY-MM-DD format
* **interval**: Identifier for the 5-minute interval in which measurement was taken

This reports answers the multiple parts of the assignment in the sections below.

## Setup the environment
Load the required libraries

```r
library(knitr)
library(lattice)
```
Since the requirement is to output all of the R code in the final report, set the global option of `echo = TRUE`. Additionally, the generated plot images are supposed to be stored in the `figure` directory.

```r
opts_chunk$set(echo = TRUE, fig.path='figure/')
```

## Loading and preprocessing the data
Since the dataset is already part of our github repo in zip format, let's load it.

```r
unzip(zipfile = 'activity.zip', overwrite = TRUE)
activityData <- read.csv(file = 'activity.csv', header = TRUE)
```

Now, quickly confirm the structure of the dataset.

```r
str(activityData)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
summary(activityData)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```

This confirms that the dataset has 3 variables of `steps`, `date` and `interval` as stated in the introduction above.

Now, let's answer the questions for the assignment.

## What is mean total number of steps taken per day?
1. Calculate the total number of steps taken per day ignoring the missing values

```r
nonNAStepData <- activityData[!is.na(activityData$steps),]
totalStepsPerDay <- aggregate(x = nonNAStepData$steps, by = list(nonNAStepData$date), FUN = sum, na.exclude = TRUE)
colnames(totalStepsPerDay) <- c('date', 'steps')
```

2. Make a histogram of the total number of steps taken each day

```r
hist(totalStepsPerDay$steps, breaks = 10, main = 'Histogram of Total number of Steps per Day', xlab = 'Steps per day', ylab = 'Days')
```

![](figure/histogramOfStepsPerDay-1.png) 


3. Calculate and report the mean and median of the total number of steps taken per day

```r
meanStepsPerDay <- mean(totalStepsPerDay$steps)
meanStepsPerDay
```

```
## [1] 10767.19
```

```r
medianStepsPerDay <- median(totalStepsPerDay$steps)
medianStepsPerDay
```

```
## [1] 10766
```
**Mean Steps Per Day**: ``10767.19``  
**Median Steps Per Day**: ``10766.00``

## What is the average daily activity pattern?
1. Time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
avgStepsPerInterval <- aggregate(x = nonNAStepData$steps, by = list(nonNAStepData$interval), FUN = mean, na.exclude = TRUE)
colnames(avgStepsPerInterval) <- c('interval', 'steps')

xyplot(as.numeric(avgStepsPerInterval$steps) ~ avgStepsPerInterval$interval, 
       type = "l", 
       main = "Time series plot of interval and the average steps", 
       xlab = "5-minute Interval",
       ylab = "Average number of Steps"
       )
```

![](figure/timeSeriesPlot-1.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
maxStepRow <- avgStepsPerInterval[order(avgStepsPerInterval$steps, decreasing = TRUE),][1,]
maxStepRow$interval
```

```
## [1] 835
```

**Interval ``835``** contains maximum number of steps averaged across all the days.

## Imputing missing values
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
missingValuesCount <- length(activityData[is.na(activityData$steps),][,1])
missingValuesCount
```

```
## [1] 2304
```

There are a **total of ``2304`` rows** with NAs in the dataset

2. As a strategy for **filling in all of the missing values we use the mean for that 5-minute interval.** So, a new dataset that is equal to the original dataset but with the missing data filled in can be calculated as below:

```r
imputedActivityData <- activityData #make a copy of the original dataset

# loop through the dataset one row at a time
for (i in 1:nrow(imputedActivityData)) {
  if (is.na(imputedActivityData$steps[i])) { # for rows where steps are missing...
    # ...set them to the mean steps for the corresponding interval
    imputedActivityData$steps[i] <- avgStepsPerInterval$steps[avgStepsPerInterval$interval == imputedActivityData$interval[i]]
  }
}
```

3. Calculate the total number of steps taken per day. This is required for the next step of building the histogram

```r
totalImputedStepsPerDay <- aggregate(x = imputedActivityData$steps, by = list(imputedActivityData$date), FUN = sum)
colnames(totalImputedStepsPerDay) <- c('date', 'steps')
```

4. Make a histogram of the total number of steps taken each day

```r
hist(totalImputedStepsPerDay$steps, breaks = 10, main = 'Histogram of the total number of steps (imputed) per day', xlab = 'Number of Steps (imputed) per day', ylab = 'Number of Days')
```

![](figure/histogramOfImputedStepsPerDay-1.png) 

5. Calculate and report the mean and median total number of steps taken per day. 

```r
meanImputedStepsPerDay <- mean(totalImputedStepsPerDay$steps)
meanImputedStepsPerDay
```

```
## [1] 10766.19
```

```r
medianImputedStepsPerDay <- median(totalImputedStepsPerDay$steps)
medianImputedStepsPerDay
```

```
## [1] 10766.19
```

**Mean Steps Per Day**: ``10766.19``  
**Median Steps Per Day**: ``10766.19``

6. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

The difference of the imputed mean value from the estimates from the first part of the assignment is **```meanStepsPerDay - meanImputedStepsPerDay``` = ``1.00``**

The difference of the imputed median value from the estimates from the first part of the assignment is **```medianStepsPerDay - medianImputedStepsPerDay``` = ``-0.19``**

Looking at the above values, **there isn't significant difference** between these estimates from the first part where we excluded the missing values.

## Are there differences in activity patterns between weekdays and weekends?
1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
# Using the weekdays function, we can add a new factor to the dataset
imputedActivityData$dayType <- as.factor(sapply(imputedActivityData$date, function(date) {
  if(weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) {
    "weekend"
  } else {
   "weekday"
  }
}))
```


2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```r
avgImputedStepsPerInterval <- aggregate(x = imputedActivityData$steps, by = list(interval = imputedActivityData$interval, dayType = imputedActivityData$dayType), FUN = mean)
colnames(avgImputedStepsPerInterval) <- c('interval', 'dayType', 'steps')

xyplot(as.numeric(avgImputedStepsPerInterval$steps) ~ avgImputedStepsPerInterval$interval | avgImputedStepsPerInterval$dayType, 
       type = "l", 
       layout=c(1, 2), 
       main = "Time series plot of interval and the average steps", 
       xlab = "Interval",
       ylab = "Number of Steps"
)
```

![](figure/plot weekDayType-1.png) 

From the above plot one can understand that on weekdays the 5-minute intervals between 500 (5:00 am) and 1000 (10:00 am) have more steps than during the same time on weekends. This reflects that on weekdays the person starts his/her activities earlier than on weekend where they probably are sleeping in. ;-)

