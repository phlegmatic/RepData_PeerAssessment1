# Reproducible Research: Assignment 1


## Loading and preprocessing the data


```r
originalData <- read.csv("activity.csv")
```

First 5 rows of original data:

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
```

## What is mean total number of steps taken per day?

Creating dataset containing the total number of steps taken each day


```r
dailyStepSum <- aggregate(originalData$steps, list(originalData$date), sum)
```
First 5 rows of daily Step Sum data:

```
##         Date Steps
## 1 2012-10-01    NA
## 2 2012-10-02   126
## 3 2012-10-03 11352
## 4 2012-10-04 12116
## 5 2012-10-05 13294
```

2. Creating histogram of daily Step Data


```r
with(dailyStepSum, {
        par(oma=c(2,0,0,0), mar=c(6.75,6.75,3,0), mgp=c(5.75,0.75,0), las=2)
        barplot(
                height=Steps,
                main="Total Steps taken per Day",
                xlab="Date",
                ylab="Step count per Day",
                names.arg=Date,
                space=c(0)
        )
})
```

![](./PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

3. Calculating the mean and median values (ignoring NA values) using the daily step count dataset.

1. Mean

```r
dailyStepMean <- mean(dailyStepSum$Steps, na.rm=TRUE)
```

```
## [1] 10766.19
```
2. Median

```r
dailyStepMedian <- median(dailyStepSum$Steps, na.rm=TRUE)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

Finding the mean (average) steps taken for eatch 5-minute time interval averaged over all the days in the data-

1. Generating the mean (average) number of steps taken (ignoring NA values) for each 5-minute interval, itself averaged across all days.


```r
intervalSteps <- aggregate(
        data=originalData,
        steps~interval,
        FUN=mean,
        na.action=na.omit
)
colnames(intervalSteps) <- c("Interval", "AvgStepsAvgAcrossDay")
```
First 5 rows in new dataset:
        
        ```
        ##   Interval AvgStepsAvgAcrossDay
        ## 1        0            1.7169811
        ## 2        5            0.3396226
        ## 3       10            0.1320755
        ## 4       15            0.1509434
        ## 5       20            0.0754717
        ```

2. A Time-Series plot is created from the above dataset


```r
with(intervalSteps, {
        plot(
                x=Interval,
                y=AvgStepsAvgAcrossDay,
                type="l",
                main="Time-Series of Average StepCount against 5 min Interval",
                xlab="5-minute Interval",
                ylab="Average StepsCount (Averaged across all Days)"

        )
})
```

![](./PA1_template_files/figure-html/unnamed-chunk-12-1.png) 

3. Finding the 5-minute interval with the maximum number of steps


```r
intervalMax <- intervalSteps[intervalSteps$AvgStepsAvgAcrossDay==max(intervalSteps$AvgStepsAvgAcrossDay),]
```

```
##     Interval AvgStepsAvgAcrossDay
## 104      835             206.1698
```
Conclusion:

The interval between **835** and  **840** minutes has the maximum number of steps.


## Imputing missing values

The goal of this section is to generate a new graph using the same data as from the first section but with its NA values replaced.

Let's use mean (average) 5-minunte interval values from the previous section to replace the NA values.

1. Total number of rows with NA values in original data.


```r
countNA <- nrow(subset(originalData, is.na(originalData$steps)))
```

```
## [1] 2304
```


Rounding decimal to a whole number.


```r
stepValues <- data.frame(originalData$steps)
stepValues[is.na(stepValues),] <- ceiling(tapply(X=originalData$steps,INDEX=originalData$interval,FUN=mean,na.rm=TRUE))
newData <- cbind(stepValues, originalData[,2:3])
colnames(newData) <- c("Steps", "Date", "Interval")
```

5 rows of new dataset:

```
##   Steps       Date Interval
## 1     2 2012-10-01        0
## 2     1 2012-10-01        5
## 3     1 2012-10-01       10
## 4     1 2012-10-01       15
## 5     1 2012-10-01       20
```

3. The total number of steps taken each day is generated using this new dataset.


```r
newDailyStepSum <- aggregate(newData$Steps, list(newData$Date), sum)
```
5 rows of new dataset:

```
##         Date Steps
## 1 2012-10-01 10909
## 2 2012-10-02   126
## 3 2012-10-03 11352
## 4 2012-10-04 12116
## 5 2012-10-05 13294
```

4. A histogram of the above data is created as a form of visual representation.


```r
with(newDailyStepSum, {
        par(oma=c(2,0,0,0), mar=c(6.75,6.75,3,0), mgp=c(5.75,0.75,0), las=2)
        barplot(
                height=Steps,
                main="Graph of StepsCount per Day",
                xlab="Dates",
                ylab="StepCount per Day",
                names.arg=Date,
                space=c(0)
        )
})
```

![](./PA1_template_files/figure-html/unnamed-chunk-21-1.png) 

5. Calculate the mean and median values of this new dataset (NA values replaced with mean).

1. Mean

```r
newDailyStepMean <- mean(newDailyStepSum$Steps)
```

```
## [1] 10784.92
```
2. Median

```r
newDailyStepMedian <- median(newDailyStepSum$Steps)
```

```
## [1] 10909
```

Observation :
Adding the missing values to the original data has caused both the mean and median values to increase.

1. Mean:
        10766 to 10784

2. Median:
        10765 to 10909


## Are there differences in activity patterns between weekdays and weekends?
>For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.
1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was creating using simulated data:

        1.  A new column indicating whether the date is a weekday or a weekend is added to the new dataset created in the previous section.


```r
dateDayType <- data.frame(sapply(X=newData$Date, FUN=function(day) {
        if (weekdays(as.Date(day)) %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")) {
                day <- "weekday"
        }
        else {
                day <- "weekend"
        }
}))

newDataWithDayType <- cbind(newData, dateDayType)

colnames(newDataWithDayType) <- c("Steps", "Date", "Interval", "DayType")
```

First 5 rows of new data:
        
        ```
        ##   Steps       Date Interval DayType
        ## 1     2 2012-10-01        0 weekday
        ## 2     1 2012-10-01        5 weekday
        ## 3     1 2012-10-01       10 weekday
        ## 4     1 2012-10-01       15 weekday
        ## 5     1 2012-10-01       20 weekday
        ```
2. The data is then separated into weekday or weekend and the mean (average) number of steps taken for each 5-minute interval, itself averaged across all weekday days or weekend days is calculated.


```r
dayTypeIntervalSteps <- aggregate(
        data=newDataWithDayType,
        Steps ~ DayType + Interval,
        FUN=mean
)
```
First 5 rows of the dataset:

```
##   DayType Interval     Steps
## 1 weekday        0 2.2888889
## 2 weekend        0 0.2500000
## 3 weekday        5 0.5333333
## 4 weekend        5 0.1250000
## 5 weekday       10 0.2888889
```

3. Finally, a panel plot of both weekend and weekday graphs is generated.


```r
library("lattice")

xyplot(
        type="l",
        data=dayTypeIntervalSteps,
        Steps ~ Interval | DayType,
        xlab="Interval",
        ylab="StepsCount",
        layout=c(1,2)
)
```

![](./PA1_template_files/figure-html/unnamed-chunk-30-1.png) 
