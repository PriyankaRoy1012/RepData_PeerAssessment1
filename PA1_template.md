---
title: "Reproducible Research Project 1"
author: "Priyanka Roy"
date: "February 10, 2017"
output: html_document
---
Reading the required packages

```r
library(plyr)
library(ggplot2)
library(lattice)
```

Reading the data:

```r
activity <- read.csv("D:/Priyanka/Coursera/5.ReproducibleResearch/activity.csv")
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

Calculating total number of steps taken per day, its mean and median as well

```r
activity_sum <- ddply(activity, "date",summarise, StepsTaken = sum(steps))
activity_sum <- na.omit(activity_sum)
hist(activity_sum$StepsTaken, main = "Steps taken per day", col = "red", xlab = "Number of steps")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

```r
mean(activity_sum$StepsTaken)
```

```
## [1] 10766.19
```

```r
median(activity_sum$StepsTaken)
```

```
## [1] 10765
```

Average daily activity pattern

```r
interval_max <- ddply(activity, "interval", summarise, Max_Interval = mean(steps, na.rm = TRUE))
plot(interval_max$interval, interval_max$Max_Interval, type = "l", main = "Average Steps on interval")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

```r
interval_max$interval[interval_max$Max_Interval ==  max(interval_max$Max_Interval)]
```

```
## [1] 835
```

Imputing Missing values:
Total number of missing vlues is:

```r
sum(is.na(activity))
```

```
## [1] 2304
```
Strategy- I am replacing all the null values by the average number of steps taken in the corresponding 5-minute interval
as calculated in interval_max table. New dataset is below:

```r
activity_imputed <- activity
for(i in 1:nrow(activity_imputed)){
  activity_imputed$steps[i]<- ifelse(is.na(activity_imputed$steps[i]),
                              interval_max$Max_Interval[interval_max$interval== activity_imputed$interval[i]],
                              activity_imputed$steps[i])
}
activity_imputed$steps <- round(activity_imputed$steps,0)
```

plot and comparison of new data with old data

```r
act_imp_sum <-  ddply(activity_imputed, "date",summarise, StepsTaken = sum(steps))
hist(act_imp_sum$StepsTaken, main = "Steps taken per day by Imputed data", col = "red", xlab = "Number of steps")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)

```r
mean(act_imp_sum$StepsTaken)
```

```
## [1] 10765.64
```

```r
median(act_imp_sum$StepsTaken)
```

```
## [1] 10762
```
After imputing the data, mean is almost the same. However, median got reduced by 4 units.

Differences in activity patterns between weekdays and weekends
Week Factor created:

```r
activity_imputed$NewDate <- weekdays(as.Date(activity_imputed$date))
activity_imputed$Week <- ifelse(activity_imputed$NewDate %in% c("Sunday","Saturday"),"weekend", "weekdays")
```
plot for weekend/weekday

```r
activity_imputed_agg <- ddply(activity_imputed, c("Week","interval"),summarise, Steps = mean(steps, na.rm = TRUE))
xyplot(Steps ~ interval | Week,data = activity_imputed_agg, type = "l", xlab = "Interval", ylab = "Steps Taken",layout = c(1,2), main = "Average Steps taken in 5-minute interval(Weekends/Weekdays)")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png)



