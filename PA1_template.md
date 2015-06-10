---
title: "PA1_template.Rmd"
author: 
date: "Tuesday, June 09, 2015"
output: html_document
---

In the following, I have not repeated those parts of the instructions that were explanatory rather providing exact instructions.



#Loading and preprocessing the data

Show any code that is needed to load the data (i.e. read.csv())


```r
library(dplyr)
activity <- unzip("repdata-data-activity.zip")
activity.data <- read.csv(activity)
```
##Calculate the total number of steps taken per day


```r
steps.total <- activity.data %>%
  group_by(date) %>%
  summarise(total.steps = sum(steps, na.rm = T))
```

##Make a histogram of the total number of steps taken each day

```r
hist(steps.total$total.steps, xlab= "Steps by Day", 
     ylab ="Frequency of total", 
     main = "Histogram of Total Number of Steps taken each day")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

##Calculate and report the mean and median of the total number of steps taken per day

```r
steps.mean <- activity.data %>%
  group_by(date) %>%
  summarise(steps.mean = mean(steps, na.rm=T))

steps.median <- activity.data %>%
  group_by(date) %>%
  summarise(median.steps = median(steps, na.rm=T))

summary(steps.mean)
```

```
##          date      steps.mean     
##  2012-10-01: 1   Min.   : 0.1424  
##  2012-10-02: 1   1st Qu.:30.6979  
##  2012-10-03: 1   Median :37.3785  
##  2012-10-04: 1   Mean   :37.3826  
##  2012-10-05: 1   3rd Qu.:46.1597  
##  2012-10-06: 1   Max.   :73.5903  
##  (Other)   :55   NA's   :8
```

```r
summary(steps.median)
```

```
##          date     median.steps
##  2012-10-01: 1   Min.   :0    
##  2012-10-02: 1   1st Qu.:0    
##  2012-10-03: 1   Median :0    
##  2012-10-04: 1   Mean   :0    
##  2012-10-05: 1   3rd Qu.:0    
##  2012-10-06: 1   Max.   :0    
##  (Other)   :55   NA's   :8
```

#What is the average daily activity pattern?

Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis)
and the average number of steps taken, averaged across all days (y-axis)

##average 5 minute intervals across days

```r
interval.mean <- activity.data %>%
  group_by(interval) %>%
  summarise(interval.steps = mean(steps, na.rm=T))
```

## make the plot


```r
plot(interval.mean$interval, interval.mean$interval.steps, type ="l", xlab ="Interval", ylab="steps")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 

  
or qplot (instructions seemed specific to plot)


```r
library(ggplot2)
qplot(interval.mean$interval, interval.mean$interval.steps, geom ="line", xlab ="Interval", ylab="steps")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png) 

   

#Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

The following is adapted from [Stack Overflow](http://stackoverflow.com/questions/9981224/search-for-index-of-a-list-entry-in-r)



```r
find.steps <- function(val)interval.mean$interval[match(val, interval.mean$interval.steps)]

max.steps <-max(interval.mean$interval.steps, na.rm="T")
```
##Imputing missing values


#Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(activity.data))
```

```
## [1] 2304
```

Devise a strategy for filling in all of the missing values in the dataset. 
The strategy does not need to be sophisticated. 
For example, you could use the mean/median for that day, 
or the mean for that 5-minute interval, etc.

The strategy used below is adapted from [r-help](http://www.mail-archive.com/r-help@r-project.org/msg58289.html). The mean number of steps per 5 minute interval.

##Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
steps.imputed <- impute.mean(activity.data$steps)
act.data2 <- activity.data
act.data2$steps <- steps.imputed
```
##Make a histogram of the total number of steps taken each day and 


```r
steps.total2 <- act.data2 %>%
  group_by(date) %>%
  summarise(total.steps = sum(steps, na.rm = T))

hist(steps.total2$total.steps, xlab= "Steps by Day", 
     ylab ="Frequency of total", 
     main = "Histogram of Total Number of Steps (with imputed steps)")
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png) 
#Calculate and report the mean and median total number of steps taken per day. 

```r
steps.mean2 <- act.data2 %>%
  group_by(date) %>%
  summarise(steps.mean = mean(steps, na.rm=T))

steps.median2 <- act.data2 %>%
  group_by(date) %>%
  summarise(median.steps = median(steps, na.rm=T))

summary(steps.mean2)
```

```
##          date      steps.mean     
##  2012-10-01: 1   Min.   : 0.1424  
##  2012-10-02: 1   1st Qu.:34.0938  
##  2012-10-03: 1   Median :37.3826  
##  2012-10-04: 1   Mean   :37.3826  
##  2012-10-05: 1   3rd Qu.:44.4826  
##  2012-10-06: 1   Max.   :73.5903  
##  (Other)   :55
```

```r
summary(steps.median2)
```

```
##          date     median.steps   
##  2012-10-01: 1   Min.   : 0.000  
##  2012-10-02: 1   1st Qu.: 0.000  
##  2012-10-03: 1   Median : 0.000  
##  2012-10-04: 1   Mean   : 4.903  
##  2012-10-05: 1   3rd Qu.: 0.000  
##  2012-10-06: 1   Max.   :37.383  
##  (Other)   :55
```
##Do these values differ from the estimates from the first part of the assignment? 
Yes. We can see this by considering the summary data of the first and second part of the assignment. The difference can be seen in the 1st and 3rd quartile values.

###Comparing Mean data

Stat   |part 1   | part 2
-------|---------|---------
Min.   | 0.1424  | 0.1424  
1st Qu.|30.6979  | 34.0938  
Median |37.3785  | 37.3826  
Mean   |37.3826  | 37.3826  
3rd Qu.|46.1597  | 44.4826  
Max.   |73.5903  | 73.5903
NA's   |8        | 0 



#What is the impact of imputing missing data on the estimates of the total daily number of steps?

The reports curves becomes more normal; the curve is a more regular bell-shape.

##Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. 
Use the dataset with the filled-in missing values for this part.

Create a new factor variable in the dataset with two levels 
- "weekday" and "weekend" indicating whether a given date is 
a weekday or weekend day.

```r
dayofweek <- weekdays(as.POSIXlt(act.data2$date))
wkd <- as.logical(dayofweek != "Saturday" & dayofweek !="Sunday")

act.data2$weekday<- 
  factor(wkd+1L, levels=1:2,labels= c("weekday","weekend"))

weekday.mean <- act.data2[act.data2$weekday=="weekday",] %>%
    group_by(interval) %>%
    summarise(interval.steps = mean(as.integer(steps, na.rm=T)))

weekend.mean <- act.data2[act.data2$weekday=="weekend",] %>%
    group_by(interval) %>%
    summarise(interval.steps = mean(as.integer(steps, na.rm=T)))
```

#Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 

```r
par(mfrow= c(2,1), mar = c(2,0,2,0))
plot(weekday.mean, type="l", xlab= "5 Minute Interval", ylab="Average Number of Steps", main ="Weekday Data")
plot(weekend.mean, type="l",xlab= "5 Minute Interval", ylab="Average Number of Steps", main ="Weekend Data")
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14-1.png) 

