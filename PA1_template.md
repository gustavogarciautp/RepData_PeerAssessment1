---
title: "Peer Assessment 1"
author: "Gustavo García"
date: "10/9/2020"
output: 
  html_document:
    keep_md: true
---



## Loading the data

```r
#Name file downloaded
name <- "activity.zip"

# Check if file already exists
if (!file.exists(name)){
  URL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
  download.file(URL, name, method="curl")
} 

if (!file.exists("activity.csv")) { 
  unzip(name) 
}
```

## Reading the data

```r
activity<- read.csv("activity.csv")
```


## 1. Preprocessing the data

```r
library(ggplot2)
library(lubridate)
activity<- transform(activity, date= as.Date(date, format="%Y-%m-%d"))
```

## 2. Histogram of the total number of steps taken each day


```r
p1<- ggplot(data=activity, aes(x=date, y=steps)) + geom_bar(stat="identity", fill="steelblue") + labs(title = "Total number of steps taken each day")
p1
```

```
## Warning: Removed 2304 rows containing missing values (position_stack).
```

![](PA1_template_files/figure-html/plot1-1.png)<!-- -->

```r
ggsave("figure/plot1.png",p1)
```

```
## Saving 7 x 5 in image
```

```
## Warning: Removed 2304 rows containing missing values (position_stack).
```

## 3. Mean and median number of steps taken each day


```r
aggregate(steps~date, activity, FUN = function(x) c(mean = mean(x), median = median(x)))
```

```
##          date steps.mean steps.median
## 1  2012-10-02  0.4375000    0.0000000
## 2  2012-10-03 39.4166667    0.0000000
## 3  2012-10-04 42.0694444    0.0000000
## 4  2012-10-05 46.1597222    0.0000000
## 5  2012-10-06 53.5416667    0.0000000
## 6  2012-10-07 38.2465278    0.0000000
## 7  2012-10-09 44.4826389    0.0000000
## 8  2012-10-10 34.3750000    0.0000000
## 9  2012-10-11 35.7777778    0.0000000
## 10 2012-10-12 60.3541667    0.0000000
## 11 2012-10-13 43.1458333    0.0000000
## 12 2012-10-14 52.4236111    0.0000000
## 13 2012-10-15 35.2048611    0.0000000
## 14 2012-10-16 52.3750000    0.0000000
## 15 2012-10-17 46.7083333    0.0000000
## 16 2012-10-18 34.9166667    0.0000000
## 17 2012-10-19 41.0729167    0.0000000
## 18 2012-10-20 36.0937500    0.0000000
## 19 2012-10-21 30.6284722    0.0000000
## 20 2012-10-22 46.7361111    0.0000000
## 21 2012-10-23 30.9652778    0.0000000
## 22 2012-10-24 29.0104167    0.0000000
## 23 2012-10-25  8.6527778    0.0000000
## 24 2012-10-26 23.5347222    0.0000000
## 25 2012-10-27 35.1354167    0.0000000
## 26 2012-10-28 39.7847222    0.0000000
## 27 2012-10-29 17.4236111    0.0000000
## 28 2012-10-30 34.0937500    0.0000000
## 29 2012-10-31 53.5208333    0.0000000
## 30 2012-11-02 36.8055556    0.0000000
## 31 2012-11-03 36.7048611    0.0000000
## 32 2012-11-05 36.2465278    0.0000000
## 33 2012-11-06 28.9375000    0.0000000
## 34 2012-11-07 44.7326389    0.0000000
## 35 2012-11-08 11.1770833    0.0000000
## 36 2012-11-11 43.7777778    0.0000000
## 37 2012-11-12 37.3784722    0.0000000
## 38 2012-11-13 25.4722222    0.0000000
## 39 2012-11-15  0.1423611    0.0000000
## 40 2012-11-16 18.8923611    0.0000000
## 41 2012-11-17 49.7881944    0.0000000
## 42 2012-11-18 52.4652778    0.0000000
## 43 2012-11-19 30.6979167    0.0000000
## 44 2012-11-20 15.5277778    0.0000000
## 45 2012-11-21 44.3993056    0.0000000
## 46 2012-11-22 70.9270833    0.0000000
## 47 2012-11-23 73.5902778    0.0000000
## 48 2012-11-24 50.2708333    0.0000000
## 49 2012-11-25 41.0902778    0.0000000
## 50 2012-11-26 38.7569444    0.0000000
## 51 2012-11-27 47.3819444    0.0000000
## 52 2012-11-28 35.3576389    0.0000000
## 53 2012-11-29 24.4687500    0.0000000
```

## 4. Time series plot of the average number of steps taken

```r
df<- aggregate(steps~interval, activity,mean)
p2<- ggplot(data=df, aes(x=interval, y=steps))+ geom_line()+ labs(title = "Time series plot of the average number of steps taken", y="Average number of steps")
p2
```

![](PA1_template_files/figure-html/plot2-1.png)<!-- -->

```r
ggsave("figure/plot2.png",p2)
```

```
## Saving 7 x 5 in image
```

## 5. The 5-minute interval that, on average, contains the maximum number of steps

```r
df[which.max(df$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```

## 6. Code to describe and show a strategy for imputing missing data

1.  Total number of missing values in the dataset

```r
sum(!complete.cases(activity))
```

```
## [1] 2304
```

2. Estrategy: Use the mean for that 5-minute interval


```r
NASteps<- which(is.na(activity$steps))
activity[NASteps,"steps"]<- sapply(activity[NASteps,"interval"],function(x) (df[which(df$interval==x),"steps"]))
```

## 7. Histogram of the total number of steps taken each day after missing values are imputed


```r
p3<- ggplot(data=activity, aes(x=date, y=steps))+geom_bar(stat="identity", fill="steelblue") + labs(title="Total number of steps taken each day")
p3
```

![](PA1_template_files/figure-html/plot3-1.png)<!-- -->

```r
ggsave("figure/plot3.png",p3)
```

```
## Saving 7 x 5 in image
```

Mean and median number of steps taken each day


```r
aggregate(steps~date, activity, FUN = function(x) c(mean = mean(x), median = median(x)))
```

```
##          date steps.mean steps.median
## 1  2012-10-01 37.3825996   34.1132075
## 2  2012-10-02  0.4375000    0.0000000
## 3  2012-10-03 39.4166667    0.0000000
## 4  2012-10-04 42.0694444    0.0000000
## 5  2012-10-05 46.1597222    0.0000000
## 6  2012-10-06 53.5416667    0.0000000
## 7  2012-10-07 38.2465278    0.0000000
## 8  2012-10-08 37.3825996   34.1132075
## 9  2012-10-09 44.4826389    0.0000000
## 10 2012-10-10 34.3750000    0.0000000
## 11 2012-10-11 35.7777778    0.0000000
## 12 2012-10-12 60.3541667    0.0000000
## 13 2012-10-13 43.1458333    0.0000000
## 14 2012-10-14 52.4236111    0.0000000
## 15 2012-10-15 35.2048611    0.0000000
## 16 2012-10-16 52.3750000    0.0000000
## 17 2012-10-17 46.7083333    0.0000000
## 18 2012-10-18 34.9166667    0.0000000
## 19 2012-10-19 41.0729167    0.0000000
## 20 2012-10-20 36.0937500    0.0000000
## 21 2012-10-21 30.6284722    0.0000000
## 22 2012-10-22 46.7361111    0.0000000
## 23 2012-10-23 30.9652778    0.0000000
## 24 2012-10-24 29.0104167    0.0000000
## 25 2012-10-25  8.6527778    0.0000000
## 26 2012-10-26 23.5347222    0.0000000
## 27 2012-10-27 35.1354167    0.0000000
## 28 2012-10-28 39.7847222    0.0000000
## 29 2012-10-29 17.4236111    0.0000000
## 30 2012-10-30 34.0937500    0.0000000
## 31 2012-10-31 53.5208333    0.0000000
## 32 2012-11-01 37.3825996   34.1132075
## 33 2012-11-02 36.8055556    0.0000000
## 34 2012-11-03 36.7048611    0.0000000
## 35 2012-11-04 37.3825996   34.1132075
## 36 2012-11-05 36.2465278    0.0000000
## 37 2012-11-06 28.9375000    0.0000000
## 38 2012-11-07 44.7326389    0.0000000
## 39 2012-11-08 11.1770833    0.0000000
## 40 2012-11-09 37.3825996   34.1132075
## 41 2012-11-10 37.3825996   34.1132075
## 42 2012-11-11 43.7777778    0.0000000
## 43 2012-11-12 37.3784722    0.0000000
## 44 2012-11-13 25.4722222    0.0000000
## 45 2012-11-14 37.3825996   34.1132075
## 46 2012-11-15  0.1423611    0.0000000
## 47 2012-11-16 18.8923611    0.0000000
## 48 2012-11-17 49.7881944    0.0000000
## 49 2012-11-18 52.4652778    0.0000000
## 50 2012-11-19 30.6979167    0.0000000
## 51 2012-11-20 15.5277778    0.0000000
## 52 2012-11-21 44.3993056    0.0000000
## 53 2012-11-22 70.9270833    0.0000000
## 54 2012-11-23 73.5902778    0.0000000
## 55 2012-11-24 50.2708333    0.0000000
## 56 2012-11-25 41.0902778    0.0000000
## 57 2012-11-26 38.7569444    0.0000000
## 58 2012-11-27 47.3819444    0.0000000
## 59 2012-11-28 35.3576389    0.0000000
## 60 2012-11-29 24.4687500    0.0000000
## 61 2012-11-30 37.3825996   34.1132075
```
## 8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends


```r
weektype <- function(date){
  if (weekdays(date) %in% c("Saturday","sunday","sabado","domingo"))
    type<- "weekend"
  else{
    type<- "weekday"
  }
   type
}
activity$type<- factor(sapply(activity$date, weektype))
df2<- aggregate(steps~interval+type, activity,mean)
p4<- ggplot(data=df2, aes(x=interval, y= steps))+facet_wrap(type~., nrow=2, ncol=1)+ geom_line()
p4
```

![](PA1_template_files/figure-html/plot4-1.png)<!-- -->

```r
ggsave("figure/plot4.png",p4)
```

```
## Saving 7 x 5 in image
```
