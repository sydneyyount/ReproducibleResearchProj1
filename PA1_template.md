---
title: "PA_template"
author: "Sydney Yount"
date: "2/15/2022"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Reading in and Processing Data

Loading in necessary packages:
```{r}
library(ggplot2)
library(plyr)
library(lattice)
```
 
Reading the data set: 
```{r}
activity <- read.csv('activity.csv')
```
Processing the data:
```{r}
cleanActivity <- activity[!is.na(activity$steps),]

aggData <- aggregate(cleanActivity$steps ~ cleanActivity$date,FUN=sum)
names(aggData)[1] <- "date"
names(aggData)[2]<- "steps"
```

## Histogram of Total Number of Steps Each Day

```{r,aggData}
hist(aggData$steps,xlab='Total Steps',main = 'Total Number of Steps Per Day')
```

## Mean and Median Number of Steps Each Day
Mean: 
```{r}
mean(aggData$steps)
```
Median:
```{r}
median(aggData$steps)
```
## Time Series Plot of Average Number of Steps Taken
Subsetting data by interval
```{r}
aggInt <- aggregate(cleanActivity$steps ~ cleanActivity$interval,FUN=mean)
names(aggInt)[1] <- "interval"
names(aggInt)[2]<- "steps"
```
Plotting of intervals and average number of steps taken 
```{r}
plot(aggInt$interval,aggInt$steps,type='l',main='Average Daily Activity Pattern',xlab = 'Interval',ylab='Average # of Steps')
```
Determining 5 minute interval that contains maximum number of steps:
```{r}
avg = max(aggInt$steps)
aggInt[aggInt$steps == avg,1]
```
# Imputing Missing Value
Determining number of NA's in data set
```{r}
sum(is.na(activity$steps))
```
Filling in missing data with mean value of data set
```{r}
newAct <- activity
newAct[is.na(newAct$steps),1] <- mean(aggInt$steps)
```
Make new histogram of steps taken each day and determining mean and median
```{r}
newAggData <- aggregate(newAct$steps ~ newAct$date,FUN=sum)
names(newAggData)[1] <- "date"
names(newAggData)[2]<- "steps"
hist(newAggData$steps,xlab='Total Steps',main = 'Total Number of Steps Per Day')
```
Determining new mean and median of data set
Mean:
```{r}
mean(newAggData$steps)
```
Median:
```{r}
median(newAggData$steps)
```
Inputting the new missing values had no affect on the mean, but changed the median value to match that of the mean. 

## Patterns between Weekends Versus Weekdays
Creating new factor variable that determines if day is a weekend or weekday
```{r}
newAct$type <-weekdays(as.Date(newAct$date))
newAct$type <-ifelse(newAct$type %in% c('Saturday','Sunday'),'Weekend','Weekday')
```
Making Panel Plot to show the average number of steps acrosss all weekend days or weekdays.
```{r}
newAggData2 <- aggregate(newAct$steps, newAct[c('interval','type')],FUN=mean)
xyplot(newAggData2$x~ newAggData2$interval|newAggData2$type,type = 'l', layout=c(1,2),main = 'Avg Steps Per Interval by Day Type',ylab = 'Avg # of Steps',xlab = 'Interval')