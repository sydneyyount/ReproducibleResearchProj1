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
![image](https://user-images.githubusercontent.com/92326165/154482583-942d4d3f-d281-4573-b94c-019ab4b11f8a.png)


## Mean and Median Number of Steps Each Day
Mean: 
```{r}
mean(aggData$steps)
```
Median:
```{r}
median(aggData$steps)
```
![image](https://user-images.githubusercontent.com/92326165/154482648-ac89090e-1615-489d-a8b5-fc016a6dd921.png)

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
![image](https://user-images.githubusercontent.com/92326165/154482710-a3058835-710e-4e2c-ba47-d858d9e14d6f.png)

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
![image](https://user-images.githubusercontent.com/92326165/154482786-ac39a02b-16a5-4e05-8aa1-873341d302b9.png)

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
![image](https://user-images.githubusercontent.com/92326165/154482844-4b9af548-0103-459c-8149-53044cbbcca0.png)

Determining new mean and median of data set
Mean:
```{r}
mean(newAggData$steps)
```
Median:
```{r}
median(newAggData$steps)
```
![image](https://user-images.githubusercontent.com/92326165/154482885-2b28e1c6-a8ce-477d-bcf4-096b57e33b7c.png)

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
```
![image](https://user-images.githubusercontent.com/92326165/154482949-9fed7187-c19d-4cf8-8b4d-2a89c3d03c42.png)
