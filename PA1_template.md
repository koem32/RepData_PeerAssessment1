---
title: "FirstAssignment"
author: "koem32"
date: "July 17, 2014"
output: html_document
---
Proprocessing stage: We first read the table. 


```r
data<-read.csv("activity.csv", colClasses=c("numeric","character","numeric"))
```
I want to make a histogram of the total number of steps taken each day. I then make a new table that will have a column of days and a column that sums the number of steps across all intervals for each day. Then I make the histogram and also calculate mean and median

```r
stepsperday<-rowsum(data$steps,data$date,na.rm=TRUE)
head(stepsperday)
```

```
##             [,1]
## 2012-10-01     0
## 2012-10-02   126
## 2012-10-03 11352
## 2012-10-04 12116
## 2012-10-05 13294
## 2012-10-06 15420
```

```r
hist(stepsperday, main="Steps Histogram",xlab = "Steps" )
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

```r
mean(stepsperday)
```

```
## [1] 9354
```

```r
median(stepsperday)
```

```
## [1] 10395
```
To average the five minute intervals across days I am going to make a vector using tapply where FUN is 'mean' and my factor is the interval. 

```r
avStepsPerInterval<-tapply(data$steps,data$interval,mean,na.rm=T)
plot(avStepsPerInterval, type="l",xlab="interval", ylab="Average steps per day for interval")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 
 To find the interval with the maximum number of steps on average I transform my vector into a table and subset it for the maximum steps.I next pull that value out from the subset just for completeness

```r
v<-cbind(read.table(text = names(avStepsPerInterval)), avStepsPerInterval)
f<-subset(v,avStepsPerInterval==max(avStepsPerInterval))
f[1]
```

```
##      V1
## 835 835
```
Before umputing the missing values I am going to count how many there are

```r
sum(is.na(data))
```

```
## [1] 2304
```
Now we need to replace the NA with imputed values. I will use the v dataframe that we created earlier to do this i.e. I will be replacing interval with missing steps values with the average number of steps for that interval over the two month period. First I give names to the columns in v and then merge it with data (our orginal dataframe) to get a dataframe with an extra column of the means. Next I replace all NAs under the Steps column with the means from the AverageSteps column and then recreate the table. 

```r
colnames(v)[1]<-"interval"
addimpute<-merge(data,v,by="interval")
my.na<-is.na(addimpute$steps)
addimpute$steps[my.na]<-addimpute$avStepsPerInterval[my.na]
head(addimpute)
```

```
##   interval steps       date avStepsPerInterval
## 1        0 1.717 2012-10-01              1.717
## 2        0 0.000 2012-11-23              1.717
## 3        0 0.000 2012-10-28              1.717
## 4        0 0.000 2012-11-06              1.717
## 5        0 0.000 2012-11-24              1.717
## 6        0 0.000 2012-11-15              1.717
```

```r
newdata<-addimpute[,c("steps", "date", "interval")]
head(newdata)
```

```
##   steps       date interval
## 1 1.717 2012-10-01        0
## 2 0.000 2012-11-23        0
## 3 0.000 2012-10-28        0
## 4 0.000 2012-11-06        0
## 5 0.000 2012-11-24        0
## 6 0.000 2012-11-15        0
```
Make histogram as before

```r
newStepsPerDay<-rowsum(newdata$steps,newdata$date)
hist(newStepsPerDay, main="Steps Histogram with NAs replaced by interval means",xlab = "Steps" )
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 

```r
mean(newStepsPerDay)
```

```
## [1] 10766
```

```r
median(newStepsPerDay)
```

```
## [1] 10766
```
So we see that there the histogram looks the same but the  mean and median are different from before and equal to eachother. 

Next we need to add a column factor of wether a date is a weekend or weekday. We use the 'newdata' table which is the one without NAs. I change my Date column to as.Date format and apply the weekday function to it. I bind the newly created column to newdata to create newdata2. I then create a new column called week and apply condiditonals on the dayOfWeek column

```r
dayOfWeek<-weekdays(as.Date(newdata$date))
newdata2<-cbind(newdata,dayOfWeek)
newdata2$week <- ifelse(newdata2$dayOfWeek =="Saturday" | newdata2$dayOfWeek =="Sunday","Weekend", "Weekday")
newdata2$week<-as.factor(newdata2$week)
head(newdata2)
```

```
##   steps       date interval dayOfWeek    week
## 1 1.717 2012-10-01        0    Monday Weekday
## 2 0.000 2012-11-23        0    Friday Weekday
## 3 0.000 2012-10-28        0    Sunday Weekend
## 4 0.000 2012-11-06        0   Tuesday Weekday
## 5 0.000 2012-11-24        0  Saturday Weekend
## 6 0.000 2012-11-15        0  Thursday Weekday
```
Next I need to find the average number of steps taken per interval on weekends and weekdays. I use tapply as before to get a table with two columns: one for average steos taken per interval on weekday and one for weekend. I then create two graphs using the base plotting system

```r
uu<-tapply(newdata2$steps,list(newdata2$interval,newdata2$week),mean)
uu<-data.frame(uu)
vV<-cbind(read.table(text = names(uu)), uu)
par(mfrow = c(2, 1))
with(vV, {
plot(Weekday, main = "Weekday",type="l",xlab="Interval",ylab="Av.Steps")
plot(Weekend, main = "Weekend",type="l",xlab="Interval",ylab="Av.Steps")
})
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9.png) 
