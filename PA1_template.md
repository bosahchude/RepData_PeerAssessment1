# Analysis of daily human personal activity
Bosah Chude  
`r format(Sys.Date(), format='%B %d, %Y.')`  


## Loading and preprocessing the data
First off, all the required libraries are loaded into the workspace.

```r
library(lubridate)
library(ggplot2)
library(gridExtra)
library(dplyr)
```
Next, `activity.zip` is unzipped, its content is loaded into a data frame.

```r
unzip("activity.zip")
activityData <- read.csv("activity.csv", stringsAsFactors = FALSE)

#Use lubridate to convert the strings to an object of date class.
activityData$date <- ymd(activityData$date)

#Previewing the data.
head(activityData, 3)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
```

## What is the mean total number of steps taken per day?
Histogram of the total number of steps taken each day, Plotted using `ggplot2` library

```r
#Get the daily sum
sumActivityData <- tapply(activityData$steps, activityData$date, sum, na.rm = TRUE)
qplot(x = ymd(row.names(sumActivityData)), y = sumActivityData) + geom_histogram(stat="identity") +
     labs(x = "", y = "Number of Steps", title = "Total Number of Steps Taken Each Day")
```

<img src="./PA1_template_files/figure-html/unnamed-chunk-3-1.png" title="" alt="" style="display: block; margin: auto;" />

Calculating the median and median

```r
#mean
mean(sumActivityData)
```

```
## [1] 9354.23
```

```r
#median
median(sumActivityData)
```

```
## [1] 10395
```

## What is the average daily activity pattern?
Using `tapply`, the mean for each interval with the missing values removed is computed.

The result is saved in a new data frame `meanDF`. This new data frame is plotted below as a time series.

```r
meanActivityData <- tapply(activityData$steps, activityData$interval, mean, na.rm = TRUE)
meanDF <- as.data.frame.array(meanActivityData)
colnames(meanDF) <- "steps"
meanDF <- mutate(meanDF, interval = as.numeric(row.names(meanDF)))

#Plot the steps of meanDF over time
ggplot(data = meanDF, aes(x = interval, y = steps)) + geom_line() +
     labs(x = "Intervals", y = "Steps", title = "Average Daily Activity Pattern")
```

<img src="./PA1_template_files/figure-html/unnamed-chunk-5-1.png" title="" alt=""  />

Using the `which.max` function, we can easily determine the maximum number of steps accorss all five minute intervals to be `206.1698`. It occurred at the `835` interval.

```r
#Get Maximun Row
meanActivityData[which.max(meanActivityData)]
```

```
##      835 
## 206.1698
```

## Imputing missing values
Computing the total number of missing values in the data set

```r
#Total Missing Values
sum(!complete.cases(activityData))
```

```
## [1] 2304
```

All missing values from the data set would be replaced with the average value from corresponding
intervals of the other days

```r
missingActivityData <- activityData[!complete.cases(activityData),]
for(i in 1:nrow(missingActivityData)) {
     #Replace NAs with the mean
    missingActivityData[i,][1] <- meanActivityData[as.character(missingActivityData[i,][3])]    
}

#Merge the new data frame with the complete cases of the old one.
missingActivityData <- rbind(missingActivityData, activityData[complete.cases(activityData),])

#Sneak peek of newly created data frame with missing values in place.
head(missingActivityData, 3)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
```

Histogram of all the steps in the new data frame.

```r
#Get the daily sum
sumActivityData <- tapply(missingActivityData$steps, missingActivityData$date, sum, na.rm = TRUE)
qplot(x = ymd(row.names(sumActivityData)), y = sumActivityData) + geom_histogram(stat="identity") +
     labs(x = "", y = "Number of Steps", title = "Total Number of Steps Taken Each Day (No Missing Values)")
```

<img src="./PA1_template_files/figure-html/unnamed-chunk-9-1.png" title="" alt="" style="display: block; margin: auto;" />

Calculating the mean and median of the new data frame.

```r
#Mean
mean(sumActivityData)
```

```
## [1] 10766.19
```

```r
#Median
median(sumActivityData)
```

```
## [1] 10766.19
```
These values differs by little amounts from when there where missing values in the data.

Imputing missing data has not has only affected the mean and median marginally. This is because the replacement for the missing values were determined via a measure of central tendency (i.e. the mean).

## Are there differences in activity patterns between weekdays and weekends?
A new column `week` is added to the data frame to signify which day of the week each specific row falls on.

This data frame is then split into two. Weekends contains Saturdays and Sundays. Weekdays contains all days except Saturdays and Sundays.

```r
mutatedActivityData <- mutate(activityData, week = weekdays(date))
head(mutatedActivityData, 3)
```

```
##   steps       date interval   week
## 1    NA 2012-10-01        0 Monday
## 2    NA 2012-10-01        5 Monday
## 3    NA 2012-10-01       10 Monday
```

```r
#Get Weekends
weekendActivityData <- filter(mutatedActivityData, week == "Saturday" | week == "Sunday" )
#Get Weekdays
weekdayActivityData <- filter(mutatedActivityData, week != "Saturday" & week != "Sunday" )
```

Here, the mean steps across all intervals for weekdays and weekends are calculated separately, the results are stored in different data frames.

```r
#Get the mean steps across intervals for weekdays and save in a new data frame
weekdaySummary <- tapply(weekdayActivityData$steps, weekdayActivityData$interval, mean, na.rm = TRUE)
weekdayDF <- as.data.frame.array(weekdaySummary)
colnames(weekdayDF) <- "steps"
weekdayDF <- mutate(weekdayDF, interval = as.numeric(row.names(weekdayDF)))

#Do the same thing for weekends
weekendSummary <- tapply(weekendActivityData$steps, weekendActivityData$interval, mean, na.rm = TRUE)
weekendDF <- as.data.frame.array(weekendSummary)
colnames(weekendDF) <- "steps"
weekendDF <- mutate(weekendDF, interval = as.numeric(row.names(weekendDF)))
```


Using the `ggplot2` and `gridExtra` libraries, the time interval plots for the weekdays and weekends are plotted on separate rows.

The noticeable difference between weekdays and weekends is that on weekends, the number of steps have a lower peak but steadily higher average than weekdays. 


```r
plot1 <- ggplot(data = weekdayDF, aes(x = interval, y = steps)) + geom_line() + ylim(-0, 245) +
     labs(x = "Intervals", y = "Steps", title = "Avreage Weekday Activity Pattern")

plot2 <- ggplot(data = weekendDF, aes(x = interval, y = steps)) + geom_line() + ylim(-0, 245) +
     labs(x = "Intervals", y = "Steps", title = "Average Weekend Activity Pattern")

#Partition the screen into two rows and plot the data.
grid.arrange(plot1, plot2, nrow = 2, ncol = 1)
```

<img src="./PA1_template_files/figure-html/unnamed-chunk-13-1.png" title="" alt="" style="display: block; margin: auto;" />






