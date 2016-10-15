
## Configure environment 
require(dplyr)
require(lubridate)
require(reshape2)
require(ggplot2)

setwd("D:/Dropbox/Education/CourseRA/Data_Science-JHU/05-Reproducible Research/Dev/Project01")

## ----------------------------------
## 1: Code for reading in the dataset and/or processing the data
## ----------------------------------
unzip("ActivityMonitorData.zip",exdir = "data")
AMD <- read.csv("data/activity.csv", stringsAsFactors=FALSE)
AMD$date <- ymd(AMD$date)

## summary(AMD)
## str(AMD)

## Transform data frame to prep for casting by date 
AMD_ETL_Date <- melt(AMD, id.vars="date", measure.vars="steps", na.rm=FALSE)

## Aggregate the Activity Date 
AMDFinalDate <- dcast(AMD_ETL_Date, date ~ variable, sum)

## ----------------------------------
## 2: Histogram of the total number of steps taken each day
## ----------------------------------
plot(AMDFinalDate$date, AMDFinalDate$steps, type="h", main="Histogram of Daily Steps", xlab="Date", ylab="Steps per Day", col="blue", lwd=8)

## ----------------------------------
## 3: Mean and median number of steps taken each day
## ----------------------------------
paste("Mean Steps per Day =", mean(AMDFinalDate$steps, na.rm=TRUE))
paste("Median Steps per Day =", median(AMDFinalDate$steps, na.rm=TRUE))

## ----------------------------------
## 4: Time series plot of the average number of steps taken
## ----------------------------------

## 1. Calculating Avg. Steps:
AMD_Interval<- AMD%>%
           group_by(interval)%>%
           filter(!is.na(steps))%>%
           summarise(avg_steps = mean(steps, na.rm=TRUE))

ggplot(AMD_Interval, aes(x =interval , y=avg_steps)) +
    geom_line(color="blue", size=1) +
    labs(title = "Avg. Daily Steps", x = "Interval", y = "Avg. Steps per day")

## ----------------------------------
## 5: The 5-minute interval that, on average, contains the maximum number of steps
## ----------------------------------

AMD_Interval[which.max(AMD_Interval$avg_steps),]

## ----------------------------------
## 6: Code to describe and show a strategy for imputing missing data
## ----------------------------------
sum(is.na(AMD$steps))

## ----------------------------------
## 7: Histogram of the total number of steps taken each day after missing values are imputed
## ----------------------------------
AMD2<- AMD
nas<- is.na(AMD2$steps)
avg_interval<- tapply(AMD2$steps, AMD2$interval, mean, na.rm=TRUE, simplify = TRUE)
AMD2$steps[nas] <- avg_interval[as.character(AMD2$interval[nas])]
names(AMD2)

## Check to see if there are any missing values
sum(is.na(AMD2))

AMD_Total_Steps <- AMD2%>%
                   group_by(date)%>%
                   summarise(total_steps = sum(steps, na.rm=TRUE))
AMD_Total_Steps

ggplot(AMD_Total_Steps, aes(x = total_steps)) +
  geom_histogram(fill = "blue", binwidth = 1000) +
  labs(title = "Daily Steps including Missing values", x = "Interval", y = "No. of Steps")

## ----------------------------------
## 8: Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
## ----------------------------------

## Check if needed
## head(AMD2)

## 8.1: Create extra object to differentiate week day type
AMD_WeekType<- AMD2%>%
               mutate(weektype= ifelse(weekdays(AMD2$date)=="Saturday" | weekdays(AMD2$date)=="Sunday", "Weekend", "Weekday"))

head(AMD_WeekType)

WeekTypeInterval <- AMD_WeekType%>%
                    group_by(interval, weektype)%>%
                    summarise(avg_steps2 = mean(steps, na.rm=TRUE))
##head(WeekTypeInterval)

ggplot(WeekTypeInterval, aes(x =interval , y=avg_steps2, color=weektype)) +
  geom_line() +
  labs(title = "Avg. Daily Steps by Weektype", x = "Interval", y = "No. of Steps") +
  facet_wrap(~weektype, ncol = 1, nrow=2)



