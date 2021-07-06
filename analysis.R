library(dplyr)
library(lubridate)
unzip("repdata_data_activity.zip")

##Loading and preprocessing the data
activity <- read.csv("activity.csv", sep=",")
activity$date <- as.Date.character(activity$date)

##What is mean total number of steps taken per day?
stepsums <- aggregate(steps ~ date, activity, sum, na.action = na.omit)
hist(stepsums$steps,breaks=10,main="Histogram of Steps by Day-Total"
     ,xlab="Day-Total Steps")
mean1 <- mean(stepsums$steps)
median1 <- median(stepsums$steps)

##What is the average daily activity pattern?
intsums <- aggregate(steps ~ interval, activity, sum, na.action = na.omit)
intsums$steps <- intsums$steps/length(unique(activity$date))
plot(intsums$interval,intsums$steps,type="l",xlab="Interval",ylab="Average Steps"
     ,main="Average Steps Over Intervals Across All Days")
maxind <- which.max(intsums$steps)
maxint <- intsums$interval[maxind]
abline(v=maxint,col="red",lw=2)

##Imputing missing values
missingind <- which(is.na.data.frame(activity))
nummissing <- length(missingind)
replacena <- function(df) {
    for(i in 1:nrow(df)) {
        if(i %in% missingind) {
            int <- df[i,]$interval
            avgsteps <- intsums[match(int,intsums$interval),]$steps
            df[i,]$steps <- avgsteps
      }
    }
  df
}
activity2 <- replacena(activity)
stepsums2 <- aggregate(steps ~ date, activity2, sum, na.action = na.omit)
hist(stepsums2$steps,breaks=10,main="Histogram of Steps by Day-Total (NAs Imputed)"
       ,xlab="Day-Total Steps")
mean2 <- mean(stepsums2$steps)
median2 <- median(stepsums2$steps)

##Are there differences in activity patterns between weekdays and weekends?
days <- wday(activity2$date)
days[days==1 | days==7] <- "weekend"
days[days %in% 2:6] <- "weekday"
activity2$days <- days
wday <- subset(activity2,days=="weekday")
wend <- subset(activity2,days=="weekend")
wdaysums <- aggregate(steps ~ interval, wday, sum)
wendsums <- aggregate(steps ~ interval, wend, sum)
wdaysums$steps <- wdaysums$steps/length(unique(wday$date))
wendsums$steps <- wendsums$steps/length(unique(wend$date))

par(mfrow=c(1,2))
plot(wdaysums$interval,wdaysums$steps,type="l",xlab="Interval",ylab="Average Steps"
     ,main="Average Steps Over Intervals Across Weekdays")
plot(wendsums$interval,wendsums$steps,type="l",ylim = range(wdaysums$steps),xlab="Interval",ylab="Average Steps"
     ,main="Average Steps Over Intervals Across Weekends")