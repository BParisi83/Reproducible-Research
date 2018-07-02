# Reproducible-Research
JHU Data Science Specialization
##Load tidyverse, set WD, and Read File
library(tidyverse)
library(lattice)

setwd("C:/Users/BParisi/Documents/R/Practice Projects/activity data/")

activitydata <- read.csv("C:/Users/BParisi/Documents/R/Practice Projects/activity data/activity.csv", header = TRUE)

#Calculates the average Steps taken per day
StepsPerDay <- tapply(activitydata$steps, activitydata$date, sum)

#Creates the plot of steps per day
hist(StepsPerDay, xlab = "Number of Steps", main = "Average Steps per Day")

#Average and Median of Steps per Day
AvgPerDay <- mean(StepsPerDay, na.rm = TRUE)
MedPerDay <- median(StepsPerDay, na.rm = TRUE)

#Finds the intervals and plots the line graph 
StepsPerInterval <- tapply(activitydata$steps, activitydata$interval, mean, na.rm = TRUE)
plot(as.numeric(names(StepsPerInterval)), 
     StepsPerInterval, xlab = "Interval", ylab = "Steps", 
     main = "Average Daily Activity Pattern", type = "l")

# Calculates the mean given the inteval to be used during the imputing
StepsPerInterval <- tapply(activitydata$steps, activitydata$interval, mean, na.rm = TRUE)

# Splits out the rows with NAs and fills he NA with the Mean Calculated above
activitydata.split <- split(activitydata, activitydata$interval)
for(i in 1:288){
        activitydata.split[[i]]$steps[is.na(activitydata.split[[i]]$steps)] <- StepsPerInterval[i]
}

#Binds the rows split off back together
activitydata.imputed <- do.call("rbind", activitydata.split)

#Sums the steps per day and prints the new histogram
StepsPerDay.imputed <- tapply(activitydata.imputed$steps, activitydata.imputed$date, sum)
Imputed <- hist(StepsPerDay.imputed, xlab = "Number of Steps", main = "Imputed Steps per Day")


#Finds the mean and median for the imputed data
MeanPerDay.imputed <- mean(StepsPerDay.imputed, na.rm = TRUE)
MedianPerDay.imputed <- median(StepsPerDay.imputed, na.rm = TRUE)

activitydata.imputed$day <- ifelse(weekdays(as.Date(activitydata.imputed$date)) == "Saturday" | 
                        weekdays(as.Date(activitydata.imputed$date)) == "Sunday", "weekend", "weekday")

# Calculate average steps per interval for weekends
StepsPerInterval.weekend <- tapply(activitydata.imputed[activitydata.imputed$day == "weekend" ,]$steps, 
                                   activitydata.imputed[activitydata.imputed$day == "weekend" ,]$interval, 
                                   mean, na.rm = TRUE)

# Calculate average steps per interval for weekdays
StepsPerInterval.weekday <- tapply(activitydata.imputed[activitydata.imputed$day == "weekday" ,]$steps, 
                                   activitydata.imputed[activitydata.imputed$day == "weekday" ,]$interval, 
                                   mean, na.rm = TRUE)


# Create the Weekday Plot
plot(as.numeric(names(StepsPerInterval.weekday)),  
     StepsPerInterval.weekday, ylab = " ", xlab = " ",
     type = "l", col ="Red", ylim=c(1,200))
par(new=TRUE)
#  Create Weekend Plot
plot(as.numeric(names(StepsPerInterval.weekend)), 
     StepsPerInterval.weekend, ylab = "Freq", xlab = "Steps",
     type = "l", col ="Blue", ylim= c(1,200))

legend("topleft", legend=c("Weekday", "Weekend"),
       col=c("red", "blue"),lty=1, cex=0.8,
        text.font=4)


