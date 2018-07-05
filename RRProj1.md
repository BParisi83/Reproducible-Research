RRProj
================
B Parisi
July 2, 2018

``` r
knitr::opts_chunk$set(include = TRUE)
```

R Markdown
----------

### Load tidyverse, set WD, and Read File

``` r
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------------------------------------------------- tidyverse 1.2.1 --

    ## v ggplot2 2.2.1     v purrr   0.2.5
    ## v tibble  1.4.2     v dplyr   0.7.5
    ## v tidyr   0.8.1     v stringr 1.3.1
    ## v readr   1.1.1     v forcats 0.3.0

    ## -- Conflicts ------------------------------------------------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(lattice)
setwd("C:/Users/BParisi/Documents/R/Practice Projects/activity data/")

activitydata <- read.csv("C:/Users/BParisi/Documents/R/Practice Projects/activity data/activity.csv", header = TRUE)
```

Calculates the average Steps taken per day
------------------------------------------

### Creates the plot of steps per day

### Average and Median of Steps per Day

``` r
StepsPerDay <- tapply(activitydata$steps, activitydata$date, sum)
hist(StepsPerDay, xlab = "Number of Steps", main = "Average Steps per Day")
```

![](RRProj1_files/figure-markdown_github/unnamed-chunk-2-1.png)

``` r
AvgPerDay <- mean(StepsPerDay, na.rm = TRUE)
MedPerDay <- median(StepsPerDay, na.rm = TRUE)

print(AvgPerDay)
```

    ## [1] 10766.19

``` r
print(MedPerDay)
```

    ## [1] 10765

Finds the intervals and plots the line graph
--------------------------------------------

### Calculates the mean given the inteval to be used during the imputing

``` r
StepsPerInterval <- tapply(activitydata$steps, activitydata$interval, mean, na.rm = TRUE)
plot(as.numeric(names(StepsPerInterval)), 
     StepsPerInterval, xlab = "Interval", ylab = "Steps", 
     main = "Average Daily Activity Pattern", type = "l")
```

![](RRProj1_files/figure-markdown_github/unnamed-chunk-3-1.png)

``` r
StepsPerInterval <- tapply(activitydata$steps, activitydata$interval, mean, na.rm = TRUE)
```

Splits out the rows with NAs and fills he NA with the Mean Calculated above
---------------------------------------------------------------------------

### Binds the rows split off back together

### Sums the steps per day and prints the new histogram

### Finds the mean and median for the imputed data

### Replaces the NAs with the means of the appropriate intervals

``` r
activitydata.split <- split(activitydata, activitydata$interval)
for(i in 1:288){
        activitydata.split[[i]]$steps[is.na(activitydata.split[[i]]$steps)] <- StepsPerInterval[i]
}

activitydata.imputed <- do.call("rbind", activitydata.split)

StepsPerDay.imputed <- tapply(activitydata.imputed$steps, activitydata.imputed$date, sum)
hist(StepsPerDay.imputed, xlab = "Number of Steps", main = "Imputed Steps per Day")
```

![](RRProj1_files/figure-markdown_github/unnamed-chunk-4-1.png)

Finds the mean and median for the imputed data
----------------------------------------------

``` r
MeanPerDay.imputed <- mean(StepsPerDay.imputed, na.rm = TRUE)
MedianPerDay.imputed <- median(StepsPerDay.imputed, na.rm = TRUE)

activitydata.imputed$day <- ifelse(weekdays(as.Date(activitydata.imputed$date)) == "Saturday" | 
                        weekdays(as.Date(activitydata.imputed$date)) == "Sunday", "weekend", "weekday")
```

Calculate average steps per interval for weekends
-------------------------------------------------

### Calculate average steps per interval for weekdays

``` r
StepsPerInterval.weekend <- tapply(activitydata.imputed[activitydata.imputed$day == "weekend" ,]$steps, 
                                   activitydata.imputed[activitydata.imputed$day == "weekend" ,]$interval, 
                                   mean, na.rm = TRUE)



StepsPerInterval.weekday <- tapply(activitydata.imputed[activitydata.imputed$day == "weekday" ,]$steps, 
                                   activitydata.imputed[activitydata.imputed$day == "weekday" ,]$interval, 
                                   mean, na.rm = TRUE)
```

Create the Weekday Plot
-----------------------

``` r
par(c(1,2))
```

    ## NULL

``` r
plot(as.numeric(names(StepsPerInterval.weekday)),  
     StepsPerInterval.weekday, ylab = " ", xlab = " ",
     type = "l", col ="Red", ylim=c(1,200))


par(new=TRUE)
plot(as.numeric(names(StepsPerInterval.weekend)), 
     StepsPerInterval.weekend, ylab = "Freq", xlab = "Steps",
     type = "l", col ="Blue", ylim= c(1,200))

legend("topleft", legend=c("Weekday", "Weekend"),
       col=c("red", "blue"),lty=1, cex=0.8,
        text.font=4)
```

![](RRProj1_files/figure-markdown_github/unnamed-chunk-7-1.png)
