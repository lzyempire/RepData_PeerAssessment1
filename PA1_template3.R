#Read the activity.csv after this file was put into the working direction
Activity <- read.csv("activity.csv")
Activity_omit <- na.omit(Activity)

#Examine the rows with missing values or not, calculate total number of each
Missing_row <- !complete.cases(Activity)
Missing_row_sum <- sum(Missing_row)
True_row <- complete.cases(Activity)
Ture_row_sum <- sum(True_row)

#Calculate the mean steps taken per 5 minutes in each day
interval_mean <- aggregate(Activity_omit$steps, by = list(Activity_omit$interval), mean)
names(interval_mean) <- c("Interval", "Mean_Steps")
interval_mean$Interval <- as.numeric(as.character(interval_mean$Interval))

#Replace the missing value with the mean value of that interval
Missing_interval <- Activity$interval[Missing_row]
Activity_True <- Activity
for(i in 1:length(Missing_interval)){
        Activity_True$steps[Missing_row][i] <- interval_mean$Mean_Steps[interval_mean$Interval == Missing_interval[i]]
}

#Find out whether the days were weekdays or not
Activity_Weekdays <- cbind(Activity_True, "Weekdays" = weekdays(as.Date(Activity_True$date)))
weekday <- c("星期一", "星期二", "星期三", "星期四", "星期五")
weekend <- c("星期六", "星期日")
Activity_weekday <- subset(Activity_Weekdays, Weekdays %in% weekday)
Activity_weekend <- subset(Activity_Weekdays, Weekdays %in% weekend)

#Calculate the mean steps of weekdays and weekends respectively
interval_weekday <- aggregate(Activity_weekday$steps, by = list(Activity_weekday$interval), mean)
names(interval_weekday) <- c("Interval", "Weekday_Steps")
interval_weekday$Interval <- as.numeric(as.character(interval_weekday$Interval))

interval_weekend <- aggregate(Activity_weekend$steps, by = list(Activity_weekend$interval), mean)
names(interval_weekend) <- c("Interval", "Weekend_Steps")
interval_weekend$Interval <- as.numeric(as.character(interval_weekend$Interval))

#Plot the mean steps of weekdays and weekends respectively
library(ggplot2)
g_weekdays <- ggplot(interval_weekday, aes(x = Interval, y = Weekday_Steps)) + 
        geom_point() + 
        geom_smooth(method = "loess", span = 0.1) +
        labs(title = "Mean steps for Weekdays", x = "Interval", y = "Mean Steps")
g_weekends <- ggplot(interval_weekend, aes(x = Interval, y = Weekend_Steps)) + 
        geom_point() + 
        geom_smooth(method = "loess", span = 0.1) +
        labs(title = "Mean steps for Weekends", x = "Interval", y = "Mean Steps")
install.packages("gridExtra")
library(gridExtra)
grid.arrange(g_weekdays, g_weekends, ncol = 2)