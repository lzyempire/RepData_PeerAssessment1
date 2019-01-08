#Read the activity.csv after this file was put into the working direction
Activity <- read.csv("activity.csv")
Activity_omit <- na.omit(Activity)

#Calculate the total number of steps taken per day
act_sum <- aggregate(Activity$steps, by = list(Activity$date), sum)
names(act_sum) <- c("Date", "Steps_sum")
#Plot the histogram of the total number of steps taken per day
library(ggplot2)
g <- ggplot(act_sum, aes(Steps_sum))
g + geom_histogram(na.rm = TRUE) + 
        labs(title = "Total number of steps for each days", x = "Steps")

#Calculate and print the mean and median of the total number of steps taken per day
act_mean <- mean(act_sum$Steps_sum, na.rm = TRUE)
act_median <- median(act_sum$Steps_sum, na.rm = TRUE)
print(act_mean)
print(act_median)
#Plot the mean and median of the total number of steps taken per day
g <- ggplot(act_sum, aes(Steps_sum))
g + geom_histogram(na.rm = TRUE) + 
        geom_vline(xintercept = act_mean, col = "red", linetype = 2, lwd = 2) +
        geom_vline(xintercept = act_median, col = "blue", linetype = 4, lwd = 2) +
        labs(title = "Total number of steps for each days", x = "Steps")

#Calculate the mean steps taken per 5 minutes in each day
interval_mean <- aggregate(Activity_omit$steps, by = list(Activity_omit$interval), mean)
names(interval_mean) <- c("Interval", "Mean_Steps")
interval_mean$Interval <- as.numeric(as.character(interval_mean$Interval))
#Plot the mean steps taken per 5 minutes in each day
g <- ggplot(interval_mean, aes(x = Interval, y = Mean_Steps))
g + geom_point() + 
        geom_smooth(method = "loess", span = 0.1) +
        labs(title = "Mean steps for one day", x = "Interval", y = "Mean Steps")

#Find the interval that max steps in one day
interval_mmax <- interval_mean$Interval[interval_mean$Mean_Steps == max(interval_mean$Mean_Steps)]
print(interval_mmax)
g + geom_point() + 
        geom_vline(xintercept = interval_mmax, col = "red", linetype = 2, lwd = 1) +
        geom_smooth(method = "loess", span = 0.1) +
        labs(title = "Mean steps for one day", x = "Interval", y = "Mean Steps")