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

#Calculate the total number of steps taken per day
act_sum_True <- aggregate(Activity_True$steps, by = list(Activity_True$date), sum)
names(act_sum_True) <- c("Date", "Steps_sum")
#Plot the histogram of the total number of steps taken per day
library(ggplot2)
g <- ggplot(act_sum_True, aes(Steps_sum))
g + geom_histogram(na.rm = TRUE) + 
        labs(title = "Total number of steps for each days", x = "Steps")

#Calculate and print the mean and median of the total number of steps taken per day
act_mean_True <- mean(act_sum_True$Steps_sum, na.rm = TRUE)
act_median_True <- median(act_sum_True$Steps_sum, na.rm = TRUE)
print(act_mean_True)
print(act_median_True)
