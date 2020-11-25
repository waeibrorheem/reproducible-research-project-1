library("kernlab")
data(spam)
str(spam[, 1:5])

set.seed(3435)
trainIndicator = rbinom(4601, size= 1, prob = 0.5)
table(trainIndicator)
names(trainSpam)
head(trainSpam)


trainSpam = spam[trainIndicator==1, ]
testSpam = spam[trainIndicator==0, ]

head(names(trainSpam), 20)

head(trainSpam[, 1:10])

table(trainSpam$type)

boxplot(capitalAve ~ type, data = trainSpam)

boxplot(log10(capitalAve+1)~type, data = trainSpam)
pairs(log10(trainSpam[, 1:4]+1))

hCluster = hclust(dist(t(trainSpam[, 1:57])))
plot(hCluster)

hClusterUpdated=hclust(dist(t(log10(trainSpam[,1:55]+1))))
plot(hClusterUpdated)

trainSpam$numType = as.numeric(trainSpam$type) - 1
costFunction = function(x,y) sum(x != (y > 0.5))
cvError = rep(NA, 55)
library(boot)

for(i in 1:55) {
  lmFormula = reformulate(names(trainSpam)[i], respon, se="numType")
  glmFit=glm(lmFormula, family="binomial", data=trainSpam)
  cvError[i]=cv.glm(trainSpam, glmFit, costFunction,2)$delta[2]
}

names(trainSpam)[which.min(cvError)]

library("knitr")


activity <- read.csv("F:/Data raw/Reproducible research")
activity <- read.csv("F:/Data raw/Reproducible research/activity.csv")
activity$date <- as.POSIXct(activity$date, "%Y-%m-%d")
weekday <- weekdays(activity$date)
activity <- cbind(activity,weekday)
summary(activity)


# What is the total number of steps taken per day?
activity_total_steps <- with(activity, aggregate(steps, by = list(date), FUN = sum, na.rm = TRUE))
names(activity_total_steps) <- c("date", "steps")
hist(activity_total_steps$steps, main = "Total number of steps taken per day", xlab = "Total steps taken per day", col = "darkblue", ylim = c(0,20), breaks = seq(0,25000, by=2500))

mean(activity_total_steps$steps)

median(activity_total_steps$steps)

#2. What is the average daily activity pattern?

average_daily_activity <- aggregate(activity$steps, by=list(activity$interval), FUN=mean, na.rm=TRUE)
names(average_daily_activity) <- c("interval", "mean")
plot(average_daily_activity$interval, average_daily_activity$mean, type = "l", col="darkblue", lwd = 2, xlab="Interval", ylab="Average number of steps", main="Average number of steps per intervals")

average_daily_activity[which.max(average_daily_activity$mean), ]$interval

#3. Imputing missing values
sum(is.na(activity$steps))
imputed_steps <- average_daily_activity$mean[match(activity$interval, average_daily_activity$interval)]
activity_imputed <- transform(activity, steps = ifelse(is.na(activity$steps), yes = imputed_steps, no = activity$steps))
total_steps_imputed <- aggregate(steps ~ date, activity_imputed, sum)
names(total_steps_imputed) <- c("date", "daily_steps")
hist(total_steps_imputed$daily_steps, col = "darkblue", xlab = "Total steps per day", ylim = c(0,30), main = "Total number of steps taken each day", breaks = seq(0,25000,by=2500))
mean(total_steps_imputed$daily_steps)
median(total_steps_imputed$daily_steps)

#4. Are there differences in activity patterns between weekdays and weekends?
activity$date <- as.Date(strptime(activity$date, format="%Y-%m-%d"))
activity$datetype <- sapply(activity$date, function(x) {
  if (weekdays(x) == "Sábado" | weekdays(x) =="Domingo") 
  {y <- "Weekend"} else 
  {y <- "Weekday"}
  y
})

activity_by_date <- aggregate(steps~interval + datetype, activity, mean, na.rm = TRUE)

plot<- ggplot(activity_by_date, aes(x = interval , y = steps, color = datetype)) +
  geom_line() +
  labs(title = "Average daily steps by type of date", x = "Interval", y = "Average number of steps") +
  facet_wrap(~datetype, ncol = 1, nrow=2)

print(plot)

