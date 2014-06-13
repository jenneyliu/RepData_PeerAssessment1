

rm(list=ls())

setwd("/Users/julia/Documents/data_mining/Data Science Track Cousera/github/github_datasciencecoursera/RepData_PeerAssessment1/myDir")

getwd()



oridata <- read.csv("activity.csv", header=TRUE, stringsAsFactors=FALSE)

oridata$date <- strptime(oridata$date, "%Y-%m-%d")
oridata$date <- as.Date(oridata$date)
dim(oridata)
head(oridata)


complete_data <- oridata[complete.cases(oridata), ]

library(sqldf)
aggdata <- sqldf("select date, ( sum(steps) )total_steps from complete_data group by date")

sum( complete_data [which(complete_data$date == as.Date("2012-10-07") ), ]$steps )  == aggdata $total_steps[aggdata $date==as.Date("2012-10-07") ]

hist(aggdata$total_steps, main = paste("Total number of steps taken each day"), xlab="Total number of steps taken each day", ylab="Frequency", col="red", breaks=8, xlim=c(0,26000), ylim=c(0,20))

mean(aggdata$total_steps)

median(aggdata$total_steps)

interval_arr <- unique(oridata$interval)

mean_steps <- numeric(length(interval_arr))
for ( ind in 1:length(interval_arr) )
{
  interval <- interval_arr[ind]
  index <- which(oridata$interval == interval)
  mean_steps[ind] <- mean(oridata$steps[index], na.rm=TRUE)
}
mean_df <- data.frame(interval=interval_arr, mean_steps=mean_steps)


mean_aggdata <- sqldf("select interval, ( avg(steps) )mean_steps from complete_data group by interval")

plot(mean_aggdata$interval, mean_aggdata$mean_steps, type="l", main = paste("Average number of steps taken across all days"), ylab="Average number of steps", xlab="The 5-minute interval")

ind <- which(mean_aggdata$mean_steps == max(mean_aggdata$mean_steps))


maxStep_inverval <- mean_aggdata$interval[ind]
maxStep_inverval

plot(mean_aggdata$interval, mean_aggdata$mean_steps, type="l", main = paste("Average number of steps taken across all days"), ylab="Average number of steps", xlab="The 5-minute interval")
abline(v=maxStep_inverval,col=3,lty=1)


complete_data <- oridata[complete.cases(oridata), ]
nrow(oridata) - nrow(complete_data)


mean_aggdata <- sqldf("select interval, ( avg(steps) )mean_steps from complete_data group by interval")
newdata <- oridata
na_ind <- which(is.na(newdata$steps))
for ( i in 1:length(na_ind) )
{
  ind <- na_ind[i]
  interval <- newdata$interval[ind]
  newdata$steps[ind] <- mean_aggdata$mean_steps[ which(mean_aggdata$interval == interval) ]     
}
# no NA data any more
complete_data2 <- newdata[complete.cases(newdata), ]
nrow(oridata) - nrow(complete_data2)
# the number of unique values to fill in the missing data
NA_fillingValue_num <- unique(newdata$steps[])





aggdata_noNA <- sqldf("select date, ( sum(steps) )total_steps from newdata group by date")

# verify the total number of steps is correct for one specific date
sum( newdata [which(newdata$date == as.Date("2012-10-07") ), ]$steps )  == aggdata_noNA$total_steps[aggdata_noNA$date==as.Date("2012-10-07") ]

hist(aggdata_noNA$total_steps, main = paste("Total number of steps taken each
day"), xlab="Total number of steps taken each day", ylab="Frequency",
col="red",breaks=8, xlim=c(0,26000), ylim=c(0,26))


mean(aggdata_noNA$total_steps)

median(aggdata_noNA$total_steps)











break_num <- 20
color1 <- rgb(0,0,1,1/4)
color2 <- rgb(1,0,0,1/4)
color3 <- rgb(1,0,1,1/4)
p1 <- hist(aggdata$total_steps, breaks=break_num)
p2 <- hist(aggdata_noNA$total_steps, breaks=break_num)

title <- sprintf("The impact of imputing missing data  (histogram breaks=%d)", break_num)
plot(x=1:26000, main = title, xlab="Total number of steps taken each day", ylab="Frequency",
xlim=c(0,26000), ylim=c(0,26), type="n")
plot(p1, col=color1, add=T)
plot(p2, col=color2,  add=T)
# abline(v=median_rmNA, col="green", lty=1)
# abline(v=median_fillInNA, col="red", lty=1)
legend("topleft", legend = c("With NA removed", "With NA filled in", "Overlapping"),fill=c(color1, color2, color3))



mean_rmNA <- mean(aggdata$total_steps)
mean_fillInNA <- mean(aggdata_noNA$total_steps)
diff_mean <- mean_rmNA - mean_fillInNA
diff_mean


median_rmNA <- median(aggdata$total_steps)
median_fillInNA <- median(aggdata_noNA$total_steps)
diff_median <- median_rmNA - median_fillInNA
diff_median




diffdate <- as.Date(  setdiff(as.character(aggdata_noNA$date), as.character(aggdata$date)) )
diffdate_df <- data.frame(date=diffdate, total_steps=numeric(length(diffdate)) )
for (i in 1:length(diffdate) )
{
    date <- diffdate_df[i, ]$date
    diffdate_df[i, ]$total_steps <- aggdata_noNA[which(aggdata_noNA$date==date), ]$total_steps
}
dim(diffdate_df)
diffdate_df
unique(diffdate_df$total_steps)




newdata$dayinweek <- weekdays(aggdata_noNA$date)

newdata$Type <- ifelse( (newdata$dayinweek=="Saturday"|newdata$dayinweek=="Sunday"), "weekend", "weekday")
newdata$Type <- as.factor( newdata$Type )




test_data <- sqldf("select interval, ( avg(steps) )avg_steps, Type from newdata group by interval, Type ")
dim(test_data)
head(test_data)
range(test_data$avg_steps)



# verify the result is correct for one specific date
mean( newdata [which(newdata$Type == "weekday" & newdata$interval == 0),]$steps) -  test_data$avg_steps[which(test_data$interval==0 & test_data$Type =="weekday")]
mean( newdata [which(newdata$Type == "weekend" & newdata$interval == 0),]$steps)  -  test_data$avg_steps[which(test_data$interval==0 & test_data$Type =="weekend")]



library(ggplot2)

# ggplot(test_data, aes(interval)) + geom_line(aes(y = avg_steps), colour=colors()[131]) + facet_wrap( ~ Type, ncol=1) + xlab("interval") + ylab("Number of steps")  + theme_bw() + theme(strip.background = element_rect(fill=colors()[486], colour='black', size=0.4)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


ggplot(test_data, aes(interval)) + geom_line(aes(y = avg_steps), colour=colors()[131]) + facet_wrap( ~ Type, ncol=1) + xlab("interval") + ylab("Number of steps") + theme(strip.background = element_rect(fill=colors()[486], colour='black')) + theme(panel.background = element_rect(fill='white', colour='black')) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())






