fdphone <- read.csv(file.choose())
attach(fdphone)
par(family = 'serif')
hist(hours, main = 'Hours on Phone per day', col = 'light blue')
abline(v = mean(hours), col = 'dark blue', lwd = 3)

t.test(hours, conf.level = 0.95) 
reps <- 9999
hoursmeans <- numeric(reps)
for(i in 1:reps)
{
  temphours <- sample(hours, length(hours), replace = TRUE)
  hoursmeans[i] <- mean(temphours)
}
hist(hoursmeans, col = 'pink', main = 'Bootstrap Sampling Distribution of Mean Hours', xlim = c(2.6,4))
abline(v = mean(hoursmeans), col = 'red', lwd = 3)
low <- mean(hoursmeans) - qt(0.975, length(hours)-1)*sd(hoursmeans)
high <- mean(hoursmeans) + qt(0.975, length(hours)-1) *sd(hoursmeans)
print(c(low,high))
hist(hoursmeans, xlim = c(2.6,4), main = 'Bootstrap t Confidence Interval', col = 'light yellow')
abline(v = mean(hoursmeans), lwd = 2, lty = 2, col = 'red')
abline(v = low, lwd = 2, lty = 2, col = 'blue')
abline(v = high, lwd = 2, lty = 2, col = 'blue')
lowpct <- quantile(hoursmeans, 0.025)
highpct <- quantile(hoursmeans, 0.975)
print(c(lowpct, highpct))
hist(hoursmeans, xlim = c(2.6,4), main = 'Bootstrap Percentile Confidence interval', col = "light yellow")
abline(v = mean(hoursmeans), lwd = 2, col = 'red')
abline(v = lowpct, lwd = 2, lty = 2, col = 'dark green')
abline(v = highpct, lwd = 2, lty = 2, col = 'dark green')
for(i in 1:reps)
{
  temphours <- sample(hours)
  diffhours[i] <- mean(temphours[1:60]) - mean(temphours[61:111])
}
pvalue <- (sum(diffhours >= diffmeans) + 1) / (reps + 1)
pvalue
boxplot(hours ~ gender, col = topo.colors(3))
hist(diffhours, main = 'Difference in hours on phone for males and females')
abline(v = diffmeans, lwd = 2, lty =2)
diffmeans <- mean(hours[gender == 'Male']) - mean(hours[gender == 'Female'])
t.test(hours[gender == 'Male'] - hours[gender == 'Female'], conf.level = 0.95)
flyingfrogs <- read.csv(file.choose(), header = TRUE)
attach(flyingfrogs)
reps <- 9999
meandiff <- numeric(reps)
tempfrogs <- numeric(length(frogdiff))
for (i in 1:reps)
{
  for(j in 1:length(frogdiff))
  {
    tempfrogs[j] <- frogdiff[j] * sample(c(-1,1),1)
  }
  meandiff[i] <- mean(tempfrogs)
}

hist(meandiff)
abline(v = mean(frogdiff), lwd = 2, lty = 2)
abline(v = -mean(frogdiff), lwd = 2, lty = 2)
pvaluefrogs <- (sum(abs(meandiff) >= abs(mean(frogdiff))))/reps
pvaluefrogs
t.test(dominant - nondominant, conf.level = 0.95)
