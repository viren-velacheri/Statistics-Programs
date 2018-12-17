mediansalaries <- read.csv(file.choose(), header = TRUE)
attach(mediansalaries)
hist(salary, main = 'Distribution of Salaries', col = 'dark green')
abline(v = mean(salary), col = 'dark blue', lwd = 3)
abline(v = median(salary), col = 'light blue', lwd = 3)
reps <- 9999
salariesmedians <- numeric(reps)
for(i in 1:reps)
{
  tempsalaries <- sample(salary,length(hours), replace = TRUE)
  salariesmedians[i] <- median(tempsalaries)
}
hist(salariesmedians, main = 'Boostrap Sampling Distribution of Median Salaries \n with Confidence Interval Endpoints',col = 'red')
abline(v = mean(salariesmedians), col = 'dark blue', lwd = 3)
low <- mean(salariesmedians) - qt(0.975, length(hours) - 1)*sd(salariesmedians) 
high <- mean(salariesmedians) + qt(0.975, length(hours) - 1)*sd(salariesmedians)
abline(v = low, lwd = 2, lty = 2, col = 'blue')
abline(v = high, lwd = 2, lty = 2, col = 'blue')
lowpct <- quantile(salariesmedians, 0.025)
highpct <- quantile(salariesmedians, 0.975)
hist(salariesmedians, main = 'Boostrap Sampling Distribution of Median Salaries \n with Percentile Confidence Interval Endpoints',col = 'red')
abline(v = mean(salariesmedians), col = 'dark blue', lwd = 3)
abline(v = lowpct, lwd = 2, lty = 2, col = 'pink')
abline(v = highpct, lwd = 2, lty = 2, col = 'pink')
twins <- read.csv(file.choose())
attach(twins)
hist(Difference, main = 'Sampling Distribution of Differences between Hippocampus \n Volume of Twins with and without Schizophrenia', col = 'blue')
reps <- 9999
meandifference <- numeric(reps)
tempdifferences <- numeric(length(Difference))
for (i in 1:reps)
{
  for(j in 1:length(Difference))
  {
    tempdifferences[j] <- Difference[j] * sample(c(-1,1),1)
  }
  meandifference[i] <- mean(tempdifferences)
}
hist(meandifference, main = 'Boostrap Sampling Distribution of Differences between \n Hippocampus Volume of Twins with and without Schizophrenia', col = 'blue')
abline(v = mean(Difference), lwd = 2, lty = 2, col = 'red')
abline(v= -mean(Difference), lwd = 2, lty =2, col = 'red' )
pvalue <- (sum(abs(meandifference) >= abs(mean(Difference))))/reps
pvalue
