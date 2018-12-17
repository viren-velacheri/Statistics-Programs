speed <- read.csv(file.choose())
attach(speed)

par(family = "serif", cex.main = 2, cex.lab = 2, pch=19,
    cex.axis = 2, mar = c(5, 5, 4, 2))

# Simple linear regression
par(mfrow=c(1,1))
plot(Year, FatalityRate, main = "Do Speed Limits Help?",
     xlab = "Year", ylab = "Fatality Rate", cex=2)
yearlm <- lm(FatalityRate ~ Year)
abline(yearlm, lwd = 3)
summary(yearlm)

# Multiple regression with indicator
yearstatelm <- lm(FatalityRate ~ Year + StateControl)
summary(yearstatelm)

plot(Year, FatalityRate, main = "Do Speed Limits Help?",
     xlab = "Year", ylab = "Fatality Rate", cex=2)
points(Year[StateControl==0], FatalityRate[StateControl==0], 
       col = "green", cex=2)
points(Year[StateControl==1], FatalityRate[StateControl==1], 
       col = "red", cex=2)
abline(a=85.2783, b=-0.04183, lwd=2, col = "green")
abline(a=85.233, b=-0.04183, lwd=2, col = "red")
legend("topright", pch = 19, 
       col = c("green", "red"), 
       legend = c("before 1995", "1995 or later"), cex=1.8)

# Multiple regression with indicator and interaction
interlm <- lm(FatalityRate ~ Year + StateControl + Year*StateControl)
summary(interlm)
plot(Year, FatalityRate, main = "Do Speed Limits Help?",
     xlab = "Year", ylab = "Fatality Rate", col = c("green", "red"),cex=2)
points(Year[StateControl==0], FatalityRate[StateControl==0], 
       col = "green", cex=2)
points(Year[StateControl==1], FatalityRate[StateControl==1], 
       col = "red", cex=2)
abline(a=216.2, b=-0.1076, lwd=2, col = "green")
abline(a=54.8, b=-0.02663, lwd=2, col = "red")
legend("topright", pch = 19, 
       col = c("green", "red"), 
       legend = c("before 1995", "1995 or later"))
plot(fitted(interlm), resid(interlm), cex=2)
