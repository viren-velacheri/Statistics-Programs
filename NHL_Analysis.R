shoot <- read.csv(file.choose())
attach(shoot)

plot(Goals ~ Shots)
nhllm <- lm(Goals ~ Shots)
summary(nhllm)

## Graph prediction and confidence intervals

# Make a vector of new x values to be plugged in to the model
#### ***I'm still playing with line 11 to get this to work***
newx <- seq(min(Shots), max(Shots), by = 1)

# Make a data frame to be used in the predict() function
newxdf <- data.frame(Shots = newx)

# df <- predict(model name, df of values, interval = "")
pconf <- predict(nhllm, newxdf, 
                 interval = "confidence")
pconf
ppred <- predict(nhllm, newxdf, interval = "prediction")

# Make a pretty scatterplot with intervals
plot(Goals ~ Shots, frame = FALSE, xlab = "Number of Shots",
     ylab = "Number of Goals", pch = 21, 
     col = "black", bg = "lightblue", cex = 1)
abline(nhllm, lwd = 2)
lines(newx, pconf[,2], lty = 3, col = "blue", lwd = 2) 
lines(newx, pconf[,3], lty = 3, col = "blue", lwd = 2)
lines(newx, ppred[,2], lty = 5, col = "red", lwd = 2) 
lines(newx, ppred[,3], lty = 5, col = "red", lwd = 2)

# Do the intervals really get smaller in the middle?
widthconf <- pconf[,3] - pconf[,2]
widthpred <- ppred[,3] - ppred[,2]
pconf2 <- cbind(pconf, widthconf)
ppred2 <- cbind(ppred, widthpred)
pconf2
ppred2
