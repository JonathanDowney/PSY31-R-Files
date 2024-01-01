### Lecture 18: Linear Regression

## Plotting the line of best fit

set.seed(123) # to ensure that the numbers in this example are replicable

# Make an independent variable with some fake observations
hours <- 40 + rnorm(n=50, mean=6, sd=15)

# Make an dependent variable with some fake observations
grays <- 3*hours + 40 + rnorm(n=50, mean = 15, sd = 35)

# Scatterplot with the two variables
plot(hours,grays,xlab = "Hours worked / wk", ylab = "Gray hairs")

# Create a linear model object with lm()
mod <- lm(grays ~ hours)
summary(mod)

# Plot the line of best fit! 
abline(lm(grays~hours), col = "red", lwd = 2)

## We can predict where new dependent values would lie based on our model

# Make a new data frame with values for the numbers of hours
newdata <- data.frame(hours = c(45,50,55))

# Predict the number of gray hairs
predict(mod,newdata)

# Plot those points over our existing plot
points(newdata$hours,predict(mod,newdata),col = "darkgreen",pch=16, cex = 2)


## Visualizing the squared residual variance in red
# (Just if you are curious!)
set.seed(321)

# Fake data
x <- 40 + rnorm(n=50, mean=6, sd=15)
y <- 3*x + rnorm(n=50, mean = 0, sd = 35)

# Best fit
plot(x,y)
mod <- lm(y~x)
summary(mod)
abline(mod, col = "blue")

# For each residual, draw a line segment
# between the observation and the fitted model
# (Again, just if you are curious!)
for(i in 1:length(x)){
  lines(c(x[i],x[i]),c(y[i],mod$fitted.values[i]),
        col = "red",
        lwd = 2*+((y[i]-mod$fitted.values[i])/sd(y))^2)
}

# Quantify the residual variance
SSresid <- sum((y-mod$fitted.values)^2)/(length(y)-1)
# or
SSresid <- sum(mod$residuals^2)/(length(y)-1)
SSresid
