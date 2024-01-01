### Week 10 Q & A Video

library(MASS) # for the survey dataset

#	Can we review the meaning of significance relative
# to the meaning of effect size?

a <- rnorm(100, 10.00, .001)
b <- rnorm(100, 10.01, .001)
t.test(a,b)

a <- rnorm(1000000, 10.00, 1)
b <- rnorm(1000000, 10.01, 1)
t.test(a,b)

# How do you compare data on different scales?
# Like in the midterm project, what's the best way
# to approach comparing test scores from 3 different tests?

test1 <- rnorm(100, 100, 10)
test2 <- rnorm(100, 300, 40)
summary(test1)
summary(test2)

Z1 <- (test1 - mean(test1))/sd(test1)
Z2 <- (test2 - mean(test2))/sd(test2)
summary(Z1)
summary(Z2)
summary(c(Z1,Z2))

## Can we get a step-by-step for adjusting p-values in r
## in different types of datasets?

my_pvals <- c(.04,.08,.45)
p.adjust(my_pvals)


## How do we center a graph in r?
## Is it similar to the out.width thing we do to zoom in/out?
```{r fig.align = 'center'}
  
````

## I'm also wondering how we can create pie charts in R. Thank you! 
slices <- c(10, 12,4, 16, 8)
  lbls <- c("US", "UK", "Australia", "Germany", "France")
  pie(slices, labels = lbls, main="Pie Chart of Countries")
  
## Chi-square questions

# How to make graphs with only qualitative data.  
  
# Using the "survey" dataset from the "MASS" package
  
table(survey$Exer, survey$Smoke)  
  
str(survey$Exer) # examine the structure of this variable
levels(survey$Exer) # Or more specifically, what are the different groups?

# Chi-square "goodness of fit"  
exp_vals <- c(.25,.25,.50)  
table(survey$Exer)
chisq.test(table(survey$Exer), p = exp_vals)

# Chi-square "test of independence"  
chisq.test(table(survey$Smoke,survey$Exer)) # not high enough cell count

# Recode to get a higher cell count
smoke_binary <- ifelse(survey$Smoke == "Heavy" |
                       survey$Smoke == "Regul"|
                       survey$Smoke == "Occas",
                       "yes", "no")

chisq.test(smoke_binary, survey$Exer) # warning, but the counts are pretty good
table(smoke_binary, survey$Exer)

# Let's say I already had expeced values for smoking:
# 11.5% of Americans are smokers in other surveys
chisq.test(table(smoke_binary), p = c(.885, .115))

# Not surprising that we reject
prop.table(table(smoke_binary))

#Is there a way to do something like paired t tests for chi-square data?
?mcnemar.test

# Can we discuss what covariance is in the context of what it describes,
# not just what the formula is? thanks

x <- c(1,3,5,7)
y <- c(1,1.5,2,2.5)
plot(x,y,ylim = c(0,7), col = "blue")
abline(lm(y~x), col = "blue")
cor(x,y)
cov(x,y)
# vs.
y <- c(1,3,5,7)
points(x,y, col = "darkgreen")
abline(lm(y~x), col = "darkgreen")
cor(x,y)
cov(x,y)

# Example of linear regression with hypothesis testing
# I suspect that as cars get heavier, miles per gallons decreases
# My H0: the regression coefficient for weight (the slope of the line)
# is less than 0 in the population. I.e. Beta1 < 0

mod <- lm(mpg ~ wt, data = mtcars)
summary(mod)

# I can get a p-value for a one-sided test too by dividing my p-value by 2

# The R-squared is very high, but logically it makes sense, and the plot supports it 
plot(mpg ~ wt, data = mtcars)

# What is the difference between r^2 and R^2?
cor(mtcars$mpg, mtcars$wt)^2

# In Foster 13.2, Yˆ, or “Y hat” is described as being the predicted value of “Y”
# for a person in the formula for the line of the best fit.
# How come the variable is Y hat and not pure “Y?”
# If this represents Y, why not use Y directly? 

newdata <- data.frame(wt = 4.35)
pred <- predict(mod, newdata)

# Let's plot it!
points(newdata,pred, col = "red", pch = 16, cex = 1.5)
