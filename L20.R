### Lecture 20

library(xtable)
library(stargazer)
library(MASS)

mod1 <- lm(Petal.Length ~ Sepal.Length + Species, data = iris)
summary(mod1)

data <- survey[survey$Age < 20,]

plot(data$Age, data$Height, xlim = c(17,20), xlab = "Age", ylab = "Height")

mod2 <- lm(Height ~ Age, data = data)

abline(mod2, col = "darkgreen", lwd =2)

summary(mod2)

## Model comparison

mod3 <- lm(Petal.Length ~ Sepal.Length, data = iris)
summary(mod3)

mod4 <- lm(Petal.Length ~ Sepal.Length + Sepal.Width, data = iris)
summary(mod4)

xtable(mod3)
xtable(mod4)

## Regression comparison table using the "stargazer" package

stargazer(mod3, mod4,
          title="Regression Results",
          dep.var.labels = "Petal Length")

## Violation of the "linearity" assumption

x <- -20:20
y <- x^2 + 2
plot(x,y)
points(mean(x),mean(y),col = "red", cex = 1.5, pch = 16)

mod5 <- lm(y~x)
summary(mod5)
abline(mod5, col = "red", lwd =2)

## A multiple regression example using the "survey" data set from the MASS package

plot(data$Sex,data$Height)
plot(data$Sex,data$Age)

plot(data$Age,data$Height)
mod6a <- lm(data$Height ~ data$Age)
abline(mod6a, col = "blue")

plot(data$Age[data$Sex=="Male"], data$Height[data$Sex=="Male"], col = "red")
points(data$Age[data$Sex=="Female"], data$Height[data$Sex=="Female"], col = "darkgreen")
mod6b <- lm(data$Height[data$Sex=="Male"] ~ data$Age[data$Sex=="Male"])
abline(mod6b, col = "red")

mod6c <- lm(data$Height[data$Sex=="Female"] ~ data$Age[data$Sex=="Female"])
abline(mod6c, col = "darkgreen")

mod6d <- lm(Height ~ Sex + Age, data = data)
summary(mod6d)

# Could it be that as someone older, that gender effects the height more?
# (or that when considering women, age effects height more than when considering men?)

plot.default(data$Sex[data$Age>=17 & data$Age<=18],
             data$Height[data$Age>=17 & data$Age<=18],
             col = "red1",
             xlab = "Gender",
             ylab = "Height (cm)",
             xaxt = "n")
axis(1, at = seq_along(levels(data$Sex)), labels = levels(data$Sex))

points(data$Sex[data$Age>18 & data$Age<=19], data$Height[data$Age>18 & data$Age<=19], col = "red3")
points(data$Sex[data$Age>19 & data$Age<=20], data$Height[data$Age>19 & data$Age<=20], col = "red4")

mod7a <- lm(data$Height[data$Age>=17 & data$Age<=18] ~ data$Sex[data$Age>=17 & data$Age<=18])
abline(a = coef(mod7a)[1]-coef(mod7a)[2], b = coef(mod7a)[2], col = "red1")

mod7b <- lm(data$Height[data$Age>18 & data$Age<=19] ~ data$Sex[data$Age>18 & data$Age<=19])
abline(a = coef(mod7b)[1]-coef(mod7b)[2], b = coef(mod7a)[2], col = "red3")

mod7c <- lm(data$Height[data$Age>19 & data$Age<=20] ~ data$Sex[data$Age>19 & data$Age<=20])
abline(a = coef(mod7c)[1]-coef(mod7c)[2], b = coef(mod7a)[2], col = "red4")

legend("topleft", legend=c("17-18","18-19","19-20"), fill = c("red1","red3","red4"), title="Age")

plot.default(data$Age[data$Sex=="Male"],
             data$Height[data$Sex=="Male"],
             col = "red1",
             xlab = "Age",
             ylab = "Height (cm)")
points(data$Age[data$Sex=="Female"],data$Height[data$Sex=="Female"], col = "red4")

mod7d <- lm(Height[data$Sex=="Male"] ~ Age[data$Sex=="Male"], data = data)
abline(mod7d, col = "red1")

mod7e <- lm(Height[data$Sex=="Female"] ~ Age[data$Sex=="Female"], data = data)
abline(mod7e, col = "red4")

legend("topleft", legend=c("Male","Female"), fill = c("red1","red4"), title="Age")

mod <- lm(Height ~ Sex * Age, data = data)
summary(mod)
