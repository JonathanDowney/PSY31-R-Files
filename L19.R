### Lecture 19: Linear Regression (Part II1)

plot(iris$Sepal.Length,iris$Petal.Length,
     xlab = "Sepal Length", ylab = "Petal Length")
plot(iris$Sepal.Length, iris$Petal.Length,
     xlab = "Sepal Length", ylab = "Petal Length")
mod1 <- lm(Petal.Length ~ Sepal.Length, data = iris)
summary(mod1)
abline(mod1, col = "deeppink3", lwd =2)

## Two independent variables:
mod2 <- lm(Petal.Length ~ Sepal.Length + Sepal.Width, data = iris)
summary(mod2)

## Categorical variables
# Let's just look at two species in the iris data
no_setosa <- iris[iris$Species !="setosa",]
no_setosa$Species <- droplevels(no_setosa$Species) 

plot(no_setosa$Species, no_setosa$Petal.Length)
plot.default(jitter(no_setosa$Petal.Length) ~ no_setosa$Species,
             xlab =  "Species",
             ylab = "Petal Length",
             pch = 16,
             xaxt = "n",
             xlim = range(as.integer(unique(no_setosa$Species))) + c(-0.4, 0.4),
             ylim = range(as.integer(unique(no_setosa$Petal.Length)))+ c(-0.4, 0.4),
             col = colors <-c("blue3", "purple")[no_setosa$Species])
axis(1, at = seq_along(levels(no_setosa$Species)), labels = levels(no_setosa$Species))


mod3 <- lm(Petal.Length ~ Species, data = no_setosa)
summary(mod3)

# Draw intercept at the versicolor category:
abline(a = coef(mod3)[1]-coef(mod3)[2], # to plot the intercept at the mean of the versicolor category 
       b = coef(mod3)[2], # the slope
       col = "deeppink3",
       lwd =2)

# The same as a t-test!
virginica.pl <- iris$Petal.Length[iris$Species == "virginica"]
versicolor.pl <- iris$Petal.Length[iris$Species == "versicolor"]
t.test(virginica.pl, versicolor.pl)

# The same! (lm vs aov)
mod4a <- lm(Petal.Length ~ Species, data = iris)
summary(mod4a)

mod4b <- aov(Petal.Length ~ Species, data = iris)
summary(mod4b)

plot.default(jitter(iris$Petal.Length) ~ iris$Species,
             xlab =  "Species",
             ylab = "Petal Length",
             pch = 16,
             xaxt = "n",
             xlim = range(as.integer(unique(iris$Species))) + c(-0.4, 0.4),
             ylim = range(as.integer(unique(iris$Petal.Length)))+ c(-0.4, 1.4),
             col = colors <-c("blue3", "purple", "violet")[iris$Species])
axis(1, at = seq_along(levels(iris$Species)), labels = levels(iris$Species))

# With interaction effect
mod5 <- lm(Petal.Length ~ Sepal.Length * Sepal.Width, data = iris)
summary(mod5)



