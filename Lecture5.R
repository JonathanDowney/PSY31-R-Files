# Lecture 5 R workout

# Exercise 1
hist(iris$Petal.Length,
     main = "Petal lengths",
     xlab = "Petal length",
     col = rainbow(30))

# Exercise 2
mydata <- read.csv("~/Desktop/ages.csv")
barplot(table(mydata), main = "Our ages", col = heat.colors(5), ylim = c(0,80))