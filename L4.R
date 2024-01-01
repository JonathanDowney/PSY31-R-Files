### Lecture 4 R examples

# EX1: Pizza bar chart 
# Don't worry, you don't need to understand the code in this example.

pizzadata <- read.csv("~/Desktop/pizzadata.csv")

barplot(
  sort(
    table(
      tolower(pizzadata$topping)
    ),
    decreasing=T)[1:10],
  col=rainbow(10),
  main="Our Top 10 Toppings")

# EX2: Boxplots

boxplot(
  airquality$Temp[airquality$Month==5],
  airquality$Temp[airquality$Month==6],
  names = c("May", "June"),
  ylab = "Temp (F)",
  main = "How much warmer was June?")

boxplot(
  airquality$Temp ~ airquality$Month,
  xlab = "Month",
  ylab = "Temp (F)",
  names = c("May","June","July","Aug","Sept"),
  main = "Summer Temps")
