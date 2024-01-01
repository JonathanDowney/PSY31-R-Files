### Lecture 24: Tidyverse (piping and ggplot2)

## Piping |>

# Too many parentheses (in my opinion)
xtable(summary(lm(Petal.Length ~ Petal.Width, head(iris, 100))))

# Too many new objects (in my opinion)
new_data <- head(iris, 100)
model <- lm(Petal.Length ~ Petal.Width, new_data)
model.summary <- summary(model)
xtable(model.summary)

# Better...
lm(Petal.Length ~ Petal.Width, new_data) |> summary() |> xtable()

# Even better yet!
lm(Petal.Length ~ Petal.Width, new_data) |>
  summary() |>
  xtable()

# Dealing with arguments
iris |> head(3)

# The magrittr pipe from Tidyverse (old school)
library(magrittr)
iris %>% head()

# Subsetting in dplyr vs base R 
library(dplyr)
filter(iris, Species == "setosa")

# vs. 

iris[iris$Species == "setosa",]

## ggplot2 examples

library(ggplot2)

# A scatter plot with colors by car class

# Blank canvas
ggplot(data = mpg)

# Add points
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))

# Add points with colors by car class
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = class)) 

# Add plot title
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = class)) + 
  ggtitle("Engine Displacement vs. Highway MPG") 

# Centered plot title
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = class)) + 
  ggtitle("Engine Displacement vs. Highway MPG") +
  theme(plot.title = element_text(hjust = 0.5))

# Same as this in base R:
plot(mpg$displ, mpg$hwy,
     col = rainbow(nlevels(factor(mpg$class)))[as.numeric(factor(mpg$class))],
     pch = 16)

## Example ggplot2 figures with midterm data

# Load data
df <- read.csv("~/path/to/my/data.csv")

# Code missing values as NA
df$gpa[df$gpa == 9] <- NA
df$score[df$score == 999] <- NA

# Multi-panel scatter plot
ggplot(data = df[df$test == "lsat",]) + 
  geom_point(mapping = aes(x = score, y = gpa, color = major)) +
  facet_wrap(~ major, nrow = 3) +
  labs(x = "LSAT score", y = "GPA") 

# Jittered scatter plot with regression line
ggplot(data = df[df$age < 22,]) + 
  geom_jitter(mapping = aes(x = age, y = gpa, color = factor(caffeine, levels = c("no", "yes"))), width = .3) +
  geom_smooth(mapping = aes(x = age, y = gpa), method = "lm", color = "black") +
  labs(x = "Age", y = "GPA") +
  scale_colour_manual(labels = c("No", "Yes", "N/A"),values = c("beige","brown4","lightblue4")) +
  theme(axis.text=element_text(size=12)) + 
  labs(color='Caffeine') + 
  ggtitle("GPA by Age") +
  theme(plot.title = element_text(hjust = 0.5))

# 2-dimensional frequency plot
ggplot(data = df[complete.cases(df),]) + 
  geom_count(mapping = aes(x = caffeine, y = exercise, size = after_stat(n), color = after_stat(n))) + 
  guides(color = 'legend') + 
  scale_size_continuous(range = c(2,20))

## Q&A examples

mpg.automatic <- mtcars$mpg[mtcars$am==0]
mpg.manual <- mtcars$mpg[mtcars$am==1]

t.test(mpg.automatic,mpg.manual)
sd(mpg.automatic)
