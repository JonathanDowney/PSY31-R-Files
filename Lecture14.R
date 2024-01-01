### Lecture 14

library(effectsize)

## Example 1: Two-way ANOVA (no interactions) example
# Controlling for a known source of noise. 

set.seed(12) # To make sure we're all using the same random variables.
n <- 50 # Our sample size

# Create independent variables
country <- sample(c("germany","austria","netherlands"),n, replace = T)
salary <- rnorm(n,10,5) # this one does not explicitly appear in the data set
techjob <- sample(c("yes","no"),n, prob = c(1,1), replace = T)

# Add some junk independent variables (i.e., they're just noise)
sport <- sample(c("football","basketball","cycling"),n, replace = T)
politics <- sample(c("left","center","right"),n, replace = T)
eyecolor <- sample(c("blue","green","brown"),n, replace = T)

# Create dependent variable: income
wealth_germany <- rnorm(n,52,6)
wealth_austria <- rnorm(n,56,6)
wealth_netherlands <- rnorm(n,59.5,6)
income <- ifelse(country == "germany",wealth_germany ,
                 ifelse(country == "austria", wealth_austria, wealth_netherlands)) +
          ifelse(techjob=="yes",1,0)*salary # Income is a function of country and techjob

# Organize variables into data frame
df1 <- data.frame(income = income, country = country, techjob = techjob)

# Here are different ANOVAs that can be conducted, depending on your hypothesis
summary(aov(income ~ country, data = df1))
summary(aov(income ~ sport, data = df1))
summary(aov(income ~ techjob, data = df1))
summary(aov(income ~ techjob + country, data = df1))
summary(aov(income ~ techjob + country + sport + politics + eyecolor, data = df1))
summary(aov(income ~ techjob * country, data = df1))

# Check out the group means
aggregate(income ~ country, data= df1, FUN = mean)
aggregate(income ~ country + techjob, data= df1, FUN = mean)

# Income by country boxplot
boxplot(income ~ country, data = df1)

## Example 2: Another two-way ANOVA example

set.seed(20)

# Independent variables
treat <- sample(c("a","b","c"), n, replace = T) # Random assignmnet into treatment groups
caffeine <- sample(c("no","yes"),n, replace = T)

# Add some junk independent variables (i.e. just noise)
city <- sample(c("New York","Boston","LA"),n, replace = T)
fav_food <- sample(c("sushi","burrito","caviar"),n, replace = T)
breakfast <- sample(c("heavy","light","none"),n, replace = T)

# Reaction times per group and a "caffeine bonus"
timeA <- rnorm(n,53.7,6)
timeB <- rnorm(n,55,6)
timeC <- rnorm(n,56.8,6)
caffeine_bonus <- rnorm(n,-10,5)

# Reaction time is a function of treatment group and caffeine
time <- ifelse(treat == "a",timeA,
                 ifelse(treat == "b",timeB, timeC)) +
                 ifelse(caffeine=="yes",caffeine_bonus,0)

df2 <- data.frame(time = time, treat = treat, caffeine = caffeine)

# Here are different ANOVAs that can be conducted, depending on your hypothesis
summary(aov(time ~ treat, data = df2))
summary(aov(time ~ city, data = df2))
summary(aov(time ~ caffeine, data = df2))
summary(aov(time ~ treat + caffeine, data = df2))
summary(aov(time ~ treat + caffeine + city + fav_food + breakfast, data = df2))
summary(aov(time ~ treat * caffeine, data = df2))

# Check out the group means
aggregate(time ~ treat, data=df2, FUN = mean)
aggregate(time ~ treat + caffeine, data=df2, FUN = mean)

# Reaction time by treatment group boxplot
boxplot(time ~ treat)

## Example 3: Two-way ANOVA example with interaction

set.seed(21)

# Independent variables 
city <- sample(c("Albany", "Boston","Chicago"),n, replace = T)
hole_in_the_wall <- sample(c("no","yes"),n, replace = T)

# Junk independent variables 
cuisine <- sample(c("italian","chinese","mexican"),n, replace = T)
mood <- sample(c("warm","chill","weird"),n, replace = T)
size <- sample(c("small","medium","large"),n, replace = T)

# Prices and "hole-in-the-wall" discount
priceA <- rnorm(n,54.2,6)
priceB <- rnorm(n,55,6)
priceC <- rnorm(n,56.4,6)
hole_in_the_wall_discount <- rnorm(n,-5,5)

# Price: Price is lower if hole in the wall in Chicago
price <- ifelse(city == "Albany",priceA,
               ifelse(city == "Boston",priceB, priceC)) +
         ifelse(hole_in_the_wall == "yes",-9,0) +
         ifelse(city == "Chicago" & hole_in_the_wall == "yes",-9,0)

df3 <- data.frame(price = price, city = city, hole_in_the_wall = hole_in_the_wall)

# Here are different ANOVAs that can be conducted, depending on your hypothesis
summary(aov(price ~ city, data = df3))
summary(aov(price ~ cuisine, data = df3))
summary(aov(price ~ hole_in_the_wall, data = df3))
summary(aov(price ~ city + hole_in_the_wall, data = df3))
summary(aov(price ~ city + hole_in_the_wall + cuisine + mood + size, data = df3))
summary(aov(price ~ city * hole_in_the_wall, data = df3))

# Check out the group means
aggregate(price ~ city, data= df3, FUN = mean)
aggregate(price ~ city + hole_in_the_wall, data= df3, FUN = mean)

# Describe and visualize 
aggregate(price ~ city + hole_in_the_wall, data=df3, FUN = mean)
boxplot(price ~ city, data=df3)
boxplot(price ~ hole_in_the_wall, data=df3, xlab = "\'Hole in the Wall\' * City")

# Multiple boxplot:
boxplot(price ~ hole_in_the_wall + city, data = df3,
        names = c("No.Alb","Yes.Alb","No.Bos","Yes.Bos","No.Chi","Yes.Chi"))

# Interaction plots from our examples (look for differences in slopes!):

interaction.plot(x.factor = df1$country, # x-axis variable
                 trace.factor = df1$techjob, # "trace" variable
                 response = df1$income, # y-axis variable
                 fun = mean, # metric to plot
                 ylab = "Income (in $1K)",
                 xlab = "Country",
                 col = c("lightblue", "violet"),
                 lty = 1, # line type: solid
                 lwd = 2, # line width: 2
                 trace.label = "Tech Job?",
                 type = "b", # both point and lines
                 pch = c(16,1)) # solid point and hollow point type

interaction.plot(x.factor = df2$treat, # x-axis variable
                 trace.factor = df2$caffeine, # "trace" variable
                 response = df2$time, # y-axis variable
                 fun = mean, # metric to plot
                 ylab = "Time (ms)",
                 xlab = "Treatment",
                 col = c("red", "green"),
                 lty = 1, # line type: solid
                 lwd = 2, # line width: 2
                 trace.label = "Caffeine",
                 type = "b", # both point and lines
                 pch = c(16,1)) # solid point and hollow point type

interaction.plot(x.factor = df3$city, # x-axis variable
                 trace.factor = df3$hole_in_the_wall, # "trace" variable
                 response = df3$price, # y-axis variable
                 fun = mean, # metric to plot
                 ylab = "Price ($)",
                 xlab = "City",
                 col = c("violet", "blue"),
                 lty = 1, # line type: solid
                 lwd = 2, # line width: 2
                 trace.label = "Hole in the wall",
                 type = "b", # both point and lines
                 pch = c(16,1)) # solid point and hollow point type

## Effect sizes (using the eta_squared() function from the effectsize package)

my_aov <- aov(income ~ techjob + country, data = df1)
eta_squared(my_aov, partial = F)

my_aov <- aov(time ~ treat + caffeine, data = df2)
eta_squared(my_aov, partial = F)

my_aov <- aov(price ~ city * hole_in_the_wall, data = df3)
eta_squared(my_aov, partial = F)



