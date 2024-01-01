## Lecture 22

## Missing values

apples <- c(2,NA,6,8,9)
bananas <- c(2,3,4,5,NA)

scores <- c(sample(1:10,18,replace = T),NA,NA)
group <- sample(c("a","b"),20,replace = T)

# Some functions will return NA
mean(apples)
sd(bananas)

# ...unless you tell them to take NAs out.
mean(apples, na.rm = T)
sd(bananas, na.rm = T)

# Other functions remove NAs automatically:
t.test(apples, bananas)
aov(scores ~ group)

midterm <- read.csv("~/Path/To/My/Data.csv")

# Which values for GPA have "missing values"? 
which(midterm$gpa == 9)

# Let's look at one:
midterm[87,]

# Replace 9 with NA
midterm$gpa[87] <- NA

# Which values for caffeine have NAs? 
which(is.na(midterm$caffeine))

# Let's look at one:
midterm[74,]

## Recoding multiple values

# Fake data
data <- data.frame(var1 = c(3,4,5,6,9,9,9),
                   var2 = c(5,4,9,9,9,2,1))

# Change 9s to NAs

data$var1[data$var1 == 9] <- NA
data

# Change NAs to 9s

data$var1[is.na(data$var1)] <- 9
data

## "Memory" example

set.seed(150)
treatA <- rnorm(100,10,5)
memory <- .1*(treatA + (rnorm(100,0,7)))

mod1 <- lm(memory~treatA)
summary(mod1)

plot(treatA, memory)
abline(mod1, col = "blue")
cor(treatA, memory)

# coincidentally high coefficients, low r2
set.seed(150)
treatB <- rnorm(100,10,5)
memory <- rnorm(100,10,5)

mod2 <- lm(memory~treatB)
summary(mod2)

plot(treatB, memory)
abline(mod2, col = "red")
cor(treatB, memory)

mod3 <- lm(memory ~ treatA + treatB)
summary(mod3)

stargazer::stargazer(mod1, mod2, dep.var.labels = c("Treatment A", "Treatment B"))

## Interaction effects practice: Theme Park

# Let's create some fake data:
set.seed(146)
n <- 175 
ride_speed <- rnorm(n,20,5)
cotton_candy <- sample(c(0,1), n, replace=T)
smiles <- .2*ride_speed + 5*cotton_candy - .2*(ride_speed*cotton_candy) + rnorm(n,10,4)
themepark <- data.frame(smiles = smiles, ride_speed = ride_speed, cotton_candy = cotton_candy)

# Hypothesis: negative interaction between ride_speed and cotton_candy

# Q1: Why might Dr. Downey hypothesize an interaction effect?

# Speed-induced tummy aches? 

# Q2: Is there evidence of an interaction effect?

mod_tp <- lm(smiles ~ ride_speed * cotton_candy, data = themepark)
summary(mod_tp)
