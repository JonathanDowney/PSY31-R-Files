### Lecture 10

## Comparing the Normal and T distributions

df <- 100 # How many degrees of freedom?
x <- seq(-10, 10, length=1000) # x coordinates
y_norm <- dnorm(x, mean=0, sd=1) # y coordinates for normal curve
y_t <- dt(x, df = df)  # y coordinates for t curve

# Plot normal Probability Density Function
plot(x, y_norm,
     type="l", lwd=1, xlim = c(-4,4),
     main = paste0("T-distribution with ",df," degrees of freedom"),
     xlab = "SD from mean",
     ylab = "Probability")

# Plot T Probability Density Function
lines(x, y_t,
      type="l", lwd=1, xlim = c(-4,4),
      col = "red")

# Add legend
legend("topright",
       c("Normal", "T"),
       fill = c("black","red"))


## One-sample independent t-test example:

summary(iris$Petal.Length[iris$Species=="virginica"])

virginica_petal <- iris$Petal.Length[iris$Species=="virginica"] 

# One-sided test. Null hypothesis:
# population mean is equal to our less than 5.3
t.test(virginica_petal, mu = 5.3, alternative = "greater")

# Or just do it yourself! Get t statistic:
tstat <- (mean(virginica_petal) - 5.3) / (sd(virginica_petal)/sqrt(50))
# and see if it's greater than that of 95% of sample means:
qt(.95, 49) # degrees of freedom = 49 because n = 50
# 3.22 > 1.67 ...so reject the null! 


## Two-sample independent t-test example:

boxplot(iris$Petal.Length ~ iris$Species,
        main = "Species vs. Petal Length",
        xlab = "Species",
        ylab = "Petal Length",
        col = c("lavender","lightblue","violet"))

# Let's just look at two:
boxplot(iris$Petal.Length[iris$Species=="versicolor"],
        iris$Petal.Length[iris$Species=="virginica"],
        main = "Species vs. Petal Length",
        xlab = "Species",
        names = c("Versicolor","Virginica"),
        ylab = "Petal Length",
        col = c("lightblue","violet"))

versi_petal <- iris$Petal.Length[iris$Species=="versicolor"] 

t.test(versi_petal, virginica_petal, alternative = "two.sided")


## Two-sample dependent t-test example:

# Let's expand the iris dataset with some made up data:
iris$time <- 1 # add a time period of 1 for existing data

# new petal lengths (a bit longer)
set.seed(13) # Ensure that the same random numbers are generated each time
new_petal <- iris$Petal.Length[iris$Species=="virginica"] + rnorm(50,0.2,0.07)

# add new "virginica" data with a time period of 2
new_iris <- data.frame(Species = "virginica",
                       Petal.Length = new_petal,
                       Petal.Width = iris$Petal.Width[iris$Species=="virginica"],
                       Sepal.Length = iris$Sepal.Length[iris$Species=="virginica"],
                       Sepal.Width = iris$Sepal.Width[iris$Species=="virginica"],
                       time = 2)

# smash them together (add the new rows at the bottom)
new_iris <- rbind(iris, new_iris)

petal.virg.1 <- new_iris$Petal.Length[new_iris$Species=="virginica" & new_iris$time == 1]
petal.virg.2 <- new_iris$Petal.Length[new_iris$Species=="virginica" & new_iris$time == 2]

# Significant (barely) at .05 critical level with independent (unpaired) test:
t.test(petal.virg.2,
       petal.virg.1,
       alternative = "greater") 

# Highly significant at .05 critical level with dependent (paired) test:
t.test(petal.virg.2,
       petal.virg.1,
       alternative = "greater",
       paired = TRUE)

## Student data example:

mydata <- read.csv("~/Path/To/My/Data.csv")

scared <- mydata$section[1:4] # TH early sections who got the "scary" question order

# Recode so that higher values mean more nervious
nervous_fac <- factor(mydata$nerv4,
                    levels = sort(
                    unique(mydata$nerv4),
                    decreasing = T))

# Recode so that higher values mean more confident
confident_fac <- factor(mydata$conf4,
                        levels = sort(
                        unique(mydata$conf4),
                        decreasing = F))

# A dubious (!!) conversion of ordinal response data to interval values
mydata$nervous <- as.numeric(nervous_fac)
mydata$confident<- as.numeric(confident_fac)

# "Nervous" scores in the "scared" group
scared_nervous <- mydata$nervous[which(mydata$section %in% scary)]

# "Nervous" scores in the "not scared" group
not_scared_nervous <- mydata$nervous[which(!mydata$section %in% scary)]

# "Confident" scores in the "scared" group
scared_confident <- mydata$confident[which(mydata$section %in% scary)]
# "Confident" scores in the "not scared" group
not_scared_confident <- mydata$confident[which(!mydata$section %in% scary)]

# Let's test the null hypothesis that the mean of "nervous" scores
# is greater in the "scared" group than in the "not scared" group:

# The difference in means between "not scared" and "scared" is actually > 0
t.test(not_scared_nervous,
       scared_nervous,
       alternative = "less")

# The difference in means between "not scared" and "scared" is actually < 0
t.test(not_scared_confident,
       scared_confident,
       alternative = "greater")

# Just to think about: Is there a problem with running these two tests back-to-back if we are
# answering the same research question?
