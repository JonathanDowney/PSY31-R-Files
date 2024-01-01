### Lecture 15: Q & A with Dr. Downey

## Describe why it's seemingly so easy for data to be significantly different, yet have almost no impact in the real world. i.e. anxiety scores in HW 7, is this bc of the faulty scale of anxiety scores?

a <- c(5.500,5.510,5.502,5.503,5.498,5.497,5.495,5.490,5.505)
b <- c(5.600,5.610,5.602,5.603,5.598,5.597,5.595,5.590,5.605)
t.test(a,b)

# Effect size (unstandardized):
mean(b)-mean(a)

## How to combine multiple columns of data where every value per row except one is NA

# Let's create some fake data
data <- data.frame(a=c(1,2,NA,NA,NA,NA),
                   b=c(NA,NA,3,4,NA,NA),
                   c=c(NA,NA,NA,NA,5,6))

# Combining all columns into 1 with no NAs
data$new[!is.na(data$a)] = data$a[!is.na(data$a)]
data$new[!is.na(data$b)] = data$b[!is.na(data$b)]
data$new[!is.na(data$c)] = data$c[!is.na(data$c)]
data$new

# Or here's a trick! 
data <- data[,-4] # remove the 4th column that we just made
rowSums(data, na.rm = T)

## You mentioned using "~" during a boxplot argument... how do I do this? I haven't been doing it.

# using Lecture 14 data:
boxplot(income ~ country, data = df1)

# Same thing as (except no labels)

boxplot(income[country=="austria"],
        income[country=="germany"],
        income[country=="netherlands"],
        data = df1,
        names = c("austria", "germany", "netherlands"),
        xlab = "country",
        ylab = "income")

## What is the cut-off value for my F statistic if 3 groups and 90 observations?

groups <- 3
n = 90
degreesfreedom1 <- groups - 1
degreesfreedom2 <- n - groups
qf(.95, degreesfreedom1, degreesfreedom2)

## Can you please review the main components of p-hacking?

pval = NULL
for(i in 1:50){
  fake_data1 <- rnorm(60,0,1)
  fake_data2 <- rnorm(60,0,1)
  out <- t.test(fake_data1,fake_data2)
  pval[i] <- out$p.value
}
sort(pval)

## Can I get a significant ANOVA and no significant pairwise t-test? Yes!

fakedata <- data.frame(group = factor(c(rep(1,4),rep(2,8),rep(3,8),rep(4,4))),
                       score = c(10.71871, 10.42931, 9.46897, 9.87644,
                       10.64672, 9.71863, 10.04724, 10.32505, 10.22259, 10.18082, 10.76919, 10.65447,
                       10.90556, 10.94722, 10.78947, 10.96914,10.37724, 10.81035, 10.79333, 9.94447, 
                       10.81105, 10.58746, 10.96241, 10.59571))

summary(aov(fakedata$score ~ fakedata$group))
pairwise.t.test(fakedata$score, fakedata$group)


