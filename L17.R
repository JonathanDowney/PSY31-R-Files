## Lecture 17: Correlation

#In order to make the random values in this example reproducible
set.seed(12345)

# Generate independent variable
x <- rnorm(n=10, mean=12, sd=1.5)
plot(x,rep(0,length(x)), ylim = c(-5,70), ylab = "y")
points(mean(x),0,col = "red", cex = 1.5, pch = 16)
plot(x,x)
points(mean(x),mean(x),col = "red", cex = 1.5, pch = 16)

# Generate the dependent variable
y <- 3*x + 6 + rnorm(n=10, mean = 0, sd = 10)

# Let's plot the x and y variables together:
plot(x,y)
points(mean(x),mean(y),col = "red", cex = 1.5, pch = 16)
sum((x-mean(x))*(y-mean(y)))/(length(x)-1)
cov(x,y)

# Let's check out some other data:
x <- rnorm(n=50, mean=3, sd=1.5)
y <- 3*x + 6 + rnorm(n=50, mean = 0, sd = 10)
plot(x,y)
points(mean(x),mean(y),col = "red", cex = 1.5, pch = 16)
cov(x,y)
cor(x,y)

x <- rnorm(n=50, mean=4, sd=1.5)
y <- -2*x + 7 + rnorm(n=50, mean = 0, sd = 10)
plot(x,y)
points(mean(x),mean(y),col = "red", cex = 1.5, pch = 16)
cov(x,y)
cor(x,y)

x <- rnorm(n=50, mean=4, sd=1.5)
y <- 4*x + 3 + rnorm(n=50, mean = 0, sd = 8)
plot(x,y)
points(mean(x),mean(y),col = "red", cex = 1.5, pch = 16)
cov(x,y)
cor(x,y)

x <- rnorm(n=50, mean=4, sd=4)
y <- -5*x + 2 + rnorm(n=50, mean = 3, sd = 18)
plot(x,y)
points(mean(x),mean(y),col = "red", cex = 1.5, pch = 16)
cov(x,y)
cor(x,y)

x <- rnorm(n=50, mean=6, sd=17)
y <- 3*x + 4 + rnorm(n=50, mean = 12, sd = 34)
plot(x,y)
points(mean(x),mean(y),col = "red", cex = 1.5, pch = 16)
cov(x,y)
cor(x,y)

x <- rnorm(n=50, mean=6, sd=15)
y <- -5*x + 4 + rnorm(n=50, mean = 12, sd = 24)
plot(x,y)
points(mean(x),mean(y),col = "red", cex = 1.5, pch = 16)
cov(x,y)
cor(x,y)

x <- rnorm(n=50, mean=6, sd=15)
y <- 5*x + 4 + rnorm(n=50, mean = 12, sd = 24)
plot(x,y)
points(mean(x),mean(y),col = "red", cex = 1.5, pch = 16)
cov(x,y)
cor(x,y)

x <- rnorm(n=50, mean=6, sd=15)
y <- -7*x + 4 + rnorm(n=50, mean = 12, sd = 24)
plot(x,y)
points(mean(x),mean(y),col = "red", cex = 1.5, pch = 16)
cov(x,y)
cor(x,y)

x <- rnorm(n=50, mean=4, sd=60)
y <- 9*x + 2 + rnorm(n=50, mean = 8, sd = 18)
plot(x,y)
points(mean(x),mean(y),col = "red", cex = 1.5, pch = 16)
cov(x,y)
cor(x,y)

x <- rnorm(n=50, mean=4, sd=90)
y <- -9*x + 2 + rnorm(n=50, mean = 8, sd = 18)
plot(x,y)
points(mean(x),mean(y),col = "red", cex = 1.5, pch = 16)
cov(x,y)
cor(x,y)

# But what about this? Covariate is sensative to scale!
x <- rnorm(n=50, mean=4000, sd=2000)
y <- .5*x + 2 + rnorm(n=50, mean = 8000, sd = 1800)
plot(x,y)
points(mean(x),mean(y),col = "red", cex = 1.5, pch = 16)
cov(x,y)
cor(x,y)

x <- rnorm(n=50, mean=.04, sd=.02)
y <- 9*x + 2 + rnorm(n=50, mean = .08, sd = .018)
plot(x,y)
points(mean(x),mean(y),col = "red", cex = 1.5, pch = 16)
cov(x,y)
cor(x,y)

# Correlations of zero (or at least almost zero):
x <- rnorm(n=50, mean= 4, sd= 4)
y <- 0*x + 2 + rnorm(n=50, mean = 8, sd = 4)
plot(x,y)
points(mean(x),mean(y),col = "red", cex = 1.5, pch = 16)
cov(x,y)
cor(x,y)

x <- rnorm(n=50, mean=8, sd=4)
y <- rep(3,length(x))
plot(x,y)
points(mean(x),mean(y),col = "red", cex = 1.5, pch = 16)
cov(x,y)
cor(x,y)

x <- -20:20
y <- x^2 + 2
plot(x,y)
points(mean(x),mean(y),col = "red", cex = 1.5, pch = 16)
cov(x,y)
cor(x,y)

# Hypothesis testing examples
set.seed(46)
x <- rnorm(n=50, mean=13, sd=4)
y <- 1*x + 6 + rnorm(n=50, mean = 0, sd = 20)
plot(x,y)
cor.test(x,y)

x <- rnorm(n=50, mean=6, sd=15)
y <- 3*x + 4 + rnorm(n=50, mean = 12, sd = 24)
plot(x,y)
cor.test(x,y)

# Correlation is not causation!
set.seed(4)
happiness <- 55 + rnorm(n=50, mean=0, sd=15)
income <- 4*happiness + 200 + rnorm(n=50, mean = 0, sd = 74)
plot(happiness,income,
     xlab = "happiness score",
     ylab = "income/week",
     col = rainbow(200)[happiness-20],
     pch = 16)
cor.test(happiness,income)

## Chi-square test of best fit: class example
# Are we linguistically representative?

data <- read.csv("~/path/to/mydata.csv")

our_langs <- data$langs
table(our_langs)

# Controversial perhaps, but sometimes we need to group language into larger families.
our_langs <- gsub("Mandarin", "Chinese", our_langs)
our_langs <- gsub("Cantonese", "Chinese", our_langs)

worldlangs <- c("English","Chinese","Hindi","Spanish","French","Arabic","Bengali","Portuguese","Russian","Urdu","Indonesian","German","Japanese","Nigerian Pidgin","Maraharti","Telugu","Turkish","Tamil","Vietnamese","Tagalog","Korean","Farsi","Italian","Gujarati","Thai","Amharic","Oromo","Greek","Haitian","Hebrew","Tibetan")
worldspeakers <- c(1456,1138,609,559,310,274,273,264,255,232,199,133,123,121,99,96,90,87,86,83,82,79,68,62,61,57,37,13,12,9,6)

data.frame(langs = worldlangs, speakers = worldspeakers)

# Remove English because we all speak it 
worldlangs <- worldlangs[-1]
worldspeakers <- worldspeakers[-1]

# How many would we predict that we speak?
expected <- round((worldspeakers / sum(worldspeakers) * 142),0)

# How many do we speak?
observed <- NULL
for(i in 1:length(worldlangs)){
  observed[i] <- length(grep(worldlangs[i],our_langs, ignore.case = T))
}

df <- data.frame(lang = worldlangs, expected = expected, observed = observed)
df

# This is problematic because many of our cell counts are so small.
# But it's just an example! If we wanted to, we might group languages by families.

chisq.test(df$expected, df$observed)
