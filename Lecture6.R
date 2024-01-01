### Lecture 6: Sampling and Distributions

## R workout

# Read in data
SAT <- read.csv("~/Desktop/SAT_scores.csv")

# Let's look at all rows, but only the test score columns
SAT[,c("SAT1","SAT2","SAT3")]

# Take the mean of the test score columns for each row
SAT$mean_score <- rowMeans(SAT[,c("SAT1","SAT2","SAT3")])

# Order by mean score
ordered <- SAT[order(SAT$mean_score),]

# Order decreasing
ordered <- SAT[order(SAT$mean_score, decreasing = TRUE),]
top10 <- ordered[1:10,]
top10

## Multiple conditional subsetting examples

SAT$mean_score[SAT$firstname == "Rosalie"]
SAT$mean_score[SAT$firstname == "Rosalie" | SAT$firstname == "Ophelia"]
SAT[SAT$firstname == "Rosalie" | SAT$firstname == "Ophelia",]
SAT[SAT$firstname == "Rosalie" & SAT$SAT1 > 1000,]

## Creating normally distributed data "from scratch":
hist(rnorm(7,5,2),
     main = "Frequency of Hiccups per Day",
     xlab = "Hiccups",
     col = rainbow(7))
