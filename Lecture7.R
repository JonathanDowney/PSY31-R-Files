# Lecture 7: Sampling and Distributions

## The Central Limit Theorem in practice

# Step 1: Generate 1000 random numbers
# Normally distributed data
 my_numbers <- rnorm(1000,100,10)

# Let's take a look at the first few:
 round(head(my_numbers),0)

# Uniformly distributed data
# my_numbers <- runif(1000,0,100)

# Exponentially distributed data
# my_numbers <- rexp(1000,1)

hist(my_numbers,
     main = "Frequency of Hiccups",
     xlab = "Number of Hiccups/Day",
     col = rainbow(10))

# Step 2: Population mean is...
mean(my_numbers)
abline(v=mean(my_numbers),col="red",lwd=3)

# Step 3: Let's sample 30 numbers from our population 150 times:
sample_size <- 30
n_of_samples <- 150
draws <- replicate(n = n_of_samples,
                   sample(my_numbers, sample_size)
                   )

# Step 4: Let's find the mean of each sample:
my_sampled_means <- apply(draws, 2, mean)
round((my_sampled_means),1)

# Step 5: Let's look at all of the means of the samples together:
hist(my_sampled_means,
     br = 30,
     xlab = "Sample Means",
     col = rainbow(30),
     main = paste0(
       "Frequency of Sample means: from ",
       n_of_samples,
       " samples of ",
       sample_size)
)

# abline(v=mean(my_sampled_means) ,col="red",lwd=3)

## The Law of Large Numbers in practice

# Generate data from any distribution. 
my_numbers <- rnorm(1000,0,2)

# Sample means and make histogram of those sample means
sample_size <- 30
draws <- replicate(n = n_of_samples, sample(my_numbers, sample_size))
my_sampled_means <- apply(draws, 2, mean)
hist(my_sampled_means,
     br = 30,
     col = rainbow(30),
     xlim = c(-2,2),
     xlab = "Sample Means",
     main = paste0("Frequency of Sample means: from ",
                   n_of_samples,
                   " samples of ",
                   sample_size)
)

#vs. 
sample_size <- 90
draws <- replicate(n = n_of_samples, sample(my_numbers, sample_size))
my_sampled_means <- apply(draws, 2, mean)
hist(my_sampled_means,
     br = 30,
     xlim = c(-5,5),
     xlab = "Sample Means",
     main = paste0("Frequency of Sample means: from ",n_of_samples," samples of ", sample_size)
)

#vs. 
sample_size <- 500
draws <- replicate(n = n_of_samples, sample(my_numbers, sample_size))
my_sampled_means <- apply(draws, 2, mean)
hist(my_sampled_means,
     br = 30,
     xlim = c(-5,5),
     xlab = "Sample Means",
     main = paste0("Frequency of Sample means: from ",n_of_samples," samples of ", sample_size)
)

## Data Cleaning: Listwise deletion of NAs

# Read-in data
SAT_withNA <- read.csv("~/Desktop/SAT_withNA.csv")

# Complete cases only
SAT_noNA <- SAT_withNA[complete.cases(SAT_withNA),]

# No first name NA
SAT_noFirstNameNA <- SAT_withNA[!is.na(SAT_withNA$firstname), ]
