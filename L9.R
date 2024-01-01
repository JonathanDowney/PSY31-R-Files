# Have Tufts students visited a different amount of states
# than the average American?

# How many states has the average American visited?

pop_mean = 12.9
pop_sd = 2.5

# Let's take a sample of Tufts students:
data <- read.csv("~/Path/To/Your/Data.csv")
sample_mean <- mean(data$states_trav, na.rm = T) 

# How many students in our sample?
n <- nrow(data)

# NOT: n <- length(data)

# Get Z-score
z_score <- (sample_mean - pop_mean) / (pop_sd / sqrt(142))
z_score

normal_dist <- rnorm(1000000,0,1)
bins <- 100
xlim <- c(-5,5)
hist(rnorm(1000000,0,1), breaks = bins)

# Critical value
critical <- .05
critical_line <- pnorm(critical)

# Get p-value 
pvalue <- 1-pnorm(z_score)

# Plot p-value (two tail)

hist(normal_dist,
     breaks = 100,
     xlim = c(-5,5),
     prob=TRUE,
     main = "Two-tailed test",
     xlab = "States Travelled To",
     ylab = "Probability")
abline(v=1.96,col="red",lwd=4)
abline(v=-1.96,col="red",lwd=4)
abline(v=z_score,col="blue",lwd=4)
legend(2.2,0.3, legend = c(paste0("Z-score = ", round(z_score,2)),
                              paste0("Critical = ", round(critical,2)),
                              paste0("P-value = ",round(pvalue,2))),
       fill = c("blue","red",0))

# Plot p-value (right tail)
hist(normal_dist,
     breaks = 100,
     xlim = c(-5,5),
     prob=TRUE,
     main = "Right-tailed test",
     xlab = "States Travelled To",
     ylab = "Probability")
abline(v=1.645,col="red",lwd=4)
abline(v=z_score,col="blue",lwd=4)
legend(2.2,0.3, legend = c(paste0("Z-score = ", round(z_score,2)),
                              paste0("Critical = ", round(critical,2)),
                              paste0("P-value = ",round(pvalue,2))),
       fill = c("blue","red",0))

# Plot p-value (left tail)

hist(normal_dist,
     breaks = 100,
     xlim = c(-5,5),
     prob=TRUE,
     main = "Left-tailed test",
     xlab = "States Travelled To",
     ylab = "Probability")
abline(v=-1.645,col="red",lwd=4)
abline(v=z_score,col="blue",lwd=4)
legend(2.2,0.3, legend = c(paste0("Z-score = ", round(z_score,2)),
                           paste0("Critical = ", round(critical,2)),
                           paste0("P-value = ",round(pvalue,2))),
                           fill = c("blue","red",0))

## R Workout

# Question #1: What will "df" look like? 

height_inch = rnorm(100,64,2.2)
height_cm = 2.54*height_inch
measure = sample(c("metric","imperial"),100,replace = T)
df <- data.frame(height = 
                   ifelse(measure == "imperial", height_inch, height_cm)
)
# or

df <- data.frame(measure = measure,
                 height_cm = height_cm,
                 height_inch, height_inch)

df$height[measure == "imperial"] <- df$height_inch[measure == "imperial"] 
df$height[measure == "metric"] <- df$height_cm[measure == "metric"]
df <- df["height"]

# Question #2: What should you not like about "df"?

# Heights are given in different units.

# Question #3: How can you fix it?

df$height <- ifelse(measure == "imperial", df$height*2.54, df$height)

colnames(df) <- "height_cm"
womens_heights <- df

# or if you want to be more verbose:
# colnames(df)[colnames(df) == "height"]<- "height in cm"

