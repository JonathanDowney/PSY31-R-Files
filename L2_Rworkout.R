# Lecture 2 "R workout"

# Read in the data from the Desktop (the "~" is the home directory on Mac)
mydata <- read.csv("~/Desktop/dataL2.csv")

# 1. Use the max() function to find out which year
# at Tufts is most represented in this room.
max(mydata$tufts_year)

# 2.Make a histogram of how many siblings we have.
# The title of the histogram should be, “Our Siblings.”
hist(mydata$siblings,
      main = "How many siblings do we have?",
      xlab = "Number of siblings"
     )

#3. Find the mean and median number of states
# that students in PSY31 have travelled to. 
mean(mydata$states_travelled)
median(mydata$states_travelled)


