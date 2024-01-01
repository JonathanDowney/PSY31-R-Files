# From the "R workout" in lecture 3
# Read in data file that I saved on my Desktop (Mac)
data <- read.csv("~/Desktop/dataL3.csv")

# Use the plot() function for a scatterplot
plot(data$apples,                  # Variable for the x axis
     data$bananas,                 # Variable for the y axis
     main = "Apples and Bananas",  # Title
     xlab = "Apples",              # The x axis label 
     ylab = "Bananas",             # The y axis label 
     col = "red",                  # Red points
     pch = 17)                     # Triangle points (type ?pch in the console for more info)